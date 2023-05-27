#include "sphereTessellation.h"

Rcpp::List joinMeshes(Rcpp::List mesh1, Rcpp::List mesh2) {
  Rcpp::NumericMatrix Vertices1 = mesh1["vertices"];
  Rcpp::NumericMatrix Vertices2 = mesh2["vertices"];
  Rcpp::NumericMatrix Normals1 = mesh1["normals"];
  Rcpp::NumericMatrix Normals2 = mesh2["normals"];
  Rcpp::IntegerMatrix Faces1 = mesh1["faces"];
  Rcpp::IntegerMatrix Faces2 = mesh2["faces"];
  //
  const int nfaces1 = Faces1.ncol();
  return Rcpp::List::create(
    Rcpp::Named("vertices") = Rcpp::cbind(Vertices1, Vertices2),
    Rcpp::Named("faces")    = Rcpp::cbind(Faces1, Faces2 + nfaces1),
    Rcpp::Named("normals")  = Rcpp::cbind(Normals1, Normals2)
  );
}


// [[Rcpp::export]]
Rcpp::List voronoi_cpp(
    Rcpp::NumericMatrix pts, double radius, Rcpp::NumericVector O, int niter
) {
  const int npoints = pts.ncol();
  std::vector<SPoint3> points;
  points.reserve(npoints);
  for(int i = 0; i < npoints; i++) {
    Rcpp::NumericVector pt_i = pts(Rcpp::_, i);
    points.emplace_back(pt_i(0), pt_i(1), pt_i(2));
  }
  // ball
  Traits ball(SPoint3(O(0), O(1), O(2)), radius);
  // projection on this ball
  Traits::Construct_point_on_sphere_2 projection =
    ball.construct_point_on_sphere_2_object();
  // make Delaunay triangulation
  DToS2 dtos(ball);
  for(const auto& pt : points) {
    dtos.insert(projection(pt));
  }
  // check dimension
  int dim = dtos.dimension();
  if(dim == -2) {
    Rcpp::stop("The triangulation is empty.");
  }
  if(dim == -1) {
    Rcpp::stop("The triangulation contains only one vertex.");
  }
  if(dim == 0) {
    Rcpp::stop("The triangulation contains only two vertices.");
  }
  if(dim == 1) {
    Rcpp::stop("The triangulation is just a polygon drawn on a circle.");
  }
  // messages
  Rcpp::Rcout << "The triangulation has "
              << dtos.number_of_vertices() << " vertices and\n";
  Rcpp::Rcout << dtos.number_of_edges() << " edges and\n";
  Rcpp::Rcout << dtos.number_of_solid_faces() << " solid faces\n";
  Rcpp::Rcout << dtos.number_of_ghost_faces() << " ghost faces\n";
  // make Voronoï cells
  const int ncells = dtos.number_of_vertices();
  Rcpp::List Voronoi(ncells);
  const DToS2::Vertex_handles vhs = dtos.vertex_handles();
  int k = 0;
  for(auto v = vhs.begin(); v != vhs.end(); v++) {
    const SPoint3 coords = (*v)->point();
    const Rcpp::NumericVector site = {coords.x(), coords.y(), coords.z()};
    const DToS2::Edge_circulator ec = dtos.incident_edges(*v);
    const CC_Edges cc_edges(ec);
    const int cellsize = std::distance(cc_edges.begin(), cc_edges.end());
    Rcpp::NumericMatrix Cell(3, cellsize);
    {
      int i = 0;
      for(auto e = cc_edges.begin(); e != cc_edges.end(); e++) {
        const Arc arc = dtos.dual_on_sphere(*e);
        const CGAL::Circular_arc_point_3 startpoint = arc.source();
        const Rcpp::NumericVector vv =
          {startpoint.x(), startpoint.y(), startpoint.z()};
        Cell(Rcpp::_, i++) = vv;
      }
    }
    Rcpp::NumericVector A = Cell(Rcpp::_, 0);
    Rcpp::NumericVector B = Cell(Rcpp::_, 1);
    Rcpp::NumericVector C = Cell(Rcpp::_, 2);
    Rcpp::List smesh = sTriangle(A, B, C, radius, O, niter);
    for(int i = 2; i < cellsize-2; i++) {
      B = C;
      C = Cell(Rcpp::_, i + 1);
      smesh = joinMeshes(smesh, sTriangle(A, B, C, radius, O, niter));
    }
    //
    Voronoi(k++) = Rcpp::List::create(
      Rcpp::Named("site") = site,
      Rcpp::Named("cell") = Cell,
      Rcpp::Named("mesh") = smesh
    );
  }
  //
  return Voronoi;
}
