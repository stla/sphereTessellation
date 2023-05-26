#include "sphereTessellation.h"

// [[Rcpp::export]]
Rcpp::List voronoi_cpp(Rcpp::NumericMatrix pts) {
  const int npoints = pts.ncol();
  std::vector<Point3> points;
  points.reserve(npoints);
  for(int i = 0; i < npoints; i++) {
    Rcpp::NumericVector pt_i = pts(Rcpp::_, i);
    points.emplace_back(pt_i(0), pt_i(1), pt_i(2))
  }
  // ball
  Traits ball(Point3(0, 0, 0)); // radius is 1 by default
  // projection on this ball
  Traits::Construct_point_on_sphere_2 cst =
    ball.construct_point_on_sphere_2_object();
  // make Delaunay triangulation
  DToS2 dtos(ball);
  for(const auto& pt : points) {
    dtos.insert(cst(pt));
  }
  Rcpp::Rcout << "The triangulation has dimension: " << dtos.dimension() << " and\n";
  Rcpp::Rcout << dtos.number_of_vertices() << " vertices and\n";
  Rcpp::Rcout << dtos.number_of_edges() << " edges and\n";
  Rcpp::Rcout << dtos.number_of_faces() << " solid faces\n";
  Rcpp::Rcout << dtos.number_of_ghost_faces() << " ghost faces\n";
  // make Voronoï cells
  const int ncells = dtos.number_of_vertices();
  Rcpp::List Voronoi(ncells);
  const DToS2::Vertex_handles vhs = dtos.vertex_handles();
  int k = 0;
  for(auto v = vhs.begin(); v != vhs.end(); v++) {
    const Point3 coords = (*v)->point();
    const Rcpp::NumericVector site = {coords.x(), coords.y(), coords.z()};
    const DToS2::Edge_circulator ec = dtos.incident_edges(*v);
    const CC_Edges cc_edges(ec);
    const int cellsize = std::distance(cc_edges.begin(), cc_edges.end());
    Rcpp::Rcout << "Cell size: " << cellsize << "\n";
    Rcpp::NumericMatrix Cell(3, cellsize);
    int i = 0;
    for(auto e = cc_edges.begin(); e != cc_edges.end(); e++) {
      const Arc arc = dtos.dual_on_sphere(*e);
      const CGAL::Circular_arc_point_3 startpoint = arc.source();
      const Rcpp::NumericVector vv =
        {startpoint.x(), startpoint.y(), startpoint.z()};
      Cell(Rcpp::_, i++) = vv;
    }
    Voronoi(k++) = Rcpp::List::create(
      Rcpp::Named("site") = site,
      Rcpp::Named("cell") = Cell
    );
  }
  return Voronoi;
}
