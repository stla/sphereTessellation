#include "sphereTessellation.h"

// [[Rcpp::export]]
Rcpp::List delaunay_cpp(
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
  // Rcpp matrix to store the projected vertices
  Rcpp::NumericMatrix Vertices(3, npoints);
  // make Delaunay triangulation
  DToS2 dtos(ball);
  {
    int i = 0;
    for(const auto& pt : points) {
      SPoint3 p = projection(pt);
      dtos.insert(p);
      Rcpp::NumericVector v_i = {p.x(), p.y(), p.z()};
      Vertices(Rcpp::_, i++) = v_i;
    }
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
  // Rcpp matrix to store the faces
  int nfaces = dtos.number_of_faces();
  Rcpp::IntegerMatrix Faces(3, nfaces);
  // Rcpp vector to store the solid faces
  int nsolidfaces = dtos.number_of_solid_faces();
  Rcpp::IntegerVector SolidFaces(nsolidfaces);
  // all vertex handles
  DToS2::Vertex_handles vhs = dtos.vertex_handles();
  // iterate over all faces
  DToS2::All_faces_iterator itbegin = dtos.all_faces_begin();
  DToS2::All_faces_iterator itend = dtos.all_faces_end();
  int faceIndex = 0;
  int solidfaceIndex = 0;
  for(auto f = itbegin; f != itend; f++) {
    if(!f->is_ghost()) {
      SolidFaces(solidfaceIndex++) = faceIndex + 1;
    }
    // make the face
    Rcpp::IntegerVector Face(3);
    // iterate over vertices
    int iter = 1;
    int count = 0;
    for(auto v = vhs.begin(); v != vhs.end(); v++) {
      int index;
      const bool test = f->has_vertex(*v, index);
      if(test) {
        Face(index) = iter;
        if(++count == 3) {
          Faces(Rcpp::_, faceIndex) = Face;
          break;
        }
      }
      iter++;
    }
    faceIndex++;
  }
  // Meshes of spherical triangles
  int nmeshes = dtos.number_of_solid_faces();
  Rcpp::List Meshes(nmeshes);
  for(int i = 0; i < nmeshes; i++) {
    Rcpp::IntegerVector face = Faces(Rcpp::_, SolidFaces(i));
    Rcpp::NumericVector A = Vertices(Rcpp::_, face(0)-1);
    Rcpp::NumericVector B = Vertices(Rcpp::_, face(1)-1);
    Rcpp::NumericVector C = Vertices(Rcpp::_, face(2)-1);
    Meshes(i) = sTriangle(A, B, C, radius, O, niter);
  }
  //
  return Rcpp::List::create(
    Rcpp::Named("vertices")   = Rcpp::transpose(Vertices),
    Rcpp::Named("faces")      = Rcpp::transpose(Faces),
    Rcpp::Named("solidFaces") = SolidFaces,
    Rcpp::Named("meshes")     = Meshes
  );
}
