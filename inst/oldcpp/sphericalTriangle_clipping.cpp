#include "sphereTessellation.h"

void clippingToPlane(Mesh3& mesh, Plane3 plane) {
  const bool clipping = PMP::clip(
    mesh, plane,
    PMP::parameters::clip_volume(false)
  );
  if(!clipping) {
    Rcpp::stop("Clipping has failed.");
  }
  mesh.collect_garbage();
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
Mesh3 icosphere(Point3 center, double radius, unsigned int iterations) {
  // !!! bug CGAL 5.4 !!! //
  double x = sqrt(1 + (1 + sqrt(5)) / 4);
  radius = radius / x;
  Mesh3 mesh;
  CGAL::make_icosahedron<Mesh3, Point3>(mesh, center, radius);
  CGAL::Subdivision_method_3::Loop_subdivision(
    mesh, CGAL::parameters::number_of_iterations(iterations)
  );
  return mesh;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
