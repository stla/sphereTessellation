#include "sphereTessellation.h"

// [[Rcpp::export]]
int test() {
  std::vector<Point3> points;
  points.emplace_back( 3,  1,  1);
  points.emplace_back(-8,  1,  1);
  points.emplace_back( 1,  2,  1);
  points.emplace_back( 1, -2,  1);
  points.emplace_back( 1,  1, 10);
  Traits traits(Point3(1,1,1)); // radius is 1 by default
  DToS2 dtos(traits);
  Traits::Construct_point_on_sphere_2 cst = traits.construct_point_on_sphere_2_object();
  for(const auto& pt : points) {
    std::cout << "----- Inserting (" << pt
              << ") at squared distance " << CGAL::squared_distance(pt, traits.center())
              << " from the center of the sphere" << std::endl;
    dtos.insert(cst(pt));
    std::cout << "The triangulation now has dimension: " << dtos.dimension() << " and\n";
    std::cout << dtos.number_of_vertices() << " vertices" << std::endl;
    std::cout << dtos.number_of_edges() << " edges" << std::endl;
    std::cout << dtos.number_of_faces() << " solid faces" << std::endl;
    std::cout << dtos.number_of_ghost_faces() << " ghost faces" << std::endl;
  }
  CGAL::IO::write_OFF("inst/result.off", dtos, CGAL::parameters::stream_precision(17));

  DToS2::Vertex_handles vhs = dtos.vertex_handles();
  for(auto v = vhs.begin(); v != vhs.end(); v++) {
    DToS2::Edge_circulator ec = dtos.incident_edges(*v);
    CC_Edges cc_edges(ec);
    int cellsize = std::distance(cc_edges.begin(), cc_edges.end());
    std::cout << "SIZE: " << cellsize << "\n" << std::endl;
    std::cout << "\n--------------\n" << std::endl;
    std::cout << "current vertex: " << (*v)->point() << std::endl;
    std::cout << "\n--------------\n" << std::endl;
    for(auto e = cc_edges.begin(); e != cc_edges.end(); e++) {
      std::cout << "\narc dual de e:\n" << std::endl;
      Arc a = dtos.dual_on_sphere(*e);
      const CGAL::Circular_arc_point_3 as = a.source();
      std::cout << "source: " << a.source() << std::endl;
      std::cout << "target: " << a.target() << std::endl;
      std::cout << "\nface de e:\n" << std::endl;
      std::cout << e->first->vertex(0)->point() << std::endl;
      std::cout << e->first->vertex(1)->point() << std::endl;
      std::cout << e->first->vertex(2)->point() << std::endl;
    }
  }

  return 1;
}



