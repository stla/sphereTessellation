#include "sphereTessellation.h"

// [[Rcpp::export]]
int test_delaunay() {
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

  // DToS2::All_face_handles fhs = dtos.all_face_handles();
  // for(auto fh = fhs.begin(); fh != fhs.end(); fh++) {
  //   std::cout << *fh << std::endl;
  // }

  DToS2::Vertex_handles vhs = dtos.vertex_handles();

  DToS2::All_faces_iterator itbegin = dtos.all_faces_begin();
  DToS2::All_faces_iterator itend = dtos.all_faces_end();
  for(auto f = itbegin; f != itend; f++) {
    // std::cout << *f << std::endl;
    // DToS2::Vertex_circulator vc = dtos.incident_vertices(f->vertex(0));
    // CC_Vertices cc_vertices(vc);
    // int facesize = std::distance(cc_vertices.begin(), cc_vertices.end());
    // std::cout << "SIZE: " << facesize << "\n" << std::endl;
    // std::cout << "\n--------------\n" << std::endl;
    // for(auto v = cc_vertices.begin(); v != cc_vertices.end(); v++) {
    //   std::cout << *v << std::endl;
    // }
    // std::cout << "\n--------------\n" << std::endl;
    // std::cout << f->for_compact_container() << std::endl;
    // std::cout << f->index(f->vertex(1)) << std::endl;
    // std::cout << f->index(f->vertex(2)) << std::endl;
    std::cout << "is ghost: " << f->is_ghost() << std::endl;

    int i = 0;
    int count = 0;
    std::cout << "\n--------------\n" << std::endl;
    for(auto v = vhs.begin(); v != vhs.end(); v++) {
      if(f->has_vertex(*v)) {
        std::cout << i << std::endl;
        count++;
        if(count == 3) {
          break;
        }
      }
      i++;
    }
  }


  return 1;
}



