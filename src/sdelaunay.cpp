#include "sphereTessellation.h"

// [[Rcpp::export]]
int sdelaunay() {
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
    Point3 xxx = cst(pt);
    dtos.insert(cst(pt));
  }
  std::cout << "The triangulation has dimension: " << dtos.dimension() << " and\n";
  std::cout << dtos.number_of_vertices() << " vertices" << std::endl;
  std::cout << dtos.number_of_edges() << " edges" << std::endl;
  std::cout << dtos.number_of_solid_faces() << " solid faces" << std::endl;
  std::cout << dtos.number_of_ghost_faces() << " ghost faces" << std::endl;

  DToS2::Vertex_handles vhs = dtos.vertex_handles();

  DToS2::All_faces_iterator itbegin = dtos.all_faces_begin();
  DToS2::All_faces_iterator itend = dtos.all_faces_end();

  int faceIndex = 0;
  for(auto f = itbegin; f != itend; f++) {
    std::cout << "\n--------------\n" << std::endl;
    std::cout << "Dealing with face " << faceIndex++ << std::endl;
    std::cout << "This face is ghost: " << f->is_ghost() << std::endl;
    std::cout << "Vertex indices of this face:" << std::endl;
    int i = 0;
    int count = 0;
    for(auto v = vhs.begin(); v != vhs.end(); v++) {
      if(f->has_vertex(*v)) {
        std::cout << i << std::endl;
        if(count++ == 3) {
          break;
        }
      }
      i++;
    }
  }

  return 1;
}



