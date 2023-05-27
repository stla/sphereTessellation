#ifndef __HEADER__
#define __HEADER__

#include <Rcpp.h>

#define CGAL_EIGEN3_ENABLED 1

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_on_sphere_2.h>
#include <CGAL/Projection_on_sphere_traits_3.h>
#include <CGAL/circulator.h>
#include <CGAL/Circular_arc_point_3.h>

#include <CGAL/Surface_mesh.h>
#include <CGAL/squared_distance_3.h>
//#include <CGAL/boost/graph/generators.h>
//#include <CGAL/Polygon_mesh_processing/clip.h>
//#include <CGAL/Plane_3.h>
//#include <CGAL/Subdivision_method_3/subdivision_methods_3.h>

typedef CGAL::Exact_predicates_inexact_constructions_kernel       K;
typedef CGAL::Projection_on_sphere_traits_3<K>                    Traits;
typedef CGAL::Delaunay_triangulation_on_sphere_2<Traits>          DToS2;
typedef Traits::Point_3                                           SPoint3;
typedef Traits::Arc_on_sphere_2                                   Arc;
typedef CGAL::Container_from_circulator<DToS2::Edge_circulator>   CC_Edges;
typedef CGAL::Container_from_circulator<DToS2::Vertex_circulator> CC_Vertices;

typedef K::Point_3                                       Point3;
typedef CGAL::Surface_mesh<Point3>                       Mesh3;
typedef Mesh3::Vertex_index                              VX3;
//typedef K::Plane_3                                       Plane3;

//namespace PMP = CGAL::Polygon_mesh_processing;

Rcpp::List sTriangle(
    Rcpp::NumericVector, Rcpp::NumericVector, Rcpp::NumericVector,
    double, Rcpp::NumericVector, int
);

#endif
