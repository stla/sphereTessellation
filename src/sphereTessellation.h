#ifndef __HEADER__
#define __HEADER__

#include <Rcpp.h>

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_on_sphere_2.h>
#include <CGAL/Projection_on_sphere_traits_3.h>
//#include <boost/iterator/transform_iterator.hpp>
#include <CGAL/circulator.h>


typedef CGAL::Exact_predicates_inexact_constructions_kernel          K;
typedef CGAL::Projection_on_sphere_traits_3<K>                       Traits;
typedef CGAL::Delaunay_triangulation_on_sphere_2<Traits>             DToS2;
typedef Traits::Point_3                                              Point_3;

typedef CGAL::Container_from_circulator<DToS2::Edge_circulator> CC_Edges;

#endif
