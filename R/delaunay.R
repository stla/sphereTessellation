#' @title Spherical Delaunay triangulation
#' @description Computes a spherical Delaunay triangulation.
#'
#' @param vertices vertices, xxx
#' @param radius radius of the sphere
#' @param center center of the sphere
#' @param iterations xxx
#'
#' @return A list with xxx
#' @export
#'
#' @details xxx
#'
#' @seealso \code{\link{plotDelaunayOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
DelaunayOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5
) {
  del <- delaunay_cpp(
    t(vertices), radius, center, as.integer(iterations)
  )
  attr(del, "radius") <- radius
  attr(del, "center") <- center
  del
}

#' @importFrom rgl tmesh3d shade3d
#' @noRd
plotDelaunayFace <- function(mesh, color) {
  rmesh <- tmesh3d(
    vertices = mesh[["vertices"]],
    indices  = mesh[["faces"]],
    normals  = t(mesh[["normals"]])
  )
  shade3d(rmesh, color = color)
}

#' @importFrom rgl arc3d
#' @noRd
plotDelaunayEdges <- function(vertices, radius, center) {
  coords <- rbind(vertices, vertices[1L, ])
  for(i in 1L:3L) {
    arc3d(coords[i, ], coords[i+1L, ], center, radius, n = 50,
          color = "black", lwd = 3, depth_test = "lequal")
  }
}

#' @title Plot spherical Delaunay triangulation
#' @description Plot a spherical Delaunay triangulation.
#'
#' @param del an output of \code{\link{DelaunayOnSphere}}
#' @param colors xxx
#' @param edges Boolean, whether to plot the edges
#' @param vertices Boolean, whether to plot the vertices
#'
#' @return No value is returned.
#' @export
#' @importFrom randomcoloR randomColor
#' @importFrom rgl spheres3d
#'
#' @examples
#' library(sphereTessellation)
plotDelaunayOnSphere <- function(
    del, colors = "random", edges = FALSE, vertices = FALSE
) {
  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(vertices))
  radius <- attr(del, "radius")
  center <- attr(del, "center")
  Vertices   <- del[["vertices"]]
  Meshes     <- del[["meshes"]]
  solidFaces <- del[["solidFaces"]]
  Faces      <- del[["faces"]][solidFaces, ]
  if(identical(colors, "random")) {
    colors <- randomColor(nrow(Faces), hue = "random", luminosity = "dark")
  }
  for(i in seq_along(Meshes)) {
    plotDelaunayFace(
      Meshes[[i]], colors[i]
    )
    if(edges) {
      face <- Faces[i, ]
      verts <- Vertices[face, ]
      plotDelaunayEdges(verts, radius, center)
    }
  }
  if(vertices) {
    spheres3d(Vertices, radius = radius/50, color = "navy")
  }
}
