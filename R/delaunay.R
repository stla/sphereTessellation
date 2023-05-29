#' @title Spherical Delaunay triangulation
#' @description Computes a spherical Delaunay triangulation.
#'
#' @param vertices vertices, a numeric matrix with three columns
#' @param radius radius of the sphere, a positive number; the vertices will
#'   be projected on this sphere
#' @param center center of the sphere, a numeric vector of length three; the
#'   vertices will be projected on this sphere
#' @param iterations positive integer, the number of iterations used to
#'   construct the meshes of the spherical faces
#'
#' @return A named list with four fields: xxx
#' @export
#'
#' @details xxx
#'
#' @seealso \code{\link{plotDelaunayOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#'
#' if(require(cooltools)) {
#' set.seed(421L)
#' vertices <- fibonaccisphere(15L)
#' del <- DelaunayOnSphere(vertices)
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)
#' }
#'
#' if(require(uniformly)) {
#' # sample vertices on a hemisphere, so there will be some ghost faces
#' set.seed(421L)
#' vertices <- rphong_on_hemisphere(7L)
#' del <- DelaunayOnSphere(vertices)
#' # the ghost faces are not plotted
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)
#' }
DelaunayOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5L
) {
  stopifnot(is.matrix(vertices), ncol(vertices) == 3L, is.numeric(vertices))
  if(anyNA(vertices)) {
    stop("Found missing values in the `vertices` matrix.")
  }
  storage.mode(vertices) <- "double"
  stopifnot(isPositiveNumber(radius))
  stopifnot(isVector3(center))
  stopifnot(isStrictPositiveInteger(iterations))
  del <- delaunay_cpp(
    t(vertices), as.double(radius), as.double(center), as.integer(iterations)
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
#' library(rgl)
#'
#' vertices <- t(cuboctahedron3d()$vb[-4L, ])
#' del <- DelaunayOnSphere(vertices, radius = sqrt(2))
#'
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotDelaunayOnSphere(del)
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
