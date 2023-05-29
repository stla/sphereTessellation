#' @title Spherical Voronoï tessellation
#' @description Computes a spherical Voronoï tessellation.
#'
#' @param vertices vertices, a numeric matrix with three columns
#' @param radius radius of the sphere, a positive number; the vertices will
#'   be projected on this sphere
#' @param center center of the sphere, a numeric vector of length three; the
#'   vertices will be projected on this sphere
#' @param iterations positive integer, the number of iterations used to
#'   construct the meshes of the spherical faces
#'
#' @return An unnamed list whose each element is a named list with three
#'   fields: xxx
#' @export
#'
#' @details xxx
#'
#' @seealso \code{\link{plotVoronoiOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#' if(require(cooltools)) {
#' set.seed(421L)
#' vertices <- fibonaccisphere(15L)
#' vor <- VoronoiOnSphere(vertices)
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotVoronoiOnSphere(vor)
#' }
VoronoiOnSphere <- function(
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
  vor <- voronoi_cpp(
    t(vertices), as.double(radius), as.double(center), as.integer(iterations)
  )
  attr(vor, "radius") <- radius
  attr(vor, "center") <- center
  vor
}


#' @importFrom s2 s2_distance s2_point
#' @noRd
geodist <- function(A, B, radius, center) {
  A <- A - center
  B <- B - center
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]),
    s2_point(B[1L], B[2L], B[3L]),
    radius = radius
  )
}

#' @importFrom grDevices colorRamp rgb
#' @importFrom rgl tmesh3d shade3d
#' @noRd
plotVoronoiCell <- function(
    site, cell, mesh, radius, center, palette
) {

  dists <- apply(cell, 2L, function(xyz) {
    geodist(xyz, site, radius, center)
  })
  maxDist <- max(dists)

  fcol <- colorRamp(palette, bias = 1, interpolate = "spline")

  clr <- function(xyz) {
    RGB <- fcol(min(1, geodist(xyz, site, radius, center) / maxDist))
    rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
  }

  colors <- c(apply(mesh[["vertices"]], 2L, clr))
  rmesh <- tmesh3d(
    vertices = mesh[["vertices"]],
    indices  = mesh[["faces"]],
    normals  = t(mesh[["normals"]]),
    material = list(color = colors)
  )
  shade3d(rmesh, meshColor = "vertices", specular = "black")
}

#' @importFrom rgl arc3d
#' @noRd
plotVoronoiEdges <- function(cell, radius, center) {
  cellsize <- ncol(cell)
  cell <- cbind(cell, cell[, 1L])
  for(i in 1L:cellsize) {
    arc3d(cell[, i], cell[, i+1L], center, radius, n = 50,
          color = "white", lwd = 3, depth_test = "lequal")
  }
}

#' @title Plot spherical Voronoï tessellation
#' @description Plot a spherical Voronoï tessellation.
#'
#' @param vor an output of \code{\link{VoronoiOnSphere}}
#' @param colors xxx
#' @param palette this argument is used only when \code{colors="gradient"}; it
#'   can be either a character vector of colors, or the name of a palette
#'   which will be passed to the \code{palette} argument of the function
#'   \code{\link[grDevices]{hcl.colors}}
#' @param edges Boolean, whether to plot the edges
#' @param sites Boolean, whether to plot the Voronoï sites
#'
#' @return No value is returned.
#'
#' @importFrom rgl spheres3d
#' @importFrom grDevices hcl.colors
#' @export
#'
#' @examples
#' library(sphereTessellation)
#' library(rgl)
#'
#' vertices <- t(cuboctahedron3d()$vb[-4L, ])
#' vor <- VoronoiOnSphere(vertices)
#'
#' open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
#' plotVoronoiOnSphere(vor)
plotVoronoiOnSphere <- function(
    vor, colors = "gradient", palette = "Rocket", edges = FALSE, sites = FALSE
) {
  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(sites))
  radius <- attr(vor, "radius")
  center <- attr(vor, "center")
  if(identical(colors, "gradient") && isString(palette)) {
    palette <- hcl.colors(255L, palette = palette)
  }
  for(i in seq_along(vor)) {
    vor_i <- vor[[i]]
    plotVoronoiCell(
      vor_i[["site"]], vor_i[["cell"]], vor_i[["mesh"]],
      radius, center, palette = palette
    )
    if(edges) {
      plotVoronoiEdges(vor_i[["cell"]], radius, center)
    }
    if(sites) {
      spheres3d(rbind(vor_i[["site"]]), radius = radius/50, color = "navy")
    }
  }
}
