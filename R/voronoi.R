#' @title Spherical Voronoï tessellation
#' @description Computes a spherical Voronoï tessellation.
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
#' @seealso \code{\link{plotVoronoiOnSphere}}
#'
#' @examples
#' library(sphereTessellation)
VoronoiOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5
) {
  vor <- voronoi_cpp(
    t(vertices), radius, center, as.integer(iterations)
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
plotVoronoiCell <- function(site, cell, mesh, radius, center) {

  dists <- apply(cell, 2L, function(xyz) {
    geodist(xyz, site, radius, center)
  })
  maxDist <- max(dists)

  fcol <- colorRamp(trek_pal("klingon"), bias = 1, interpolate = "spline")

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
#' @param edges Boolean, whether to plot the edges
#' @param sites Boolean, whether to plot the Voronoï sites
#'
#' @return No value is returned.
#'
#' @importFrom rgl spheres3d
#' @export
#'
#' @examples
#' library(sphereTessellation)
plotVoronoiOnSphere <- function(vor, edges = FALSE, sites = FALSE) {
  stopifnot(isBoolean(edges))
  stopifnot(isBoolean(sites))
  radius <- attr(vor, "radius")
  center <- attr(vor, "center")
  for(i in seq_along(vor)) {
    vor_i <- vor[[i]]
    plotVoronoiCell(
      vor_i[["site"]], vor_i[["cell"]], vor_i[["mesh"]],
      radius, center
    )
    if(edges) {
      plotVoronoiEdges(vor_i[["cell"]], radius, center)
    }
    if(sites) {
      spheres3d(rbind(vor_i[["site"]]), radius = radius/50, color = "navy")
    }
  }
}
