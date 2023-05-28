library(sphereTessellation)
library(rgl)
library(s2)
library(cooltools)
library(trekcolors)

VoronoiOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5
) {
  vor <- sphereTessellation:::voronoi_cpp(
    t(vertices), radius, center, as.integer(iterations)
  )
  attr(vor, "radius") <- radius
  attr(vor, "center") <- center
  vor
}


geodist <- function(A, B, radius, center) {
  A <- A - center
  B <- B - center
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]),
    s2_point(B[1L], B[2L], B[3L]),
    radius = radius
  )
}


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

plotVoronoiEdges <- function(cell, radius, center) {
  cellsize <- ncol(cell)
  cell <- cbind(cell, cell[, 1L])
  for(i in 1L:cellsize) {
    arc3d(cell[, i], cell[, i+1L], center, radius, n = 50,
          color = "white", lwd = 3, depth_test = "lequal")
  }
}

plotVoronoiOnSphere <- function(vor, edges = FALSE, sites = FALSE) {
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


vertices <- fibonaccisphere(50)
vor <- VoronoiOnSphere(vertices)


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
plotVoronoiOnSphere(vor)

