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
  vor
}


geodist <- function(A, B, radius) {
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]),
    s2_point(B[1L], B[2L], B[3L]),
    radius = radius
  )
}


plotVoronoiCell <- function(site, cell, mesh, radius) {

  dists <- apply(cell, 2L, function(xyz) {
    geodist(xyz, site, radius)
  })
  maxDist <- max(dists)

  fcol <- colorRamp(trek_pal("klingon"), bias = 1, interpolate = "spline")

  clr <- function(xyz) {
    RGB <- fcol(min(1, geodist(xyz, site, radius) / maxDist))
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

# plotVoronoiEdges <- function(vcell) {
#   plgn <- vcell[["cell"]]
#   cellsize <- ncol(plgn)
#   for(i in 1L:(cellsize-1L)) {
#     arc3d(plgn[, i], plgn[, i+1], c(0, 0, 0), 1, n = 50,
#           color = "white", lwd = 3, depth_test = "lequal")
#   }
#   arc3d(plgn[, cellsize], plgn[, 1L], c(0, 0, 0), 1, n = 50,
#         color = "white", lwd = 3, depth_test = "lequal")
#   #points3d(t(plgn), color = "navy", size = 13)
# }

plotVoronoi <- function(vor) {
  radius <- attr(vor, "radius")
  for(i in seq_along(vor)) {
    vor_i <- vor[[i]]
    plotVoronoiCell(vor_i[["site"]], vor_i[["cell"]], vor_i[["mesh"]], radius)
  }
}


vertices <- fibonaccisphere(50)
vor <- VoronoiOnSphere(vertices)


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
plotVoronoi(vor)

# for(i in seq_along(vor)) {
#   plotVoronoiEdges(vor[[i]])
# }


