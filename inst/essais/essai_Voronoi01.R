library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(randomcoloR)
library(pracma)

n <- 20L
vertices <- runif_on_sphere(n, 3L)

vor <- sphereTessellation:::voronoi_cpp(t(vertices))

plotVoronoiCell <- function(cell, color) {
  plgn <- cell[["cell"]]
  cellsize <- ncol(plgn)
  A <- plgn[, 1L]
  #A <- cart2sph(A)[2:1]
  for(i in 2L:(cellsize-1L)) {
    B <- plgn[, i]
    #B <- cart2sph(B)[2:1]
    C <- plgn[, i+1L]
    #C <- cart2sph(C)[2:1]
    mesh <- sphericalTriangle(A, B, C)
    rmesh <- mesh$getMesh()
    shade3d(rmesh, color = color)
  }
  for(i in 1L:(cellsize-1L)) {
    arc3d(plgn[, i], plgn[, i+1], c(0, 0, 0), 1, n = 50)
  }
  arc3d(plgn[, cellsize], plgn[, 1L], c(0, 0, 0), 1, n = 50)
  points3d(t(plgn), color = "navy", size = 13)
}

colors <- randomColor(length(vor), hue = "random", luminosity = "dark")

open3d(windowRect = 50 + c(0, 0, 512, 512))
for(i in seq_along(vor)) {
  plotVoronoiCell(vor[[i]], colors[i])
}
