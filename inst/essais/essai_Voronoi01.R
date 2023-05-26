library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(randomcoloR)

n <- 20L
vertices <- runif_on_sphere(n, 3L)

vor <- sphereTessellation:::voronoi_cpp(t(vertices))

plotVoronoiCell <- function(cell, color) {
  plgn <- cell[["cell"]]
  cellsize <- ncol(plgn)
  A <- plgn[, 1L]
  for(i in 2L:(cellsize-1L)) {
    B <- plgn[, i]
    C <- plgn[, i+1L]
    mesh <- sphericalTriangle(A, B, C)
    rmesh <- mesh$getMesh()
    shade3d(rmesh, color = color)
  }
}

colors <- randomColor(length(vor), hue = "random", luminosity = "dark")

open3d(windowRect = 50 + c(0, 0, 512, 512))
for(i in seq_along(vor)) {
  plotVoronoiCell(vor[[i]], colors[i])
}
