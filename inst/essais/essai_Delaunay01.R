library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(randomcoloR)


n <- 20L
vertices <- runif_on_sphere(n, 3L)

del <- sphereTessellation:::delaunay_cpp(t(vertices))
Faces <- del[["faces"]]
Vertices <- del[["vertices"]]

plotDelaunayFace <- function(i, color) {
  face <- Faces[i, ]
  A <- Vertices[face[1L], ]
  B <- Vertices[face[2L], ]
  C <- Vertices[face[3L], ]
  mesh <- sphericalTriangle(A, B, C)
  rmesh <- mesh$getMesh()
  shade3d(rmesh, color = color)
}

nfaces <- nrow(Faces)
colors <- randomColor(nfaces, hue = "random", luminosity = "bright")

open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(30, 30, zoom = 0.8)
for(i in 1L:nfaces) {
  plotDelaunayFace(i, colors[i])
}
