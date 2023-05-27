library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(randomcoloR)


set.seed(314L)
n <- 6L
vertices <- runif_on_sphere(n, 3L)

del <- sphereTessellation:::delaunay_cpp(t(vertices))

Faces <- del[["faces"]]
Vertices <- del[["vertices"]]

colors <- rep("red", nrow(Faces))
colors[del$solidFaces] <- "blue"
rmesh <- tmesh3d(
  vertices = t(Vertices),
  indices = t(Faces),
  material = list(color = colors)
)
shade3d(rmesh, meshColor = "faces")



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
view3d(30, 30, zoom = 0.7)
for(i in del[["solidFaces"]]) {
  plotDelaunayFace(i, colors[i])
}

# animation ####
movie3d(
  spin3d(axis = c(1, 1, 1), rpm = 10),
  duration = 6, fps = 20,
  movie = "zzpic", dir = ".",
  convert = FALSE,
  startTime = 1/20,
  webshot = FALSE
)

library(gifski)
pngfiles <- Sys.glob("zzpic*.png")
gifski(
  png_files = pngfiles,
  gif_file = "delaunay_spherical-septuaginta01.gif",
  width = 512,
  height = 512,
  delay = 1/12
)

file.remove(pngfiles)
