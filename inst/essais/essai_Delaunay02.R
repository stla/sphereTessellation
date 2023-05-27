library(sphereTessellation)
library(rgl)
library(randomcoloR)
library(cooltools)


vertices <- fibonaccisphere(300)

del <- sphereTessellation:::delaunay_cpp(
  t(vertices), radius = 1, O = c(0, 0, 0), niter = 5
)
Meshes <- del[["meshes"]]

plotDelaunayFace <- function(i, color) {
  smesh <- Meshes[[i]]
  rmesh <- tmesh3d(
    vertices = smesh[["vertices"]],
    indices  = smesh[["faces"]],
    normals  = t(smesh[["normals"]])
  )
  shade3d(rmesh, color = color)
}

nfaces <- length(Meshes)
colors <- randomColor(nfaces, hue = "random", luminosity = "dark")

open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(30, 30, zoom = 0.7)
for(i in 1:nfaces) {
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
