setwd(here::here("inst", "essais"))

library(maps)
data(worldMapEnv)

# make a PNG image of the world with filled countries
world <- map("world", plot = FALSE, fill = TRUE, wrap = c(0, 360))

png("world2D.png", width = 1024L, height = 1024L)
opar <- par(mar = c(0, 0, 0, 0))
plot(world$x, world$y, type = "n", xlab = NA, ylab = NA, axes = FALSE,
     xaxs = "i", yaxs = "i", xlim = c(0, 360), ylim = c(-90, 90))
polygon(world$x, world$y, col="yellow", border = "red", lwd = 2)
par(opar)
dev.off()


# use spherical coordinates to make a mesh of the unit sphere
library(cgalMeshes)
sphericalCoordinates <- function(θ, ϕ){
  x <- cos(θ) * sin(ϕ)
  y <- sin(θ) * sin(ϕ)
  z <- cos(ϕ)
  rbind(x, y, z)
}
rmesh <- parametricMesh(
  sphericalCoordinates, urange = c(0, 2*pi), vrange = c(0, pi),
  periodic = c(TRUE, TRUE), nu = 1024L, nv = 1024L
)
rmesh$normals <- rmesh$vb[-4L, ]

# get the angles θ and ϕ of the vertices of the mesh
UV <- cooltools::car2sph(t(rmesh$vb[-4L, ]))
UV <- cbind(UV[, 3L], UV[, 2L])


# now load the PNG image
library(imager)
img <- load.image("world2D.png")
# take the r, g, b channels
r <- squeeze(R(img))
g <- squeeze(G(img))
b <- squeeze(B(img))

# make interpolation functions to get the colors of the UV points
library(cooltools) # to get the `approxfun2` function
x_ <- seq(0, 2*pi, length.out = 1024L)
y_ <- seq(0, pi, length.out = 1024L)
f_r <- approxfun2(x_, y_, r)
f_g <- approxfun2(x_, y_, g)
f_b <- approxfun2(x_, y_, b)

# now, interpolate the r, g, b values
UV_r <- f_r(UV[, 1L], UV[, 2L])
UV_g <- f_g(UV[, 1L], UV[, 2L])
UV_b <- f_b(UV[, 1L], UV[, 2L])

# convert rgb to hex codes
clrs <- rgb(UV_r, UV_g, UV_b)
clrs[clrs == "#FFFFFF"] <- "black" # replace white with black


# assign the colors to the vertices of the mesh
# I don't know why, but one has to reverse the colors
rmesh$material <- list(color = rev(clrs))


# plot
library(rgl)
open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.7)
shade3d(rmesh, meshColor = "vertices")
snapshot3d("world3D.png", webshot = FALSE)


# now take the capitals
data(world.cities)
capitals_ll <- as.matrix(
  subset(world.cities, capital == 1, select = c("long", "lat"))
) * pi / 180
capitals <- t(sphericalCoordinates(
  capitals_ll[, 1L] + pi, capitals_ll[, 2L] + pi/2
))
capitals[, 1L] <- -capitals[, 1L] # don't know why we have to negate

# Voronoî
library(sphereTessellation)
vor <- VoronoiOnSphere(capitals)

open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.7)
shade3d(rmesh, meshColor = "vertices", polygon_offset = 1)
plotVoronoiOnSphere(
  vor, colors = NA, edges = TRUE, ecolor = "black",
  sites = TRUE, scolor = "blue"
)

# animation ####
movie3d(
  spin3d(axis = c(0, 0, 1), rpm = 10),
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
  gif_file = "voronoi_capitals.gif",
  width = 512,
  height = 512,
  delay = 1/8
)

file.remove(pngfiles)
