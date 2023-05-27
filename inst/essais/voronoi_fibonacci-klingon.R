library(sphereTessellation)
library(rgl)
library(s2)
library(cooltools)
library(trekcolors)


vertices <- fibonaccisphere(300)

vor <- sphereTessellation:::voronoi_cpp(
  t(vertices), radius = 1, O = c(0, 0, 0), niter = 5
)


geodist <- function(A, B) {
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]), s2_point(B[1L], B[2L], B[3L]), radius = 1
  )
}

sites <- vapply(vor, function(vcell) {
  vcell[["site"]]
}, FUN.VALUE = numeric(3L))

maxdist <- function(vcell) {
  plg <- vcell[["cell"]]
  dists <- apply(plg, 2L, function(xyz) {
    geodist(xyz, vcell[["site"]])
  })
  max(dists)
}

maxDists <- vapply(vor, maxdist, FUN.VALUE = numeric(1L))

ndist <- function(xyz, siteIndex) {
  maxd <- maxDists[siteIndex]
  site <- vor[[siteIndex]][["site"]]
  min(1, geodist(xyz, site) / maxd)
}

fcol <- colorRamp(trek_pal("klingon"), bias = 1, interpolate = "spline")

clr <- function(xyz, i) {
  RGB <- fcol(ndist(xyz, i))
  rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
}


plotVoronoiCell <- function(i) {
  smesh <- vor[[i]][["mesh"]]
  vertices <- smesh[["vertices"]]
  colors <- c(apply(vertices, 2L, function(xyz) clr(xyz, i)))
  rmesh <- tmesh3d(
    vertices = smesh[["vertices"]],
    indices  = smesh[["faces"]],
    normals  = t(smesh[["normals"]]),
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


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
for(i in seq_along(vor)) {
  plotVoronoiCell(i)
}
# for(i in seq_along(vor)) {
#   plotVoronoiEdges(vor[[i]])
# }


# animation ####
M <- par3d("userMatrix")
movie3d(
  par3dinterp(
    time = seq(0, 1, len = 9),
    userMatrix = list(
      M,
      rotate3d(M, pi, 1, 0, 0),
      rotate3d(M, pi, 1, 1, 0),
      rotate3d(M, pi, 1, 1, 1),
      rotate3d(M, pi, 0, 1, 1),
      rotate3d(M, pi, 0, 1, 0),
      rotate3d(M, pi, 1, 0, 1),
      rotate3d(M, pi, 0, 0, 1),
      M
    )
  ),
  fps = 120,
  duration = 1,
  dir = ".",
  movie = "zzpic",
  convert = FALSE,
  webshot = FALSE
)

library(gifski)
pngfiles <- Sys.glob("zzpic*.png")
gifski(
  png_files = pngfiles,
  gif_file = "voronoi_spherical-fibonacci-klingon.gif",
  width = 512,
  height = 512,
  delay = 1/10
)

file.remove(pngfiles)
