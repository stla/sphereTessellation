library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(s2)

geodist <- function(A, B) {
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]), s2_point(B[1L], B[2L], B[3L]), radius = 1
  )
}


n <- 40L
vertices <- runif_on_sphere(n, 3L)

vor <- sphereTessellation:::voronoi_cpp(t(vertices))


sites <- vapply(vor, function(vcell) {
  vcell[["site"]]
}, FUN.VALUE = numeric(3L))

nearestSite <- function(xyz) {
  dists <- apply(sites, 2L, function(pt) {
    geodist(pt, xyz)
  })
  which.min(dists)
}

maxdist <- function(vcell) {
  plg <- vcell[["cell"]]
  dists <- apply(plg, 2L, function(xyz) {
    geodist(xyz, vcell[["site"]])
  })
  max(dists)
}

maxDists <- vapply(vor, maxdist, FUN.VALUE = numeric(1L))

ndist <- function(xyz, siteIndex) {
  #siteIndex <- nearestSite(xyz)
  maxd <- maxDists[siteIndex]
  site <- vor[[siteIndex]][["site"]]
  min(1, geodist(xyz, site) / maxd)
}

fcol <- colorRamp(hcl.colors(100L, "Grays"), bias = 0.5, interpolate = "spline")

grays <- vapply(seq(0, 1, len = 256), function(u) rgb(u, u, u), character(1L))
fcol <- colorRamp(grays, bias = 0.5, interpolate = "spline")

clr <- function(xyz, i) {
  RGB <- fcol(ndist(xyz, i))
  rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
}


plotVoronoiCell <- function(j) {
  plgn <- vor[[i]][["cell"]]
  cellsize <- ncol(plgn)
  A <- plgn[, 1L]
  for(i in 2L:(cellsize-1L)) {
    B <- plgn[, i]
    C <- plgn[, i+1L]
    mesh <- sphericalTriangle(-A, -B, -C, iterations = 5L)
    vs <- mesh$getVertices()
    colors <- c(apply(vs, 1L, function(xyz) clr(xyz, j)))
    mesh$assignVertexColors(colors)
    rmesh <- mesh$getMesh()
    shade3d(rmesh, meshColor = "vertices", specular = "black")
  }
}

plotVoronoiEdges <- function(vcell) {
  plgn <- vcell[["cell"]]
  cellsize <- ncol(plgn)
  for(i in 1L:(cellsize-1L)) {
    arc3d(plgn[, i], plgn[, i+1], c(0, 0, 0), 1, n = 50,
          color = "white", lwd = 3, depth_test = "lequal")
  }
  arc3d(plgn[, cellsize], plgn[, 1L], c(0, 0, 0), 1, n = 50,
        color = "white", lwd = 3, depth_test = "lequal")
  #points3d(t(plgn), color = "navy", size = 13)
}


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
for(i in seq_along(vor)) {
  plotVoronoiCell(i)
}
for(i in seq_along(vor)) {
  plotVoronoiEdges(vor[[i]])
}


# animation ####
movie3d(
  spin3d(axis = c(0, 1, 0), rpm = 10),
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
  gif_file = "voronoi_spherical-septuaginta01.gif",
  width = 512,
  height = 512,
  delay = 1/12
)

file.remove(pngfiles)
