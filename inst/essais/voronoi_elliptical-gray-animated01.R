library(sphereTessellation)
library(rgl)
library(s2)
library(cooltools)

geodist <- function(A, B, radius, center) {
  A <- A - center
  B <- B - center
  s2_distance(
    s2_point(A[1L], A[2L], A[3L]),
    s2_point(B[1L], B[2L], B[3L]),
    radius = radius
  )
}


calculate_geodistances <- function(site, cell, mesh, radius, center) {

  dists <- apply(cell, 2L, function(xyz) {
    geodist(xyz, site, radius, center)
  })
  maxDist <- max(dists)

  clr <- function(xyz) {
    min(1, geodist(xyz, site, radius, center) / maxDist)
  }

  c(apply(mesh[["vertices"]], 2L, clr))
}

plotVoronoiCell <- function(mesh, geodistances, bias) {

  fcol <- colorRamp(hcl.colors(255, "Grays"), bias = bias, interpolate = "spline")

  clr <- function(gd) {
    RGB <- fcol(gd)
    rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
  }

  colors <- c(sapply(geodistances, clr))

  rmesh <- scale3d(tmesh3d(
    vertices = mesh[["vertices"]],
    indices  = mesh[["faces"]],
    normals  = t(mesh[["normals"]]),
    material = list(color = colors)
  ), 3, 2, 1)
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

plotVoronoiOnSphere <- function(vor, bias, edges = FALSE, sites = FALSE) {
  radius <- attr(vor, "radius")
  center <- attr(vor, "center")
  for(i in seq_along(vor)) {
    vor_i <- vor[[i]]
    plotVoronoiCell(
      vor_i$mesh, Geodistances[[i]], bias
    )
    if(edges) {
      plotVoronoiEdges(vor_i[["cell"]], radius, center)
    }
    if(sites) {
      spheres3d(rbind(vor_i[["site"]]), radius = radius/50, color = "navy")
    }
  }
}


vertices <- fibonaccisphere(200)
vor <- VoronoiOnSphere(vertices)

Geodistances <- lapply(vor, function(v) {
  radius <- attr(vor, "radius")
  center <- attr(vor, "center")
  calculate_geodistances(v$site, v$cell, v$mesh, radius, center)
})

b_ <- seq(0.3, 2, length.out = 35)

open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(10, -20, zoom = 0.8)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
for(i in seq_along(b_)) {
  plotVoronoiOnSphere(vor, bias = b_[i])
  snapshot3d(sprintf("zpic%03d.png", i), webshot = FALSE)
  clear3d()
}

cmd <-
  "mygifski.R --frames=zpic*.png --fps=12 -b -o voronoi_elliptical-gray-animated.gif"
system(cmd)

file.remove(Sys.glob("zpic*.png"))

