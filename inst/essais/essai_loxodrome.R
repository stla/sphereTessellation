library(sphereTessellation)
library(rgl)
library(cgalMeshes)
library(randomcoloR)
library(s2)


lox <- function(t, a){
  d <- sqrt(1 + a*a*t*t)
  cbind(cos(t)/d, sin(t)/d, -a*t/d)
}

npts <- 200L
h <- 20
pts <- lox(seq(-h, h, length.out = npts), a = .075)
pts <- rbind(pts, c(0, 0, 1), c(0, 0, -1))

spheres3d(0, 0, 0, color = "yellow", alpha = 0.3)
points3d(pts)

del <- sphereTessellation:::delaunay_cpp(t(pts))
Faces <- del[["faces"]]
Vertices <- del[["vertices"]]

#vor <- sphereTessellation:::voronoi_cpp(t(pts))

# striangle <- wk::xyz(pts[,1], pts[,2], pts[,3])
# s2_centroid_agg(striangle) -> cc

geodist <- function(centroid, B) {
  s2_distance(
    centroid, s2_point(B[1L], B[2L], B[3L]), radius = 1
  )
}

maxdist <- function(face) {
  pts <- Vertices[face, ]
  striangle <- wk::xyz(pts[,1L], pts[,2L], pts[,3L])
  centroid <- s2_centroid_agg(striangle)
  dists <- apply(pts, 1L, function(xyz) {
    geodist(centroid, xyz)
  })
  max(dists)
}

maxDists <- apply(Faces, 1L, maxdist)

ndist <- function(xyz, faceIndex) {
  maxd <- maxDists[faceIndex]
  pts <- Vertices[Faces[faceIndex, ], ]
  striangle <- wk::xyz(pts[,1L], pts[,2L], pts[,3L])
  centroid <- s2_centroid_agg(striangle)
  min(1, geodist(centroid, xyz) / maxd)
}

#fcol <- colorRamp(hcl.colors(100L, "Grays"), bias = 0.5, interpolate = "spline")
grays <- vapply(seq(0, 1, len = 256), function(u) rgb(u, u, u), character(1L))
fcol <- colorRamp(grays, bias = 0.3, interpolate = "linear")

clr <- function(xyz, i) {
  RGB <- fcol(ndist(xyz, i))
  rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
}


plotDelaunayFace <- function(i) {
  face <- Faces[i, ]
  A <- Vertices[face[1L], ]
  B <- Vertices[face[2L], ]
  C <- Vertices[face[3L], ]
  mesh <- sphericalTriangle(-A, -B, -C)
  vs <- mesh$getVertices()
  colors <- apply(vs, 1L, function(xyz) clr(xyz, i))
  mesh$assignVertexColors(colors)
  rmesh <- mesh$getMesh()
  shade3d(rmesh, meshColor = "vertices", specular = "yellow")
}

open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
#clear3d(type = "lights")
#light3d(x = -50, y = 100, z = 100, ambient = "white")
for(i in del[["solidFaces"]]) {
  plotDelaunayFace(i)
}
