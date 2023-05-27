library(sphereTessellation)
library(rgl)
library(s2)
library(trekcolors)

pts <- t(icosahedron3d()$vb[-4L, ])
radius <- sqrt(c(crossprod(pts[1L, ])))

del <- sphereTessellation:::delaunay_cpp(
  t(pts)/radius, radius = 1, O = c(0, 0, 0), niter = 5
)
Faces <- del$faces
Vertices <- del$vertices

geodist <- function(centroid, B) {
  s2_distance(
    centroid, s2_point(B[1L], B[2L], B[3L]), radius = 1
  )
}

maxdist <- function(face) {
  pts <- Vertices[face, ]
  lls <- celestial::car2sph(pts)
  striangle <- wk::xy(lls[,1L], lls[,2L])
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
  lls <- celestial::car2sph(pts)
  striangle <- wk::xy(lls[,1L], lls[,2L])
  centroid <- s2_centroid_agg(striangle)
  min(1, geodist(centroid, xyz) / maxd)
}

fcol <- colorRamp(trek_pal("klingon"), bias = 1, interpolate = "linear")

clr <- function(xyz, i) {
  RGB <- fcol(ndist(xyz, i))
  rgb(RGB[1L, 1L], RGB[1L, 2L], RGB[1L, 3L], maxColorValue = 255)
}

Meshes <- del[["meshes"]]

plotDelaunayFace <- function(i) {
  smesh <- Meshes[[i]]
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


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
#clear3d(type = "lights")
#light3d(x = -50, y = 100, z = 100, ambient = "white")
for(i in seq_along(Meshes)) {
  plotDelaunayFace(i)
}
