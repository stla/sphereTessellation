library(sphereTessellation)
library(rgl)

vertices <- t(cuboctahedron3d()$vb[-4L, ])
del <- DelaunayOnSphere(vertices, radius = sqrt(2))

open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
plotDelaunayOnSphere(del)

library(sphereTessellation)
library(rgl)
library(uniformly)

# sample vertices on a hemisphere, so there will be some ghost faces
set.seed(421L)
vertices <- rphong_on_hemisphere(7L)
del <- DelaunayOnSphere(vertices)
open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
plotDelaunayOnSphere(del)


library(sphereTessellation)
library(rgl)
library(cooltools)

vertices <- fibonaccisphere(8L)
del <- DelaunayOnSphere(vertices)
open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
plotDelaunayOnSphere(del)
