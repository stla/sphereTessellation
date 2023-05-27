library(sphereTessellation)
library(uniformly)
library(rgl)
library(cgalMeshes)
library(randomcoloR)
library(pracma)

n <- 6L
vertices <- runif_on_sphere(n, 3L)

del <- sphereTessellation:::delaunay_cpp(t(vertices))

