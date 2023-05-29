library(sphereTessellation)
library(rgl)

vertices <- t(cuboctahedron3d()$vb[-4L, ])
vor <- VoronoiOnSphere(vertices, radius = sqrt(2))

open3d(windowRect = 50 + c(0, 0, 512, 512), zoom = 0.8)
plotVoronoiOnSphere(vor, sites = TRUE)

