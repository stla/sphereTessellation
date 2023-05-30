library(sphereTessellation)
library(rgl)
vertices <- t(cuboctahedron3d()$vb[-4L, ])
vor <- VoronoiOnSphere(vertices)
open3d(windowRect = 50 + c(0, 0, 900, 300), zoom = 0.8)
mfrow3d(1, 3)
plotVoronoiOnSphere(vor, palette = "Viridis", bias = 0.5)
next3d()
plotVoronoiOnSphere(vor, palette = "Viridis", bias = 0.8)
next3d()
plotVoronoiOnSphere(vor, palette = "Viridis", bias = 1.1)


