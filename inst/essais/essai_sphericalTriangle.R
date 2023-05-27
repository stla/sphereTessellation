library(sphereTessellation)
library(rgl)

A <- c(1, 0, 0)
B <- c(0, 1, 0)
C <- c(0, 0, 1)

smesh <- sphereTessellation:::sTriangle(A, B, C, 1, c(0, 0, 0), 5)

rmesh <- tmesh3d(
  vertices = smesh$vertices,
  indices  = smesh$faces,
  normals = t(smesh$normals)
)

shade3d(rmesh, color="blue")
points3d(rbind(A, B, C), size = 12)
spheres3d(0, 0, 0, color = "yellow", alpha = 0.3)
