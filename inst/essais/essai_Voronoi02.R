library(sphereTessellation)
library(rgl)
library(randomcoloR)

vertices <- t(icosahedron3d()$vb[-4L, ])
radius <- sqrt(c(crossprod(vertices[1L, ])))

vor <- sphereTessellation:::voronoi_cpp(
  t(vertices), radius = radius, O = c(0, 0, 0), niter = 3
)

plotVoronoiCell <- function(i, color) {
  smesh <- vor[[i]][["mesh"]]
  rmesh <- tmesh3d(
    vertices = smesh[["vertices"]],
    indices  = smesh[["faces"]],
    normals  = t(smesh[["normals"]])
  )
  shade3d(rmesh, color = color)
}

colors <- randomColor(length(vor), hue = "random", luminosity = "dark")

open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(30, 30, zoom = 0.8)
for(i in seq_along(vor)) {
  plotVoronoiCell(i, colors[i])
}
