library(sphereTessellation)
library(rgl)
library(randomcoloR)
library(cooltools)

DelaunayOnSphere <- function(
    vertices, radius = 1, center = c(0, 0, 0), iterations = 5
) {
  del <- sphereTessellation:::delaunay_cpp(
    t(vertices), radius, center, as.integer(iterations)
  )
  attr(del, "radius") <- radius
  attr(del, "center") <- center
  del
}

plotDelaunayFace <- function(mesh, color) {
  rmesh <- tmesh3d(
    vertices = mesh[["vertices"]],
    indices  = mesh[["faces"]],
    normals  = t(mesh[["normals"]])
  )
  shade3d(rmesh, color = color)
}

plotDelaunayEdges <- function(vertices, radius, center) {
  coords <- rbind(vertices, vertices[1L, ])
  for(i in 1L:3L) {
    arc3d(coords[i, ], coords[i+1L, ], center, radius, n = 50,
          color = "black", lwd = 3, depth_test = "lequal")
  }
}

plotDelaunayOnSphere <- function(
    del, colors = "random", edges = FALSE, vertices = FALSE
) {
  radius <- attr(del, "radius")
  center <- attr(del, "center")
  Vertices   <- del[["vertices"]]
  Meshes     <- del[["meshes"]]
  solidFaces <- del[["solidFaces"]]
  Faces      <- del[["faces"]][solidFaces, ]
  if(identical(colors, "random")) {
    colors <- randomColor(nrow(Faces), hue = "random", luminosity = "dark")
  }
  for(i in seq_along(Meshes)) {
    plotDelaunayFace(
      Meshes[[i]], colors[i]
    )
    if(edges) {
      face <- Faces[i, ]
      verts <- Vertices[face, ]
      plotDelaunayEdges(verts, radius, center)
    }
  }
  if(vertices) {
    spheres3d(Vertices, radius = radius/50, color = "navy")
  }
}


vertices <- fibonaccisphere(50)
del <- DelaunayOnSphere(vertices)


open3d(windowRect = 50 + c(0, 0, 512, 512))
view3d(0, 0, zoom = 0.7)
clear3d(type = "lights")
light3d(x = -50, y = 100, z = 100, ambient = "white")
plotDelaunayOnSphere(del, edges = TRUE, vertices = TRUE)

