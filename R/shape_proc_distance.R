#' Adapted from "shapes" package by Ian L. Dryden
#' Calculates Procrustes distances for all nC2 combinations of a 3-dimensional array of shapes
#'
#' @param x 3-dimensional array of shapes. Every zth element should be a different shape.
#'
#' @param type string indicating the type of distance; "full" full Procrustes distance,
#' "partial" partial Procrustes distance,
#' "Riemannian" Riemannian shape distance,
#' "sizeandshape" size-and-shape Riemannian/Procrustes distance
#' @param reflect Logical. If reflect = TRUE then reflection invariance is included.
#'
#' @return A matrix of all calculated distance. Each distance is labeled by the shapes used for calculation.
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com} Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
#' @export
#'
#' @importFrom shapes
#'
#' @examples
#'
#' \dontrun{
#'bone_shapes = bone_list$x
#'bone_shapes = cbind(bone_shapes, bone_list$y)
#'shape_proc_distance(bone_shapes, type = "full")
#' }
#'

shape_proc_distance <- function (x, type = "full", reflect = FALSE){

  distances = vector()

  for(i in 1:dim(x)[3]){

    x_ref = combn(dim(x)[3], 2)[1,i]
    y_ref = combn(dim(x)[3], 2)[2,i]

    if (type == "full") {
      distances[i] = sin(riemdist(x[,,x_ref], x[,,y_ref], reflect = reflect))
    }
    if (type == "partial") {
      distances[i] <- sqrt(2) * sqrt(abs(1 - cos(riemdist(x[x_ref], x[,,y_ref], reflect = reflect))))
    }
    if (type == "Riemannian") {
      distances[i] <- riemdist(x[x_ref], x[,,y_ref], reflect = reflect)
    }
    if (type == "sizeandshape") {
      distances[i] <- ssriemdist(x[,,x_ref], x[,,y_ref], reflect = reflect)
    }

    names(distances)[i] = paste(x_ref, y_ref, sep = " ")
  }

  distances
}

