#' Calculates Elastic distances for all nC2 combinations of a 3-dimensional array of shapes

#' Adapted from "fdasrvf" package by  J. Derek Tucker <jdtuck@sandia.gov>
#'
#' @param x 3-dimensional array of shapes. Every zth element should be a different shape.
#'
#' @param mode Open ("O") or Closed ("C") curves
#'
#' @return A matrix of all calculated distance. Each distance is labeled by the shapes used for calculation.
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com}
#'
#' @export
#'
#'@import fdasrvf
#'
#' @importFrom utils combn
#'
#' @examples
#'
#' \dontrun{
#'bone_shapes = bones_shapes$x
#'bone_shapes = cbind(bone_shapes, bone_list$y)
#'bone_1 = bone_shapes[1:100,]
#'bone_2 = bone_shapes[101:200,]
#'bone_3 = bone_shapes[201:300,]
#'bones = array(c(bone_1, bone_2, bone_3), dim = c(100, 2, 3))
#'shape_elastic_distance(bones, mode = "C")
#' }
#'

shape_elastic_distance <- function(x, mode = "C"){

  for(i in 1:dim(x)[3]){

    x_ref = combn(dim(x)[3], 2)[1,i]
    y_ref = combn(dim(x)[3], 2)[2,i]

    f1 = x[,,x_ref]
    f2 = x[,,y_ref]

    fxn = inverse_exp_coord(t(f1), t(f2), mode)

    distances[i] = fxn$d

    names(distances)[i] = paste(x_ref, y_ref, sep = " ")
  }

  return(distances)
}



