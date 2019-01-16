library(gtools)
library(fdasrvf)
library(shapes)

# PROCRUSTES DISTANCE FUNCTION DEVELOPMENT
# -----------------------------------------------------------

bone_shapes = bone_list$x

bone_shapes = cbind(bone_shapes, bone_list$y)

bone_1 = bone_shapes[1:100,]
bone_2 = bone_shapes[101:200,]
bone_3 = bone_shapes[201:300,]

bones = array(c(bone_1, bone_2, bone_3), dim = c(100, 2, 3))

# bones_half = bones[,,combn(dim(bones)[3], 2)[1,]]
# bones_half2 = bones[,,combn(dim(bones)[3], 2)[2,]]

# distances = vector()
#
# for(i in 1:dim(bones)[3]){
#
#   x_ref = combn(dim(bones)[3], 2)[1,i]
#   y_ref = combn(dim(bones)[3], 2)[2,i]
#
#   distances = c(distances, procdist(bones[,,x_ref], bones[,,y_ref]))
#
#   names(distances)[i] = paste(x_ref, y_ref, sep = " ")
# }
#
# distances

shape_proc_distance <- function (x, type = "full", reflect = FALSE){

  distances = vector()

  for(i in 1:dim(x)[3]){

    x_ref = combn(dim(x)[3], 2)[1,i]
    y_ref = combn(dim(x)[3], 2)[2,i]

    if (type == "full") {
      distances = c(distances, sin(riemdist(x[,,x_ref], x[,,y_ref], reflect = reflect)))
    }
    if (type == "partial") {
      distances <- c(distances, sqrt(2) * sqrt(abs(1 - cos(riemdist(x[x_ref], x[,,y_ref], reflect = reflect)))))
    }
    if (type == "Riemannian") {
      distances <- c(distances, riemdist(x[x_ref], x[,,y_ref], reflect = reflect))
    }
    if (type == "sizeandshape") {
      distances <- c(distances, ssriemdist(x[,,x_ref], x[,,y_ref], reflect = reflect))
    }

    names(distances)[i] = paste(x_ref, y_ref, sep = " ")
  }

  distances
}

shape_proc_distance(bones, type = "full", reflect = FALSE)

# DEVELOPMENT
# ----------------------------------

    # # Procrustes Distance
    #
    # bone_shapes = bone_list$x
    #
    # bone_shapes = cbind(bone_shapes, bone_list$y)
    #
    # bone_1 = bone_shapes[1:100,]
    # bone_2 = bone_shapes[101:200,]
    # bone_3 = bone_shapes[201:300,]
    #
    # bones = array(c(bone_1, bone_2, bone_3), dim = c(100, 2, 3))
    #
    # # dists = sapply(x = combinations(3, 2, names(bones))[1], procdist, combinations(3, 2, bones)[2] )
    #
    # # bone_distances = sapply()
    #
    # shape_proc_distance(bone_1, bone_2, type = "full")
    #
    # shape_proc_distance <- function (x, y, type = "full", reflect = FALSE){
    #   if (type == "full") {
    #     out <- sin(riemdist(x, y, reflect = reflect))
    #   }
    #   if (type == "partial") {
    #     out <- sqrt(2) * sqrt(abs(1 - cos(riemdist(x, y, reflect = reflect))))
    #   }
    #   if (type == "Riemannian") {
    #     out <- riemdist(x, y, reflect = reflect)
    #   }
    #   if (type == "sizeandshape") {
    #     out <- ssriemdist(x, y, reflect = reflect)
    #   }
    #   out
    # }
    #
    # shape_distance(bone_1, bone_2, type= "full")

# ELASTIC DISTANCE FUNCTION DEVELOPMENT
# -----------------------------------------------

  elastic.distance(bone_1, bone_2, 1:dim(bone_1)[1])
