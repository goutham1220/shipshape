library(fdasrvf)

bone_shapes = bone_list$x

bone_shapes = cbind(bone_shapes, bone_list$y)

bone_1 = bone_shapes[1:100,]
bone_2 = bone_shapes[101:200,]
bone_3 = bone_shapes[201:300,]

bones = array(c(bone_1, bone_2, bone_3), dim = c(100, 2, 3))

shape_elastic_distance = function(x, mode = "C"){

  distance = vector()

  for(i in 1:dim(bones)[3]){

    x_ref = combn(dim(x)[3], 2)[1,i]
    y_ref = combn(dim(x)[3], 2)[2,i]

    f1 = x[,,x_ref]
    f2 = x[,,y_ref]

    t(f1)

    fxn = fdasrvf:::inverse_exp_coord(t(f1), t(f2), "C")

    distances = c(distances, fxn$dist)

    names(distances)[i] = paste(x_ref, y_ref, sep = " ")
    }

  return(distances$dist)
}

shape_elastic_distance(bones, mode = "C")


