
library(gtools)

testarr = array(1:27, dim = c(25, 2, 3))
testarr2 = array(1:27, dim = c(25, 2, 3))

outer(testarr, testarr, FUN = procdist)

nrow(combinations(dim(testarr)[3], 2))

shape_distance <- function (x, type = "full", reflect = FALSE){
  if (type == "full") {
    out <- vector()

    for(i in 1:nrow(combinations(dim(x)[3], 2)))
  }
  if (type == "partial") {
    out <- sqrt(2) * sqrt(abs(1 - cos(riemdist(x, y, reflect = reflect))))
  }
  if (type == "Riemannian") {
    out <- riemdist(x, y, reflect = reflect)
  }
  if (type == "sizeandshape") {
    out <- ssriemdist(x, y, reflect = reflect)
  }
  out
}

