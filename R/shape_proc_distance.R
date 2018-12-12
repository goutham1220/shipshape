library(shapes)
library(fdasrvf)

# Procrustes Distance

bone_shapes = bone_list$x

bone_shapes = cbind(bone_shapes, bone_list$y)

bone_1 = bone_shapes[1:100,]
bone_2 = bone_shapes[101:200,]
bone_3 = bone_shapes[201:300,]

bones = array(c(bone_1, bone_2, bone_3), dim = c(100, 2, 3))

dists = sapply(x = combinations(3, 2, names(bones))[1], procdist, combinations(3, 2, bones)[2] )

bone_distances = sapply()

shape_proc_distance <- function (x, y, type = "full", reflect = FALSE){
  if (type == "full") {
    out <- sin(riemdist(x, y, reflect = reflect))
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

shape_distance(bone_1, bone_2, type= "full")

# riemdist

riemdist <- function (x, y, reflect = FALSE){
  if (sum((x - y)^2) == 0) {
    riem <- 0
  }
  if (sum((x - y)^2) != 0) {
    if (reflect == FALSE) {
      if (ncol(as.matrix(x)) < 3) {
        if (is.complex(x) == FALSE) {
          x <- realtocomplex(x)
        }
        if (is.complex(y) == FALSE) {
          y <- realtocomplex(y)
        }
        riem <- c(acos(min(1, (Mod(st(preshape(x)) %*%
                                     preshape(y))))))
      }
      else {
        m <- ncol(x)
        z <- preshape(x)
        w <- preshape(y)
        Q <- t(z) %*% w %*% t(w) %*% z
        ev <- eigen(t(z) %*% w)$values
        check <- 1
        for (i in 1:m) {
          check <- check * ev[i]
        }
        ev <- sqrt(abs(eigen(Q, symmetric = TRUE)$values))
        if (Re(check) < 0)
          ev[m] <- -ev[m]
        riem <- acos(min(sum(ev), 1))
      }
    }
    if (reflect == TRUE) {
      m <- ncol(x)
      z <- preshape(x)
      w <- preshape(y)
      Q <- t(z) %*% w %*% t(w) %*% z
      ev <- sqrt(abs(eigen(Q, symmetric = TRUE)$values))
      riem <- acos(min(sum(ev), 1))
    }
  }
  riem
}

# preshape

preshape = function (x){
  if (is.complex(x)) {
    k <- nrow(as.matrix(x))
    h <- defh(k - 1)
    zstar <- x
    ztem <- h %*% zstar
    size <- sqrt(diag(Re(st(ztem) %*% ztem)))
    if (is.vector(zstar))
      z <- ztem/size
    if (is.matrix(zstar))
      z <- ztem %*% diag(1/size)
  }
  else {
    if (length(dim(x)) == 3) {
      k <- dim(x)[1]
      h <- defh(k - 1)
      n <- dim(x)[3]
      m <- dim(x)[2]
      z <- array(0, c(k - 1, m, n))
      for (i in 1:n) {
        z[, , i] <- h %*% x[, , i]
        size <- centroid.size(x[, , i])
        z[, , i] <- z[, , i]/size
      }
    }
    else {
      k <- nrow(as.matrix(x))
      h <- defh(k - 1)
      ztem <- h %*% x
      size <- centroid.size(x)
      z <- ztem/size
    }
  }
  z
}

# realtocomplex
realtocomplex = function (x){
  k <- nrow(x)
  zstar <- x[, 1] + (0+1i) * x[, 2]
  zstar
}

# Enormalize

Enormalize = function (x){
  return(x/Enorm(x))
}

# Enorm
Enorm = function (X){
  if (is.complex(X)) {
    n <- sqrt(Re(c(st(X) %*% X)))
  }
  else {
    n <- sqrt(sum(diag(t(X) %*% X)))
  }
  n
}
