#' riemdist
#'
#' @param x x
#'
#' @param y y
#'
#' @param reflect reflect
#'
#' @export
#'

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

#' ssriemdist
#'
#' @param x x
#' @param y y
#' @param reflect reflect
#'
#' @export
#'

ssriemdist <- function (x, y, reflect = FALSE)
{
  sx <- centroid.size(x)
  sy <- centroid.size(y)
  sd <- sx^2 + sy^2 - 2 * sx * sy * cos(riemdist(x, y, reflect = reflect))
  sqrt(abs(sd))
}

#' preshape
#'
#' @param x x
#'
#' @export
#'


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

#' realtocomplex
#'
#' @param x x
#'
#' @export
#'

realtocomplex = function (x){
  k <- nrow(x)
  zstar <- x[, 1] + (0+1i) * x[, 2]
  zstar
}

#' Enormalize
#'
#' @param x x
#'
#' @export
#'

Enormalize = function (x){
  return(x/Enorm(x))
}

#' Enorm
#'
#' @param X X
#'
#' @export
#'

Enorm = function (X){
  if (is.complex(X)) {
    n <- sqrt(Re(c(st(X) %*% X)))
  }
  else {
    n <- sqrt(sum(diag(t(X) %*% X)))
  }
  n
}

#' st
#'
#' @param zstar zstar
#'
#' @export
#'

st = function (zstar)
{
  st <- t(Conj(zstar))
  st
}

#' defh
#'
#' @param nrow nrow
#'
#' @export
#'

defh = function (nrow)
{
  k <- nrow
  h <- matrix(0, k, k + 1)
  j <- 1
  while (j <= k) {
    jj <- 1
    while (jj <= j) {
      h[j, jj] <- -1/sqrt(j * (j + 1))
      jj <- jj + 1
    }
    h[j, j + 1] <- j/sqrt(j * (j + 1))
    j <- j + 1
  }
  h
}

#' centroid.size
#'
#' @param x x
#'
#' @export
#'

centroid.size = function (x)
{
  if ((is.vector(x) == FALSE) && is.complex(x)) {
    k <- nrow(x)
    n <- ncol(x)
    tem <- array(0, c(k, 2, n))
    tem[, 1, ] <- Re(x)
    tem[, 2, ] <- Im(x)
    x <- tem
  }
  {
    if (length(dim(x)) == 3) {
      n <- dim(x)[3]
      sz <- rep(0, times = n)
      k <- dim(x)[1]
      h <- defh(k - 1)
      for (i in 1:n) {
        xh <- h %*% x[, , i]
        sz[i] <- sqrt(sum(diag(t(xh) %*% xh)))
      }
      sz
    }
    else {
      if (is.vector(x) && is.complex(x)) {
        x <- cbind(Re(x), Im(x))
      }
      k <- nrow(x)
      h <- defh(k - 1)
      xh <- h %*% x
      size <- sqrt(sum(diag(t(xh) %*% xh)))
      size
    }
  }
}
