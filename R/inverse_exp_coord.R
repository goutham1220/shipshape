#' Main internal function for calculating Elastic Distance

#' Adapted from "fdasrvf" package by  J. Derek Tucker <jdtuck@sandia.gov>
#'
#' @author Goutham Swaminathan \email{goutham1220@gmail.com}
#'
#' @param beta1 first curve
#'
#' @param beta2 second curve
#'
#' @param mode open curve ("O") or closed curve("C")
#'
#' @param rotated account for rotation
#'
#' @export
#'
#' @import fdasrvf
#'
#' @importFrom utils getFromNamespace
#'

inverse_exp_coord <- function (beta1, beta2, mode = "O", rotated = T)
{

  calculatecentroid = getFromNamespace("calculatecentroid", "fdasrvf")
  find_rotation_seed_coord = getFromNamespace("find_rotation_seed_coord", "fdasrvf")
  group_action_by_gamma_coord = getFromNamespace("group_action_by_gamma_coord", "fdasrvf")
  innerprod_q2 = getFromNamespace("innerprod_q2", "fdasrvf")
  project_curve = getFromNamespace("project_curve", "fdasrvf")
  shift_f = getFromNamespace("shift_f", "fdasrvf")

  T1 = ncol(beta1)
  centroid1 = calculatecentroid(beta1)
  dim(centroid1) = c(length(centroid1), 1)
  beta1 = beta1 - repmat(centroid1, 1, T1)
  centroid2 = calculatecentroid(beta2)
  dim(centroid2) = c(length(centroid2), 1)
  beta2 = beta2 - repmat(centroid2, 1, T1)
  q1 = curve_to_q(beta1)
  if (mode == "C") {
    isclosed = TRUE
  }
  out = reparam_curve(beta1, beta2, rotated = rotated, isclosed = isclosed,
                      mode = mode)
  if (mode == "C")
    beta2 = shift_f(beta2, out$tau)
  beta2 = out$R %*% beta2
  gamI = invertGamma(out$gam)
  beta2 = group_action_by_gamma_coord(beta2, gamI)
  if (rotated) {
    out = find_rotation_seed_coord(beta1, beta2, mode)
    q2n = curve_to_q(out$beta2new)
  }
  else {
    q2n = curve_to_q(beta2)
  }
  if (mode == "C") {
    q2n = project_curve(q2n)
  }
  q1dotq2 = innerprod_q2(q1, q2n)
  if (q1dotq2 > 1) {
    q1dotq2 = 1
  }
  dist = acos(q1dotq2)
  u = q2n - q1dotq2 * q1
  normu = sqrt(innerprod_q2(u, u))
  if (normu > 1e-04) {
    v = u * acos(q1dotq2)/normu
  }
  else {
    v = matrix(0, nrow(beta1), T1)
  }
  return(list(v = v, dist = dist))
}
