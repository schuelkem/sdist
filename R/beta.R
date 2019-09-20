#' The Beta Distribution
#'
#' @description Descriptive statistics for the Beta distribution with parameters shape1 and shape2.
#'
#' @param shape1,shape2 non-negative parameters of the Beta distribution
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Beta}}
#'
#'     \url{https://en.wikipedia.org/wiki/Beta_distribution}
#'
#' @examples
#' # Jeffreys' Prior
#' curve(dbeta(x, 0.5, 0.5))
#' sbeta(0.5, 0.5)
#'
#' curve(dbeta(x, 5, 1))
#' sbeta(5, 1)
#'
#' curve(dbeta(x, 1, 3))
#' sbeta(1, 3)
#'
#' curve(dbeta(x, 2, 2))
#' sbeta(2, 2)
#'
#' curve(dbeta(x, 2, 5))
#' sbeta(2, 5)
#'
#' # Bayesian prior
#' curve(dbeta(x, 1, 1))
#' sbeta(1, 1)
sbeta <- function(shape1, shape2, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = shape1 / (shape1 + shape2)),
         "median" = c("median" = stats::qbeta(0.5, shape1, shape2)),
         "sd"     = c("sd" = sqrt(sbeta(shape1, shape2, "var"))),
         "var"    = c("var" = (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))),
         "skew"   = c("skew" = (2 * (shape2 - shape1) * sqrt(shape1 + shape2 + 1)) / ((shape1 + shape2 + 2) * sqrt(shape1 * shape2))),
         "kurt"   = c("kurt" = (6 * ((shape1 - shape2)^2 * (shape1 + shape2 + 1) - shape1 * shape2 * (shape1 + shape2 + 2))) / (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))),
         "all"    = {
           all <- rlang::eval_tidy(formals(sbeta)[["statistic"]])[-1]
           vapply(all, function(statistic, shape1, shape2) sbeta(shape1, shape2, statistic), numeric(1), shape1, shape2)
         }
  )
}
