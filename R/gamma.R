#' The Gamma Distribution
#'
#' @description Descriptive statistics for the Gamma distribution with parameters shape and rate.
#'
#' @param shape shape parameter. Must be positive
#' @param rate rate parameter. Must be positive
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{GammaDist}}
#'
#'     \url{https://en.wikipedia.org/wiki/Gamma_distribution}
#'
#' @examples
#' curve(dgamma(x, 1, 1/2), xlim = c(0, 20))
#' sgamma(1, 1/2)
#'
#' curve(dgamma(x, 2, 1/2), xlim = c(0, 20))
#' sgamma(2, 1/2)
#'
#' curve(dgamma(x, 3, 1/2), xlim = c(0, 20))
#' sgamma(3, 1/2)
#'
#' curve(dgamma(x, 5, 1), xlim = c(0, 20))
#' sgamma(5, 1)
#'
#' curve(dgamma(x, 9, 2), xlim = c(0, 20))
#' sgamma(9, 2)
#'
#' curve(dgamma(x, 7.5, 1), xlim = c(0, 20))
#' sgamma(7.5, 1)
#'
#' curve(dgamma(x, 0.5, 1), xlim = c(0, 20))
#' sgamma(0.5, 1)
sgamma <- function(shape, rate = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = shape / rate),
         "median" = c("median" = stats::qgamma(0.5, shape, rate)),
         "sd"     = c("sd" = sqrt(sgamma(shape, rate, "var"))),
         "var"    = c("var" = shape / rate^2),
         "skew"   = c("skew" = 2 / sqrt(shape)),
         "kurt"   = c("kurt" = 6 / shape),
         "all"    = {
           all <- rlang::eval_tidy(formals(sgamma)[["statistic"]])[-1]
           vapply(all, function(statistic, shape, rate) sgamma(shape, rate, statistic), numeric(1), shape, rate)
         }
  )
}
