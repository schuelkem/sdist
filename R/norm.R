#' The Normal Distribution
#'
#' @description Descriptive statistics for the normal distribution with mean equal to mean and standard deviation equal to sd.
#'
#' @param mean mean
#' @param sd standard deviation
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Normal}}
#'
#'     \url{https://en.wikipedia.org/wiki/Normal_distribution}
#'
#' @examples
#' curve(dnorm(x, 0, 0.2), xlim = c(-5, 5))
#' snorm(0, 0.2)
#'
#' curve(dnorm(x), xlim = c(-5, 5))
#' snorm()
#'
#' curve(dnorm(x, 0, 5), xlim = c(-5, 5))
#' snorm(0, 5)
#'
#' curve(dnorm(x, -2, 0.5), xlim = c(-5, 5))
#' snorm(-2, 0.5)
snorm <- function(mean = 0, sd = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = mean),
         "median" = c("median" = snorm(mean, sd, "mean")),
         "sd"     = c("sd" = sd),
         "var"    = c("var" = sd^2),
         "skew"   = c("skew" = 0),
         "kurt"   = c("kurt" = 0),
         "all"    = {
           all <- rlang::eval_tidy(formals(snorm)[["statistic"]])[-1]
           vapply(all, function(statistic, mean, sd) snorm(mean, sd, statistic), numeric(1), mean, sd)
         }
  )
}
