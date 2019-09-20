#' The Uniform Distribution
#'
#' @description Descriptive statistics for the uniform distribution on the interval from min to max.
#'
#' @param min,max lower and upper limits of the distribution. Must be finite.
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Uniform}}
#'
#'     \url{https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)}
#'
#' @examples
#' curve(dunif(x))
#' sunif()
sunif <- function(min = 0, max = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = (min + max) / 2),
         "median" = c("median" = sunif(min, max, "mean")),
         "sd"     = c("sd" = sqrt(sunif(min, max, "var"))),
         "var"    = c("var" = (1 / 12) * (max - min)^2),
         "skew"   = c("skew" = 0),
         "kurt"   = c("kurt" = -6 / 5),
         "all"    = {
           all <- rlang::eval_tidy(formals(sunif)[["statistic"]])[-1]
           vapply(all, function(statistic, min, max) sunif(min, max, statistic), numeric(1), min, max)
         }
  )
}
