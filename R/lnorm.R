#' The Lognormal Distribution
#'
#' @description Descriptive statistics for the log normal distribution whose logarithm has mean equal to meanlog and standard deviation equal to sdlog.
#'
#' @param meanlog,sdlog mean and standard deviation of the distribution on the log scale with default values of 0 and 1 respectively.
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Lognormal}}
#'
#'     \url{https://en.wikipedia.org/wiki/Log-normal_distribution}
#'
#' @examples
#' curve(dlnorm(x, 0, 0.25), xlim = c(0, 2.5))
#' slnorm(0, 0.25)
#'
#' curve(dlnorm(x, 0, 0.5), xlim = c(0, 2.5))
#' slnorm(0, 0.5)
#'
#' curve(dlnorm(x), xlim = c(0, 2.5))
#' slnorm()
slnorm <- function(meanlog = 0, sdlog = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = exp(meanlog + sdlog^2 / 2)),
         "median" = c("median" = exp(meanlog)),
         "sd"     = c("sd" = sqrt(slnorm(meanlog, sdlog, "var"))),
         "var"    = c("var" = (exp(sdlog^2) - 1) * exp(2 * meanlog + sdlog^2)),
         "skew"   = c("skew" = (exp(sdlog^2) + 2) * sqrt(exp(sdlog^2) - 1)),
         "kurt"   = c("kurt" = exp(4 * sdlog^2) + 2 * exp(3 * sdlog^2) + 3 * exp(2 * sdlog^2) - 6),
         "all"    = {
           all <- rlang::eval_tidy(formals(slnorm)[["statistic"]])[-1]
           vapply(all, function(statistic, meanlog, sdlog) slnorm(meanlog, sdlog, statistic), numeric(1), meanlog, sdlog)
         }
  )
}
