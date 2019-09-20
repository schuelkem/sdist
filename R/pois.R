#' The Poisson Distribution
#'
#' @description Descriptive statistics for the Poisson distribution with parameter lambda.
#'
#' @param lambda mean (non-negative)
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Poisson}}
#'
#'     \url{https://en.wikipedia.org/wiki/Poisson_distribution}
#'
#' @examples
#' support <- 0:20
#' plot(support, dpois(support, 1))
#' spois(1)
#'
#' support <- 0:20
#' plot(support, dpois(support, 4))
#' spois(4)
#'
#' support <- 0:20
#' plot(support, dpois(support, 10))
#' spois(10)
spois <- function(lambda, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = lambda),
         "median" = c("median" = lambda + 1 / 3 - 0.02 / lambda),
         "sd"     = c("sd" = sqrt(spois(lambda, "var"))),
         "var"    = c("var" = lambda),
         "skew"   = c("skew" = lambda^(-1 / 2)),
         "kurt"   = c("kurt" = 1 / lambda),
         "all"    = {
           all <- rlang::eval_tidy(formals(spois)[["statistic"]])[-1]
           vapply(all, function(statistic, lambda) spois(lambda, statistic), numeric(1), lambda)
         }
  )
}
