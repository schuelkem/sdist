#' The Exponential Distribution
#'
#' @description Descriptive statistics for the exponential distribution with rate rate (i.e., mean 1/rate).
#'
#' @param rate rate parameter
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Exponential}}
#'
#'     \url{https://en.wikipedia.org/wiki/Exponential_distribution}
#'
#' @examples
#' curve(dexp(x, 0.5), xlim = c(0, 5))
#' sexp(0.5)
#'
#' curve(dexp(x, 1), xlim = c(0, 5))
#' sexp()
#'
#' curve(dexp(x, 1.5), xlim = c(0, 5))
#' sexp(1.5)
sexp <- function(rate = 1, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = 1 / rate),
         "median" = c("median" = rate^(-1) * log(2)),
         "sd"     = c("sd" = sqrt(sexp(rate, "var"))),
         "var"    = c("var" = rate^(-2)),
         "skew"   = c("skew" = 2),
         "kurt"   = c("kurt" = 6),
         "all"    = {
           all <- rlang::eval_tidy(formals(sexp)[["statistic"]])[-1]
           vapply(all, function(statistic, rate) sexp(rate, statistic), numeric(1), rate)
         }
  )
}
