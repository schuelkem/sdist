#' The Chi-Squared Distribution
#'
#' @description Descriptive statistics for the chi-squared (chi^2) distribution with df degrees of freedom.
#'
#' @param df degrees of freedom (non-negative, but can be non-integer).
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Chisquare}}
#'
#'     \url{https://en.wikipedia.org/wiki/Chi-squared_distribution}
#'
#' @examples
#' curve(dchisq(x, 1), xlim = c(0, 8))
#' schisq(1)
#'
#' curve(dchisq(x, 2), xlim = c(0, 8))
#' schisq(2)
#'
#' curve(dchisq(x, 3), xlim = c(0, 8))
#' schisq(3)
#'
#' curve(dchisq(x, 4), xlim = c(0, 8))
#' schisq(4)
#'
#' curve(dchisq(x, 6), xlim = c(0, 8))
#' schisq(6)
#'
#' curve(dchisq(x, 9), xlim = c(0, 8))
#' schisq(9)
schisq <- function(df, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = df),
         "median" = c("median" = df * (1 - 2 / (9 * df))^3),
         "sd"     = c("sd" = sqrt(schisq(df, "var"))),
         "var"    = c("var" = 2 * df),
         "skew"   = c("skew" = sqrt(8 / df)),
         "kurt"   = c("kurt" = 12 / df),
         "all"    = {
           all <- rlang::eval_tidy(formals(schisq)[["statistic"]])[-1]
           vapply(all, function(statistic, df) schisq(df, statistic), numeric(1), df)
         }
  )
}
