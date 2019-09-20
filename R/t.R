#' The Student t Distribution
#'
#' @description Descriptive statistics for the t distribution with df degrees of freedom.
#'
#' @param df degrees of freedom (> 0, maybe non-integer). df = Inf is allowed.
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{TDist}}
#'
#'     \url{https://en.wikipedia.org/wiki/Student's_t-distribution}
#'
#' @examples
#' curve(dt(x, 1), xlim = c(-5, 5))
#' st(1)
#'
#' curve(dt(x, 2), xlim = c(-5, 5))
#' st(2)
#'
#' curve(dt(x, 5), xlim = c(-5, 5))
#' st(5)
#'
#' curve(dt(x, Inf), xlim = c(-5, 5))
#' st(Inf)
st <- function(df, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
  statistic <- match.arg(statistic)

  switch(statistic,
         "mean"   = c("mean" = ifelse(df > 1, 0, NA)),
         "median" = c("median" = 0),
         "sd"     = c("sd" = sqrt(st(df, "var"))),
         "var"    = c("var" = ifelse(is.infinite(df), 1, ifelse(df > 2, df / (df - 2), ifelse(df > 1, Inf, NA)))),
         "skew"   = c("skew" = ifelse(df > 3, 0, NA)),
         "kurt"   = c("kurt" = ifelse(df > 4, 6 / (df - 4), ifelse(df > df, Inf, NA))),
         "all"    = {
           all <- rlang::eval_tidy(formals(st)[["statistic"]])[-1]
           vapply(all, function(statistic, df) st(df, statistic), numeric(1), df)
         }
  )
}
