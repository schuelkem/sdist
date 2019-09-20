#' The Binomial Distribution
#'
#' @description Descriptive statistics for the Binomial distribution with parameters size and prob.
#'
#' @param size number of trials (zero or more).
#' @param prob probability of success on each trial.
#' @param statistic desired descriptive statistic
#'
#' @return named numeric vector
#' @export
#'
#' @seealso
#'     \code{\link[stats]{Binomial}}
#'
#'     \url{https://en.wikipedia.org/wiki/Binomial_distribution}
#'
#' @examples
#' support <- 0:2
#' plot(support, dbinom(support, 2, 1/2))
#' sbinom(2, 1/2)
#'
#' support <- 0:20
#' plot(support, dbinom(support, 20, 1/2))
#' sbinom(20, 1/2)
#'
#' support <- 0:20
#' plot(support, dbinom(support, 20, 0.7))
#' sbinom(20, 0.7)
#'
#' support <- 0:40
#' plot(support, dbinom(support, 40, 0.5))
#' sbinom(40, 0.5)
sbinom <- function(size, prob, statistic = c("all", "mean", "median", "sd", "var", "skew", "kurt")) {
statistic <- match.arg(statistic)

switch(statistic,
       "mean"   = c("mean" = size * prob),
       "median" = c("median" = sbinom(size, prob, "mean")),
       "sd"     = c("sd" = sqrt(sbinom(size, prob, "var"))),
       "var"    = c("var" = size * prob * (1 - prob)),
       "skew"   = c("skew" = (1 - 2 * prob) / (sqrt(size * prob * (1 - prob)))),
       "kurt"   = c("kurt" = (1 - 6 * prob * (1 - prob)) / (size * prob * (1 - prob))),
       "all"    = {
         all <- rlang::eval_tidy(formals(sbinom)[["statistic"]])[-1]
         vapply(all, function(statistic, size, prob) sbinom(size, prob, statistic), numeric(1), size, prob)
       }
  )
}
