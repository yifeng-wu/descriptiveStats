################################################################################
#
# all descriptive statistics functions
#
################################################################################


#' Calculate arithmetic mean
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_mean(c(1:10))
#'
calc_mean <- function(x) {
  mean(x)
}

#' Calculate median
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_mean(c(1:10))
calc_median <- function(x) {
  median(x)
}

#' Calculate mode (handle ties and no mode cases)
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_mode(c(1:10))
calc_mode <- function(x) {
  mode(x)
}

#' Calculate first quartile (Q1)
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_q1(c(1:10))
calc_q1 <- function(x) {
  quantile(x, .25)
}

#' Calculate third quartile (Q3)
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_q3(c(1:10))
calc_q3 <- function(x) {
  quantile(x, .75)
}

#' Calculate Interquartile Range
#'
#' @param x A numeric vector
#'
#' @returns A scalar
#' @export
#'
#' @examples
#' calc_iqr(c(1:10))
calc_iqr <- function(x) {
  quantile(x, .75)
}
