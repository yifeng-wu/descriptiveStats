################################################################################
#
# all descriptive statistics functions
#
################################################################################



#' Validate Numeric Input
#'
#' Internal helper to validate input vector.
#'
#' @param x A numeric vector.
#' @return A numeric vector with NA removed.
#' @keywords internal
validate_numeric <- function(x) {

  if (missing(x)) {
    cli::cli_abort(
      "{.arg x} is missing. Please supply a numeric vector.",
      class = "descriptiveStats_missing_input"
    )
  }

  if (!is.numeric(x)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a numeric vector.",
        "i" = "You supplied an object of class {.cls {class(x)}}."
      ),
      class = "descriptiveStats_invalid_input"
    )
  }

  if (length(x) == 0) {
    cli::cli_abort(
      "{.arg x} is an empty vector.",
      class = "descriptiveStats_empty_input"
    )
  }

  # Count NA values
  na_count <- sum(is.na(x))

  if (na_count > 0) {
    cli::cli_inform(
      c(
        "{.arg x} contains {na_count} missing value{?s}.",
        "!" = "{na_count} missing value{?s} will be removed before calculation."
        ),
      class = "descriptiveStats_na_removed"
    )
  }



  # Remove NA values
  x_clean <- x[!is.na(x)]

  # Abort if nothing remains
  if (length(x_clean) == 0) {
    cli::cli_abort(
      c(
        "All values in {.arg x} are missing.",
        "x" = "No non-missing values available for calculation."
      ),
      class = "descriptiveStats_all_na"
    )
  }

  x_clean
}

#-------------------------------------------------------------------------------
#' Calculate Arithmetic Mean
#'
#' Computes the arithmetic mean of a numeric vector
#'
#' @param x A numeric vector
#'
#' @returns A scalar numeric value representing the mean.
#' @export
#' @examples
#' # Basic usage
#' calc_mean(c(1,2,3))
#' calc_mean(c(1,2,3,4))
#'
#' # NA values are removed with explicit information
#' calc_mean(c(1,2,3, NA))
#'
#' # All values missing -> error
#' \dontrun{
#' calc_mean(c(NA, NA, NA))
#' }
#'
#' # Non-numeric input -> error
#' \dontrun{
#' calc_mean("a")
#' }
calc_mean <- function(x) {
  x <- validate_numeric(x)
  sum(x) / length(x)
}

#-------------------------------------------------------------------------------
#' Calculate Median
#'
#' Computes the median of a numeric vector.
#'
#' @param x A numeric vector
#'
#' @returns A scalar numeric value representing the median
#' @export
#' @examples
#' # Basic usage
#' calc_median(c(1,2,3))
#' calc_median(c(1,2,3,4))
#'
#' # NA values are removed with explicit information
#' calc_median(c(1,2,3, NA))
#'
#' # All values missing -> error
#' \dontrun{
#' calc_median(c(NA, NA, NA))
#' }
#'
#' # Non-numeric input -> error
#' \dontrun{
#' calc_median("a")
#' }
calc_median <- function(x) {
  x <- validate_numeric(x)
  stats::median(x)
}

#-------------------------------------------------------------------------------
#' Calculate Mode
#'
#' Computes the statistical mode (most frequent value).
#'
#' @param x A numeric vector
#'
#' @returns A numeric vector of modal value(s).
#' @export
#'
#' @examples
#' # Single mode
#' calc_mode(c(1, 2, 2, 3))
#'
#' # Multiple modes (tie)
#' calc_mode(c(1, 1, 2, 2))
#'
#' # All values unique (all returned)
#' calc_mode(c(1, 2, 3))
#'
#' # NA handling
#' calc_mode(c(1, 2, 2, NA))
#'
#' # Invalid input -> error
#' \dontrun{
#' calc_mode("a")
#' }
calc_mode <- function(x) {
  x <- validate_numeric(x)

  freq_table <- table(x)
  max_freq <- max(freq_table)

  modes <- as.numeric(names(freq_table)[freq_table == max_freq])

  modes
}

#-------------------------------------------------------------------------------
#' Calculate First Quartile (Q1)
#'
#' Computes the first quartile (25th percentile).
#'
#' @param x A numeric vector
#'
#' @returns A scalar numeric value representing Q1
#' @export
#'
#' @examples
#' # Basic usage
#' calc_q1(c(1,2,3))
#' calc_q1(c(1,2,3,4))
#'
#' # NA values are removed with explicit information
#' calc_q1(c(1,2,3, NA))
#'
#' # All values missing -> error
#' \dontrun{
#' calc_q1(c(NA, NA, NA))
#' }
#'
#' # Non-numeric input -> error
#' \dontrun{
#' calc_q1("a")
#' }
calc_q1 <- function(x) {
  x <- validate_numeric(x)
  stats::quantile(x, .25, name = FALSE)
}

#-------------------------------------------------------------------------------
#' Calculate Third Quartile (Q3)
#'
#' Computes the third quartile (75th percentile).
#'
#' @param x A numeric vector
#'
#' @returns A scalar numeric value representing Q3
#' @export
#'
#' @examples
#' # Basic usage
#' calc_q3(c(1,2,3))
#' calc_q3(c(1,2,3,4))
#'
#' # NA values are removed with explicit information
#' calc_q3(c(1,2,3, NA))
#'
#' # All values missing -> error
#' \dontrun{
#' calc_q3(c(NA, NA, NA))
#' }
#'
#' # Non-numeric input -> error
#' \dontrun{
#' calc_q3("a")
#' }
calc_q3 <- function(x) {
  x <- validate_numeric(x)
  stats::quantile(x, .75, name = FALSE)
}

#-------------------------------------------------------------------------------
#' Calculate Interquartile Range (IQR)
#'
#' Computes the interquartile range (Q3 - Q1).
#'
#' @param x A numeric vector
#'
#' @returns A scalar numeric value representing IQR
#' @export
#'
#' @examples
#' # Basic usage
#' calc_iqr(c(1,2,3))
#' calc_iqr(c(1,2,3,4))
#'
#' # NA values are removed with explicit information
#' calc_iqr(c(1,2,3, NA))
#'
#' # All values missing -> error
#' \dontrun{
#' calc_iqr(c(NA, NA, NA))
#' }
#'
#' # Non-numeric input -> error
#' \dontrun{
#' calc_iqr("a")
#' }
calc_iqr <- function(x) {
  x <- validate_numeric(x)
  stats::IQR(x)
}
