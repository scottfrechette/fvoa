#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


prob_to_odds <- function(x) {
  # convert decimal to whole number
  if (x < 1) {
    x <- x * 100
  }

  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }

  # convert to American odds
  x <- 100/x

  # convert favorite to American odds
  if (x >= 2 & !is.na(x)) {
    x <- (x - 1)* 100
  }
  # convert underdog to American odds
  if (x < 2 & !is.na(x)) {
    x <- (-100) / (x - 1)
  }

  # round to whole number
  x <- round(x * 0.04)/0.04

  if (is.na(x)) {
    NA
  } else if (x < 0) {
    paste(x)
  } else {
    paste0("+", x)
  }
} # Convert win percentage dataframe to winning odds

odds_to_prob <- function(x) {
  x <- gsub("\\+", "", x)
  x <- as.numeric(x)
  if (is.na(x)){
    x <- NA
  } else if (x > 0) {
    x <- x/100 + 1
  } else {
    x <- (-100 / x) + 1
  }
  x <- 1/x
  convert_percent(x)
} # convert odds to win percentage

format_pct <- function (x, accuracy = 1) {
  paste0(round(x * 100, accuracy), "%")
} # convert number to percent format

count_to_pct <- function(df, ..., col = n) {

  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)

  df %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()

} # convert count to percent (can group)

select_rankings <- function(df, ...) {
  columns <- quos(...)

  if(length(columns) == 0) {
    z
  } else {
    z %>%
      select(!!! columns)
  }
}
