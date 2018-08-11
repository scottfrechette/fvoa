weight_games <- function(x, reg_games = 6) {

  # Slightly penalize outliers
  bottom_outlier <- quantile(x, 0.25) - IQR(x) * 1.25
  top_outlier <- quantile(x, 0.75) + IQR(x) * 1.25
  outlier_weights <- ifelse(x < bottom_outlier, 1 - (bottom_outlier - x)/bottom_outlier,
                            ifelse(x > top_outlier, 1 - (x - top_outlier)/top_outlier, 1))

  # Add extra term for regression to mean weeks
  if (length(x) < reg_games) {outlier_weights <- c(outlier_weights, 1)}

  # Make sure no weight is negative due to extreme score
  outlier_weights <- ifelse(outlier_weights <= 0, 0.1, outlier_weights)


  if (length(x) < reg_games) {

    weekly_weights <- replicate(length(x), NA)
    uniform_weights <- rep(((10 - reg_games + length(x))/10)/length(x), length(x))
    if(length(x) %% 2 == 0) { #even
      index <- length(x)/2
      for (i in 1:length(x)) {
        weekly_weights[i] <- ifelse(i <= index, uniform_weights - ((index+1)-i)* .01, uniform_weights + (i-(index))* .01) # fix
      }
    } else { #odd
      index <- (length(x)+1)/2
      for (i in 1:length(x)) {
        if (i != index) {
          weekly_weights[i] <- ifelse(i < index, uniform_weights - (index-i)*.01, uniform_weights + (i-index)*.01)
        } else weekly_weights[i] <- uniform_weights[i]
      }
    }

    weekly_weights <- c(weekly_weights, (reg_games - length(x))/10)
    weekly_weights <- ifelse(weekly_weights < 0, 0, weekly_weights)

  } else {
    # Give more weight to recent games
    # Extra weight increases throughout season
    weekly_weights <- replicate(length(x), NA)
    uniform_weights <- 1/length(x)
    if(length(x) %% 2 == 0) { #even
      index <- length(x)/2
      for (i in 1:length(x)) {
        weekly_weights[i] <- ifelse(i <= index, uniform_weights - ((index+1)-i)* .01, uniform_weights + (i-(index))* .01) # fix
      }
    } else { #odd
      index <- (length(x)+1)/2
      for (i in 1:length(x)) {
        if (i != index) {
          weekly_weights[i] <- ifelse(i < index, uniform_weights - (index-i)*.01, uniform_weights + (i-index)*.01)
        } else {weekly_weights[i] <- uniform_weights}
      }
    }
  }

  combined_weights <- outlier_weights * weekly_weights
  combined_weights <- combined_weights/sum(combined_weights)

  ifelse(combined_weights < 0, 0, combined_weights)
}

weighted_sd <- function (x, weights = NULL, normwt = FALSE,
                         na.rm = TRUE, method = c("unbiased", "ML")) {
  method <- match.arg(method)
  if (!length(weights)) {
    if (na.rm)
      x <- x[!is.na(x)]
    return(sd(x))
  }
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  if (normwt || method == "ML")
    return(as.numeric(sqrt(stats::cov.wt(cbind(x), weights, method = method)$cov)))
  sw <- sum(weights)
  if (sw <= 1)
    warning("only one effective observation; variance estimate undefined")
  xbar <- sum(weights * x)/sw
  sqrt(sum(weights * ((x - xbar)^2))/(sw - 1))
}

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

format_pct <- function (x) {
  paste0(round(x * 100, 1), "%")
} # convert number to percent format

count_to_pct <- function(df, ..., col = n) {

  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)

  df %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()

} # convert count to percent (can group)

wide_to_long <- function(df, new, ...) {

  new_name = enquo(new) %>% quo_name()
  old_cols = quos(...)

  df %>%
    unite(tmp, !!!old_cols) %>%
    mutate(tmp = map(tmp, strsplit, "_")) %>%
    unnest() %>%
    unnest() %>%
    mutate(tmp = factor(tmp)) %>%
    rename(!!new_name := tmp)

}

long_to_wide <- function(df, old, ..., sep = ",")  {

  old_col = enquo(old)
  new_col = quo_name(old_col)
  grouping = quos(...)


  df  %>%
    group_by(!!!grouping) %>%
    summarise(tmp = paste(!!old_col, collapse = sep)) %>%
    ungroup() %>%
    separate(tmp, into = paste0(new_col, 1:length(grouping)), sep = sep) %>%
    mutate_if(is.character, factor)

}

select_rankings <- function(df, ...) {
  columns <- quos(...)

  if(length(columns) == 0) {
    z
  } else {
    z %>%
      select(!!! columns)
  }
}