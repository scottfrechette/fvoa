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

wide_to_long <- function(df, new, ...) {

  new_name = enquo(new) %>% quo_name()
  old_cols = quos(...)

  df %>%
    unite(tmp, !!!old_cols) %>%
    mutate(tmp = map(tmp, strsplit, "_")) %>%
    unnest(tmp) %>%
    unnest(tmp) %>%
    rename(!!new_name := tmp)

}

long_to_wide <- function(df, old, ..., sep = ",")  {

  old_col = enquo(old)
  new_col = quo_name(old_col)
  grouping = quos(...)


  df  %>%
    group_by(!!!grouping) %>%
    summarise(tmp = paste(!!old_col, collapse = sep),
              .groups = "drop") %>%
    ungroup() %>%
    separate(tmp, into = paste0(new_col, 1:length(grouping)), sep = sep)

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

slot_name_to_id <- function(x) {
  dplyr::case_when(
    x == "QB" ~ 0L,
    x == "TQB" ~ 1L, # team quarterback
    x == "RB" ~ 2L,
    x == "RB/WR" ~ 3L,
    x == "WR" ~ 4L,
    x == "WR/TE" ~ 5L,
    x == "TE" ~ 6L,
    x == "OP" ~ 7L, # offensive player
    x == "DT" ~ 8L,
    x == "DE" ~ 9L,
    x == "LB" ~ 10L,
    x == "DL" ~ 11L,
    x == "CB" ~ 12L,
    x == "S" ~ 13L,
    x == "DB" ~ 14L,
    x == "DP" ~ 15L, # defensive player
    x == "DST" ~ 16L,
    x == "K" ~ 17L,
    x == "P" ~ 18L,
    x == "HC" ~ 19L, # head coach
    x == "FLEX" ~ 23L,
    x == "EDR" ~ 24L,
    TRUE ~ NA_integer_
  )
}

slot_id_to_name <- function(x) {
  x <- as.numeric(x)
  dplyr::case_when(
    x == 0 ~ "QB",
    x == 1 ~ "TQB", # team quarterback
    x == 2 ~ "RB",
    x == 3 ~ "RB/WR",
    x == 4 ~ "WR",
    x == 5 ~ "WR/TE",
    x == 6 ~ "TE",
    x == 7 ~ "OP", # offensive player
    x == 8 ~ "DT",
    x == 9 ~ "DE",
    x == 10 ~ "LB",
    x == 11 ~ "DL",
    x == 12 ~ "CB",
    x == 13 ~ "S",
    x == 14 ~ "DB",
    x == 15 ~ "DP", # defensive player
    x == 16 ~ "DST",
    x == 17 ~ "K",
    x == 18 ~ "P",
    x == 19 ~ "HC",
    x == 23 ~ "FLEX",
    x == 24 ~ "EDR", # edge rusher
    TRUE ~ paste0("slot_", x)
  )
}

camel_to_snake <- function(x) {
  tolower(gsub("(?<!^)(?=[A-Z])", "_", x, perl = TRUE))
}

as_tibble_snake <- function(x) {
  df <- tibble::as_tibble(x)
  colnames(df) <- camel_to_snake(colnames(df))
  df
}

tidy_projections <- function(x) {
  # convert json to data frame
  x <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(x$players)))

  # convert column types
  x$id <- as.character(x$id)

  if ("waiverProcessDate" %in% names(x)) {
    x <- unnest(x, waiverProcessDate, keep_empty = T)

    x$waiverProcessDate <- as.POSIXct(x$waiverProcessDate/1000, origin = "1970-01-01", tz = "America/New_York")
  }

  # parse rating
  ratings <- purrr::map(x$ratings, tidy_projection_ratings)

  # parse player
  player <-  purrr::simplify_all(purrr::transpose(x$player))
  player$id <- as.character(player$id)

  player$team <- nfl_teamid_to_name(player$proTeamId)
  player$proTeamId <- NULL

  if ("lastNewsDate" %in% names(player)) {
    player$lastNewsDate <- list_to_dt(player$lastNewsDate)
  }

  if ("lastVideoDate" %in% names(player)) {
    player$lastVideoDate <- list_to_dt(player$lastVideoDate)
  }

  if ("injuryStatus" %in% names(player)) {
    player$injuryStatus <- purrr::simplify(tidyr::replace_na(player$injuryStatus, NA_character_))
  }

  if ("jersey" %in% names(player)) {
    player$jersey <- purrr::simplify(tidyr::replace_na(player$jersey, NA_character_))
  }

  if ("seasonOutlook" %in% names(player)) {
    if (!is.list(player$seasonOutlook)) {
      player$seasonOutlook <- dplyr::if_else(nchar(player$seasonOutlook) == 0, NA_character_, player$seasonOutlook)
    }
  }

  player$defaultPosition <- pos_id_to_name(player$defaultPositionId)
  player$defaultPositionId <- NULL

  player$stats <- purrr::map(player$stats, tidy_projection_stats)
  player$eligibleSlots <- purrr::simplify_all(player$eligibleSlots)
  player$eligibleSlots <- purrr::map(player$eligibleSlots, slot_id_to_name)

  player <- tibble::as_tibble(player)

  player$teamID <- x$onTeamId

  ownership <- lapply(player$ownership, tidyr::replace_na, NA_real_)
  ownership <- purrr::map(ownership, function(x) {
    if (is.list(x)) {
      x$date <- list_to_dt(x$date)
      return(x)
    } else {
      return(list())
    }

  })

  ownership <- purrr::map(ownership, as_tibble_snake)
  player$ownership <- ownership

  player <- unnest(player, ownership, keep_empty = TRUE)

  if ("rankings" %in% names(player)) {
    player$rankings <- purrr::map(player$rankings, tidy_projection_rankings)
  }

  player$draftRanksByRankType <- purrr::map(player$draftRanksByRankType, tidy_projection_draft_ranks)

  x <- player
  x <- dplyr::select(x, "id", "fullName", "team", "defaultPosition", dplyr::everything())
  x$ratings <- ratings

  colnames(x) <- camel_to_snake(colnames(x))
  x <- dplyr::select(
    x,
    "teamID" = "team_i_d",
    "playerID" = "id",
    "player" = "full_name",
    "position" = "default_position",
    "nfl_team" = "team",
    "is_active" = "active",
    "is_droppable" = "droppable",
    "is_injured" = "injured",
    "pct_owned" = "percent_owned",
    "pct_start" = "percent_started",
    "pct_change" = "percent_change",
    # "slots" = "eligible_slots",
    # "draft_ranks" = "draft_ranks_by_rank_type",
    "ratings",
    # "notes" = "season_outlook",
    "stats"
  )

  x
}

tidy_projection_ratings <- function(ratings) {
  if (is.null(ratings)) return(tibble::tibble())

  ratings <- purrr::simplify_all(purrr::transpose(ratings))
  df <- as_tibble_snake(ratings)
  df$week <- as.integer(names(ratings[[1]]))
  df <- dplyr::select(df, "week", dplyr::everything())
  df
}

tidy_projection_stats <- function(stats) {
  if (length(stats) == 0) return(tibble::tibble())

  if (!is.list(stats)) print(stats)
  fpts_proj <- stats[[1]]$appliedTotal

  stats <- stats[[1]]$stats
  names(stats) <- stat_id_to_name(names(stats))

  df <- as_tibble_snake(stats)
  df$fpts_proj <- fpts_proj

  df <- dplyr::select(df, -dplyr::starts_with("stat_"), dplyr::starts_with("stat_"))

  return(df)
}

tidy_projection_rankings <- function(x) {
  if (length(x) == 0) return(tibble::tibble())
  x <- x[[1]]
  df <- tibble::as_tibble(purrr::simplify_all(purrr::transpose(x)))
  colnames(df) <- camel_to_snake(colnames(df))
  df
}

tidy_projection_draft_ranks <- function(x) {
  if (length(x) == 0) return(tibble::tibble())
  purrr::map_dfr(x, as_tibble_snake)
}

is_scalar <- function(x) identical(length(x), 1L)

list_to_dt <- function(x) {
  x <- vapply(x, function(d) if (is.null(d)) NA_real_ else d/1000, double(1L))
  as.POSIXct(x, origin = "1970-01-01", tz = "America/New_York")
}

nfl_teamid_to_name <- function(id) {
  rows <- purrr::map_int(id, function(x) {
    matches <- x == nfl_teamIDs$id
    if (any(matches)) which(matches)[1]
    else NA_integer_
  })

  nfl_teamIDs$name[rows]
}

nfl_team_name_to_id <- function(name) {
  rows <- purrr::map_int(name, function(x) {
    matches <- x == nfl_teamIDs$name
    if (any(matches)) which(matches)[1]
    else NA_integer_
  })

  nfl_teamIDs$id[rows]
}

stat_id_to_name <- function(id) {
  dplyr::case_when(
    # passing
    id == 0 ~ "pass_att",
    id == 1 ~ "pass_cmp",
    id == 2 ~ "pass_inc",
    id == 3 ~ "pass_yds",
    id == 4 ~ "pass_tds",
    #id == 10 ~ "pass_sacked",
    id == 15 ~ "pass_tds_40_plus_yds",
    id == 16 ~ "pass_tds_50_plus_yds",
    id == 17 ~ "pass_yds_300_399",
    id == 18 ~ "pass_yds_400_plus",
    id == 19 ~ "pass_2pt",
    id == 20 ~ "pass_int",

    # rushing
    id == 23 ~ "rush_att",
    id == 24 ~ "rush_yds",
    id == 25 ~ "rush_tds",
    id == 26 ~ "rush_2pt",
    id == 35 ~ "rush_td_40_plus_yds",
    id == 36 ~ "rush_td_50_plus_yds",
    id == 37 ~ "rush_yds_100_199",
    id == 38 ~ "rush_yds_200_plus",

    # receptions
    id == 42 ~ "rec_yds",
    id == 43 ~ "rec_tds",
    id == 44 ~ "rec_2pt",
    id == 53 ~ "rec_cmp",
    id == 58 ~ "rec_tgt",

    # misc
    id == 64 ~ "sacked",
    id == 68 ~ "fumbles",
    id == 72 ~ "fumbles_lost",

    # kicking
    id == 74 ~ "fg_cmp_50",
    id == 75 ~ "fg_att_50",
    id == 76 ~ "fg_miss_50",
    id == 77 ~ "fg_cmp_40_49",
    id == 78 ~ "fg_att_40_49",
    id == 79 ~ "fg_miss_40_49",
    id == 80 ~ "fg_cmp_1_39",
    id == 81 ~ "fg_att_1_39",
    id == 82 ~ "fg_miss_1_39",
    id == 83 ~ "fg_cmp_tot",
    id == 84 ~ "fg_att_tot",
    id == 85 ~ "fg_miss_tot",
    id == 86 ~ "fg_cmp_xp",
    id == 87 ~ "fg_att_xp",
    id == 88 ~ "fg_miss_xp",

    # defense
    id == 89 ~ "def_pts_against_0",
    id == 90 ~ "def_pts_against_1_6",
    id == 91 ~ "def_pts_against_7_13",
    id == 92 ~ "def_pts_against_14_17",
    id == 93 ~ "def_block_ret_tds",
    id == 94 ~ "def_total_ret_tds",
    id == 95 ~ "def_ints",
    id == 96 ~ "def_fumbles_recovered",
    id == 97 ~ "def_blocks",
    id == 98 ~ "def_safeties",
    id == 99 ~ "def_sacks",
    id == 101 ~ "special_kick_ret_tds",
    id == 102 ~ "special_punt_ret_tds",
    id == 103 ~ "def_fumble_ret_tds",
    id == 104 ~ "def_int_ret_tds",
    id == 105 ~ "def_tds",
    id == 106 ~ "def_fumbles_forced",
    id == 107 ~ "def_tackles_assisted", # tackles assisted
    id == 108 ~ "def_tackles_solo",
    id == 109 ~ "def_tackles_total", # total tackles
    id == 110 ~ "def_stuffs",
    id == 113 ~ "def_passes_defended", # passes defended
    id == 120 ~ "def_pts_against",
    id == 121 ~ "def_pts_against_18_20",
    id == 122 ~ "def_pts_against_21_27",
    id == 123 ~ "def_pts_against_28_34",
    id == 124 ~ "def_pts_against_35_45",
    id == 125 ~ "def_pts_against_46_plus",
    id == 127 ~ "def_yds_against",

    # punts
    id == 138 ~ "punts",
    id == 139 ~ "punts_yds",
    id == 140 ~ "punts_inside_10",
    id == 141 ~ "punts_inside_20",
    id == 142 ~ "punts_blocked",
    id == 143 ~ "punts_returned",
    id == 144 ~ "punts_return_yds",
    id == 145 ~ "punts_touchbacks",
    id == 146 ~ "punts_fair_catches",

    # punts (need to be verified)
    id == 148 ~ "punts_44_plus",
    id == 149 ~ "punts_42_44",
    id == 150 ~ "punts_40_42",
    id == 151 ~ "punts_38_40",
    id == 152 ~ "punts_36_38",
    id == 153 ~ "punts_34_36",
    id == 154 ~ "punts_32_34",

    # misc
    id == 210 ~ "games",
    TRUE ~ paste0("stat_", id)
  )
}

pos_id_to_name <- function(x) {
  dplyr::case_when(
    x == 1 ~ "QB",
    x == 2 ~ "RB",
    x == 3 ~ "WR",
    x == 4 ~ "TE",
    x == 5 ~ "K",
    x == 7 ~ "P",
    x == 9 ~ "DT",
    x == 10 ~ "DE",
    x == 11 ~ "LB",
    x == 12 ~ "CB",
    x == 13 ~ "S",
    x == 16 ~ "DST",
    TRUE ~ paste0("pos_", x)
  )
}

slot_names <- c("QB", "TQB", "RB", "RB/WR", "WR", "WR/TE", "TE", "OP",
                "DT", "DE", "LB", "DL", "CB", "S", "DB", "DP", "DST",
                "K", "P", "HC", "FLEX", "EDR")
