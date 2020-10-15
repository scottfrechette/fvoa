#' @export
calculate_rankings <- function(schedule, scores, league) {

  team_col <- names(select(scores, starts_with("team")))

  calculate_stats(schedule, scores, league = league) %>%
    left_join(calculate_fvoa(scores),
              by = team_col) %>%
    left_join(calculate_strength_schedule(schedule, scores),
              by = team_col) %>%
    left_join(calculate_colley(schedule, scores),
              by = team_col)
}

#' @export
calculate_fvoa_season <- function(scores) {

  fvoa <- list()

  for (i in 1:n_distinct(scores$week)) {

    scores_tmp <- scores %>%
      filter(week %in% 1:i)
    fvoa_rankings <- calculate_fvoa(scores_tmp) %>%
      mutate(week = i,
             .before = 1)
    fvoa[i] <- list(fvoa_rankings)

  }

  tibble(fvoa) %>%
    unnest(fvoa)

}

#' @export
calculate_quadrants <- function(schedule, scores) {

  num_games <- max(scores$week)

  if("team" %in% names(schedule) | "teamID" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule <- doublewide_schedule(schedule)

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  schedule %>%
    left_join(scores %>% rename(score1 = score),
              by = c("week", "team1" = "team")) %>%
    left_join(scores %>% rename(score2 = score),
              by = c("week", "team2" = "team")) %>%
    drop_na() %>%
    mutate(diff = score1 - score2) %>%
    select(week, team = team1,
           score1, score2,
           diff) %>%
    group_by(team) %>%
    summarise(pf = sum(score1),
              pa = sum(score2),
              delta = sum(diff),
              wp = sum(diff > 0) / num_games,
              .groups = "drop") %>%
    set_names(team_col, "pf", "pa", "delta", "wp")

}

# Helper Functions --------------------------------------------------------


calculate_stats <- function(schedule, scores, league = "Yahoo") {

  if("type" %in% names(scores)) {
    scores <- scores %>%
      filter(type == "act")
  }

  if("team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  rank_col <- paste(league, "rank", sep = "_")

  schedule %>%
    left_join(scores %>%
                rename(team1 = team,
                       score1 = score),
              by = c("week", "team1")) %>%
    left_join(scores %>%
                rename(team2 = team,
                       score2 = score),
              by = c("week", "team2")) %>%
    drop_na() %>%
    gather(x, team, team1:team2) %>%
    mutate(score = if_else(x == "team1", score1, score2),
           tie = if_else(score1 == score2, 1, 0),
           win = if_else(x == "team1" & score1 - score2 > 0, 1,
                         if_else(x == "team2" & score2 - score1 > 0, 1, 0)),
           lose = if_else(tie == 0 & win == 0, 1, 0)) %>%
    group_by(team) %>%
    summarise(points = sum(score),
              wins = sum(win),
              losses = sum(lose),
              tie = sum(tie)) %>%
    mutate(percent = round(wins/(wins + losses + tie), 3)) %>%
    arrange(-wins, -points) %>%
    mutate(rank = row_number(),
           percent = format_pct(percent)) %>%
    arrange(-wins, -points) %>%
    unite(record, c(wins, losses, tie), sep = "-") %>%
    set_names(team_col, "points", "record",
              "percent", rank_col)
}

calculate_fvoa <- function(scores) {

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  compare_league(scores, .reps = 1e6) %>%
    spread_league(.output = "wp") %>%
    select(-team) %>%
    map_df(function(x) {round((mean(100 - x, na.rm = T) - 50) / 0.5, 2)}) %>%
    gather(team, fvoa) %>%
    arrange(-fvoa) %>%
    mutate(team = type.convert(team, as.is = TRUE)) %>%
    set_names(team_col, "fvoa") %>%
    mutate(fvoa_rank = dense_rank(-fvoa))

}

calculate_strength_schedule <- function(schedule, scores) {

  if("team" %in% names(schedule) | "teamID" %in% schedule) {
    schedule <- spread_schedule(schedule)
  }

  schedule <- doublewide_schedule(schedule)

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  calculate_fvoa(scores) %>%
    select(team2 = team, fvoa) %>%
    right_join(schedule, by = "team2") %>%
    rename(team = team1) %>%
    group_by(team) %>%
    summarise(sos = round(mean(fvoa), 2)) %>%
    arrange(sos) %>%
    set_names(team_col, "sos") %>%
    mutate(sos_rank = min_rank(-sos))

}

calculate_colley <- function(schedule, scores) {

  if("team" %in% names(schedule) | "teamID" %in% schedule) {
    schedule <- spread_schedule(schedule)
  }

  schedule <- doublewide_schedule(schedule)

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  colley_df <- schedule %>%
    left_join(scores, by = c("week", "team1" = "team")) %>%
    left_join(scores, by = c("week", "team2" = "team")) %>%
    mutate(x = if_else(score.x > score.y, "W",
                       if_else(score.y > score.x, "L", "")) )%>%
    group_by(team1, team2) %>%
    summarise(x = paste(x, collapse = ""),
              .groups = "drop") %>%
    spread(team2, x) %>%
    mutate_if(is.character, replace_na, "X") %>%
    unite(outcomes, -team1, sep = "_") %>%
    mutate(W = str_count(outcomes, "W"),
           L = str_count(outcomes, "L"),
           t = W + L,
           C = t + 2,
           b = 1 + (W - L) / 2)

  colley_transform <- function(x) {
    if(x == 0) {
      0
    } else if(x == "X") {
      3
    } else {
      -nchar(x)
    }
  }

  colley_invmat <- colley_df %>%
    select(team1, outcomes) %>%
    separate(outcomes, as.character(colley_df$team1)) %>%
    gather(team, value, -team1) %>%
    mutate(x = map_dbl(value, colley_transform)) %>%
    select(-value) %>%
    spread(team, x) %>%
    select(-team1) %>%
    solve(tol = 1e-20)

  colley_df %>%
    mutate(rating = colley_invmat %*% colley_df$b %>%
             as.numeric() %>%
             round(4),
           rank = min_rank(-rating)) %>%
    select(team = team1, colley_rating = rating, colley_rank = rank) %>%
    arrange(colley_rank) %>%
    set_names(team_col, "colley_rating", "colley_rank")
}
