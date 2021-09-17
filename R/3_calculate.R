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

    scores_tmp <- filter(scores, week %in% 1:i)
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

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)

  rank_col <- paste(league, "rank", sep = "_")

  schedule %>%
    left_join(scores, by = c("week", "team")) %>%
    left_join(scores %>%
                rename(opponent = team,
                       opp_score = score),
              by = c("week", "opponent")) %>%
    drop_na() %>%
    mutate(tie = if_else(score == opp_score, 1, 0),
           win = if_else(score > opp_score, 1, 0),
           lose = if_else(score < opp_score, 1, 0)) %>%
    group_by(team) %>%
    summarise(pf = sum(score),
              pa = sum(opp_score),
              wins = sum(win),
              losses = sum(lose),
              tie = sum(tie),
              .groups = "drop") %>%
    mutate(wp = wins / (wins + losses)) %>%
    arrange(-wp, -pf) %>%
    mutate(rank = row_number(),
           wp = format_pct(wp, accuracy = 0)) %>%
    unite(record, c(wins, losses, tie), sep = "-") %>%
    set_names(team_col, "pf", "pa",
              "record", "wp", rank_col)
}

calculate_fvoa <- function(scores) {

  fit <- fit_model(scores)

  extract_draws(scores, fit) %>%
    ungroup() %>%
    mutate(avg = mean(.prediction)) %>%
    group_by(team) %>%
    summarize(fvoa_wins = mean(.prediction > avg),
              fvoa_score = mean(.prediction - avg),
              fvoa_sd = sd(.prediction - avg)) %>%
    arrange(-fvoa_score) %>%
    select(team, fvoa = fvoa_score) %>%
    mutate(fvoa_rank = min_rank(-fvoa))

}

calculate_strength_schedule <- function(schedule, scores) {

  fvoa <- calculate_fvoa(scores)

  schedule %>%
    left_join(fvoa, by = c("team2" = "team")) %>%
    group_by(team = team1) %>%
    summarize(sos = round(mean(fvoa), 2)) %>%
    arrange(-sos) %>%
    mutate(sos_rank = min_rank(sos))

}

calculate_colley <- function(schedule, scores) {

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

calculate_proj <- function(scores) {

  sliding_proj <- function(score, .reg_games = 6, .reg_points = 110) {

    weighting <- weight_games(score, .reg_games)

    if (length(score) < .reg_games) {

      score <- c(score, .reg_points)

    }

    weighted.mean(score, weighting)

  }

  scores %>%
    select(team = starts_with("team"), everything()) %>%
    group_by(team) %>%
    mutate(fvoa = slide_dbl(score, sliding_proj, .before = Inf),
           fvoa_proj = lag(fvoa)) %>%
    ungroup() %>%
    select(team, week, fvoa_proj) %>%
    drop_na()

}
