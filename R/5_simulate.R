#' @export
simulate_season_scores <- function(schedule, fit,
                                   change_prob = 0.1,
                                   score_adj = 15) {

  set.seed(42)

  scores <- fit$data %>%
    as_tibble() %>%
    select(-weight)

  sims <- schedule %>%
    distinct(week, team) %>%
    tidybayes::add_predicted_draws(fit, seed = 42) %>%
    ungroup() %>%
    select(sim = .draw, week, team, score = .prediction) %>%
    mutate(change = rbinom(n = n(), size = 1, prob = change_prob),
           dir = sample(x = c(-1, 1), size = n(),
                        replace = T, prob =  c(0.5, 0.5)),
           change_dir = change * dir) %>%
    group_by(sim, team) %>%
    mutate(changes = cumsum(change_dir),
           adj_score = score + changes * score_adj) %>%
    ungroup()

  sim <- bind_rows(
    crossing(
      distinct(sims, sim),
      schedule %>%
        filter(week <= max(scores$week)) %>%
        left_join(rename(scores, score1 = score),
                  by = c("week", "team")) %>%
        left_join(rename(scores, score2 = score),
                  by = c("week", "opponent" = "team")) %>%
        mutate(win = score1 > score2)),
    schedule %>%
      filter(week > max(scores$week)) %>%
      left_join(rename(sims, score1 = adj_score),
                by = c("week", "team")) %>%
      left_join(rename(sims, score2 = adj_score),
                by = c("week", "opponent" = "team", "sim")) %>%
      mutate(win = score1 > score2) %>%
      select(week, team, opponent,
             score1, score2, win, sim)
  )

  if ("gameID" %in% names(schedule)) {

    sim <- mutate(sim, gameID = schedule$gameID, .after = 1)

  }

  return(sim)

}

#' @export
simulate_season_standings <- function(simulated_scores) {

  leverage_week <- simulated_scores %>%
    count(week, score1, score2) %>%
    filter(n == max(n)) %>%
    summarize(weeks_played = max(week) + 1) %>%
    pull()

  simulated_scores %>%
    group_by(sim, team) %>%
    summarize(pf = sum(score1),
              pa = sum(score2),
              wins = sum(score1 > score2),
              losses = sum(score1 < score2),
              tie = sum(score1 == score2),
              wp = wins / n(),
              leverage_week = leverage_week,
              leverage_win = sum(score1 > score2 & week == leverage_week)) %>%
    group_by(sim) %>%
    arrange(-wins, -pf) %>%
    mutate(playoffs = row_number() <= 4) %>%
    ungroup()

}

#' @export
simulate_final_standings <- function(simulated_standings, .verbose = T) {

  if (.verbose) {

    simulated_standings %>%
      group_by(team) %>%
      summarize(pf = mean(pf),
                pa = mean(pa),
                wins = mean(wins),
                losses = mean(losses),
                ties = mean(tie),
                wp = mean(wp),
                playoffs = mean(playoffs)) %>%
      arrange(-playoffs) %>%
      mutate(rank = 1:n()) %>%
      mutate(season = as.integer(current_season),
             week = as.integer(weeks_played),
             .before = 1) %>%
      select(season, week, team, pf:rank)

  } else {

    simulated_standings %>%
      group_by(team) %>%
      summarize(wins = mean(wins),
                points = mean(pf),
                playoffs = mean(playoffs)) %>%
      arrange(-playoffs)

  }




}
