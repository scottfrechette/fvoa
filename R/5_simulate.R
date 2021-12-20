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
    distinct(week, team, score1) %>%
    group_by(week) %>%
    filter(n() == n_distinct(simulated_scores$team)) %>%
    distinct(week) %>%
    max(.$week) + 1

  simulated_scores %>%
    group_by(sim, team) %>%
    summarize(pf = sum(score1),
              pa = sum(score2),
              wins = sum(score1 > score2),
              losses = sum(score1 < score2),
              tie = sum(score1 == score2),
              wp = wins / n(),
              leverage_week = leverage_week,
              leverage_win = sum(score1 > score2 & week == leverage_week),
              .groups = "drop") %>%
    group_by(sim) %>%
    arrange(-wins, -pf) %>%
    mutate(rank = row_number(),
           playoffs = rank <= 4) %>%
    ungroup() %>%
    arrange(sim, rank)

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
      select(team, pf:rank)

  } else {

    simulated_standings %>%
      group_by(team) %>%
      summarize(wins = mean(wins),
                points = mean(pf),
                playoffs = mean(playoffs)) %>%
      arrange(-playoffs)

  }

}

#' @export
simulate_final_standings_season <- function(fit_team_season_df, schedule) {

  fit_team_season_df %>%
    mutate(simulated_scores = map(model, ~ simulate_season_scores(schedule, .x)),
           simulated_standings = map(simulated_scores, simulate_season_standings),
           simulated_final_standings = map(simulated_standings, simulate_final_standings)) %>%
    select(week, simulated_final_standings) %>%
    unnest(simulated_final_standings)

}
