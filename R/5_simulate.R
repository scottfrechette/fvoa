#' @export
simulate_season_scores <- function(scores, schedule, model,
                                   change_prob = 0.1,
                                   score_adj = 15) {

  set.seed(42)

  sims <- schedule %>%
    distinct(week, team = team1) %>%
    tidybayes::add_predicted_draws(model, seed = 42) %>%
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
                  by = c("week", "team1" = "team")) %>%
        left_join(rename(scores, score2 = score),
                  by = c("week", "team2" = "team")) %>%
        mutate(win = score1 > score2)),
    schedule %>%
      filter(week > max(scores$week)) %>%
      left_join(rename(sims, score1 = adj_score),
                by = c("week", "team1" = "team")) %>%
      left_join(rename(sims, score2 = adj_score),
                by = c("week", "team2" = "team", "sim")) %>%
      mutate(win = score1 > score2) %>%
      select(week, team1, team2,
             score1, score2, win, sim)
  )

  if ("gameID" %in% names(schedule)) {

    sim <- mutate(sim, gameID = schedule$gameID, .after = 1)

  }

  return(sim)

}

#' @export
simulate_season_standings <- function(sim_scores) {

  leverage_week <- sim_scores %>%
    count(week, score1, score2) %>%
    filter(n == max(n)) %>%
    summarize(weeks_played = max(week) + 1) %>%
    pull()

  sim_scores %>%
    group_by(sim, team = team1) %>%
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
simulate_final_standings <- function(sim_standings) {

  sim_standings %>%
    group_by(team) %>%
    summarize(wins = mean(wins),
              points = mean(pf),
              playoffs = mean(playoffs)) %>%
    arrange(-playoffs)

}
