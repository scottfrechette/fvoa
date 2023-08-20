
# Main Functions ----------------------------------------------------------

#' @export
simulate_season_scores <- function(schedule,
                                   fit,
                                   sims = 1000,
                                   .last_n = 3,
                                   .change_prob = 0.25,
                                   .score_adj = 25) {

  next_week_sim <- tibble(week = max(fit$data$week + 1),
                          team = unique(fit$data$team)) %>%
    tidybayes::add_predicted_draws(fit, ndraws = sims) %>%
    ungroup() %>%
    select(sim = .draw, week, team, score = .prediction)

  sim_scores <- crossing(sim = 1:sims, fit$data) %>%
    bind_rows(next_week_sim) %>%
    nest(.by = c(team, sim)) %>%
    mutate(pred = map2(data, sim,
                       ~sim_season(.x$score, .y,
                                   last_n = .last_n,
                                   change_prob = .change_prob,
                                   score_adj = .score_adj))) %>%
    select(-data) %>%
    unnest(pred)

  sim_season <- crossing(sim = 1:sims, schedule) %>%
    left_join(rename(sim_scores, score1 = score),
              by = c('team', 'sim', 'week')) %>%
    left_join(rename(sim_scores, opponent = team, score2 = score),
              by = c('opponent', 'sim', 'week')) %>%
    mutate(win = score1 > score2) %>%
    select(week, team, opponent,
           score1, score2, win, sim)

  if ("gameID" %in% names(schedule)) {

    sim_season <- mutate(sim_season, gameID = schedule$gameID, .after = 1)

  }

  return(sim_season)

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


# Helper Functions --------------------------------------------------------

sim_team_season <- function(scores,
                            seed = 42,
                            last_n = 3,
                            change_prob = 0.25,
                            score_adj = 25) {

  set.seed(seed)

  score_tmp <- rep(NA, 15)
  score_tmp[1:length(score)] <- score

  for (i in (length(scores) + 1):15) {

    # dist <- estimate_normal(110, 25,
    #                         mean(tail(score_tmp[!is.na(score_tmp)], last_n)),
    #                         sd(tail(score_tmp[!is.na(score_tmp)], last_n)),
    #                         length(tail(score_tmp, last_n)))

    dist <- estimate_normal(110, 25,
                            tail(ema(score_tmp[!is.na(score_tmp)], last_n), 1),
                            tail(emsd(score_tmp[!is.na(score_tmp)], last_n), 1),
                            length(tail(score_tmp, last_n)))

    pred <- rnorm(1, dist$mean, dist$sd) +
      rbinom(n = 1, size = 1, prob = change_prob) *
      rnorm(1, 0, score_adj)

    score_tmp[i] <- pred

  }

  tibble(week = 1:15, score = score_tmp)

}

ema <- function (x, window = NULL, ratio = NULL) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  c(stats::filter(x = x * ratio, filter = 1 - ratio, method = "recursive", init = x[1]))

}

emsd <- function(x, window = NULL, ratio = NULL, initial = 15) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  n <- length(x)

  out <- sapply(
    1:n,
    function(i, x, ratio) {
      y <- x[1:i]
      m <- length(y)
      weights <- (1 - ratio)^((m - 1):0)
      ewma <- sum(weights * y) / sum(weights)
      bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
      ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
    },
    x = x,
    ratio = ratio
  )

  out[1] <- initial

  out
}
