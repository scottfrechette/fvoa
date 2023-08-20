
# Main Functions ----------------------------------------------------------

#' @export
compare_teams <- function(team1, team2,
                          fit = NULL, draws = NULL,
                          .output = c("prob", "odds", "spread"),
                          .verbose = FALSE) {

  .output <- match.arg(.output)

  if (!is.null(fit)) {

    sim <- tibble(week = max(fit$data$week), team = c(team1, team2)) %>%
      tidybayes::add_predicted_draws(fit, seed = 42) %>%
      ungroup() %>%
      select(team, sim = .draw, score = .prediction) %>%
      spread(team, score) %>%
      rename(team1 = all_of(team1), team2 = all_of(team2)) %>%
      mutate(diff = team1 - team2) %>%
      pull(diff)

  } else if (!is.null(draws)) {

    sim <- draws %>%
      filter(team %in% c(team1, team2)) %>%
      ungroup() %>%
      select(team, sim = .draw, score = .prediction) %>%
      spread(team, score) %>%
      rename(team1 = all_of(team1), team2 = all_of(team2)) %>%
      mutate(diff = team1 - team2) %>%
      pull(diff)

  } else {

    "ERROR"

  }

  if(.output == "prob") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)
    t2wins <- round(pnorm(0, mean(sim), sd(sim))*100 - tie/2, 2)

    if(.verbose) {

      t1squeak <- t1wins - round(pnorm(5, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
      t1blowout <- round(pnorm(20, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
      t1normal <- t1wins - t1squeak - t1blowout
      t2squeak <- t2wins - round(pnorm(-5, mean(sim), sd(sim))* 100, 2)
      t2blowout <- round(pnorm(-20, mean(sim), sd(sim))*100, 2)
      t2normal <- t2wins - t2squeak - t2blowout

      tibble(Winner = c(team1, team1, team1, "Tie", team2, team2, team2),
             Type = c("Blowout", "Comfortable", "Squeaker", "Tie", "Squeaker", "Comfortable", "Blowout"),
             MarginVictory = c("20+ points", "5-20", "<5 points", "-", "<5 points", "5-20", "20+ points"),
             PctChance = c(t1blowout, t1normal, t1squeak, tie, t2squeak, t2normal, t2blowout))

    } else {

      return(t1wins)

    }

  } else if (.output == "odds") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)

    odds <- prob_to_odds(t1wins)

    return(odds)

  } else if (.output == "spread") {

    spread <- -mean(sim)
    spread <- round(spread * 2) / 2
    spread <- if_else(spread > 0, paste0("+", spread),
                      if_else(spread < 0, paste(spread), paste(0)))
    return(spread)

  } else {NA}

}

#' @export
compare_league <- function(fit) {

  team_sims <- tibble(week = max(fit$data$week),
           team = unique(fit$data$team)) %>%
    tidybayes::add_predicted_draws(fit, seed = 42) %>%
    ungroup() %>%
    select(team, score = .prediction) %>%
    nest(data = -team)

  crossing(rename(team_sims, team1 = team, data1 = data),
           rename(team_sims, team2 = team, data2 = data)) %>%
    filter(team1 != team2) %>%
    mutate(sims = map2(data1, data2, ~.x - .y),
           wp = map_dbl(sims, ~mean(.x$score > 0)),
           odds = map_chr(wp, prob_to_odds),
           spread = map_dbl(sims, ~mean(.x$score)),
           spread = round(spread * 2) / 2) %>%
    select(team1, team2, wp, odds, spread)

}

#' @export
compare_current_matchups <- function(schedule,
                                     fit,
                                     win_prob = NULL,
                                     quality = FALSE,
                                     sim_standings = NULL) {

  scores <- as_tibble(fit$data)

  cutoff <- length(unique(scores$team)) / 2

  current_matchups <- schedule %>%
    filter(week == max(scores$week) + 1) %>%
    mutate(fvoa_wp = map2_dbl(team, opponent,
                              ~compare_teams(fit, team1 = .x, team2 = .y))) %>%
    arrange(-fvoa_wp) %>%
    head(cutoff) %>%
    mutate(Line = map_chr(fvoa_wp, prob_to_odds),
           fvoa_wp = round(fvoa_wp/100, 2) %>% format_pct,
           Spread = map2_chr(team, opponent,
                             ~compare_teams(fit, team1 = .x, team2 = .y,
                                            .output = "spread"))) %>%
    select(Winner = team,
           Loser = opponent,
           FVOA = fvoa_wp,
           Spread,
           Line)

  if(!is.null(win_prob)) {

    current_matchups <- current_matchups %>%
      left_join(win_prob %>%
                  rename(Yahoo = wp),
                by = c("Winner" = "team")) %>%
      select(Winner, Loser,
             FVOA, Yahoo,
             Spread,
             Line)

  }

    if (quality) {

      fvoa <- calculate_fvoa(fit)

      fvoa_max <- fvoa %>%
        slice_head(n = 2) %>%
        summarize(max = sum(fvoa)) %>%
        pull()

      fvoa_min  <- fvoa %>%
        slice_tail(n = 2) %>%
        summarize(min = sum(fvoa)) %>%
        pull()

      current_matchups <- current_matchups %>%
        left_join(select(fvoa, Winner = team, fvoa_winner = fvoa), by = "Winner") %>%
        left_join(select(fvoa, Loser = team, fvoa_loser = fvoa), by = "Loser") %>%
        mutate(fvoa_sum = fvoa_winner + fvoa_loser,
               Quality = round((fvoa_sum - fvoa_min) / (fvoa_max - fvoa_min) * 100)) %>%
        select(-fvoa_winner, -fvoa_loser, -fvoa_sum)

    }

    if (!is.null(sim_standings)) {

      leverage <- sim_standings %>%
        group_by(team, leverage_win) %>%
        summarize(playoffs = mean(playoffs),
                  .groups = "drop") %>%
        mutate(leverage_win = if_else(leverage_win == 1, "win", "lose")) %>%
        spread(leverage_win, playoffs) %>%
        mutate(leverage = win - lose) %>%
        arrange(-leverage)

      leverage_max <- leverage %>%
        slice_head(n = 2) %>%
        summarize(max = sum(leverage)) %>%
        pull()

      leverage_min  <- leverage %>%
        slice_tail(n = 2) %>%
        summarize(min = sum(leverage)) %>%
        pull()

      current_matchups <- current_matchups %>%
        left_join(select(leverage, Winner = team, leverage_winner = leverage), by = "Winner") %>%
        left_join(select(leverage, Loser = team, leverage_loser = leverage), by = "Loser") %>%
        mutate(Importance = round((leverage_winner + leverage_loser) / 2 * 100)) %>%
        select(-leverage_winner, -leverage_loser)
    }


    if(quality & !is.null(sim_standings)) {

      current_matchups <- mutate(current_matchups, Overall = round((Quality + Importance) / 2))

    }

  current_matchups
}

#' @export
compare_playoff_teams <- function(fit, seed1, seed2, seed3, seed4) {

  # Simulate each possible matchup
  t1r1 <- compare_teams(seed1, seed4, fit)/100
  t2r1 <- compare_teams(seed2, seed3, fit)/100
  t3r1 <- compare_teams(seed3, seed2, fit)/100
  t4r1 <- compare_teams(seed4, seed1, fit)/100
  t12r2 <- compare_teams(seed1, seed2, fit)/100
  t13r2 <- compare_teams(seed1, seed3, fit)/100
  t21r2 <- compare_teams(seed2, seed1, fit)/100
  t24r2 <- compare_teams(seed2, seed4, fit)/100
  t31r2 <- compare_teams(seed3, seed1, fit)/100
  t34r2 <- compare_teams(seed3, seed4, fit)/100
  t42r2 <- compare_teams(seed4, seed2, fit)/100
  t43r2 <- compare_teams(seed4, seed3, fit)/100

  # Calculate chances of each team winning both rounds
  t1wins <- round(t1r1 * ((t2r1 * t12r2) + (t3r1 * t13r2)), 4) * 100
  t2wins <- round(t2r1 * ((t1r1 * t21r2) + (t4r1 * t24r2)), 4) * 100
  t3wins <- round(t3r1 * ((t1r1 * t31r2) + (t4r1 * t34r2)), 4) * 100
  t4wins <- round(t4r1 * ((t2r1 * t42r2) + (t3r1 * t43r2)), 4) * 100

  # Calculate odds of winning
  t1odds <- round(round(100/t1wins, 2) * 2)/2
  t2odds <- round(round(100/t2wins, 2) * 2)/2
  t3odds <- round(round(100/t3wins, 2) * 2)/2
  t4odds <- round(round(100/t4wins, 2) * 2)/2

  t1odds <- if_else(round(t1odds) == t1odds, paste0(t1odds, ":", 1), paste0(t1odds * 2, ":", 2))
  t2odds <- if_else(round(t2odds) == t2odds, paste0(t2odds, ":", 1), paste0(t2odds * 2, ":", 2))
  t3odds <- if_else(round(t3odds) == t3odds, paste0(t3odds, ":", 1), paste0(t3odds * 2, ":", 2))
  t4odds <- if_else(round(t4odds) == t4odds, paste0(t4odds, ":", 1), paste0(t4odds * 2, ":", 2))

  t1Amodds <- prob_to_odds(t1wins)
  t2Amodds <- prob_to_odds(t2wins)
  t3Amodds <- prob_to_odds(t3wins)
  t4Amodds <- prob_to_odds(t4wins)

  tibble(Winner = c(seed1, seed2, seed3, seed4),
         Percent = c(t1wins, t2wins, t3wins, t4wins) / 100,
         Odds = c(t1odds, t2odds, t3odds, t4odds),
         BettingLine = as.factor(c(t1Amodds, t2Amodds, t3Amodds, t4Amodds))) %>%
    arrange(-Percent) %>%
    mutate(Percent = scales::percent(Percent, accuracy = 1))
}


# Helper Functions --------------------------------------------------------

# convert_spread <- function(sim) {
#
#   spread <- -mean(sim)
#   spread <- round(spread * 2) / 2
#   spread <- if_else(spread > 0, paste0("+", spread),
#                     if_else(spread < 0, paste(spread), paste(0)))
#
#   return(spread)
#
# }

#' @export
spread_league <- function(league_comparison,
                          .output = c("wp", "odds", "spread"),
                          .matrix = FALSE) {

  .output = match.arg(.output)

  out <- select(league_comparison, starts_with("team"), .output) %>%
    rename(team = 1) %>%
    spread(2, 3, fill = 0)

  if(.matrix) {

    out <- as.matrix(out)

    row.names(out) <- out[,1]
    out <- out[,-1]

  }

  return(out)

}


# Experimental ------------------------------------------------------------

compare_teams_player <- function(schedule,
                                 roster_draws,
                                 current_week,
                                 .summarize = T) {

  tmp <- schedule %>%
    filter(week == current_week) %>%
    left_join(roster_draws, by = "team") %>%
    left_join(rename(roster_draws, opponent = team, opponent_points = points),
              by = c("opponent", "sim"))

  if (.summarize) {

    tmp <- tmp %>%
    group_by(team, opponent) %>%
    summarize(score = mean(points),
              opponent_score = mean(opponent_points),
              margin = mean(points - opponent_points),
              win = mean(points > opponent_points),
              tie = mean(points == opponent_points),
              loss = mean(points < opponent_points),
              .groups = "drop") %>%
    arrange(-win)

  }

  return(tmp)

}
