
# Main Functions ----------------------------------------------------------

#' @export
compare_teams <- function(scores,
                          team1,
                          team2,
                          .fun = simulate_score,
                          .output = c("prob", "odds", "spread", "over_under", "plot"),
                          .verbose = FALSE,
                          .interactive = FALSE,
                          ...) {

  set.seed(42)

  .output <- match.arg(.output)

  t1_sim <- .fun(scores, team1, ...)

  t2_sim <- .fun(scores, team2, ...)

  sim <- t1_sim - t2_sim

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
             Type = c("Blowout", "Normal", "Squeaker", "Tie", "Squeaker", "Normal", "Blowout"),
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

  }  else if (.output == "over_under") {

    ou <- round(mean(t1_sim + t2_sim))

    return(ou)

  } else if (.output == "plot") {

    tbl <- tibble(t1 = sample(t1_sim, 10000),
                  t2 = sample(t2_sim, 10000)) %>%
      mutate(winner = if_else(t1 > t2, team1, team2)) %>%
      add_count(winner) %>%
      mutate(prop = scales::percent(n / 10000, accuracy = 0.1))

    p <- tbl %>%
      ggplot(aes(t1, t2, color = factor(winner),
                 text = str_glue("{team1}: {t1}\n{team2}: {t2}"))) +
      geom_point(alpha = 0.3) +
      scale_x_continuous(limits = c(min(tbl$t1) - 2, max(tbl$t1) + 2),
                         expand = c(0, NA)) +
      scale_y_continuous(limits = c(min(tbl$t2) - 2, max(tbl$t2) + 2),
                         expand = c(0, NA)) +
      labs(x = str_glue("Simulated points: {team1}"),
           y = str_glue("Simulated points:  {team2}"),
           color = "Winner") +
      annotate("text",
               x = min(tbl$t1) + ((max(tbl$t1) - min(tbl$t1)) * 0.85),
               y = min(tbl$t1) + ((max(tbl$t2) - min(tbl$t2)) * 0.15),
               size = 5,
               label = str_glue("{team1} better {unique(tbl[tbl$winner == team1,]$prop)}")) +
      annotate("text",
               y = min(tbl$t1) + ((max(tbl$t1) - min(tbl$t1)) * 0.85),
               x = min(tbl$t1) + ((max(tbl$t2) - min(tbl$t2)) * 0.15),
               size = 5,
               label = str_glue("{team2} better {unique(tbl[tbl$winner == team2,]$prop)}")) +
      theme_fvoa() +
      theme(legend.position = "none",
            legend.key = element_blank(),
            panel.grid.major.y = element_blank())

    if(.interactive) p <- plotly::ggplotly(p, tooltip = "text") %>% plotly::config(displayModeBar = F)

    return(p)

  } else {NA}

}


#' @export
compare_league <- function(scores,
                           .fun = simulate_score,
                           ...) {

  scores <- extract_scores(scores)

  teams <- unique(pull(select(scores, starts_with("team"))))

  set.seed(42)

  team_scores <- tibble(team1 = teams) %>%
    mutate(score1 = map(team1,
                        .fun,
                        scores = scores,
                        ...))

  crossing(team_scores,
           rename(team_scores, team2 = team1, score2 = score1)) %>%
    filter(team1 != team2) %>%
    mutate(sim = map2(score1, score2, ~ .x - .y),
           wp = map_dbl(sim, convert_wp),
           odds = map_chr(wp, prob_to_odds),
           spread = map_chr(sim, convert_spread)) %>%
    select(starts_with("team"), wp:spread)

}

#' @export
compare_current_matchups <- function(scores,
                                     schedule,
                                     current_week,
                                     win_prob = NULL,
                                     leverage = NULL,
                                     .fun = simulate_score,
                                     ...) {

  set.seed(42)

  scores <- select(scores, week, team = starts_with("team"), score)

  if("team" %in% names(schedule) | "teamID" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule <- doublewide_schedule(schedule)

  cutoff <- length(unique(scores$team)) / 2

  current_matchups <- schedule %>%
    filter(week == current_week) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(fvoa_wp = map2_dbl(team1, team2,
                              compare_teams,
                              scores = scores,
                              ...)) %>%
    arrange(-fvoa_wp) %>%
    head(cutoff) %>%
    mutate(Line = map_chr(fvoa_wp, prob_to_odds),
           fvoa_wp = round(fvoa_wp/100, 2) %>% format_pct,
           Spread = map2_chr(team1, team2,
                             compare_teams,
                             scores = scores,
                             .output = "spread",
                             ...),
           OU = map2_dbl(team1, team2,
                             compare_teams,
                             scores = scores,
                             .output = "over_under",
                             ...)) %>%
    select(Winner = team1,
           Loser = team2,
           `Win%` = fvoa_wp,
           Spread,
           Line,
           `O/U` = OU)

  if(!is.null(win_prob)) {
    current_matchups <- current_matchups %>%
      left_join(win_prob %>%
                  rename(`Yahoo Win%` = wp),
                by = c("Winner" = "team")) %>%
      select(Winner, Loser,
             `FVOA Win%` = `Win%`,
             `Yahoo Win%`,
             Spread,
             Line,
             `O/U`)
  }

  if(!is.null(leverage)) {

    leverage_tmp <- select(leverage, team = starts_with("team"), sim:weeks_played)

    team_leverage <- leverage_tmp %>%
      group_by(team, leverage_win) %>%
      dplyr::summarize(playoffs = mean(playoffs),
                .groups = "drop") %>%
      mutate(leverage_win = if_else(leverage_win == 1, "Win", "Lose")) %>%
      spread(leverage_win, playoffs) %>%
      mutate(leverage = Win - Lose) %>%
      select(team, leverage)

    current_matchups <- current_matchups %>%
      left_join(team_leverage %>%
                  rename(Winner = team, leverage_w = leverage),
                by = "Winner") %>%
      left_join(team_leverage %>%
                  rename(Loser = team, leverage_l = leverage),
                by = "Loser") %>%
      mutate(Leverage = format_pct(leverage_w + leverage_l, accuracy = 0)) %>%
      select(-leverage_w, -leverage_l)

  }

  current_matchups
}

#' @export
compare_playoff_teams <- function(scores,
                                  seed1, seed2, seed3, seed4,
                                  .output = "prob",
                                  .fun = simulate_score,
                                  ...) {

  set.seed(42)

  # Simulate each possible matchup
  t1r1 <- compare_teams(scores, seed1, seed4,
                        .fun = .fun, .reps = 1e6)/100
  t2r1 <- compare_teams(scores, seed2, seed3,
                        .fun = .fun, .reps = 1e6)/100
  t3r1 <- compare_teams(scores, seed3, seed2,
                        .fun = .fun, .reps = 1e6)/100
  t4r1 <- compare_teams(scores, seed4, seed1,
                        .fun = .fun, .reps = 1e6)/100
  t12r2 <- compare_teams(scores, seed1, seed2,
                         .fun = .fun, .reps = 1e6)/100
  t13r2 <- compare_teams(scores, seed1, seed3,
                         .fun = .fun, .reps = 1e6)/100
  t21r2 <- compare_teams(scores, seed2, seed1,
                         .fun = .fun, .reps = 1e6)/100
  t24r2 <- compare_teams(scores, seed2, seed4,
                         .fun = .fun, .reps = 1e6)/100
  t31r2 <- compare_teams(scores, seed3, seed1,
                         .fun = .fun, .reps = 1e6)/100
  t34r2 <- compare_teams(scores, seed3, seed4,
                         .fun = .fun, .reps = 1e6)/100
  t42r2 <- compare_teams(scores, seed4, seed2,
                         .fun = .fun, .reps = 1e6)/100
  t43r2 <- compare_teams(scores, seed4, seed3,
                         .fun = .fun, .reps = 1e6)/100

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
    mutate(Percent = scales::percent(Percent))
}


# Helper Functions --------------------------------------------------------

convert_wp <- function(sim) {

  round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE) * 100 -
          round(dnorm(0, mean(sim), sd(sim)) * 100, 2)/2, 2)

}

convert_spread <- function(sim) {

  spread <- -mean(sim)
  spread <- round(spread * 2) / 2
  spread <- if_else(spread > 0, paste0("+", spread),
                    if_else(spread < 0, paste(spread), paste(0)))

  return(spread)

}

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
