compare_teams <- function(scores,
                          team1,
                          team2,
                          reps = 1e6,
                          output = c("prob", "odds", "spread"),
                          verbose = FALSE,
                          min_score = 50,
                          reg_games = 6,
                          reg_points = 108) {

  set.seed(42)

  output <- match.arg(output)

  t1_sim <- simulate_score(scores,
                           team1,
                           reps = reps,
                           min_score = min_score,
                           reg_games = reg_games,
                           reg_points = reg_points)

  t2_sim <- simulate_score(scores,
                           team2,
                           reps = reps,
                           min_score = min_score,
                           reg_games = reg_games,
                           reg_points = reg_points)

  sim <- t1_sim - t2_sim

  if(output == "prob") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)
    t2wins <- round(pnorm(0, mean(sim), sd(sim))*100 - tie/2, 2)

  } else if (output == "odds") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)

    odds <- prob_to_odds(t1wins)

    return(odds)

  } else if (output == "spread") {

    spread <- -mean(sim)
    spread <- round(spread * 2) / 2
    spread <- if_else(spread > 0, paste0("+", spread),
                      if_else(spread < 0, paste(spread), paste(0)))
    return(spread)

  } else {NA}

  if(verbose) {

    t1squeak <- t1wins - round(pnorm(5, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
    t1blowout <- round(pnorm(20, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
    t1normal <- t1wins - t1squeak - t1blowout
    t2squeak <- t2wins - round(pnorm(-5, mean(sim), sd(sim))* 100, 2)
    t2blowout <- round(pnorm(-20, mean(sim), sd(sim))*100, 2)
    t2normal <- t2wins - t2squeak - t2blowout

    tibble(Winner = c(team1, team1, team1, "Tie", team2, team2, team2),
           output = c("Blowout", "Normal", "Squeaker", "Tie", "Squeaker", "Normal", "Blowout"),
           MarginVictory = c("20+ points", "5-20", "<5 points", "-", "<5 points", "5-20", "20+ points"),
           PctChance = c(t1blowout, t1normal, t1squeak, tie, t2squeak, t2normal, t2blowout))

  } else {

    t1wins

  }
}

compare_league <- function(scores,
                           output = c("prob", "odds", "spread"),
                           .fun = compare_teams,
                           reg_games = 6,
                           reps = 1e6,
                           matrix = FALSE) {

  set.seed(42)

  output <- match.arg(output)

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)
  teams <- unique(scores$team)

  tmp <- crossing(team1 = teams, team2 = teams) %>%
    filter(team1 != team2) %>%
    mutate(data = list(scores),
           output = output)

  if(output == "prob") {

    matchups <- tmp %>%
      mutate(x = pmap_dbl(list(data, team1, team2, output = output), .fun)) %>%
      select(-data, -output) %>%
      spread(team2, x) %>%
      mutate_if(is.numeric, replace_na, 0)

  } else {

    matchups <- tmp %>%
      mutate(x = pmap_chr(list(data, team1, team2, output = output), .fun)) %>%
      select(-data, -output) %>%
      spread(team2, x) %>%
      mutate_if(is.character, replace_na, 0)

  }

  matchups <- rename(matchups, team = team1)

  if(matrix) {

    matchups <- as.matrix(matchups)

    row.names(matchups) <- matchups[,1]
    matchups <- matchups[,-1]

  }

  matchups

}

compare_current_matchups <- function(scores, schedule,
                                     current_week,
                                     .fun = compare_teams,
                                     win_prob = NULL) {

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
    mutate(data = list(scores),
           fvoa_wp = pmap_dbl(list(data, team1, team2), .fun)) %>%
    arrange(-fvoa_wp) %>%
    head(cutoff) %>%
    mutate(Line = map_chr(fvoa_wp, prob_to_odds),
           fvoa_wp = round(fvoa_wp/100, 2) %>% format_pct,
           Spread = pmap_chr(list(data, team1, team2, output = "spread"),
                             compare_teams)) %>%
    select(Winner = team1,
           Loser = team2,
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
             Spread, Line)
  }

  current_matchups
}

compare_playoff_teams <- function(scores, seed1, seed2, seed3, seed4,
                                  reps = 1e6, reg_games = 6, output = "prob",
                                  .fun = compare_teams) {

  set.seed(42)

  # Identify league and teams
  t1 <- enquo(seed1)
  t2 <- enquo(seed2)
  t3 <- enquo(seed3)
  t4 <- enquo(seed4)

  # Simulate each possible matchup
  t1r1 <- .fun(scores, !!t1, !!t4)/100
  t2r1 <- .fun(scores, !!t2, !!t3)/100
  t3r1 <- .fun(scores, !!t3, !!t2)/100
  t4r1 <- .fun(scores, !!t4, !!t1)/100
  t12r2 <- .fun(scores, !!t1, !!t1)/100
  t13r2 <- .fun(scores, !!t1, !!t3)/100
  t21r2 <- .fun(scores, !!t2, !!t1)/100
  t24r2 <- .fun(scores, !!t2, !!t4)/100
  t31r2 <- .fun(scores, !!t3, !!t1)/100
  t34r2 <- .fun(scores, !!t3, !!t4)/100
  t42r2 <- .fun(scores, !!t4, !!t2)/100
  t43r2 <- .fun(scores, !!t4, !!t3)/100

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

  # List results
  tibble(Winner = c(quo_name(t1), quo_name(t2), quo_name(t3), quo_name(t4)),
         Percent = c(t1wins, t2wins, t3wins, t4wins) / 100,
         Odds = c(t1odds, t2odds, t3odds, t4odds),
         BettingLine = as.factor(c(t1Amodds, t2Amodds, t3Amodds, t4Amodds))) %>%
    arrange(-Percent) %>%
    mutate(Percent = scales::percent(Percent))
}
