matchup <- function(scores, team1, team2, reps = 1e6,
                    type = "prob", breakdown = FALSE,
                    reg_games = 6, reg_points = 108) {

  set.seed(42)

  # Identify teams and weeks
  t1 <- quo_name(enquo(team1))
  t2 <- quo_name(enquo(team2))
  wk <- max(scores$Week)

  # Extract team scores
  t1_scores <- scores %>% filter(Team == t1) %>% pull(Score)
  t2_scores <- scores %>% filter(Team == t2) %>% pull(Score)

  # Calculate team weighted mean/SD
  if (wk < reg_games) {
    t1_mean <- weighted.mean(c(t1_scores, reg_points), weight_games(t1_scores, reg_games))
    t1_sd <- weighted_sd(c(t1_scores, reg_points), weight_games(t1_scores, reg_games), method="ML")
    t2_mean <- weighted.mean(c(t2_scores, reg_points), weight_games(t2_scores, reg_games))
    t2_sd <- weighted_sd(c(t2_scores, reg_points), weight_games(t2_scores, reg_games), method="ML")
  } else {
    t1_mean <- weighted.mean(t1_scores, weight_games(t1_scores, reg_games))
    t1_sd <- weighted_sd(t1_scores, weight_games(t1_scores, reg_games), method="ML")
    t2_mean <- weighted.mean(t2_scores, weight_games(t2_scores, reg_games))
    t2_sd <- weighted_sd(t2_scores, weight_games(t2_scores, reg_games), method="ML")
  }

  # Simulate repeated scores from team's mean/SD
  t1_sim <- rnorm(reps, t1_mean, t1_sd)
  t2_sim <- rnorm(reps, t2_mean, t2_sd)

  # Calculate probability of each team winning or tie for the 1e6 scores
  sim <- t1_sim - t2_sim

  if(type == "prob") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)
    t2wins <- round(pnorm(0, mean(sim), sd(sim))*100 - tie/2, 2)

  } else if (type == "odds") {

    tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)

    odds <- prob_to_odds(t1wins)

    return(odds)

  } else if (type == "spread") {

    spread <- -mean(sim)
    spread <- round(spread * 2) / 2
    spread <- if_else(spread > 0, paste0("+", spread),
                      if_else(spread < 0, paste(spread), paste(0)))
    return(spread)

  } else {NA}

  if(breakdown == TRUE) {

    t1squeak <- t1wins - round(pnorm(5, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
    t1blowout <- round(pnorm(20, mean(sim), sd(sim), lower.tail=FALSE)*100, 2)
    t1normal <- t1wins - t1squeak - t1blowout
    t2squeak <- t2wins - round(pnorm(-5, mean(sim), sd(sim))* 100, 2)
    t2blowout <- round(pnorm(-20, mean(sim), sd(sim))*100, 2)
    t2normal <- t2wins - t2squeak - t2blowout

    data.frame(Winner = c(t1, t1, t1, "Tie", t2, t2, t2),
               Type = c("Blowout", "Normal", "Squeaker", "Tie", "Squeaker", "Normal", "Blowout"),
               MarginVictory = c("20+ points", "5-20", "<5 points", "-", "<5 points", "5-20", "20+ points"),
               PctChance = c(t1blowout, t1normal, t1squeak, tie, t2squeak, t2normal, t2blowout))

  } else {

    t1wins

  }
}

all_matchups <- function(scores, type = "prob",
                         reg_games = 6, reps = 1e6,
                         matrix = FALSE) {

  teams <- scores %>% distinct(Team)

  tmp <- teams %>%
    rename(Team1 = Team) %>%
    crossing(teams) %>%
    filter(Team != Team1) %>%
    mutate(data = list(scores),
           type = type)

  if(type == "prob") {

    matchups <- tmp %>%
      mutate(x = pmap_dbl(list(data, Team, Team1, type = type), matchup)) %>%
      select(-data, -type) %>%
      spread(Team1, x) %>%
      mutate_if(is.numeric, replace_na, 0)

  } else {

    matchups <- tmp %>%
      mutate(x = pmap_chr(list(data, Team, Team1, type = type), matchup)) %>%
      select(-data, -type) %>%
      spread(Team1, x) %>%
      mutate_if(is.character, replace_na, 0)

  }

  if(matrix) {

    matchups <- as.matrix(matchups)

    row.names(matchups) <- matchups[,1]
    matchups <- matchups[,-1]

  }

  matchups

}

current_matchups <- function(week, schedule, scores, win_prob = NULL) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

  current_matchups <- schedule %>%
    filter(Week == week) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(data = list(scores),
           fvoa_wp = pmap_dbl(list(data, Team1, Team2), matchup)) %>%
    filter(fvoa_wp >= 50) %>%
    arrange(-fvoa_wp) %>%
    mutate(Line = map_chr(fvoa_wp, prob_to_odds),
           fvoa_wp = round(fvoa_wp/100, 2) %>% format_pct,
           Spread = pmap_chr(list(data, Team1, Team2, type = "spread"),
                             matchup)) %>%
    select(Winner = Team1,
           Loser = Team2,
           FVOA = fvoa_wp,
           Spread,
           Line)

  if(!is.null(win_prob)) {
    current_matchups <- current_matchups %>%
      left_join(win_prob %>%
                  rename(yahoo_wp = win_prob),
                by = c("Winner" = "Team")) %>%
      select(Winner, Loser, FVOA,
             Yahoo = yahoo_wp,
             Spread, Line)
  }

  current_matchups
}


playoff_matchups <- function(scores, team1, team2, team3, team4,
                             reps = 1e6, reg_games = 6, type = "prob") {

  set.seed(42)

  # Identify league and teams
  t1 <- enquo(team1)
  t2 <- enquo(team2)
  t3 <- enquo(team3)
  t4 <- enquo(team4)

  # Simulate each possible matchup
  t1r1 <- matchup(scores, !!t1, !!t2)/100
  t2r1 <- matchup(scores, !!t2, !!t1)/100
  t3r1 <- matchup(scores, !!t3, !!t4)/100
  t4r1 <- matchup(scores, !!t4, !!t3)/100
  t13r2 <- matchup(scores, !!t1, !!t3)/100
  t14r2 <- matchup(scores, !!t1, !!t4)/100
  t23r2 <- matchup(scores, !!t2, !!t3)/100
  t24r2 <- matchup(scores, !!t2, !!t4)/100
  t31r2 <- matchup(scores, !!t3, !!t1)/100
  t32r2 <- matchup(scores, !!t3, !!t2)/100
  t41r2 <- matchup(scores, !!t4, !!t1)/100
  t42r2 <- matchup(scores, !!t4, !!t2)/100

  # Calculate chances of each team winning both rounds
  t1wins <- round(t1r1 * ((t3r1 * t13r2) + (t4r1 * t14r2)), 4) * 100
  t2wins <- round(t2r1 * ((t3r1 * t23r2) + (t4r1 * t24r2)), 4) * 100
  t3wins <- round(t3r1 * ((t1r1 * t31r2) + (t2r1 * t32r2)), 4) * 100
  t4wins <- round(t4r1 * ((t1r1 * t41r2) + (t2r1 * t42r2)), 4) * 100

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
  data_frame(Winner = c(quo_name(t1), quo_name(t2), quo_name(t3), quo_name(t4)),
                     Percent = c(t1wins, t2wins, t3wins, t4wins) / 100,
                     Odds = c(t1odds, t2odds, t3odds, t4odds),
                     BettingLine = as.factor(c(t1Amodds, t2Amodds, t3Amodds, t4Amodds))) %>%
    arrange(-Percent) %>%
    mutate(Percent = scales::percent(Percent))
}
