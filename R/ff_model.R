# Packages and Helper Functions -------------------------------------------

# Load Packages
library(tidyverse)
library(rlang)
library(lubridate)
library(rvest)
library(glue)
library(Hmisc)
library(scales)
library(progress)
# library(googlesheets)
# library(httr)
# library(jsonlite)

# Helper functions/settings
reps <- 1e6
current_year = year(Sys.Date())
weight_games <- function(scores, reg_games = 6) {
  
  # Slightly penalize outliers
  bottom.outlier <- quantile(scores, 0.25) - IQR(scores) * 1.25
  top.outlier <- quantile(scores, 0.75) + IQR(scores) * 1.25
  outlier.weights <- ifelse(scores < bottom.outlier, 1 - (bottom.outlier - scores)/bottom.outlier, 
                            ifelse(scores > top.outlier, 1 - (scores - top.outlier)/top.outlier, 1))
  
  # Add extra term for regression to mean weeks
  if (length(scores) < reg_games) {outlier.weights <- c(outlier.weights, 1)}
  
  # Make sure no weight is negative due to extreme score
  outlier.weights <- if_else(outlier.weights <= 0, 0.1, outlier.weights)
  
  
  if (length(scores) < reg_games) {
    
    weekly.weights <- replicate(length(scores), NA)
    uniform.weights <- rep(((10 - reg_games + length(scores))/10)/length(scores), length(scores))
    if(length(scores) %% 2 == 0) { #even
      index <- length(scores)/2
      for (i in 1:length(scores)) {
        weekly.weights[i] <- ifelse(i <= index, uniform.weights - ((index+1)-i)* .01, uniform.weights + (i-(index))* .01) # fix
      }
    } else { #odd
      index <- (length(scores)+1)/2
      for (i in 1:length(scores)) {
        if (i != index) { 
          weekly.weights[i] <- ifelse(i < index, uniform.weights - (index-i)*.01, uniform.weights + (i-index)*.01)
        } else weekly.weights[i] <- uniform.weights[i]
      }
    }
    
    weekly.weights <- c(weekly.weights, (reg_games - length(scores))/10)
    weekly.weights <- if_else(weekly.weights < 0, 0, weekly.weights)
    
  } else {
    # Give more weight to recent games
    # Extra weight increases throughout season
    weekly.weights <- replicate(length(scores), NA)
    uniform.weights <- 1/length(scores)
    if(length(scores) %% 2 == 0) { #even
      index <- length(scores)/2
      for (i in 1:length(scores)) {
        weekly.weights[i] <- ifelse(i <= index, uniform.weights - ((index+1)-i)* .01, uniform.weights + (i-(index))* .01) # fix
      }
    } else { #odd
      index <- (length(scores)+1)/2
      for (i in 1:length(scores)) {
        if (i != index) { 
          weekly.weights[i] <- ifelse(i < index, uniform.weights - (index-i)*.01, uniform.weights + (i-index)*.01)
        } else {weekly.weights[i] <- uniform.weights}
      }
    }
  }
  
  combined.weights <- outlier.weights * weekly.weights
  combined.weights <- combined.weights/sum(combined.weights)
  
  if_else(combined.weights < 0, 0, combined.weights)
}
odds_conversion <- function(x) {
  toAmericanOdds <- function(x) { 
    if (!is.numeric(x))
      x <- as.numeric(x)
    x[x >= 2.0 & !is.na(x)] <- (x[x >= 2.0 & !is.na(x)] - 1) * 100
    x[x < 2.0 & !is.na(x)] <- (-100) / (x[x < 2.0 & !is.na(x)] - 1)
    # x <- round(x)
    x <- round(x * 0.04)/0.04
    return(x)
  } # Convert number to American odds
  ifelse(is.na(x), NA, if (toAmericanOdds(100/x) < 0) toAmericanOdds(100/x) else paste("+", toAmericanOdds(100/x), sep=""))
} # Convert win percentage dataframe to winning odds

# Import Data -------------------------------------------------------------

# CLT Weekly from static file
clt_weekly <- read_csv("ff/clt_weekly.csv")

# Updated on work computer w/Excel
clt_colley <- read_csv("ff/clt_colley.csv")
clt_proj <- read_csv("ff/clt_proj.csv")
clt_tidy <- read_csv("ff/clt_tidy.csv")

# Updated on home computer w/Google Sheets
clt_colley <- gs_title("clt_colley") %>% gs_read()
clt_proj <- gs_title("clt_proj") %>% gs_read()
clt_tidy <- clt_proj %>% 
  filter(Type == "act") %>% 
  select(-Type)


# Scrape Data -------------------------------------------------------------

scrape_team <- function(week, team_id, league, league_id, season = 2017) {
  
  league <- str_to_lower(league)

  stopifnot(week %in% 1:17,
            team_id %in% 1:20,
            league %in% c("espn", "yahoo"),
            is.numeric(league_id),
            is.numeric(season)
  )
  
  if(league == "yahoo") {
    
    url <- glue("https://football.fantasysports.yahoo.com/f1/", league_id, 
                "/matchup?week=", week, "&mid1=", team_id)
    
    page <- read_html(url)
    
    name <- page %>% 
      html_nodes(".Ta-end .F-link") %>% 
      html_text()
    
    starters <- page %>% 
      html_nodes("#statTable1") %>% 
      html_table() %>% 
      flatten_dfc() %>% 
      select(1:5) %>% 
      filter(Pos != "Total")
    
    bench <- page %>% 
      html_nodes("#statTable2") %>% 
      html_table(fill = T) %>% 
      flatten_dfc() %>% 
      select(1:5) %>% 
      mutate(Proj = as.numeric(Proj),
             `Fan Pts` = as.numeric(`Fan Pts`)) %>% 
      replace_na(list(`Fan Pts` = 0)) %>%
      filter(Pos != "Total")
    
    bind_rows(starters, 
              bench) %>% 
      mutate(Team = name) %>% 
      rename(Points = `Fan Pts`,
             Lineup = Pos) %>% 
      separate(Player, c("notes", "Player"), "Notes\\s+|Note\\s+") %>% 
      separate(Player, c("Player", "result"), "Final|Bye") %>% 
      separate(Player, c("Player", "Position"), " - ") %>% 
      mutate(Player = str_replace(Player, "\\s[:alpha:]+$", ""),
             Position = str_extract(Position, "[:alpha:]+"),
             Score = sum(starters$`Fan Pts`)) %>% 
      select(Team:Score, Player:Position, Lineup, Proj, Points) %>% 
      ungroup
    
  } else if (league == "espn") {
    
    url <- glue("http://games.espn.com/ffl/boxscorequick?leagueId=", league_id, 
                "&teamId=", team_id, "&scoringPeriodId=", week, "&seasonId=",
                season, "&view=scoringperiod&version=quick")
    
    page <- read_html(url)
    
    name <- page %>% 
      html_nodes("#teamInfos div:nth-child(1) div .bodyCopy div b") %>% 
      html_text()
    
    starters <- page %>% 
      html_nodes("#playertable_0") %>% 
      html_table(fill = T) %>% 
      flatten_dfc() %>% 
      select(1, 2, 5) %>% 
      slice(-c(1:3)) %>% 
      set_names("Lineup", "Player", "Points") %>% 
      mutate(Points = as.numeric(Points),
             Player = str_replace_all(Player, "\\s+Q$|\\s+IR$", ""),
             Position = str_extract(Player, "[:graph:]+$")) %>%  
      separate(Player, c("Player", "Team_Pos"), ",") %>% 
      mutate(Player = str_replace_all(Player, " D/ST", "")) %>% 
      select(Player, Position, Lineup, Points)
    
    bench <- page %>% 
      html_nodes("#playertable_1") %>% 
      html_table() %>% 
      flatten_dfc() %>% 
      select(1, 2, 5) %>% 
      slice(-1) %>% 
      set_names("Lineup", "Player", "Points") %>% 
      mutate(Points = as.numeric(Points),
             Player = str_replace_all(Player, "\\s+Q$|\\s+IR$", ""),
             Position = str_extract(Player, "[:graph:]+$")) %>%  
      separate(Player, c("Player", "Team_Pos"), ",") %>% 
      mutate(Player = str_replace_all(Player, " D/ST", "")) %>% 
      select(Player, Position, Lineup, Points)
    
    bind_rows(starters, bench) %>% 
      replace_na(list(Points = 0)) %>% 
      mutate(Team = name,
             Score = sum(starters$Points, na.rm = T)) %>% 
      select(Team, Score, everything())
    
  }
}
extract_weekly_scores <- function(df) {
  
  if("Points" %in% names(df)) {
    df %>% 
      filter(!Lineup %in% c("BN", "Bench")) %>% 
      group_by(Week, Team) %>% 
      summarise(Proj = sum(Proj),
                Score = sum(Points)) %>% 
      distinct()
    
  } else {
    df %>% 
      distinct(Week, Team, Score)
  }
}

sx_team <- data_frame(Week = 1:2, team_id = list(1:20)) %>%
  unnest() %>% 
  mutate(x = pmap(list(Week, team_id, "espn", 299999), possibly(scrape_team, NA)),
         len = map_dbl(x, length)) %>% 
  filter(len > 1) %>% 
  select(-len) %>% 
  unnest()


wf_team <- data_frame(Week = 1:14, team_id = list(1:20)) %>%
  unnest() %>% 
  mutate(x = pmap(list(Week, team_id, "espn", 266506), possibly(scrape_team, NA)),
         len = map_dbl(x, length)) %>% 
  filter(len > 1) %>% 
  select(-len) %>% 
  unnest()

clt_team <- data_frame(Week = 1:2, team_id = list(1:10)) %>% 
  unnest() %>% 
  mutate(x = pmap(list(Week, team_id, "yahoo", 174230), possibly(scrape_team, NA))) %>% 
  unnest()
  
# Matchups and Playoff Simulations ----------------------------------------

# Matchup functions
matchup <- function(team1, team2, league = clt, reg_games = 6, breakdown = FALSE) {
  set.seed(42)
  
  # Identify teams and league
  l <- quo_name(enquo(league))
  t1 <- quo_name(enquo(team1))
  t2 <- quo_name(enquo(team2))
  league <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  wk <- max(league$Week)
  
  # Extract team scores
  t1.scores <- league %>% filter(Team == t1) %>% pull(Score)
  t2.scores <- league %>% filter(Team == t2) %>% pull(Score)
  
  # Calculate team weighted mean/SD
  if (wk < reg_games) {
    t1.mean <- weighted.mean(c(t1.scores, 115), weight_games(t1.scores, 6))
    t1.sd <- sqrt(wtd.var(c(t1.scores, 115), weight_games(t1.scores, 6), method="ML"))
    t2.mean <- weighted.mean(c(t2.scores, 115), weight_games(t2.scores, 6))
    t2.sd <- sqrt(wtd.var(c(t2.scores, 115), weight_games(t2.scores, 6), method="ML"))
  } else {
    t1.mean <- weighted.mean(t1.scores, weight_games(t1.scores, 6))
    t1.sd <- sqrt(wtd.var(t1.scores, weight_games(t1.scores, 6), method="ML"))
    t2.mean <- weighted.mean(t2.scores, weight_games(t2.scores, 6))
    t2.sd <- sqrt(wtd.var(t2.scores, weight_games(t2.scores, 6), method="ML"))
  }
  
  # Simulate 1e6 scores from team's mean/SD
  t1.sim <- rnorm(reps, t1.mean, t1.sd)
  t2.sim <- rnorm(reps, t2.mean, t2.sd)
  
  # Calculate probability of each team winning or tie for the 1e6 scores
  sim <- t1.sim-t2.sim
  tie <- round(dnorm(0, mean(sim), sd(sim))*100, 2)
  t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)*100 - tie/2, 2)
  t2wins <- round(pnorm(0, mean(sim), sd(sim))*100 - tie/2, 2)
  
  if(breakdown == TRUE) {
    # probability of blowout, normal, squeaker, or tie in matchup
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
    # Matchup determined by simulating 1e6 matchups from weighted scores
    data.frame(Winner = c(t1, t2, "Tie"), Percent = c(t1wins, t2wins, tie)) 
  }
}
all_matchups <- function(league = clt, type = prob, reg_games = 6) {
  set.seed(42)
  
  # Identify league and teams 
  l <- quo_name(enquo(league))
  league <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  type <- quo_name(enquo(type))
  teams <- league %>% select(Team) %>% distinct()
  wk <- max(league$Week)
  
  ifnot(type %in% c("prob", "spread"),
            is.numeric(reg_games))
  
  # Create empty data frame
  team.names <- teams[[1]]
  df <- as.data.frame(matrix(nrow=nrow(teams), ncol=nrow(teams)))
  rownames(df) <- team.names
  colnames(df) <- team.names
  
  # Calculate each team's weighted mean/SD
  if (wk < reg_games) {
    league_boot <- league %>% 
      group_by(Team) %>% 
      mutate(team_mean = weighted.mean(c(Score, 115), weight_games(Score, reg_games)),
             team_sd = sqrt(wtd.var(c(Score, 115), weight_games(Score, reg_games), method="ML"))) %>%
      select(Team, team_mean, team_sd) %>% 
      ungroup() %>% 
      distinct()
  } else {
    league_boot <- league %>% 
      group_by(Team) %>% 
      mutate(team_mean = weighted.mean(Score, weight_games(Score, reg_games)),
             team_sd = sqrt(wtd.var(Score, weight_games(Score, reg_games), method="ML"))) %>%
      select(Team, team_mean, team_sd) %>% 
      ungroup() %>% 
      distinct()
  }
  
  # Simulate 1e6 scores from team's mean/SD
  sims <- list()
  sim_scores <- for(i in 1:nrow(teams)) {sims[[i]] <- rnorm(reps, league_boot[[i, 2]], league_boot[[i,3]])}
  names(sims) <- teams[[1]]
  
  # Function to calculate each individual matchup
  matchup_season <- function(i, j) {
    t1 <- rownames(df)[i] # index ID
    t2 <- colnames(df)[j]
    sim <- sims[[t1]] - sims[[t2]]
    if(type == "prob") { # calculate win probability of each game
      round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)* 100, 2)
    } else if (type == "spread") { # calculate spread of each game
      spread <- -mean(sim)
      spread <- round(spread * 2) / 2
      if_else(spread > 0, paste0("+", spread), 
              if_else(spread < 0, paste(spread), paste(0)))
    } else {NA}
  }
  
  # Loop over every combination of possible matchups
  for(i in 1:dim(df)[1]) {
    for(j in 1:dim(df)[2]) {
      if (i == j) {df[i, j] <- "0"} 
      else if (is.na(df[i,j])==F) {next}
      else {
        df[i,j] = matchup_season(i, j)
      }
    }
  }
  df
} # Calculate spread of all matchups

# Gambling lines for every possible matchup
clt_matchups_prob <- all_matchups(clt, type = prob) 
clt_lines <- as.data.frame(apply(clt_matchups_prob, c(1, 2), odds_conversion))

# Playoff function
playoff <- function(team1, team2, team3, team4, league = clt) { 
  set.seed(42)
  
  # Identify league and teams
  l <- quo_name(enquo(league))
  t1 <- quo_name(enquo(team1))
  t2 <- quo_name(enquo(team2))
  t3 <- quo_name(enquo(team3))
  t4 <- quo_name(enquo(team4))
  league <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  
  # Extract each team's score, calculate weighted mean/SD, and simulate 1e6 scores
  t1.scores <- league %>% filter(Team == t1) %>% pull(Score)
  t1.mean <- weighted.mean(t1.scores, weight_games(t1.scores, 6))
  t1.sd <- sqrt(wtd.var(t1.scores, weight_games(t1.scores, 6), method="ML"))
  t1.sim <- rnorm(reps, t1.mean, t1.sd)
  t2.scores <- league %>% filter(Team == t2) %>% pull(Score)
  t2.mean <- weighted.mean(t2.scores, weight_games(t2.scores, 6))
  t2.sd <- sqrt(wtd.var(t2.scores, weight_games(t2.scores, 6), method="ML"))
  t2.sim <- rnorm(reps, t2.mean, t2.sd)
  t3.scores <- league %>% filter(Team == t3) %>% pull(Score)
  t3.mean <- weighted.mean(t3.scores, weight_games(t3.scores, 6))
  t3.sd <- sqrt(wtd.var(t3.scores, weight_games(t3.scores, 6), method="ML"))
  t3.sim <- rnorm(reps, t3.mean, t3.sd)
  t4.scores <- league %>% filter(Team == t4) %>% pull(Score)
  t4.mean <- weighted.mean(t4.scores, weight_games(t4.scores, 6))
  t4.sd <- sqrt(wtd.var(t4.scores, weight_games(t4.scores, 6), method="ML"))
  t4.sim <- rnorm(reps, t4.mean, t4.sd)
  sims <- list(t1 = t1.sim, t2 = t2.sim, t3 = t3.sim, t4 = t4.sim)
  
  # Function to determine probability of winner winning
  matchup <- function(winner, loser) {
    winner <- quo_name(enquo(winner))
    loser <- quo_name(enquo(loser))
    sim <- sims[[winner]] - sims[[loser]]
    round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE), 4)
  } 
  toAmericanOdds <- function(x) { 
    if (!is.numeric(x))
      x <- as.numeric(x)
    # For decimal odds greater than or equal to 2, 
    # American odds = (decimal odds ??? 1) * 100 
    x[x >= 2.0 & !is.na(x)] <- (x[x >= 2.0 & !is.na(x)] - 1) * 100
    # For decimal odds less than 2,
    # American odds = (-100) / (decimal odds - 1)
    x[x < 2.0 & !is.na(x)] <- (-100) / (x[x < 2.0 & !is.na(x)] - 1)
    # Remove decimal places
    # x <- round(x)
    x <- round(x * 0.04)/0.04
    return(x)
  } # Convert decimal odds to American odds
  
  # Simulate each possible matchup
  t1r1 <- matchup(t1, t2)
  t2r1 <- matchup(t2, t1)
  t3r1 <- matchup(t3, t4)
  t4r1 <- matchup(t4, t3)
  t13r2 <- matchup(t1, t3)
  t14r2 <- matchup(t1, t4)
  t23r2 <- matchup(t2, t3)
  t24r2 <- matchup(t2, t4)
  t31r2 <- matchup(t3, t1)
  t32r2 <- matchup(t3, t2)
  t41r2 <- matchup(t4, t1)
  t42r2 <- matchup(t4, t2)
  
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
  
  # t1odds <- paste(round(100/t1wins, 2), ":", 1, sep="")
  # t2odds <- paste(round(100/t2wins, 2), ":", 1, sep="")
  # t3odds <- paste(round(100/t3wins, 2), ":", 1, sep="")
  # t4odds <- paste(round(100/t4wins, 2), ":", 1, sep="")
  
  t1Amodds <- if (toAmericanOdds(100/t1wins) < 0) toAmericanOdds(100/t1wins) else 
    paste("+", toAmericanOdds(100/t1wins), sep="")
  t2Amodds <- if (toAmericanOdds(100/t2wins) < 0) toAmericanOdds(100/t2wins) else 
    paste("+", toAmericanOdds(100/t2wins), sep="")
  t3Amodds <- if (toAmericanOdds(100/t3wins) < 0) toAmericanOdds(100/t3wins) else 
    paste("+", toAmericanOdds(100/t3wins), sep="")
  t4Amodds <- if (toAmericanOdds(100/t4wins) < 0) toAmericanOdds(100/t4wins) else 
    paste("+", toAmericanOdds(100/t4wins), sep="")
  
  # List results
  arrange(data_frame(Winner = c(t1, t2, t3, t4), 
                     Percent = percent(c(t1wins, t2wins, t3wins, t4wins)/100), 
                     Odds = c(t1odds, t2odds, t3odds, t4odds), 
                     BettingLine = as.factor(c(t1Amodds, t2Amodds, t3Amodds, t4Amodds))),
          desc(Percent))
}

# Model Evaluation --------------------------------------------------------
# Evaluate model for how many games were correctly predicted and plots results
# Details include matchup breakdowns, team summary, and percentage tiers
# If print = F it lists breakdown of each week for exploration

evaluate_model <- function(league = clt, details=F, print=T, shiny=F, reg_games=6) {
  set.seed(42)
  l <- quo_name(enquo(league))
  league_df <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  teams <- league_df %>% select(Team) %>% distinct()
  team_names <- teams[[1]]
  weekly_sims <- sapply(paste("Week", 1:n_distinct(league_df$Week)), function(x) NULL)
  brier_sims <- data_frame(Week = 1, brier = NA) %>% nest(brier)
  perc_tiers <- data_frame(week = 1, tier = NA, n = NA, percent.correct = NA, correct = NA) %>% nest(tier:correct)
  for (w in seq_along(weekly_sims)) {
    if (w == 1) {
      weekly_sims[[w]] <- NA
    } else { 
      df <- as.data.frame(matrix(nrow=nrow(teams), ncol=nrow(teams)))
      rownames(df) <- team_names
      colnames(df) <- team_names
      if (reg_games != 0 & w <= reg_games) {
        league_sim <- league_df %>% 
          group_by(Team) %>% 
          filter(Week %in% 1:(w-1)) %>% 
          mutate(team_mean = weighted.mean(c(Score, 115), weight_games(Score, reg_games)),
                 team_sd = sqrt(wtd.var(c(Score, 115), weight_games(Score, reg_games), method="ML"))) %>%
          select(Team, team_mean, team_sd) %>% 
          ungroup() %>% 
          distinct()
      } else {
        league_sim <- league_df %>% 
          group_by(Team) %>% 
          filter(Week %in% 1:(w-1)) %>% 
          mutate(team_mean = weighted.mean(Score, weight_games(Score, reg_games)),
                 team_sd = sqrt(wtd.var(Score, weight_games(Score, reg_games), method="ML"))) %>%
          select(Team, team_mean, team_sd) %>% 
          ungroup() %>% 
          distinct()
      }
      league_scores <- league_df %>% 
        filter(Week == w) %>% 
        select(Team, Score)
      sims <- list()
      for (i in 1:nrow(teams)) {sims[[i]] <- rnorm(reps, league_sim[[i, 2]], league_sim[[i, 3]])}
      names(sims) <- teams[[1]]
      matchup_season <- function(i, j) {
        t1 <- rownames(df)[i] # index ID
        t2 <- colnames(df)[j]
        sim <- sims[[t1]] - sims[[t2]]
        t1_proj_win <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)* 100, 2)
        t1_actual_margin <- league_scores %>% filter(Team == t1) %>% pull(Score) - 
          league_scores %>% filter(Team == t2) %>% pull(Score)
        correct <- ifelse(t1_proj_win < 50 & t1_actual_margin < 0 | t1_proj_win > 50 & t1_actual_margin > 0 | 
                            t1_proj_win == 50 & t1_actual_margin == 0, 1, 0)
        ifelse(t1_proj_win == 0 & correct == 1, 1000, 
               ifelse(t1_proj_win == 0 & correct == 0, -1000,
                      ifelse(correct == 1, t1_proj_win, -t1_proj_win)))
      }
      for(i in 1:dim(df)[1]) {
        for(j in 1:dim(df)[2]) {
          if (i == j) {NA} 
          else if (is.na(df[i,j])==F) {next} 
          else {
            df[i,j] = matchup_season(i, j)
            df[j,i] = df[i, j]
          }
        }
      }
      convert_binary <- function(x) ifelse(x < 0, 0, 1)
      tmp <- map(df, convert_binary) %>% as.data.frame()
      row.names(tmp) <- colnames(tmp)
      weekly_sims[[w]] <- tmp 
      tiers <- df %>%
        gather() %>% 
        drop_na() %>% 
        mutate(correct = ifelse(value > 0, 1, 0),
               sum=sum(correct),
               tier = case_when(
                 abs(value) == 100 | abs(value) == 1000 ~ 100,
                 abs(value) >= 90 | abs(value) <= 10 ~ 90,
                 abs(value) >= 80 | abs(value) <= 20 ~ 80,
                 abs(value) >= 70 | abs(value) <= 30 ~ 70,
                 abs(value) >= 60 | abs(value) <= 40 ~ 60,
                 abs(value) >= 50 | abs(value) <= 50 ~ 50)) %>%
        group_by(tier) %>% 
        mutate(n = n(),
               percent.correct = round(sum(correct)/n,2)) %>% 
        select(tier, n, percent.correct) %>% 
        arrange(-tier) %>%
        mutate(correct = round(n * percent.correct, 0)) %>% 
        distinct()
      perc_tiers <- perc_tiers %>% add_row(week = w, data = list(tiers))
      brier <- df %>%
        gather() %>% 
        drop_na() %>% 
        mutate(outcome = if_else((value < 0 & abs(value) > 50) | (value > 0 & abs(value) < 50), 0, 1),
               value = if_else(abs(value) == 1000, 0, value),
               pred = abs(value)/100,
               brier = (0.25 - (pred-outcome)^2) * 100) %>% 
        select(team = key, pred, outcome, brier)
      brier_sims <- brier_sims %>% add_row(Week = w, data = list(brier))
    }
  }
  correct <- round(sum(map_dbl(weekly_sims, sum, na.rm=T))/((nrow(teams)^2 - nrow(teams)) * (length(weekly_sims)-1)), 4) * 100
  total_brier <- brier_sims %>% unnest() %>% drop_na() %>% summarise(round(mean(brier), 3)) %>% pull()
  brier_statement <- paste("The model got a final Brier score of", total_brier)
  brier_breakdown <- brier_sims %>% unnest() %>% drop_na() %>% filter(Week == nrow(brier_sims)) %>% 
    group_by(team) %>% summarise(total = round(mean(brier), 2)) %>% spread(team, total) %>% map_dbl(sum) %>% sort()
  statement <- paste0("The model correctly predicted ", correct, "% of games")
  benchmark <- (nrow(teams)^2 - nrow(teams))
  team_accuracy <- sapply(paste("Week", 1:n_distinct(league_df$Week)), function(x) NULL)
  for (i in seq_along(team_accuracy)) {team_accuracy[[i]] <- map_dbl(weekly_sims[[i]], sum, na.rm=T) %>% sort()}
  plot <- data_frame(weekly = map_dbl(weekly_sims, sum, na.rm=T)) %>% 
    mutate(week = 1:length(weekly),
           delta = weekly - benchmark/2,
           percent = round(weekly/benchmark * 100, 1),
           sign = ifelse(delta > 0, "positive", ifelse(delta < 0, "negative", "equal"))) %>% 
    slice(-1) %>% 
    ggplot(aes(week, delta, fill=sign, label=percent)) + 
    geom_bar(stat = 'identity') + 
    geom_text(size = 3, alpha = 0.7) +
    scale_x_continuous(name = "Week", breaks = 2:n_distinct(league_df$Week)) +
    scale_y_continuous(limits = c(0-benchmark/2, benchmark/2), 
                       breaks = c(0-benchmark/2, ((0-benchmark/2)/2), 0, benchmark/4, benchmark/2),
                       labels = c(0, 25, 50, 75, 100)) +
    scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
    labs(title = "Weekly Evaluation of Model", x = "Week (starting with Week 2)", y = "Percent Correct") +
    theme(panel.background= element_blank(), panel.border = element_blank()) +
    guides(fill=F)
  perc_tiers_all <- perc_tiers %>%
    unnest() %>% 
    drop_na() %>% 
    # filter(week == nrow(perc_tiers)) %>% 
    group_by(tier) %>% 
    transmute(n = sum(n),
              correct = sum(correct),
              percent = round(correct/n, 4) * 100) %>% 
    distinct() %>% 
    arrange(-tier)
  perc_plot <- ggplot(perc_tiers_all, aes(tier, percent)) +
    # geom_point() +
    geom_text(aes(label = percent), size = 3, alpha = 0.7, vjust = "outward") +
    geom_line() +
    geom_abline(color = "red") + 
    geom_abline(color = "red", intercept = 10) +
    scale_x_continuous(limits = c(50, 100)) +
    scale_y_continuous(limits = c(30, 100)) +
    labs(x = "Tier", y = "Percent Correct", title = "Calibration of Weekly Predictions")
  plots <- cowplot::plot_grid(plot, perc_plot, nrow = 2)
  if (!details & print==T) {
    print(statement)
    print(brier_statement)
    print(plots)
  } else if (print == T) {
    print(weekly_sims[[length(weekly_sims)]])
    cat("\n")
    print(team_accuracy[[length(weekly_sims)]])
    cat("\n")
    print(brier_breakdown)
    cat("\n")
    print(perc_tiers_all)
    cat("\n")
    print(statement)
    cat("\n")
    print(brier_statement)
    print(plots)
  } else if (shiny == T) {
    shiny_df <- data_frame(x = weekly_sims) %>% mutate(week = 1:length(weekly_sims)) %>% slice(-1) %>% unnest()
    write_csv(shiny_df, "ff/model_eval.csv")
  } else {
    list(Weekly = weekly_sims, Team = team_accuracy, Tiers = perc_tiers,
         Brier = brier_sims, Accuracy = statement, Plot = plots)
  }
}

# Save model evaluation for Shiny app
evaluate_model(print = F, shiny = T)
evaluate_model()

# Season Simulation -------------------------------------------------------

simulate_season <- function(league = clt, sims = 1000, reg_games = 6) {
  
  # Identify league, teams, and weeks played
  l <- quo_name(enquo(league))
  league_tidy <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  league_weekly <- eval_tidy(parse_quosure(paste0(quo_name(l), "_weekly")))
  teams <- league_tidy %>% pull(Team) %>% unique()
  weeks_played <- league_tidy %>% distinct(Week) %>% pull()
  file_simulation <- paste0(l, "_simulated_season_", current_year, ".csv")
  
  # Load previous simulations unless week 1 not run yet
  if (length(weeks_played) == 1) {
    tryCatch(previous_simulations <- suppressMessages(read_csv(file_simulation)),
             error = function(e) previous_simulations <- data_frame(Week = 0))
  } else {
    tryCatch(previous_simulations <- suppressMessages(read_csv(file_simulation)),
             error = function(e) print("No simulations found"))
  }
  
  # Determine if season already simulated for current week
  if (max(previous_simulations$Week) == max(weeks_played)) {
    
    # If yes return current simulation
    simulated_season <- previous_simulations
    
  } else { # If no run simulation
    
    # Determine is all games played
    if (max(weeks_played) == max(league_weekly$Week)) {
      
      # Get final standings
      t1_stats <- league_tidy %>% rename(Team1 = Team, Score1 = Score)
      t2_stats <- league_tidy %>% rename(Team2 = Team, Score2 = Score)
      suppressMessages(stats <- league_weekly %>%
                         inner_join(t1_stats) %>%
                         inner_join(t2_stats) %>%
                         mutate(diff = Score1 - Score2,
                                win = if_else(diff > 0, 1, 0),
                                lose = if_else(diff < 0, 1, 0),
                                tie = if_else(diff == 0, 1, 0)) %>%
                         group_by(Team1) %>%
                         summarise(Wins = sum(win)) %>%
                         rename(Team = Team1) %>% 
                         arrange(-Wins) %>%
                         mutate_if(is.numeric, as.integer))
      
      # If simulating all weeks add final tick
      if (exists("pb")) {
        pb$tick()
        Sys.sleep(1/100)
      }
      
      # Join final standings with total points
      final_standings <- league_tidy %>% 
        group_by(Team) %>% 
        summarise(Points = round(sum(Score), 2)) %>% 
        left_join(stats, by = "Team") %>% 
        arrange(-Wins, -Points) %>% 
        mutate(Percent = c(100, 100, 100, 100, 0, 0, 0, 0, 0, 0),
               Rank = 1L:10L,
               Week = max(weeks_played))
      
      # Merge final standings with season simulations
      simulated_season <- bind_rows(previous_simulations, final_standings)
      
      # Save full season to CSV
      write_csv(simulated_season, file_simulation)
      write_csv(simulated_season, "ff/simulated_seasons.csv")
      
    } else { 
      
      # Create progress bar if not already running
      if (!exists("pb")) {
        pb <- progress_bar$new(
          format = "  simulating [:bar] :percent eta: :eta",
          total = sims, clear = FALSE, width= 80)
      }
      
      # Identify unplayed weeks
      remaining_weeks <- league_weekly %>% 
        filter(!Week %in% weeks_played) %>% 
        distinct(Week) %>% 
        pull()
      
      # Create list for simulated seasons
      sim_seasons <- list()
      
      # Simulate remaining weeks [sims] times to get reliable estimates
      for (i in 1:sims) {
        
        # Create temporary df of current scores
        tmp_tidy <- league_tidy
        
        # Simulate each week sequentially to let the scores carry forward
        for (w in remaining_weeks) {
          
          # # Extract team scores, including simulated scores for previous weeks
          for (t in teams) {
            current_scores <- tmp_tidy %>% filter(Team == t) %>% pull(Score)
            
            # If less than 6 weeks played use historical regression to mean to supplement scores
            if (w <= reg_games) {
              current_mean <-  weighted.mean(c(current_scores, 115), weight_games(current_scores, reg_games))
              current_sd <- sqrt(wtd.var(c(current_scores, 115), weight_games(current_scores, reg_games), method="ML"))
              
              # Otherwise simulate score from team's distribution
            } else {
              # Calculate team weighted mean/SD
              current_mean <- weighted.mean(current_scores, weight_games(current_scores, reg_games))
              current_sd <- sqrt(wtd.var(current_scores, weight_games(current_scores, reg_games), method = "ML"))
              
            }
            
            # Calculate random score from team's weighted distribution
            simulated_score <- rnorm(1, current_mean, current_sd) 
            
            # Ensure no negative scores by assigning it to historical low
            simulated_score <- if_else(simulated_score <= 0, 50, simulated_score)
            
            # Add this simulated score to temporary df along with week and team
            tmp_tidy <- tmp_tidy %>% add_row(Week = w, Team = t, Score = round(simulated_score, 2))
          }
        }
        
        # Determine total points for each team for current simulation
        total_points <- tmp_tidy %>% group_by(Team) %>% summarise(Points = sum(Score)) %>% arrange(-Points)
        
        # Get final standings
        t1_stats <- tmp_tidy %>% rename(Team1 = Team, Score1 = Score)
        t2_stats <- tmp_tidy %>% rename(Team2 = Team, Score2 = Score)
        suppressMessages(final_record <- league_weekly %>%
                           inner_join(t1_stats, by = c("Week", "Team1")) %>%
                           inner_join(t2_stats, by = c("Week", "Team2")) %>%
                           mutate(diff = Score1 - Score2,
                                  win = if_else(diff > 0, 1, 0),
                                  pl_win = if_else(Week == max(weeks_played) + 1, 0, if_else(diff > 0, 1, 0)),
                                  lose = if_else(diff < 0, 1, 0),
                                  tie = if_else(diff == 0, 1, 0)) %>%
                           group_by(Team1) %>%
                           summarise(Wins = sum(win),
                                     pl_wins = sum(pl_win),
                                     Losses = sum(lose),
                                     Tie = sum(tie)) %>%
                           rename(Team = Team1) %>% 
                           arrange(-Wins, - Losses) %>%
                           mutate_if(is.numeric, as.integer))
        
        # Join final standings with total points
        sim_seasons[i] <- list(total_points %>% 
                                 right_join(final_record, by = "Team") %>% 
                                 arrange(-Wins, -Points))
        
        # Add tick for each simulation
        pb$tick()
        Sys.sleep(1/100)
      }
      
      # Save all simulations for playoff leverage chart
      write_csv(data_frame(sim_seasons) %>% 
                  mutate(sim = row_number()) %>% 
                  unnest(), 
                "ff/playoff_leverage.csv")
      
      # Tidy simulated data for reporting/visualizing after all simulations run
      final_df <- data_frame(sim_seasons) %>% 
        unnest() %>% 
        group_by(Team) %>%
        
        # Top 4 teams from each simulation make the playoffs
        mutate(playoff = map(sim_seasons, function(x) x %>% slice(1:4) %>% pull(Team))) %>% 
        unnest() %>% 
        
        # Calculate percentage each team made the playoffs
        mutate(playoffs = if_else(Team == playoff, 1, 0)) %>% 
        
        # Calculate average total points/wins and playoff chances for simulated season
        summarise(Points = round(mean(Points), 1),
                  Wins = round(mean(Wins), 1),
                  Percent = sum(playoffs)/sims * 100) %>% 
        arrange(-Percent, -Wins, -Points) %>% 
        mutate(Rank = 1L:10L,
               Week = max(league_tidy$Week))
      
      if (!file.exists(file_simulation)) {
        
        # Create file and save to CSV
        simulated_season <- final_df
        write_csv(simulated_season, file_simulation)
        write_csv(simulated_season, "ff/simulated_seasons.csv")
        
      } else {
        
        # Merge final standings with season simulations
        previous_simulations <- suppressMessages(read_csv(file_simulation))
        simulated_season <- bind_rows(previous_simulations, final_df)
        
        # Save simulated season to CSV
        write_csv(simulated_season, paste0(file_simulation))
        write_csv(simulated_season, "ff/simulated_seasons.csv")
      }
    }
  }
  simulated_season
}

clt_simulated_season <- simulate_season(clt)

# FVOA --------------------------------------------------------------------
fvoa <- list()
clt_tidy_orig <- clt_tidy

for (i in 1:n_distinct(clt_tidy$Week)) {
  clt_tidy <- clt_tidy_orig %>% filter(Week %in% 1:i)
  matchups_prob <- all_matchups_prob()
  fvoa_rankings <- matchups_prob %>% 
    map_df(function(x) {round((mean(100 - x, na.rm=T) - 50)/.5, 2)}) %>% 
    gather(Team, FVOA) %>% 
    arrange(-FVOA) %>% 
    mutate(`FVOA Rank` = dense_rank(-FVOA),
           Week = i)
  fvoa[i] <- list(fvoa_rankings)
}
fvoa_season <- data_frame(fvoa) %>% 
  unnest()

fvoa_season %>% 
  ggplot(aes(x = Week, y = FVOA, color = Team)) +
  geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
  geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
  geom_point(aes(group=Team, color=Team)) +
  # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
  scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
  labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
  guides(color=FALSE) +
  ff_theme()

# Plots -------------------------------------------------------------------
# Theme function
ff_theme <- function(base_size = 12, base_family = "Helvetica") {
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour="grey50"),
        strip.background = element_rect(color = "black"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        strip.text = element_text(size =12))
}

# Plot functions

weekly_plots <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_tidy <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  ggplot(league_tidy, aes(x=Week, y=Score, group=Team, color = Team)) + 
    geom_line(size = 1.5) +
    geom_point(size = 2) + 
    facet_wrap(~reorder(Team, -Score, FUN = mean), ncol=n_distinct(league_tidy$Team)/2) +
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = pretty_breaks(n = 7)) +
    labs(y = "Score", x = "Week", title = "Weekly Scores") +
    guides(color=FALSE) + 
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") + 
    ff_theme()
}
boxplots <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_tidy <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  ggplot(league_tidy, aes(x=reorder(Team, -Score, fun=mean), y=Score, fill=Team)) + 
    geom_boxplot(coef = 1.25, outlier.alpha = 0.6) + 
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend=FALSE) + 
    guides(fill=F) +
    labs(y = "Score", x = "", title = "Team Boxplots") +
    ff_theme() + 
    theme(panel.border = element_blank())
}
joy_plots <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_tidy <- eval_tidy(parse_quosure(paste0(quo_name(l), "_tidy")))
  ggplot(league_tidy, aes(x = Score, y = reorder(Team, Score, FUN = mean), fill = Team)) + 
    geom_density_ridges() + 
    geom_vline(aes(xintercept = mean(Score)), alpha = 0.5) +
    labs(x = "Distribution of Scores", y = "", title = "Team Density Plots") +
    guides(fill=FALSE) + 
    ff_theme()
}
plot_matchups <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_temp <- eval_tidy(parse_quosure(paste0(quo_name(l), "_matchups_prob")))
  league_temp$winner <- rownames(league_temp)
  league_tidy <- gather(league_temp, loser, score, 1:nrow(league_temp))
  league_tidy <- na.omit(league_tidy)
  matchups_plot <- ggplot(league_tidy, aes(reorder(winner, -score, FUN = mean), score)) + 
    geom_point(aes(color=loser)) + 
    geom_hline(yintercept=50, color="darkgrey") + 
    labs(x = "", y="% Chance to Win", color="") + 
    theme_bw() + 
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
  matchups_plot
} 
matchups_hm <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_temp <- eval_tidy(parse_quosure(paste0(quo_name(l), "_matchups")))
  league_temp$winner <- rownames(league_temp)
  league_tidy <- gather(league_temp, loser, score, -winner)
  league_tidy$loser <- with(league_tidy,factor(loser,levels = sort(unique(as.character(loser)))))
  matchups_plot <- ggplot(league_tidy, aes(loser, winner)) + 
    geom_tile(aes(fill=score)) +
    scale_fill_distiller(palette = "Spectral", direction=1) +
    #scale_fill_gradient2(low=muted("red"), mid="white", high=muted("green"), midpoint=50)+
    #scale_fill_viridis() +
    theme(panel.background=element_rect(fill="white", colour="white")) +
    ylim(rev(levels(league_tidy$loser))) +
    labs(x = "Team 2", y="Team 1", fill="% Chance", title="Chance Team 1 Beats Team 2")
  matchups_plot
}
team_evaluation <- function(league = clt) {
  l <- quo_name(enquo(league))
  league_proj <- eval_tidy(parse_quosure(paste0(quo_name(l), "_proj")))
  league_proj %>%
    spread(Type, Score) %>% 
    group_by(Team) %>% 
    mutate(margin = act-proj,
           sign = if_else(margin >=0, "positive", "negative"),
           avg = mean(margin),
           pos_count = sum(if_else(sign == "positive", 1, 0))) %>%
    ggplot(aes(x= Week, y = margin, fill=sign)) +
    geom_bar(stat="identity") + 
    facet_wrap(~reorder(Team, - pos_count), ncol=n_distinct(league_proj$Team)/2) + 
    guides(fill=FALSE) +
    labs(title = "Weekly Projection v Actual Results", y = "Margin") +
    ff_theme() + 
    theme(panel.grid.major.y = element_blank())
}
simulation_plot <- function(league = clt, plot = wins) {
  plots <- data_frame("Projected Wins by Week", 
                      "Projected Total Points by Week", 
                      "Projected Chance of Making Playoffs by Week") %>% 
    set_names("wins", "points", "percent")
  
  l <- quo_name(enquo(league))
  league_sim <- eval_tidy(parse_quosure(paste0(quo_name(l), "_simulated_season")))
  t <- quo_name(enquo(plot))
  # sim_plot <- eval_tidy(parse_quosure(quo_name(t)))
  
  if (t == "wins") {
    p <- simulated_season %>% ggplot(aes(Week, Wins, color = Team))
  } else if (t == "points") {
    p <- simulated_season %>% ggplot(aes(Week, Points, color = Team))
  } else if (t == "percent") {
    p <- simulated_season %>% ggplot(aes(Week, Percent, color = Team))
  } else {
    return("Please list a valid simulation plot")
  }
  p +
    geom_line(size=1.5) + 
    geom_point(size = 2) + 
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") + 
    facet_wrap(~reorder(Team, Rank, FUN = last), ncol = 5) +  
    labs(y = "Wins", x = "Week", title = plots[[t]]) +
    guides(color=FALSE) + 
    scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    ff_theme()
}
playoff_leverage_plot <- function() {
  
  playoff_leverage <- read_csv("ff/playoff_leverage.csv")
  sims <- max(playoff_leverage$sim)
  
  clt_weekly %>% 
    filter(Week == max(clt_tidy$Week) + 1) %>% 
    mutate(x = list(playoff_leverage)) %>% 
    unnest() %>%
    mutate(sim_wins = Wins,
           loss_wins = pl_wins,
           win_wins = pl_wins + 1L,
           wins = if_else(Team == Team1, win_wins, 
                          if_else(Team == Team2, loss_wins, sim_wins))) %>% 
    select(Week:Team, wins) %>%
    arrange(-wins) %>% 
    group_by(Team1, sim) %>% 
    mutate(row = row_number(), 
           playoff = if_else(row <= 4, 1, 0)) %>% 
    ungroup() %>% 
    group_by(Team1, Team) %>% 
    summarise(Percent = sum(playoff/sims * 100)) %>% 
    arrange(Team1, -Percent) %>% 
    rename(Winner = Team1) %>% 
    nest(Team:Percent) %>% 
    left_join(clt_weekly %>% 
                filter(Week == 11) %>% 
                select(-Week) %>% 
                rename(Winner = Team1, Loser = Team2), by = "Winner") %>% 
    select(Winner, Loser, data) %>% 
    unnest() %>% 
    filter(Winner == Team | Loser == Team) %>% 
    arrange(Team) %>% 
    mutate(style = if_else(Winner == Team, "Win", "Lose")) %>% 
    select(Team:style) %>% 
    spread(style, Percent) %>% 
    mutate(Delta = round(Win - Lose, 1), Total = 100) %>% 
    ggplot(aes(reorder(Team, Win), y = Total)) + 
    geom_bar(stat = "identity", fill = "white", color = "grey", alpha = 0.4) +
    geom_bar(stat = "identity", aes(y = Win, fill = Team), alpha = 0.5) +
    geom_bar(stat = "identity", aes(y = Lose, fill = Team)) +
    geom_text(aes(y = Total + 0.5, label = paste0(Delta, "%")), color = "grey30", hjust = 0) +
    # geom_text(aes(label = paste0(Delta, "%"), group = Team), color = "grey30", nudge_y = 5) +
    scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75, 100)) +
    guides(fill = FALSE) +
    labs(x = "", y = "Chance to Make Playoffs", title = "Playoff Probability Leverage (Week 11)") +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(color = "black"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          strip.text = element_text(size =12))
}
# Management Evaluation ---------------------------------------------------
# Rank position scores, assign starters, remove WR/RB starters, determine top flex, 
# margin of best and started, rank each team on management, chart each week

evaluate_management <- function(df, qb = 1, rb = 2, wr = 2, te = 1, dst = 1, 
                                k = 1, flex = 1, rb_wr = 1, dl = 0, db = 0, 
                                fa = NULL, transactions = NULL) {
  
  x <- quo_name(enquo(df))
  league_team <- eval(parse(text=x))
  
  if("Proj" %in% names(league_team)) {
    league_team %>% select(-Proj)
  }
  
  if(!is.null(fa)) {
    league_team <- bind_rows(league_team, fa)
  }
  
  if(!is.null(transactions)) {
    league_team <- bind_rows(league_team, transactions)
  }

  teams <- league_team %>% filter(!is.na(Team)) %>% distinct(Team) %>% pull()
  weeks <- league_team %>% distinct(Week) %>% pull()
  
  for (t in teams) {
    for (w in weeks) {
      QB <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position == "QB") %>% 
        arrange(-Points) %>% slice(1:qb)
      RB <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position == "RB") %>% 
        arrange(-Points) %>% slice(1:rb)
      WR <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position == "WR") %>% 
        arrange(-Points) %>% slice(1:wr)
      TE <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position == "TE") %>% 
        arrange(-Points) %>% slice(1:te)
      DST <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position %in% c("DST", "D/ST", "DEF")) %>% 
        arrange(-Points) %>% slice(1:dst)
      K <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position == "K") %>% 
        arrange(-Points) %>% slice(1:k)
      DB <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position %in% c("DB", "S", "CB")) %>% 
        arrange(-Points) %>% slice(1:db)
      DL <- league_team %>% filter(Team %in% c(t, NA) & Week == w & Position %in% c("DE", "DL", "DT")) %>% 
        arrange(-Points) %>% slice(1:dl)
      best_team <- bind_rows(QB, RB, WR, TE, DST, K, DB, DL)
      if(rb_wr > 0) {
        RB_WR <- league_team %>% anti_join(best_team, by = c("Week", "Team", "Position", "Points", "Score")) %>% 
          filter(Team == t & Week == w & Position %in% c('RB', 'WR')) %>% 
          arrange(-Points) %>% slice(rb_wr)
        best_team <- bind_rows(best_team, RB_WR)
      }
      if(flex > 0) {
        Flex <- league_team %>% anti_join(best_team, RB_WR, by = c("Week", "Team", "Position", "Points", "Score")) %>% 
          filter(Team == t & Week == w & Position %in% c("RB", "WR", "TE")) %>% 
          arrange(-Points) %>% slice(flex)
        best_team <- bind_rows(best_team, Flex)
      }
      
      best_team <- best_team %>% mutate(Team = t)
      
      if (exists("management_evaluation")) {
        management_evaluation <- management_evaluation %>% 
          bind_rows(best_team)
      } else {
        management_evaluation <- best_team
      }
    }
  }
  
  management_evaluation %>% 
    filter(!is.na(Team)) %>% 
    group_by(Team, Week) %>% 
    mutate(Max = sum(Points),
           Delta = Score - Max) %>% 
    select(Week, Team, Score, Max, Delta) %>% 
    distinct() %>% 
    mutate(sign = if_else(Delta >=0, "positive", "negative"),
           avg = mean(Delta)) %>% 
    ggplot(aes(reorder(Team, avg), Delta, fill = Delta)) + 
    geom_bar(stat = 'identity') +
    scale_x_discrete()+
    facet_wrap(~Week, ncol=n_distinct(league_team$Team)/2) +
    guides(fill=FALSE) +
    labs(title = "Weekly Manager Evaluation", y = "Margin") +
    ff_theme() +
    theme(panel.grid.major.y = element_blank()) +
    scale_fill_distiller(palette = "YlOrRd", direction = -1) +
    coord_flip()
    # ggplot(aes(Week, Delta, fill = sign)) + 
    # geom_bar(stat = 'identity') +
    # scale_x_discrete()+ 
    # facet_wrap(~reorder(Team, - avg), ncol=n_distinct(league_team$Team)/2) +
    # guides(fill=FALSE) +
    # labs(title = "Weekly Manager Evaluation", y = "Margin") +
    # ff_theme() + 
    # theme(panel.grid.major.y = element_blank())
  
}
# Ranking & Strength of Schedule ------------------------------------------

# Ranking functions
rank_teams  <- function(league = clt) {
  l <- quo_name(enquo(league))
  matchups <- eval_tidy(parse_quosure(paste0(quo_name(l), "_matchups")))
  matchups %>% 
    map_df(function(x) {round((mean(100 - x, na.rm=T) - 50)/.5, 2)}) %>% 
    gather(Team, FVOA) %>% 
    arrange(-FVOA) %>% 
    mutate(Rank = dense_rank(-FVOA))
}

# Strength of Schedule
schedule_strength <- function(league = clt) {
  l <- quo_name(enquo(league))
  rankings <- eval_tidy(parse_quosure(paste0(quo_name(l), "_rankings")))
  weekly <- eval_tidy(parse_quosure(paste0(quo_name(l), "_weekly")))
  rankings %>% 
    select(Team2 = Team, Strength) %>% 
    right_join(weekly, by = "Team2") %>% 
    rename(Team = Team1) %>% 
    group_by(Team) %>% 
    summarise(Schedule = round(mean(Strength), 2)) %>% 
    arrange(Schedule) %>% 
    mutate("SoS" = 1:nrow(.)) %>% 
    left_join(rankings) %>% 
    select(SoS, Team, Strength, Schedule, -Rank) %>% 
    mutate(mean = Strength - Schedule + 50) %>% 
    arrange(-mean)
}

# Shiny Files -------------------------------------------------------------

write_csv(clt_tidy, "ff/clt_tidy.csv")
write_csv(clt_proj, "ff/clt_proj.csv")
write_csv(clt_weekly, "ff/clt_weekly.csv")
write_csv(clt_colley, "ff/clt_colley.csv")
write_csv(fvoa_season, "ff/fvoa_season.csv")

# Deploy ------------------------------------------------------------------

rsconnect::deployApp(upload = TRUE, appName = "fantasy_football", 
                     appDir = "C:/Users/Scott/Google Drive/Data Science/AA Projects/fantasy/ff")
