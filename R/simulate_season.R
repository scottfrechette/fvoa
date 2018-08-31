simulate_season <- function(schedule, scores,
                            sims = 1000, reg_games = 6,
                            progress = FALSE) {

  # Identify league, teams, and weeks played

  teams <- scores %>% distinct(Team) %>% pull()
  weeks_played <- max(scores$Week)

  league_quo <- enquo(schedule)
  league <- str_split(quo_name(league_quo), "_", simplify = TRUE)[[1]]

  file_simulation <- paste0(league, "_simulated_season", ".csv")

  # Load previous simulations unless week 1 not run yet
  if (length(weeks_played) == 1) {

    tryCatch(previous_simulations <- suppressMessages(read_csv(file_simulation)),
             error = function(e) previous_simulations <- data_frame(Week = 0))

  } else {

    tryCatch(previous_simulations <- suppressMessages(read_csv(file_simulation)),
             error = function(e) print("No simulations found"))

  }

  # Determine if season already simulated for current week
  if (max(previous_simulations$Week) == weeks_played) {

    # If yes return current simulation
    simulated_season <- previous_simulations

  } else { # If no run simulation

    # Determine is all games played
    if (weeks_played == max(schedule$Week)) {

      # Get final standings
      t1_stats <- scores %>% rename(Team1 = Team, Score1 = Score)
      t2_stats <- scores %>% rename(Team2 = Team, Score2 = Score)
      suppressMessages(stats <- schedule %>%
                         inner_join(t1_stats) %>%
                         inner_join(t2_stats) %>%
                         mutate(diff = Score1 - Score2,
                                win = if_else(diff > 0, 1, 0)) %>%
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
      final_standings <- scores %>%
        group_by(Team) %>%
        summarise(Points = round(sum(Score), 2)) %>%
        left_join(stats, by = "Team") %>%
        arrange(-Wins, -Points) %>%
        mutate(Percent = c(100, 100, 100, 100, 0, 0, 0, 0, 0, 0),
               Rank = 1L:10L,
               Week = weeks_played)

      # Merge final standings with season simulations
      simulated_season <- bind_rows(previous_simulations, final_standings)

      # Save full season to CSV
      write_csv(simulated_season, file_simulation)
      # write_csv(simulated_season, "ff/simulated_seasons.csv")

    } else {

      # Create progress bar if not already running
      if (progress == TRUE &
          !exists("pb") &
          requireNamespace("progress", quietly = TRUE)) {
        pb <- progress::progress_bar$new(
          format = "  simulating [:bar] :percent eta: :eta",
          total = sims, clear = FALSE, width= 80)
      }

      # Identify unplayed weeks
      remaining_weeks <- schedule %>%
        filter(!Week %in% 1:weeks_played) %>%
        distinct(Week) %>%
        pull()

      # Create list for simulated seasons
      sim_seasons <- list()

      # Simulate remaining weeks [sims] times to get reliable estimates
      for (i in 1:sims) {

        # Create temporary df of current scores
        tmp_tidy <- scores

        # Simulate each week sequentially to let the scores carry forward
        for (w in remaining_weeks) {

          # # Extract team scores, including simulated scores for previous weeks
          for (t in teams) {
            current_scores <- tmp_tidy %>% filter(Team == t) %>% pull(Score)

            # If less than 6 weeks played use historical regression to mean to supplement scores
            if (w <= reg_games) {
              current_mean <-  weighted.mean(c(current_scores, 115),
                                             weight_games(current_scores, reg_games))
              current_sd <- weighted_sd(c(current_scores, 115),
                                        weight_games(current_scores, reg_games),
                                        method="ML")

              # Otherwise simulate score from team's distribution
            } else {
              # Calculate team weighted mean/SD
              current_mean <- weighted.mean(current_scores,
                                            weight_games(current_scores, reg_games))
              current_sd <- weighted_sd(current_scores,
                                        weight_games(current_scores, reg_games),
                                        method = "ML")

            }

            # Calculate random score from team's weighted distribution
            simulated_score <- rnorm(1, current_mean, current_sd)

            # Ensure no negative scores by assigning it to historical low
            simulated_score <- if_else(simulated_score <= 0, 50, simulated_score)

            # Add this simulated score to temporary df along with week and team
            tmp_tidy <- tmp_tidy %>%
              add_row(Week = w,
                      Team = t,
                      Score = round(simulated_score, 2))
          }
        }

        # Determine total points for each team for current simulation
        total_points <- tmp_tidy %>%
          group_by(Team) %>%
          summarise(Points = sum(Score)) %>%
          arrange(-Points)

        # Get final standings
        t1_stats <- tmp_tidy %>% rename(Team1 = Team, Score1 = Score)
        t2_stats <- tmp_tidy %>% rename(Team2 = Team, Score2 = Score)
        suppressMessages(final_record <- schedule %>%
                           inner_join(t1_stats, by = c("Week", "Team1")) %>%
                           inner_join(t2_stats, by = c("Week", "Team2")) %>%
                           mutate(diff = Score1 - Score2,
                                  win = if_else(diff > 0, 1, 0),
                                  pl_win = if_else(Week == weeks_played + 1, 0,
                                                   if_else(diff > 0, 1, 0)),
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
        if(exists(pb)) {
          pb$tick()
          Sys.sleep(1/100)
        }
      }

      # Save all simulations for playoff leverage chart
      write_csv(data_frame(sim_seasons) %>%
                  mutate(sim = row_number()) %>%
                  unnest(),
                "playoff_leverage.csv")

      # Tidy simulated data for reporting/visualizing after all simulations run
      final_df <- data_frame(sim_seasons) %>%
        unnest() %>%
        group_by(Team) %>%

        # Top 4 teams from each simulation make the playoffs
        mutate(playoff = map(sim_seasons, function(x) x %>%
                               slice(1:4) %>%
                               pull(Team))) %>%
        unnest() %>%

        # Calculate percentage each team made the playoffs
        mutate(playoffs = if_else(Team == playoff, 1, 0)) %>%

        # Calculate average total points/wins and playoff chances for simulated season
        summarise(Points = round(mean(Points), 1),
                  Wins = round(mean(Wins), 1),
                  Percent = sum(playoffs)/sims * 100) %>%
        arrange(-Percent, -Wins, -Points) %>%
        mutate(Rank = 1L:10L,
               Week = max(scores$Week))

      if (!file.exists(file_simulation)) {

        # Create file and save to CSV
        simulated_season <- final_df
        write_csv(simulated_season, file_simulation)

      } else {

        # Merge final standings with season simulations
        previous_simulations <- suppressMessages(read_csv(file_simulation))
        simulated_season <- bind_rows(previous_simulations, final_df)

        # Save simulated season to CSV
        write_csv(simulated_season, paste0(file_simulation))
      }
    }
  }
  simulated_season
}
