simulate_scores <- function(scores, weeks, n_sims,
                            reg_games = 6, set_seed = TRUE,
                            progress = FALSE) {

  teams <- unique(scores$Team)
  weeks_played <- max(scores$Week)
  remaining_weeks <- subset(1:weeks, 1:weeks > weeks_played)

  library(furrr)
  plan(multiprocess)

  single_simulation <- function(scores, sim = NULL,
                                reg_games, set_seed) {

    if (!is.null(sim) & set_seed) {
      set.seed(sim)
    }

    for (w in remaining_weeks) {
      for (t in teams) {
        current_scores <- scores %>% filter(Team == t) %>% pull(Score)
        if (w <= reg_games) {
          current_mean <- weighted.mean(c(current_scores, 115),
                                        weight_games(current_scores, reg_games))
          current_sd <- weighted_sd(c(current_scores, 115),
                                    weight_games(current_scores, reg_games),
                                    method = "ML")
        }
        else {
          current_mean <- weighted.mean(current_scores,
                                        weight_games(current_scores, reg_games))
          current_sd <- weighted_sd(current_scores,
                                    weight_games(current_scores, reg_games),
                                    method = "ML")
        }
        simulated_score <- rnorm(1, current_mean,
                                 current_sd)
        simulated_score <- if_else(simulated_score <= 0, 50, simulated_score)
        scores <- scores %>%
          add_row(Week = w,
                  Team = t,
                  Score = round(simulated_score, 2))
      }
    }

    scores

  }

  tibble(sim = 1:n_sims) %>%
    mutate(data = future_map(sim,
                             ~ single_simulation(scores, .x,
                                                 reg_games = reg_games,
                                                 set_seed = set_seed),
                             .progress = progress)) %>%
    unnest(data)

}

plot_sim_matchup <- function(sim_scores, team1, team2, week,
                             square = FALSE) {

  sim_scores_subset <- sim_scores %>%
    filter(Team %in% c(team1, team2),
           Week == week)

  lo <- min(sim_scores_subset$Score) - 10
  hi <- max(sim_scores_subset$Score) + 10

  sim_scores_final <- sim_scores_subset %>%
    spread(Team, Score) %>%
    select(1:2, team1, team2) %>%
    mutate(winner = .[[3]] > .[[4]])

  wp <- sim_scores_final %>%
    summarize(team1 = sum(winner) / n()) %>%
    mutate(team2 = 1 - team1) %>%
    mutate_all(~ paste0(round(.x, 2) * 100, "%"))

  p <- sim_scores_final %>%
    ggplot(aes(.[[3]], .[[4]], color = winner)) +
    geom_point(alpha = 0.5) +
    geom_abline(color = "grey30", linetype = 2) +
    guides(color = FALSE) +
    labs(x = str_glue(team1, " ({wp[[1]]})"),
         y = str_glue(team2, " ({wp[[2]]})"))

  if (square) {

    p <- p +
      coord_cartesian(xlim = c(lo, hi), ylim = c(lo, hi))

  }

  p

}

simulate_seasons <- function(sim_scores, schedule) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

  final_record <- schedule %>%
    mutate_if(is.factor, as.character) %>%
    inner_join(sim_scores %>%
                 rename(Team1 = Team,
                        Score1 = Score),
               by = c("Week", "Team1")) %>%
    inner_join(sim_scores %>%
                 rename(Team2 = Team,
                        Score2 = Score),
               by = c("Week", "Team2", "sim")) %>%
    mutate(diff = Score1 - Score2,
           win = diff > 0,
           pl_win = case_when(
             Week == weeks_played + 1 ~ 0,
             diff > 0                 ~ 1,
             TRUE                     ~ 0),
           lose = diff < 0,
           tie = diff == 0) %>%
    group_by(Team1, sim) %>%
    summarise(Wins = sum(win),
              pl_wins = sum(pl_win),
              Losses = sum(lose),
              Tie = sum(tie)) %>%
    ungroup() %>%
    rename(Team = Team1) %>%
    arrange(sim, -Wins, -Losses) %>%
    mutate_if(is.numeric, as.integer)

  sim_scores %>%
    group_by(Team, sim) %>%
    summarise(Points = sum(Score)) %>%
    ungroup() %>%
    arrange(sim, -Points) %>%
    right_join(final_record,
               by = c("sim", "Team")) %>%
    arrange(sim, -Wins, -Points)

}

simulate_weekly_projections <- function(sim_seasons, all_simulations, week) {

  teams <- unique(sim_season$Team)

  if(week > max(all_simulations$Week)) {

    all_simulations <- bind_rows(
      all_simulations,
      sim_season %>%
        nest(data = -sim) %>%
        mutate(playoff = map(data,
                             ~ .x %>%
                               slice(1:4) %>%
                               pull(Team))) %>%
        unnest(playoff) %>%
        unnest(data) %>%
        mutate(playoffs = if_else(Team == playoff, 1, 0)) %>%
        group_by(Team) %>%
        summarise(Points = round(mean(Points), 1),
                  Wins = round(mean(Wins), 1),
                  Percent = sum(playoffs)/n_distinct(.$sim) * 100) %>%
        ungroup() %>%
        arrange(-Percent, -Wins, -Points) %>%
        mutate(Rank = 1L:length(teams),
               Week = week)
    )

  }

  all_simulations

}
