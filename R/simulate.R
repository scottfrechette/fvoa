simulate_scores <- function(scores,
                            weeks,
                            n_sims,
                            reg_games = 6,
                            set_seed = TRUE,
                            progress = FALSE) {

  teams <- unique(scores$team)
  weeks_played <- max(scores$week)
  remaining_weeks <- subset(1:weeks, 1:weeks > weeks_played)

  library(furrr)
  plan(multiprocess)

  single_simulation <- function(scores,
                                sim = NULL,
                                reg_games,
                                set_seed) {

    if (!is.null(sim) & set_seed) {set.seed(sim)}

    for (w in remaining_weeks) {

      for (t in teams) {

        current_scores <- scores %>% filter(team == t) %>% pull(score)

        if (w <= reg_games) {

          current_mean <- weighted.mean(c(current_scores, 115),
                                        weight_games(current_scores, reg_games))
          current_sd <- weighted_sd(c(current_scores, 115),
                                    weight_games(current_scores, reg_games),
                                    method = "ML")

        } else {

          current_mean <- weighted.mean(current_scores,
                                        weight_games(current_scores, reg_games))
          current_sd <- weighted_sd(current_scores,
                                    weight_games(current_scores, reg_games),
                                    method = "ML")
        }

        simulated_score <- rnorm(1, current_mean, current_sd)
        simulated_score <- if_else(simulated_score <= 0, 50, simulated_score)

        scores <- scores %>%
          add_row(week = w,
                  team = t,
                  score = round(simulated_score, 2))

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

simulate_seasons <- function(sim_scores, schedule) {

  if("team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

  final_record <- schedule %>%
    mutate_if(is.factor, as.character) %>%
    inner_join(sim_scores %>%
                 rename(team1 = team,
                        score1 = score),
               by = c("week", "team1")) %>%
    inner_join(sim_scores %>%
                 rename(team2 = team,
                        score2 = score),
               by = c("week", "team2", "sim")) %>%
    mutate(diff = score1 - score2,
           win = diff > 0,
           pl_win = case_when(
             week == weeks_played + 1 ~ 0,
             diff > 0                 ~ 1,
             TRUE                     ~ 0),
           lose = diff < 0,
           tie = diff == 0) %>%
    group_by(team1, sim) %>%
    summarise(wins = sum(win),
              pl_wins = sum(pl_win),
              losses = sum(lose),
              tie = sum(tie)) %>%
    ungroup() %>%
    rename(team = team1) %>%
    arrange(sim, -wins, -losses) %>%
    mutate_if(is.numeric, as.integer)

  sim_scores %>%
    group_by(team, sim) %>%
    summarise(points = sum(score)) %>%
    ungroup() %>%
    arrange(sim, -points) %>%
    right_join(final_record,
               by = c("sim", "team")) %>%
    arrange(sim, -wins, -points)

}

simulate_weekly_projections <- function(sim_seasons) {

  teams <- unique(sim_season$team)

  sim_season %>%
    nest(data = -sim) %>%
    mutate(playoff = map(data,
                         ~ .x %>%
                           slice(1:4) %>%
                           pull(team))) %>%
    unnest(playoff) %>%
    unnest(data) %>%
    mutate(playoffs = if_else(team == playoff, 1, 0)) %>%
    group_by(team) %>%
    summarise(points = round(mean(points), 1),
              wins = round(mean(wins), 1),
              percent = sum(playoffs)/n_distinct(.$sim) * 100,
              .groups = "drop") %>%
    arrange(-percent, -wins, -points) %>%
    mutate(rank = 1L:length(teams),
           week = current_week)


}
