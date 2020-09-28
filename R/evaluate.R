

# Rank position scores, assign starters, remove WR/RB starters, determine top flex,
# margin of best and started, rank each team on management, chart each week

# lineup_df columns: team, score, player, position, Lineup, Proj, points

max_points_position <- function(lineup_df, team, week,
                                position, roster_spots) {

  lineup_df %>%
    filter(team %in% c(team, NA),
           week == week,
           position == position) %>%
    top_n(roster_spots, points) %>%
    mutate(max_points = sum(points)) %>%
    select(max_points, player)

}

max_points_flex <- function(best_lineup, lineup_df, team, week,
                            rb_wr = 1, flex = 1) {

  best_lineup <- best_lineup %>%
    filter(team == team,
           week == week)

  lineup_df <- lineup_df %>%
    filter(team %in% c(team, NA),
           week == week)

  if(rb_wr > 0) {

    chosen_players <- best_lineup %>%
      unnest(player) %>%
      select(player)

    best_rb_wr <- lineup_df %>%
      anti_join(chosen_players, by = "player") %>%
      filter(position %in% c("RB", "WR")) %>%
      top_n(rb_wr, points) %>%
      mutate(max_points = sum(points),
             position = "RB_WR") %>%
      nest(player, .key = "player") %>%
      select(team, week, position, max_points, player)

    best_lineup <- bind_rows(best_lineup,
                             best_rb_wr)


  }

  if(flex > 0) {

    chosen_players <- best_lineup %>%
      unnest(player) %>%
      select(player)

    best_flex <- lineup_df %>%
      anti_join(chosen_players, by = "player") %>%
      filter(position %in% c("RB", "WR", "TE")) %>%
      top_n(flex, points) %>%
      mutate(max_points = sum(points),
             position = "Flex") %>%
      nest(player, .key = "player") %>%
      select(team, week, position, max_points, player)

    best_lineup <- bind_rows(best_lineup,
                             best_flex)

  }

  best_lineup %>%
    select(-player) %>%
    summarise(max_points = sum(max_points))


}

evaluate_lineup <- function(lineup_df, qb = 1, rb = 2,
                            wr = 3, te = 1, dst = 1,
                            k = 1, flex = 1, rb_wr = 1,
                            dl = 1, db = 1, fa = NULL,
                            transactions = NULL, plot = FALSE) {

  lineup_df <- lineup_df %>%
    mutate(position = case_when(
      position %in% c("DB", "CB", "S") ~ "DB",
      position %in% c("DL", "DE", "DT", "LB") ~ "DL",
      position %in% c("DST", "D/ST", "DEF") ~ "DST",
      TRUE ~ position
    ))

  if("Proj" %in% names(lineup_df)) {
    lineup_df <- lineup_df %>% select(-Proj)
  }

  if(!is.null(fa)) {
    lineup_df <- bind_rows(lineup_df, fa)
  }

  if(!is.null(transactions)) {
    lineup_df <- bind_rows(lineup_df, transactions)
  }

  teams <- lineup_df %>%
    filter(!is.na(team)) %>%
    distinct(team)
  weeks <- lineup_df %>% distinct(week)
  positions <- tibble(qb, rb, wr, te,
                          dst, k, dl, db) %>%
    gather(position, roster_spots) %>%
    filter(roster_spots > 0) %>%
    mutate(position = toupper(position))

  best_lineup <- crossing(teams, weeks, positions) %>%
    mutate(data = list(lineup_df),
           tmp = pmap(list(data, team, week,
                           position, roster_spots),
                      max_points_position)) %>%
    unnest(tmp) %>%
    select(team, week, position, max_points, player) %>%
    nest(player, .key = "player")

  best_lineup <- crossing(teams, weeks) %>%
    mutate(best_lineup = list(best_lineup),
           lineup_df = list(lineup_df),
           rb_wr = rb_wr,
           flex = flex,
           tmp = pmap(list(best_lineup, lineup_df,
                           team, week, rb_wr, flex),
                      max_points_flex)) %>%
    unnest(tmp) %>%
    select(team, week, max = max_points) %>%
    left_join(lineup_df %>%
                select(team, week, score) %>%
                distinct(),
              by = c("team", "week"))

  best_lineup_final <- best_lineup %>%
    mutate(delta = max - score,
           sign = if_else(delta <= 0, "positive", "negative"),
           avg = mean(delta))

  if (plot) {

    best_lineup_final %>%
      ggplot(aes(week, delta, fill = delta)) +
      geom_bar(stat = 'identity', color = "black") +
      scale_x_continuous(breaks = 1:max(lineup_df$week),
                         labels = paste("week", 1:max(lineup_df$week)),
                         trans = "reverse") +
      facet_wrap(~reorder(team, avg), ncol = n_distinct(lineup_df$team)/2) +
      guides(fill=FALSE) +
      labs(title = "weekly Manager Evaluation",
           subtitle = "How many points you left on your bench each week",
           x = NULL, y = "Lost points") +
      theme_fvoa() +
      theme(panel.grid.major.y = element_blank()) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      coord_flip()

  } else {

    return(best_lineup_final)

  }
}




evaluation_matchup <- function(actual_scores, model_scores, .fun) {

  teams <- actual_scores %>% distinct(team)

  matchup_evals <- crossing(teams %>% rename(team1 = team),
                            teams %>% rename(team2 = team)) %>%
    filter(team1 != team2) %>%
    left_join(actual_scores %>% rename(score1 = score),
              by = c("team1" = "team")) %>%
    left_join(actual_scores %>% rename(score2 = score),
              by = c("team2" = "team")) %>%
    mutate(model_scores = list(model_scores),
           win_prob = pmap_dbl(list(model_scores, team1, team2), .fun),
           pred_outcome = if_else(win_prob > 50, 1, 0),
           actual_outcome = if_else(score1 - score2 > 0, 1, 0),
           correct = if_else(pred_outcome == actual_outcome, 1, 0),
           sim = case_when(
             win_prob == 0 & correct == 1 ~ 1000,
             win_prob == 0 & correct == 0 ~ -1000,
             correct == 1 ~ win_prob,
             TRUE ~ -win_prob)
    )

  tiers <- matchup_evals %>%
    select(sim, correct) %>%
    mutate(tier = case_when(
      abs(sim) == 100 | abs(sim) == 1000 ~ 100,
      abs(sim) >= 90 | abs(sim) <= 10 ~ 90,
      abs(sim) >= 80 | abs(sim) <= 20 ~ 80,
      abs(sim) >= 70 | abs(sim) <= 30 ~ 70,
      abs(sim) >= 60 | abs(sim) <= 40 ~ 60,
      abs(sim) >= 50 | abs(sim) <= 50 ~ 50)) %>%
    group_by(tier) %>%
    mutate(n = n(),
           correct = sum(correct),
           perc_correct = round(correct / n, 2)) %>%
    ungroup() %>%
    select(tier, n, correct, perc_correct) %>%
    arrange(-tier) %>%
    distinct()

  brier <- matchup_evals %>%
    select(team = team1, win_prob, sim, actual_outcome) %>%
    mutate(sim = if_else(abs(sim) == 1000, 0, sim),
           pred = win_prob / 100,
           brier = (0.25 - (pred - actual_outcome)^2) * 100) %>%
    group_by(team) %>%
    summarise(brier = round(mean(brier), 2)) %>%
    ungroup() %>%
    select(team, brier)

  team_accuracy <- matchup_evals %>%
    select(team = team1, correct) %>%
    group_by(team) %>%
    summarise(Correct = sum(correct)) %>%
    ungroup() %>%
    arrange(-Correct)

  tibble(tiers = list(tiers),
             brier = list(brier),
             team_accuracy = list(team_accuracy))
}

evaluate_model <- function(scores,
                           output = c("shiny", "summary", "details"),
                           .fun = compare_teams,
                           reg_games = 6, reps = 1e6) {

  output <- output[[1]]

  set.seed(42)

  teams <- scores %>% distinct(team)
  weeks <- scores %>% distinct(week)

  model_scores <- weeks %>%
    mutate(scores = list(scores)) %>%
    unnest() %>%
    filter(week1 < week) %>%
    rename(join_week = week,
           week = week1) %>%
    arrange(team) %>%
    nest(model_scores = c(week, team, score))

  evaluation_df <- weeks %>%
    filter(week != 1) %>%
    mutate(scores = list(scores)) %>%
    unnest() %>%
    filter(week1 == week) %>%
    select(-week1) %>%
    nest(actual_scores = c(team, score)) %>%
    left_join(model_scores, by = c("week" = "join_week")) %>%
    mutate(tmp = map2(actual_scores, model_scores,
                      ~evaluation_matchup(.x, .y, .fun))) %>%
    unnest(tmp) %>%
    select(week, tiers, brier, team_accuracy)

  correct_preds <- evaluation_df %>%
    unnest(tiers) %>%
    summarise(n = sum(n),
              correct = sum(correct),
              pct_correct = round(correct / n * 100)) %>%
    pull(pct_correct)

  statement <- paste0("The model correctly predicted ", correct_preds, "% of games")

  total_brier <- evaluation_df %>%
    unnest(brier) %>%
    summarise(round(mean(brier), 2)) %>%
    pull()

  brier_statement <- paste("The model got a final brier score of", total_brier)

  team_accuracy <- evaluation_df %>% unnest(team_accuracy)

  benchmark <- (nrow(teams)^2 - nrow(teams))

  plot <- evaluation_df %>%
    unnest(tiers) %>%
    group_by(week) %>%
    summarise(weekly = sum(correct),
              delta = weekly - benchmark/2,
              percent = round(weekly/benchmark * 100, 1),
              sign = ifelse(delta > 0, "positive",
                            ifelse(delta < 0, "negative", "equal"))) %>%
    ggplot(aes(week, delta, fill = sign, label = percent)) +
    geom_bar(stat = 'identity') +
    geom_text(size = 3, alpha = 0.7) +
    scale_x_continuous(name = "week", breaks = 2:max(evaluation_df$week)) +
    scale_y_continuous(limits = c(0-benchmark/2, benchmark/2),
                       breaks = c(0-benchmark/2,
                                  ((0-benchmark/2)/2),
                                  0,
                                  benchmark/4,
                                  benchmark/2),
                       labels = c(0, 25, 50, 75, 100)) +
    scale_fill_manual(values = c(equal = "#619CFF",
                                 negative = "#F8766D",
                                 positive = "#00BA38")) +
    labs(title = "weekly Evaluation of Model",
         x = "week (starting with week 2)",
         y = "Percent Correct") +
    theme(panel.background= element_blank(),
          panel.border = element_blank()) +
    guides(fill=F)

  perc_plot <- evaluation_df %>%
    unnest(tiers) %>%
    group_by(tier) %>%
    summarise(n = sum(n),
              correct = sum(correct),
              percent = round(correct/n * 100, 2)) %>%
    ggplot(aes(tier, percent)) +
    geom_text(aes(label = percent), size = 3,
              alpha = 0.7, vjust = "outward") +
    geom_line() +
    geom_abline(color = "red") +
    geom_abline(color = "red", intercept = 10) +
    scale_x_continuous(limits = c(50, 100)) +
    scale_y_continuous(limits = c(30, 100)) +
    labs(x = "Tier", y = "Percent Correct",
         title = "Calibration of weekly Predictions") +
    theme_fvoa()

  if(output == "shiny") {

    list(statement = statement, plot = plot, team_accuracy = team_accuracy)

  } else if (output == "summary") {

    if("cowplot" %in% rownames(installed.packages())) {

      plots <- cowplot::plot_grid(plot, perc_plot, nrow = 2)


    } else if ("gridExtra" %in% rownames(installed.packages())) {

      plots <- gridExtra::arrangeGrob(plot, perc_plot)

    }

    print(statement)
    cat("\n")
    print(brier_statement)
    cat("\n")
    print(plots)

  } else {

    print(weekly_sims[[length(weekly_sims)]])
    cat("\n")
    print(team_accuracy %>% filter(week == max(week)))
    cat("\n")
    print(perc_tiers_all)
    cat("\n")

  }

}
