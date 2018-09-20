# Evaluate model for how many games were correctly predicted and plots results
# Details include matchup breakdowns, team summary, and percentage tiers
# If print = F it lists breakdown of each week for exploration

evaluation_matchup <- function(actual_scores, model_scores) {

  # actual_scores <- actual_scores %>% unnest()
  # model_scores <- model_scores %>% unnest

  teams <- actual_scores %>% distinct(Team)

  matchup_evals <- crossing(teams %>% rename(Team1 = Team),
                            teams %>% rename(Team2 = Team)) %>%
    filter(Team1 != Team2) %>%
    left_join(actual_scores %>% rename(Score1 = Score),
              by = c("Team1" = "Team")) %>%
    left_join(actual_scores %>% rename(Score2 = Score),
              by = c("Team2" = "Team")) %>%
    # left_join(model_scores,
    #           by = c("Week")) %>%
    mutate(model_scores = list(model_scores),
           win_prob = pmap_dbl(list(model_scores, Team1, Team2), matchup),
           pred_outcome = if_else(win_prob > 50, 1, 0),
           actual_outcome = if_else(Score1 - Score2 > 0, 1, 0),
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
    select(Team = Team1, win_prob, sim, actual_outcome) %>%
    mutate(sim = if_else(abs(sim) == 1000, 0, sim),
           pred = win_prob / 100,
           brier = (0.25 - (pred - actual_outcome)^2) * 100) %>%
    group_by(Team) %>%
    summarise(Brier = round(mean(brier), 2)) %>%
    ungroup() %>%
    select(Team, Brier)

  team_accuracy <- matchup_evals %>%
    select(Team = Team1, correct) %>%
    group_by(Team) %>%
    summarise(Correct = sum(correct)) %>%
    ungroup() %>%
    arrange(-Correct)

  data_frame(tiers = list(tiers),
             brier = list(brier),
             team_accuracy = list(team_accuracy))
}

evaluate_model <- function(scores,
                            summary = T, details = F,
                            shiny = F, save = F,
                            reg_games = 6, reps = 1e6) {
  set.seed(42)

  teams <- scores %>% distinct(Team)
  weeks <- scores %>% distinct(Week)

  model_scores <- weeks %>%
    mutate(scores = list(scores)) %>%
    unnest() %>%
    filter(Week1 < Week) %>%
    rename(join_week = Week,
           Week = Week1) %>%
    arrange(Team) %>%
    nest(-join_week, .key = "model_scores")

  evaluation_df <- weeks %>%
    filter(Week != 1) %>%
    mutate(scores = list(scores)) %>%
    unnest() %>%
    filter(Week1 == Week) %>%
    select(-Week1) %>%
    nest(Team:Score, .key = "actual_scores") %>%
    # mutate(tmp = map(actual_scores, evaluation_matchup, model_scores)) %>%
    left_join(model_scores, by = c("Week" = "join_week")) %>%
    mutate(tmp = map2(actual_scores, model_scores,
                      evaluation_matchup)) %>%
    unnest(tmp) %>%
    select(Week, tiers, brier, team_accuracy)

  correct_preds <- evaluation_df %>%
    unnest(tiers) %>%
    summarise(n = sum(n),
              correct = sum(correct),
              pct_correct = round(correct / n * 100)) %>%
    pull(pct_correct)

  statement <- paste0("The model correctly predicted ", correct_preds, "% of games")

  total_brier <- evaluation_df %>%
    unnest(brier) %>%
    summarise(round(mean(Brier), 2)) %>%
    pull()

  brier_statement <- paste("The model got a final Brier score of", total_brier)

  # brier_breakdown <- brier_sims %>%
  #   unnest() %>%
  #   drop_na() %>%
  #   filter(Week == nrow(brier_sims)) %>%
  #   group_by(team) %>%
  #   summarise(total = round(mean(brier), 2)) %>%
  #   spread(team, total) %>%
  #   map_dbl(sum) %>%
  #   sort()

  team_accuracy <- evaluation_df %>% unnest(team_accuracy)

  benchmark <- (nrow(teams)^2 - nrow(teams))

  plot <- evaluation_df %>%
    unnest(tiers) %>%
    group_by(Week) %>%
    summarise(weekly = sum(correct),
              delta = weekly - benchmark/2,
              percent = round(weekly/benchmark * 100, 1),
              sign = ifelse(delta > 0, "positive",
                            ifelse(delta < 0, "negative", "equal"))) %>%
    ggplot(aes(Week, delta, fill = sign, label = percent)) +
    geom_bar(stat = 'identity') +
    geom_text(size = 3, alpha = 0.7) +
    scale_x_continuous(name = "Week", breaks = 2:max(evaluation_df$Week)) +
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
    labs(title = "Weekly Evaluation of Model",
         x = "Week (starting with Week 2)",
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
    # geom_point() +
    geom_text(aes(label = percent), size = 3,
              alpha = 0.7, vjust = "outward") +
    geom_line() +
    geom_abline(color = "red") +
    geom_abline(color = "red", intercept = 10) +
    scale_x_continuous(limits = c(50, 100)) +
    scale_y_continuous(limits = c(30, 100)) +
    labs(x = "Tier", y = "Percent Correct",
         title = "Calibration of Weekly Predictions") +
    theme_fvoa()

  plots <- cowplot::plot_grid(plot, perc_plot, nrow = 2)

  if(shiny) {
    # data_frame(x = weekly_sims) %>%
    #   mutate(week = 1:length(weekly_sims)) %>%
    #   slice(-1) %>%
    #   unnest() %>%
    #   write_csv("ff/model_eval.csv")

    list(statement, plot, team_accuracy) %>%
      saveRDS("ff/model_eval.RDS")
  }

  if(save) {

    list(Weekly = weekly_sims, Team = team_accuracy, Tiers = perc_tiers,
         # Brier = brier_sims,
         Accuracy = statement, Plot = plots)

  }

  if(details) {

    print(weekly_sims[[length(weekly_sims)]])
    cat("\n")
    print(team_accuracy %>% filter(Week == max(Week)))
    cat("\n")
    # print(brier_breakdown)
    # cat("\n")
    print(perc_tiers_all)
    cat("\n")

  }

  if(summary) {

    print(statement)
    cat("\n")
    print(brier_statement)
    cat("\n")
    print(plots)

  }

}
