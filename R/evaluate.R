
#' @export
evaluate_lineup <- function(lineup_df,
                            qb = 1, rb = 2,
                            wr = 3, te = 1,
                            dst = 1, k = 1,
                            flex = 1, rb_wr = 1,
                            dl = 1, db = 1,
                            fa = NULL,
                            transactions = NULL,
                            plot = FALSE) {

  team_col <- names(select(lineup_df, starts_with("team")))
  lineup_df <- select(lineup_df, league:week,
                      team = starts_with("team"), score:act_pts)
  teams <- unique(lineup_df$team)

  weeks <- unique(lineup_df$week)


  lineup_df <- lineup_df %>%
    mutate(position = case_when(
      position %in% c("DB", "CB", "S") ~ "DB",
      position %in% c("DL", "DE", "DT", "LB") ~ "DL",
      position %in% c("DST", "D/ST", "DEF") ~ "DST",
      TRUE ~ position
    ))

  if("proj" %in% names(lineup_df)) {
    lineup_df <- lineup_df %>% select(-proj)
  }

  if(!is.null(fa)) {
    lineup_df <- bind_rows(lineup_df, fa)
  }

  if(!is.null(transactions)) {
    lineup_df <- bind_rows(lineup_df, transactions)
  }

  positions <- tibble(qb, rb, wr, te,
                      dst, k, dl, db) %>%
    gather(position, roster_spots) %>%
    filter(roster_spots > 0) %>%
    mutate(position = toupper(position))

  best_lineup <- crossing(team = teams, week = weeks, positions) %>%
    mutate(data = list(lineup_df),
           tmp = pmap(list(data, team, week,
                           position, roster_spots),
                      max_points_position)) %>%
    unnest(tmp) %>%
    select(team, week, position, max_points, player) %>%
    nest(player = player)

  best_lineup <- crossing(team = teams, week = weeks) %>%
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
      mutate(team = factor(team)) %>%
      ggplot(aes(week, delta, fill = delta)) +
      geom_bar(stat = 'identity', color = "black") +
      scale_x_continuous(breaks = 1:max(lineup_df$week),
                         labels = paste("week", 1:max(lineup_df$week)),
                         trans = "reverse") +
      facet_wrap(~reorder(team, avg), ncol = n_distinct(lineup_df$team)/2) +
      guides(fill=FALSE) +
      labs(title = "Weekly Manager Evaluation",
           subtitle = "How many points you left on your bench each week",
           x = NULL, y = "Lost points") +
      theme_fvoa() +
      theme(panel.grid.major.y = element_blank()) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      coord_flip()

  } else {

    best_lineup_final %>%
      set_names(team_col, "week", "max", "score",
                "delta", "sign", "avg")

  }
}

#' @export
evaluate_model <- function(scores,
                           evaluation_week,
                           .fun = simulate_score,
                           ...) {

  team_col <- names(select(scores, starts_with("team")))
  scores_tmp <- scores %>%
    extract_scores() %>%
    select(week, team = starts_with("team"), score)
  teams <- unique(scores_tmp$team)

  previous_scores <- scores_tmp %>%
    filter(week < evaluation_week) %>%
    mutate(evaluation_week = evaluation_week)

  actual_scores <- scores_tmp %>%
    filter(week == evaluation_week) %>%
    select(team1 = team, score1 = score)

  set.seed(42)

  pred_scores <- actual_scores %>%
    mutate(sim1 = map(team1, .fun,
                      scores = previous_scores,
                      ...))

  out <- crossing(pred_scores,
                  rename(pred_scores,
                         team2 = team1, score2 = score1, sim2 = sim1)) %>%
    filter(team1 != team2) %>%
    mutate(margin = score1 - score2,
           sim = map2(sim1, sim2, ~ .x - .y),
           win_prob = map_dbl(sim, convert_wp),
           lower80 = map_dbl(sim, ~ quantile(.x, 0.1)),
           upper80 = map_dbl(sim, ~ quantile(.x, 0.9)),
           lower95 = map_dbl(sim, ~ quantile(.x, 0.025)),
           upper95 = map_dbl(sim, ~ quantile(.x, 0.975)),
           pred_outcome = if_else(win_prob > 50, 1, 0),
           act_outcome = if_else(score1 - score2 > 0, 1, 0),
           correct = if_else(pred_outcome == act_outcome, 1, 0),
           range80 = if_else(between(margin, lower80, upper80), 1, 0),
           range95 = if_else(between(margin, lower95, upper95), 1, 0),
           sim = case_when(
             win_prob == 0 & correct == 1 ~ 1000,
             win_prob == 0 & correct == 0 ~ -1000,
             correct == 1 ~ win_prob,
             TRUE ~ -win_prob),
           week = evaluation_week) %>%
    select(week, team1, team2, margin,
           lower80, upper80, lower95, upper95,
           range80, range95,
           win_prob, pred_outcome, act_outcome,
           correct, sim) %>%
    set_names("week", team_col, "opp", "margin",
              "lower80", "upper80", "lower95", "upper95",
              "range80", "range95",
              "win_prob", "pred_outcome", "act_outcome",
              "correct", "sim")

  if("league" %in% names(scores)) {

    out <- out %>%
      mutate(league = scores$league[1],
             leagueID = scores$leagueID[1],
             season = scores$season[1],
             .before = 1)

  }

  return(out)

}

#' @export
evaluate_brier <- function(evaluation_df) {

  team_col <- names(select(evaluation_df, starts_with("team")))

  evaluation_df %>%
    select(team = starts_with("team"),
           win_prob, sim, act_outcome) %>%
    mutate(sim = if_else(abs(sim) == 1000, 0, sim),
           pred = win_prob / 100,
           brier = (0.25 - (pred - act_outcome)^2) * 100) %>%
    group_by(team) %>%
    summarise(brier = round(mean(brier), 2),
              .groups = "drop") %>%
    select(team, brier) %>%
    set_names(team_col, "brier")

}

#' @export
evaluate_team_accuracy <- function(evaluation_df) {

  team_col <- names(select(evaluation_df, starts_with("team")))

  evaluation_df %>%
    select(team = starts_with("team"), correct) %>%
    group_by(team) %>%
    summarise(correct = sum(correct),
              .groups = "drop") %>%
    arrange(-correct) %>%
    set_names(team_col, "correct")

}

#' @export
evaluate_tiers <- function(evaluation_df) {

  evaluation_df %>%
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

}

# Helper Functions --------------------------------------------------------

max_points_position <- function(lineup_df, .team, .week,
                                .position, roster_spots) {

  lineup_df %>%
    filter(team == .team,
           week == .week,
           position == .position) %>%
    top_n(roster_spots, act_pts) %>%
    mutate(max_points = sum(act_pts)) %>%
    select(max_points, player)

}

max_points_flex <- function(best_lineup, lineup_df, .team, .week,
                            rb_wr = 1, flex = 1) {

  best_lineup <- best_lineup %>%
    filter(team == .team,
           week == .week)

  lineup_df <- lineup_df %>%
    filter(team %in% c(.team, NA),
           week == .week)

  if(rb_wr > 0) {

    chosen_players <- best_lineup %>%
      unnest(player) %>%
      select(player)

    best_rb_wr <- lineup_df %>%
      anti_join(chosen_players, by = "player") %>%
      filter(position %in% c("RB", "WR")) %>%
      top_n(rb_wr, act_pts) %>%
      mutate(max_points = sum(act_pts),
             position = "RB_WR") %>%
      nest(player = player) %>%
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
      top_n(flex, act_pts) %>%
      mutate(max_points = sum(act_pts),
             position = "Flex") %>%
      nest(player = player) %>%
      select(team, week, position, max_points, player)

    best_lineup <- bind_rows(best_lineup,
                             best_flex)

  }

  best_lineup %>%
    select(-player) %>%
    summarise(max_points = sum(max_points))

}
