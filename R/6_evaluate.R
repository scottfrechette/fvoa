
#' @export
evaluate_lineup <- function(lineup_df,
                            qb = 1,
                            rb = 2,
                            wr = 3,
                            te = 1,
                            dst = 1,
                            k = 1,
                            flex = 1,
                            rb_wr = 1,
                            dl = 1,
                            db = 1,
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
evaluate_model <- function(fit_team_season_df, schedule = NULL) {

  sims <- fit_team_season_df %>%
    filter(week < max(fit_team_season_df$week)) %>%
    mutate(week = week + 1L,
           sims = map(model,
                      ~ fit_team_season_df$scores_filtered[[1]]["team"] %>%
                        tidybayes::add_predicted_draws(.x, seed = 42, value = "score") %>%
                        ungroup() %>%
                        nest(data = -team))) %>%
    select(week, sims) %>%
    unnest(sims)

  if(!is.null(schedule)) {

    tmp <-  schedule %>%
      filter(week > 1) %>%
      inner_join(scores, by = c("week", "team")) %>%
      left_join(rename(scores, opponent = team, opp_score = score), by = c("week", "opponent")) %>%
      left_join(sims, by = c("week", "team")) %>%
      left_join(rename(sims, opponent = team, opp_data = data), by = c("week", "opponent"))

  } else {

    tmp <- crossing(week = 2:max(scores$week),
                    team = unique(scores$team),
                    opponent = unique(scores$team)) %>%
      filter(team != opponent) %>%
      left_join(scores, by = c("week", "team")) %>%
      left_join(rename(scores, opponent = team, opp_score = score), by = c("week", "opponent")) %>%
      left_join(sims, by = c("week", "team")) %>%
      left_join(rename(sims, opponent = team, opp_data = data), by = c("week", "opponent"))

  }

  tmp %>%
    mutate(margin = score - opp_score,
           sims = map2(data, opp_data, ~.x - .y),
           win_prob = map_dbl(sims, ~convert_wp(.$score)),
           lower50 = map_dbl(sims, ~ quantile(.$score, 0.25)),
           upper50 = map_dbl(sims, ~ quantile(.$score, 0.75)),
           lower80 = map_dbl(sims, ~ quantile(.$score, 0.1)),
           upper80 = map_dbl(sims, ~ quantile(.$score, 0.9)),
           lower95 = map_dbl(sims, ~ quantile(.$score, 0.025)),
           upper95 = map_dbl(sims, ~ quantile(.$score, 0.975)),
           pred_outcome = if_else(win_prob > 50, 1L, 0L),
           act_outcome = if_else(score - opp_score > 0, 1L, 0L),
           correct = if_else(pred_outcome == act_outcome, 1L, 0L),
           range50 = pmap_int(list(margin, lower50, upper50),
                              ~if_else(between(..1, ..2, ..3), 1L, 0L)),
           range80 = pmap_int(list(margin, lower80, upper80),
                              ~if_else(between(..1, ..2, ..3), 1L, 0L)),
           range95 = pmap_int(list(margin, lower95, upper95),
                              ~if_else(between(..1, ..2, ..3), 1L, 0L)),
           sim = case_when(
             win_prob == 0 & correct == 1 ~ 1000,
             win_prob == 0 & correct == 0 ~ -1000,
             correct == 1 ~ win_prob,
             TRUE ~ -win_prob)) %>%
    select(week, team, opp = opponent, margin, act_outcome,
           win_prob, pred_outcome, correct, sim,
           lower50, upper50, range50,
           lower80, upper80, range80,
           lower95, upper95, range95)

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
evaluate_team_accuracy <- function(evaluation_df, .latest = TRUE) {

  team_col <- names(select(evaluation_df, starts_with("team")))

  if(.latest) filter(evaluation_df, week == max(week))

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

#' @export
evaluate_projections <- function(projections, schedule = NULL,
                                 .summary = FALSE) {

  if (!is.null(schedule)) {

    out <- schedule %>%
      inner_join(projections, by = c("week", "team")) %>%
      left_join(rename(projections, opponent = 2,
                       opp_proj = 3, opp_act = 4),
                by = c("week", "opponent")) %>%
      mutate(proj_win = projected > opp_proj,
             act_win = actual > opp_act,
             correct = proj_win == act_win) %>%
      group_by(week) %>%
      summarize(correct = mean(correct),
                .groups = 'drop')

  } else {

    out <- crossing(team = unique(projections$team),
             opponent = unique(projections$team)) %>%
      filter(team != opponent) %>%
      inner_join(projections, by = "team") %>%
      left_join(rename(projections, opponent = 2,
                       opp_proj = 3, opp_act = 4),
                by = c("week", "opponent")) %>%
      mutate(proj_win = projected > opp_proj,
             act_win = actual > opp_act,
             correct = proj_win == act_win)

  }

  if (.summary) {

    out <- out %>%
      group_by(week) %>%
      summarize(correct = mean(correct),
                .groups = "drop")

  }

  return(out)

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

convert_wp <- function(sim) {

  round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE) * 100 -
          round(dnorm(0, mean(sim), sd(sim)) * 100, 2)/2, 2)

}
