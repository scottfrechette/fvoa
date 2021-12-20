#' @export
theme_fvoa <- function(base_size = 12, base_family = "Helvetica") {
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey50"),
        strip.background = element_rect(color = "black"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        strip.text = element_text(size = 12))
}


# Weekly ------------------------------------------------------------------

#' @export
plot_scores <- function(scores, x = week, y = score, group = team) {

  x_quo <- enquo(x)
  y_quo <- enquo(y)
  group_quo <-enquo(group)


  ggplot(scores, aes(!!x_quo, !!y_quo, color = !!group_quo)) +
    geom_line(size = 1.5) +
    geom_point(size = 2) +
    facet_wrap(~reorder(team, -score, FUN = mean), ncol=n_distinct(scores$team)/2) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    labs(y = quo_name(y_quo), x = quo_name(x_quo), title = "weekly scores") +
    guides(color = "none") +
    stat_smooth(se = FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    theme_fvoa()
}

#' @export
plot_fvoa <- function(fvoa_df, x = week, y = fvoa, group = team) {

  x_quo <- enquo(x)
  y_quo <- enquo(y)
  group_quo <-enquo(group)

  fvoa_df %>%
    ggplot(aes(!!x_quo, !!y_quo, color = !!group_quo)) +
    # geom_smooth(se=F, color = "darkgrey",
    #             # n = n_distinct(!!x_quo),
    #             linetype=2, formula = y ~ x, method = "loess") +
    geom_line(alpha = 0.5, size = 1.5) +
    geom_point() +
    geom_hline(yintercept = 0, color = "darkgrey", linetype = 2) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
    guides(color = "none") +
    theme_fvoa()
}

#' @export
plot_simulation <- function(simulated_season_df,
                            plot = c(wins, points, playoffs)) {

  plots <- tibble(wins = "Projected Wins by week",
                  points = "Projected Total Points by week",
                  playoffs = "Projected Chance of Making Playoffs by week")

  plot_quo <- enquo(plot)

  simulated_season_df %>%
    ggplot(aes(week, !!plot_quo, color = team)) +
    geom_line(size=1.5) +
    geom_point(size = 2) +
    stat_smooth(se = FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    facet_wrap(~reorder(team, rank, FUN = last), ncol = 5) +
    labs(y = "Wins", x = "week",
         title = pull(plots, !!plot_quo)) +
    guides(color = "none") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    theme_fvoa()
}

#' @export
plot_roster_skills <- function(lineup_evaluation) {

  # Horizontal Bars by Team
  lineup_evaluation %>%
    group_by(team) %>%
    mutate(delta = optimal - actual,
           avg = mean(delta)) %>%
    ungroup() %>%
    ggplot(aes(week, delta, fill = delta)) +
    geom_bar(stat = 'identity', color = "black") +
    scale_x_continuous(breaks = 1:max(lineup_evaluation$week),
                       labels = paste("Week", 1:max(lineup_evaluation$week)),
                       trans = "reverse") +
    scale_y_continuous(expand = c(0, NA)) +
    scale_fill_gradient(low = "white", high = "#0072B2", limits = c(0, NA)) +
    facet_wrap(~reorder(team, -avg), ncol = n_distinct(lineup_evaluation$team)/2) +
    guides(fill = "none") +
    labs(title = "Weekly Roster Evaluation",
         # subtitle = "How many points did you leave on your bench each week?",
         x = NULL,
         y = "Points left on Bench") +
    coord_flip() +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())

  # # Vertical Bars by Team
  # lineup_evaluation %>%
  #   group_by(team) %>%
  #   mutate(delta = optimal - actual,
  #          avg = mean(delta)) %>%
  #   ungroup() %>%
  #   ggplot(aes(week, delta, fill = delta)) +
  #   geom_bar(stat = 'identity', color = "black") +
  #   scale_x_continuous(breaks = 1:max(lineup_evaluation$week)) +
  #   facet_wrap(~reorder(team, -avg), ncol = n_distinct(lineup_evaluation$team)/2) +
  #   guides(fill = "none") +
  #   labs(title = "Weekly Manager Evaluation",
  #        subtitle = "How many points you left on your bench each week",
  #        x = NULL, y = "Lost points") +
  #   theme_fvoa() +
  #   theme(panel.grid.major.y = element_blank()) +
  #   scale_fill_gradient(low = "white", high = "#0072B2")
  #
  # # Score Rank By Week
  # lineup_evaluation %>%
  #   mutate(delta = optimal - actual,
  #          team_score = reorder_within(team, actual, week)) %>%
  #   ggplot(aes(y = team_score, color = team)) +
  #   geom_point(aes(x = actual), size = 3) +
  #   geom_point(aes(x = optimal), size = 3, shape = 8) +
  #   geom_segment(aes(yend = team_score, x = actual, xend = optimal)) +
  #   scale_y_reordered() +
  #   facet_wrap(~ week,
  #              scales = "free_y",
  #              ncol = 3,
  #              labeller = labeller(week = ~paste("Week", .))) +
  #   labs(x = "Score",
  #        y = NULL,
  #        title = "Difference in score and optimal lineup") +
  #   guides(color = "none") +
  #   theme_fvoa()

}

#' @export
plot_projected_margin <- function(team) {

  team %>%
    extract_projections() %>%
    mutate(margin = actual - projected,
           sign = margin >= 0,
           fill_label = case_when(
             margin > 0 ~ "positive",
             margin < 0 ~ "negative",
             TRUE       ~ "equal"
           ),
           avg = mean(margin, na.rm = T),
           pos_count = sum(sign)) %>%
    ggplot(aes(x = week, y = margin, fill = fill_label)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(equal = "#619CFF",
                                 negative = "#F8766D",
                                 positive = "#00BA38")) +
    # scale_x_continuous(breaks = 1:max(team$week)) +
    facet_wrap(~reorder(team, - pos_count), ncol = 5) +
    guides(fill = "none") +
    labs(title = "Weekly Projection v Actual Results",
         x = NULL,
         y = "Margin") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
}

# Teams -------------------------------------------------------------------

#' @export
plot_team_fvoa <- function(fit,
                           .label = T,
                           .average = T) {

  tmp <- tidybayes::add_epred_draws(distinct(fit$data, team), fit, seed = 42) %>%
    mutate(fvoa = .epred - 115) %>%
    tidybayes::median_hdi(fvoa, .width = c(.89, .5)) %>%
    mutate(label = str_glue("{team} ({round(fvoa, 1)})")) %>%
    arrange(-fvoa) %>%
    left_join(as_tibble(fit$data) %>%
                group_by(team) %>%
                summarize(avg = mean(score) - 115),
              by = "team")

  p <- tmp %>%
    ggplot(aes(y = reorder(team, fvoa),
               yend = reorder(team, fvoa))) +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.89),
                 size = 0.5, color = "#6497b1") +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.5),
                 size = 2, color = "#03396c") +
    geom_point(aes(x = fvoa),
               size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    labs(x = "FVOA", y = NULL) +
    theme_fvoa() +
    theme(axis.text.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          panel.grid.major.y = element_blank())

  if(.label) {

    p <- p +
      geom_text(aes(label = label, x = fvoa),
                vjust = -1, alpha = 0.5, size = 3.5) +
      theme(axis.text.y = element_blank(),
            panel.border = element_blank())

  }

  if (.average) {

    p <- p +
      geom_point(aes(x = avg),
                 size = 4)

  }

  return(p)

}

#' @export
plot_boxplots <- function(scores) {
  ggplot(scores, aes(x=reorder(team, -score, fun=mean), y=score, fill=team)) +
    geom_boxplot(coef = 1.25, outlier.alpha = 0.6) +
    stat_summary(fun = mean, geom="point", shape=18, size=3, show.legend = FALSE) +
    guides(fill = "none") +
    labs(y = "score", x = "", title = "Team Boxplots") +
    theme_fvoa() +
    theme(panel.border = element_blank())
}

#' @export
plot_joy_plots <- function(scores) {

  requireNamespace("ggridges", quietly = TRUE)

  ggplot(scores, aes(x = score, y = reorder(team, score, FUN = mean), fill = team)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept = mean(score)), alpha = 0.5) +
    labs(x = "Distribution of scores", y = "", title = "Team Density Plots") +
    guides(fill = "none") +
    theme_fvoa()
}

#' @export
plot_h2h_matchup <- function(team1, team2,
                             fit = NULL, draws = NULL,
                             square = FALSE) {

  if (!is.null(fit)) {

    sim_scores_subset <- as_tibble(fit$data) %>%
      extract_team_draws(fit) %>%
      ungroup() %>%
      select(sim = .draw, team, score = .prediction) %>%
      filter(team %in% c(team1, team2))

  } else if (!is.null(draws)) {

    sim_scores_subset <- draws %>%
      ungroup() %>%
      select(sim = .draw, team, score = .prediction) %>%
      filter(team %in% c(team1, team2))

  } else {

    "ERROR"

  }

  lo <- min(sim_scores_subset$score) - 10
  hi <- max(sim_scores_subset$score) + 10

  sim_scores_final <- sim_scores_subset %>%
    spread(team, score) %>%
    select(sim, tm1 = !!team1, tm2 = !!team2) %>%
    mutate(margin = tm1 - tm2,
           winner = tm1 > tm2)

  wp_points <- sim_scores_final %>%
    summarize(tm1_min = min(tm1),
              tm1_max = max(tm1),
              tm2_min = min(tm2),
              tm2_max = max(tm2))

  wp_labels <- sim_scores_final %>%
    summarize(tm1_wins = mean(margin > 0),
              tm1_blowout = mean(margin >= 20),
              tm1_comfortable = mean(margin >= 5 & margin < 20),
              tm1_squeaker = mean(margin > 0 & margin < 5),
              tie = mean(margin == 0),
              tm2_wins = mean(margin < 0),
              tm2_blowout = mean(margin <= -20),
              tm2_comfortable = mean(margin <= -5 & margin > -20),
              tm2_squeaker = mean(margin < 0 & margin > -5)) %>%
    mutate(across(everything(), ~scales::percent(.)))

  p <- sim_scores_final %>%
    ggplot(aes(tm1, tm2, color = winner)) +
    geom_point(alpha = 0.1) +
    geom_abline(color = "grey30", linetype = 2) +
    annotate("text",
             x = wp_points$tm1_min,
             y = wp_points$tm2_max - 5,
             hjust = 0,
             label = str_glue("Squeaker (<5 points): {wp_labels$tm1_squeaker}\nBlowout (>20 points): {wp_labels$tm1_blowout}"),
             color = "grey65") +
    annotate("text",
             x = wp_points$tm1_max - 80,
             y = wp_points$tm2_min + 5,
             hjust = 0,
             label = str_glue("Squeaker (<5 points): {wp_labels$tm2_squeaker}\nBlowout (>20 points): {wp_labels$tm2_blowout}"),
             color = "grey65") +
    guides(color = "none") +
    labs(x = str_glue(team1, " Simulated Scores \n(Win Probability: {wp_labels[['tm1_wins']]})"),
         y = str_glue(team2, " Simulated Scores \n(Win Probability: {wp_labels[['tm2_wins']]})")) +
    theme_fvoa()

  # p <- sim_scores_final %>%
  #   ggplot(aes(tm1, tm2, color = winner)) +
  #   geom_point(alpha = 0.1) +
  #   geom_abline(color = "grey30", linetype = 2) +
  #   guides(color = "none") +
  #   labs(x = str_glue(team1, " Simulated Scores \n(Win Probability: {wp_labels[['tm1_wins']]})"),
  #        y = str_glue(team2, " Simulated Scores \n(Win Probability: {wp_labels[['tm2_wins']]})")) +
  #   theme_fvoa()

  if (square) {

    p <- p +
      coord_cartesian(xlim = c(lo, hi), ylim = c(lo, hi))

  }

  p

}

#' @export
plot_matchups <- function(all_matchups_df) {

  matchup_df <- all_matchups_df %>%
    select(winner = team1, loser = team2, wp) %>%
    # rename(winner = team) %>%
    # gather(loser, score, -winner) %>%
    mutate(winner = as_factor(winner) %>%
             fct_rev(),
           loser = as_factor(loser))

  matchup_df %>%
    ggplot(aes(reorder(winner, -wp, FUN = mean), wp)) +
    geom_point(aes(color = loser)) +
    geom_hline(yintercept = 0.5, color = "darkgrey", linetype = 2) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", y = "% Chance to Win", color = "") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
}

#' @export
plot_matchups_hm <- function(all_matchups_df) {

  hm_df <- all_matchups_df %>%
    select(winner = team1, loser = team2, wp) %>%
    # rename(winner = team) %>%
    # gather(loser, score, -winner) %>%
    mutate(winner = as_factor(winner) %>%
             fct_rev(),
           loser = as_factor(loser))

  hm_df %>%
    ggplot(aes(loser, winner, fill = wp)) +
    geom_tile() +
    scale_fill_distiller(palette = "Spectral", direction = 1, limits = c(0, 1)) +
    theme(panel.background=element_rect(fill="white", colour="white")) +
    labs(x = "Opponent",
         y = "Team",
         fill = "% Chance",
         title = "Who are the strongest teams?")

}



#' @export
plot_playoff_leverage <- function(sim_standings) {

  leverage_week <- unique(sim_standings$leverage_week)

  sim_standings %>%
    group_by(team, leverage_win) %>%
    summarize(playoffs = mean(playoffs),
              .groups = "drop") %>%
    mutate(leverage_win = if_else(leverage_win == 1, "Win", "Lose")) %>%
    spread(leverage_win, playoffs) %>%
    mutate(delta = Win - Lose,
           Total = 1) %>%
    ggplot(aes(reorder(team, Win), y = Total)) +
    geom_bar(stat = "identity", fill = "white", color = "grey", alpha = 0.4) +
    geom_bar(stat = "identity", aes(y = Win, fill = team), alpha = 0.5) +
    geom_bar(stat = "identity", aes(y = Lose, fill = team)) +
    geom_text(aes(y = Total + 0.005,
                  label = scales::percent(delta, accuracy = 1)),
              color = "grey30", hjust = 0) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1.05),
                       expand = c(0, NA),
                       breaks = c(0, .25, .50, .75, 1)) +
    guides(fill = "none") +
    labs(x = NULL,
         y = NULL,
         title = str_glue("Playoff Probability Leverage (Week {leverage_week})")) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
          axis.text.x = element_text(size = 12, color = "grey"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(color = "black"),
          panel.ontop = T,
          panel.grid = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "white", size = 0.2),
          strip.text = element_text(size =12))

}


# Luck --------------------------------------------------------------------

#' @export
plot_points_luck <- function(schedule,
                             scores,
                             x = c("pf", "pa", "delta")) {

  x <- match.arg(x)

  x_label <- case_when(
    x == "pf" ~ "Points Scored",
    x == "pa" ~ "Points Against",
    x == "delta" ~ "Point Differential"
  )

  num_games <- max(scores$week)

  quadrants <- schedule %>%
    left_join(scores, by = c("week", "team")) %>%
    left_join(rename(scores, opponent = team, opp_score = score),
              by = c("week", "opponent")) %>%
    drop_na() %>%
    mutate(diff = score - opp_score) %>%
    select(week, team,
           score, opp_score,
           diff) %>%
    group_by(team) %>%
    summarise(pf = sum(score),
              pa = sum(opp_score),
              delta = sum(diff),
              wp = sum(diff > 0) / num_games,
              .groups = "drop") %>%
    select(team, wp, x_axis = x)

  x_intercept <- case_when(
    x == "pf" ~ mean(quadrants$x_axis),
    x == "pa" ~ mean(quadrants$x_axis),
    x == "delta" ~ 0
  )

  quadrants %>%
    ggplot(aes(x_axis, wp)) +
    geom_point() +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = x_intercept) +
    annotate("text",
             x = (max(quadrants$x_axis) - x_intercept) / 2 + x_intercept,
             # y = (max(quadrants$wp) - 0.5) / 2 + 0.5,
             y = 0.75,
             size = 8,
             label = "Good",
             color = "grey65") +
    annotate("text",
             x = (max(quadrants$x_axis) - x_intercept) / 2 + x_intercept,
             # y = (min(quadrants$wp) - 0.5) / 2 + 0.5,
             y = 0.25,
             size = 8,
             label = "Underrated",
             color = "grey65") +
    annotate("text",
             x = x_intercept - (x_intercept - min(quadrants$x_axis)) / 2,
             # y = (max(quadrants$wp) - 0.5) / 2 + 0.5,
             y = 0.75,
             size = 8,
             label = "Overrated",
             color = "grey65") +
    annotate("text",
             x = x_intercept - (x_intercept - min(quadrants$x_axis)) / 2,
             # y = (min(quadrants$wp) - 0.5) / 2 + 0.5,
             y = 0.25,
             size = 8,
             label = "Bad",
             color = "grey65") +
    ggrepel::geom_text_repel(aes(label = team)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1),
                       expand = c(0, 0)) +
    labs(y = "Win Percentage",
         x = x_label) +
    guides(color = "none") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())

}

#' @export
plot_schedule_luck <- function(schedule,
                               scores,
                               owners,
                               sims = 100,
                               tries = 0.1 * sims) {

  owners_tmp <- owners %>%
    semi_join(scores, by = "team") %>%
    mutate(team_id = 1:n())

  sim_schedules <- ffsched::generate_schedules(league_size = n_distinct(schedule$team),
                                               weeks = max(schedule$week),
                                               sims = sims,
                                               seed_init = 42,
                                               export = FALSE) %>%
    left_join(owners_tmp, by = "team_id") %>%
    left_join(rename(owners_tmp, opponent = team, opponent_id = team_id), by = "opponent_id") %>%
    select(sim = idx_sim, week, team, opponent) %>%
    inner_join(scores, by = c("week", "team")) %>%
    left_join(rename(scores, opponent = team, opp_score = score), by = c("week", "opponent")) %>%
    mutate(win = score > opp_score)

  sim_schedule_standings <- sim_schedules %>%
    group_by(sim, team) %>%
    summarize(wins = sum(win),
              points = sum(score),
              .groups = "drop") %>%
    group_by(sim) %>%
    arrange(-wins, -points) %>%
    mutate(rank = 1:n()) %>%
    ungroup() %>%
    arrange(sim, rank) %>%
    count(team, rank)

  sim_schedule_standings_full <- crossing(team = unique(sim_schedule_standings$team),
                                          rank = 1:n_distinct(sim_schedule_standings$team)) %>%
    left_join(sim_schedule_standings, by = c("team", "rank")) %>%
    replace_na(list(n = 0)) %>%
    mutate(pct = n / sims) %>%
    left_join(calculate_stats(schedule, scores) %>%
                select(team, actual_rank = 6),
              by = "team") %>%
    left_join(sim_schedule_standings %>%
                group_by(team) %>%
                slice_max(n) %>%
                ungroup() %>%
                select(team, sim_rank = rank),
              by = "team") %>%
    mutate(team = fct_reorder(team, -sim_rank))

  sim_schedule_standings_full %>%
    ggplot(aes(y = team, x = rank)) +
    geom_tile(aes(fill = pct), alpha = 0.5, na.rm = FALSE) +
    geom_tile(data = distinct(sim_schedule_standings_full, team, rank = actual_rank),
              fill = NA, color = 'black', size = 3) +
    geom_tile(data = distinct(sim_schedule_standings_full, team, rank = sim_rank),
              fill = NA, color = 'black', linetype = 2) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1))) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1)),
              data = filter(sim_schedule_standings_full, rank == actual_rank)) +
    scale_x_continuous(breaks = 1:n_distinct(schedule$team), expand = c(0, 0)) +
    scale_fill_gradient(low = "white", high = "#0072B2") +
    guides(fill = "none") +
    theme_minimal() +
    theme(axis.text.y = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold", size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = 'Simulated standings positions',
         subtitle = sprintf('Based on %s unique schedules', scales::comma(sims)),
         x = "Rank",
         y = NULL)

}

#' @export
plot_wp_allplay <- function(schedule, scores) {

  tmp <- left_join(scores,
            rename(scores, opponent = team, opp_score = score),
            by = "week") %>%
    filter(team != opponent) %>%
    left_join(mutate(schedule, scheduled = T),
              by = c("week", "team", "opponent")) %>%
    replace_na(list(scheduled = FALSE)) %>%
    mutate(win = score > opp_score,
           scheduled_win = scheduled & win) %>%
    group_by(team) %>%
    summarize(scheduled = sum(scheduled),
              scheduled_wins = sum(scheduled_win),
              possible = n(),
              possible_wins = sum(win)) %>%
    mutate(scheduled_wp = scheduled_wins / scheduled,
           possible_wp = possible_wins / possible,
           wp_delta = format_pct(possible_wp - scheduled_wp, 0.1),
           label_x = if_else(scheduled_wp > possible_wp,
                             (scheduled_wp - possible_wp) / 2 + possible_wp,
                             (possible_wp - scheduled_wp) / 2 + scheduled_wp),
           label = str_glue("{team} ({wp_delta})"),
           team = fct_reorder(team, scheduled_wp))

  tmp %>%
    ggplot(aes(x = scheduled_wp,
               xend = possible_wp,
               y = team,
               yend = team)) +
    geom_segment(aes(color = scheduled_wp < possible_wp),
                 alpha = 0.2,
                 size = 1.5) +
    geom_segment(data = filter(tmp, scheduled_wp != possible_wp),
                 aes(color = scheduled_wp < possible_wp),
                 alpha = 0.2,
                 size = 1.5,
                 arrow = arrow(type = "open", ends = "last")) +
    geom_point(data = filter(tmp, scheduled_wp != possible_wp),
               alpha = 0.5,
               shape = 21,
               size = 4) +
    geom_point(data = filter(tmp, scheduled_wp != possible_wp),
               aes(x = possible_wp,
                   color = scheduled_wp < possible_wp),
               alpha = 0.7,
               size = 4) +
    geom_point(data = filter(tmp, scheduled_wp == possible_wp),
               shape = 21,
               size = 6) +
    geom_text(aes(x = label_x,
                  label = wp_delta,
                  color = scheduled_wp < possible_wp),
              vjust = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = c(0, 0)) +
    scale_color_manual(values = c("red", "darkgreen")) +
    labs(y = NULL,
         x = "Win Percentage",
         title = "Change in Win Percentage from Head-to-Head to All Play") +
    guides(color = "none") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())

}

#' @export
# add win bins?
plot_exp_wpct <- function(scores, schedule) {

  # wins or win_pct?
  # pythagorean or other?

  schedule %>%
    # filter(week < 2) %>%
    mutate_at(vars(team1, team2), as.character) %>%
    inner_join(scores %>%
                 rename(t1_points = score),
               by = c("week", "team1" = "team")) %>%
    inner_join(scores %>%
                 rename(t2_points = score),
               by = c("week", "team2" = "team")) %>%
    group_by(team = team1) %>%
    summarize(PF = sum(t1_points),
              PA = sum(t2_points),
              wins = sum(t1_points > t2_points),
              games = n()) %>%
    mutate(exp_wpct_pyth = (PF ^ 2.37 / (PF ^ 2.37 + PA ^ 2.37)),
           exp_wins = (PF ^ 2.37 / (PF ^ 2.37 + PA ^ 2.37) * games),
           exp_wpct = 1 / (1 + (PA / PF) ^2),
           wpct = wins / games) %>%
    ggplot(aes(wpct, exp_wpct, label = team)) +
    ggrepel::geom_text_repel() +
    geom_point() +
    geom_abline(linetype = 2) +
    annotate(geom="text", x = 0.15, y = 0.85,
             color = "grey65",
             label = "Won less than expected") +
    annotate(geom="text", x = 0.85, y = 0.15,
             color = "grey65",
             label = "Won more than expected") +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = "Winning Percentage",
         y = "Expected Winning Percentage",
         title = "Real and Expected Win Percentage for each team") +
    theme_fvoa()

}

# Simulations -------------------------------------------------------------

#' @export
plot_simulated_wins <- function(simulated_season_standings) {

  simulated_season_standings %>%
    group_by(team) %>%
    mutate(avg = mean(wins)) %>%
    group_by(team, avg, wins) %>%
    summarize(pct = n() / 10000,
              .groups = "drop") %>%
    ggplot(aes(wins, pct, fill = team)) +
    geom_col() +
    geom_vline(aes(xintercept = avg), linetype = 2, color = 'red') +
    scale_x_continuous(breaks = 1:max(schedule$week), labels = 1:max(schedule$week)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~ reorder(team, -avg), ncol = 5) +
    theme_fvoa() +
    guides(fill = "none") +
    labs(x = "# Wins",
         y = "% of Simulations",
         title = "Simulated Team Win Distribution")


}

#' @export
plot_simulated_weekly_points <- function(simulated_season_scores, n = 100) {

  simulated_season_scores %>%
    nest(data = -sim) %>%
    slice_sample(n = n) %>%
    unnest(data) %>%
    select(sim, week, team = team1, score = score1) %>%
    group_by(week, team) %>%
    mutate(avg = mean(score)) %>%
    ungroup() %>%
    mutate(team = fct_reorder(team, avg, .fun = mean, .desc = T)) %>%
    ggplot(aes(x = week, y = score, color = team, group = sim)) +
    geom_line(alpha = 0.1) +
    geom_line(aes(y = avg), size = 1.5) +
    facet_wrap(~ team) +
    labs(x = "Week", y = "Score") +
    theme_fvoa() +
    guides(color = "none")

}

#' @export
plot_simulated_cumulative_points <- function(simulated_season_scores, n = 100) {

  simulated_season_scores %>%
    nest(data = -sim) %>%
    slice_sample(n = n) %>%
    unnest(data) %>%
    select(sim, week, team = team1, score = score1) %>%
    group_by(sim, team) %>%
    mutate(points = cumsum(score)) %>%
    group_by(week, team) %>%
    mutate(avg = mean(points)) %>%
    ungroup() %>%
    mutate(team = fct_reorder(team, avg, .fun = last, .desc = T)) %>%
    ggplot(aes(x = week, y = points, color = team, group = sim)) +
    geom_line(alpha = 0.1) +
    geom_line(aes(y = avg), size = 1.5) +
    facet_wrap(~ team) +
    labs(x = "Week", y = "Total Points") +
    theme_fvoa() +
    guides(color = "none")
}

#' @export
plot_simulated_wins <- function(simulated_standings) {

  simulated_standings %>%
    add_count(team, wt = wins, name = "total_wins") %>%
    mutate(team = fct_reorder(team, -total_wins)) %>%
    count(team, wins) %>%
    group_by(team) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(wins, pct, fill = team)) +
    geom_col() +
    scale_x_continuous(breaks = 1:max(simulated_standings$wins)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~ team, nrow = 2) +
    labs(x = "Simulated Wins",
         y = NULL,
         title = str_glue("Projected Wins based on {scales::comma(max(simulated_standings$sim))} Simulations")) +
    guides(fill = "none") +
    theme_fvoa()

}

#' @export
plot_simulated_rank <- function(simulated_standings,
                                type = c("facet","grid")) {

  type <- match.arg(type)

  if (type == "facet") {

    simulated_standings %>%
      arrange(-wins, -pf) %>%
      group_by(sim) %>%
      mutate(rank = 1:n()) %>%
      ungroup() %>%
      add_count(team, wt = rank, name = "total_rank") %>%
      mutate(team = fct_reorder(team, total_rank)) %>%
      count(team, rank) %>%
      group_by(team) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(rank, pct, fill = team)) +
      geom_col() +
      scale_x_continuous(breaks = 1:n_distinct(simulated_standings$team)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      facet_wrap(~ team, nrow = 2) +
      labs(x = "Simulated Rank",
           y = NULL,
           title = str_glue("Projected Final Rank based on {scales::comma(max(simulated_standings$sim))} Simulations")) +
      guides(fill = "none") +
      theme_fvoa()

  } else {

    simulated_standings %>%
      group_by(team) %>%
      count(rank) %>%
      mutate(pct = n / sum(n),
             overall_rank = sum(rank * n)) %>%
      ungroup() %>%
      mutate(team = fct_reorder(team, -overall_rank),
             rank = recode(rank,
                           "1" = "1st",
                           "2" = "2nd",
                           "3" = "3rd",
                           "4" = "4th",
                           '5' = "5th",
                           "6" = "6th",
                           "7" = "7th",
                           "8" = "8th",
                           "9" = "9th",
                           "10" = "10th"),
             rank = fct_inorder(rank)) %>%
      select(team, rank, pct) %>%
      complete(team, rank, fill = list(pct = 0)) %>%
      mutate(pct_label = format_pct(pct, accuracy = 0)) %>%
      ggplot(aes(rank, team)) +
      geom_tile(aes(fill = pct), alpha = 0.5, na.rm = F) +
      geom_text(aes(label = pct_label)) +
      scale_fill_gradient(low = "white", high = "#0072B2", limits = c(0, NA)) +
      guides(fill = 'none') +
      theme_minimal() +
      theme(axis.text.y = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold", size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(y = NULL,
           x = "Chances of Final Rank",
           title = str_glue("Projected Final Rank based on {scales::comma(max(simulated_standings$sim))} Simulations"))

  }

}

#' @export
plot_simulated_points <- function(simulated_standings) {

  simulated_standings %>%
    add_count(team, wt = pf, name = "total_points") %>%
    mutate(team = fct_reorder(team, -total_points),
           pf_rounded = ceiling(pf / 50) * 50) %>%
    count(team, pf_rounded) %>%
    group_by(team) %>%
    mutate(pct = n / sum(n)) %>%
    ggplot(aes(pf_rounded, pct, fill = team)) +
    geom_col(color = 'white') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~team, nrow = 2) +
    labs(x = "Simulated Points",
         y = NULL,
         title = str_glue("Projected Points based on {scales::comma(max(simulated_standings$sim))} Simulations")) +
    guides(fill = "none") +
    theme_fvoa()

}

# Model Check -------------------------------------------------------------

#' @export
plot_model_eval_weekly <- function(evaluation_df) {

  n_teams <- n_distinct(select(evaluation_df, starts_with("team")))
  benchmark <- n_teams^2 - n_teams

  evaluation_df %>%
    group_by(week) %>%
    summarise(weekly = sum(correct),
              delta = weekly - benchmark/2,
              percent = round(weekly/benchmark * 100, 1),
              sign = ifelse(delta > 0, "positive",
                            ifelse(delta < 0, "negative", "equal")),
              .groups = "drop") %>%
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
    labs(title = "Weekly Evaluation of FVOA Projections",
         subtitle = paste("Overall accuracy of all possible matchups:",
                          format_pct(mean(evaluation_df$correct), accuracy = 0.1)),
         x = "Week (starting with week 2)",
         y = "Percent Correct") +
    theme(panel.background= element_blank(),
          panel.border = element_blank()) +
    guides(fill = "none")
}

#' @export
plot_model_eval_team <- function(evaluation_df) {

  evaluation_df %>%
    group_by(team, week) %>%
    summarize(correct = mean(correct),
              .groups = "drop") %>%
    mutate(team = fct_reorder(team, -correct, .fun = mean)) %>%
    ggplot(aes(x = week, y = correct, fill = team)) +
    geom_col() +
    scale_x_continuous(name = "week", breaks = 2:max(evaluation_df$week)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = c(0, 0)) +
    facet_wrap(~ team,
               ncol = n_distinct(evaluation_df$team) / 2,
               scales = "free_x") +
    labs(x = "Week",
         y = "% Correct",
         title = "Percent of Possible Matchups Predicted Correctly") +
    guides(fill = "none") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_line(color = "white", size = 0.2),
          panel.ontop = T)

}

#' @export
plot_model_eval_calibration <- function(evaluation_tiers) {

  evaluation_tiers %>%
    group_by(tier) %>%
    summarise(n = sum(n),
              correct = sum(correct),
              percent = round(correct/n * 100, 2)) %>%
    ggplot(aes(tier, percent)) +
    geom_text(aes(label = percent), size = 3,
              alpha = 0.7, vjust = -1) +
    geom_point(aes(size = n)) +
    geom_line() +
    geom_abline(color = "red") +
    geom_abline(color = "red", intercept = 10) +
    scale_x_continuous(limits = c(50, 100)) +
    scale_y_continuous(limits = c(30, 100)) +
    labs(x = "Tier",
         y = "Percent Correct",
         title = "Calibration of Weekly Predictions")

}

#' @export
plot_projection_eval <- function(projection_eval, n_teams = 10) {

  benchmark <- n_teams^2 - n_teams

  projection_eval %>%
    group_by(week) %>%
    summarize(correct = mean(correct),
              .groups = 'drop') %>%
    mutate(delta = correct * 100 - 50,
           overall_delta = mean(correct) * 100,
           percent = scales::percent(correct, accuracy = 0.1),
           sign = case_when(
             delta > 0 ~ "positive",
             delta < 0 ~ "negative",
             TRUE ~ "equal"
           )) %>%
    ggplot(aes(week, delta, fill = sign, label = percent)) +
    geom_bar(stat = 'identity') +
    geom_text(size = 3, alpha = 0.7, vjust = "outward") +
    scale_x_continuous(breaks = 1:max(projection_eval$week)) +
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
    labs(title = "Weekly Evaluation of League Projections",
         subtitle = paste("Overall accuracy of all possible matchups:",
                          format_pct(mean(projection_eval$correct), accuracy = 0.1)),
         x = "Week",
         y = "Percent Correct") +
    theme(panel.background= element_blank(),
          panel.border = element_blank()) +
    guides(fill = "none")
}

# Helper Functions --------------------------------------------------------

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

# Experimental ------------------------------------------------------------


# evaluate_lineup() %>%
#   ggplot(aes(y = reorder(team, actual))) +
#   geom_point(aes(x = projected), color = 'red') +
#   ggalt::geom_dumbbell(aes(x = max, xend = actual), colour_x = 'red', size_x = 3, size_xend = 3) +
#   theme_fvoa()

#' @export
plot_pos_contribution <- function(teams,
                                  team = NULL,
                                  season = F,
                                  group = c("team", "Position")) {

  if(season) {

    if(group[1] == "Position") {

      teams %>%
        filter(!roster %in% c("BN", "IR", "BE"), week < 16) %>%
        mutate(Position = case_when(
          position %in% c("CB", "S", "DB") ~ "DB",
          position %in% c("DE", "DT", "DL", "LB") ~ "DL",
          TRUE ~ position) %>%
            fct_relevel("QB", "RB", "WR", "TE",
                        "DST", "K", "DB", "DL")) %>%
        group_by(team, Position) %>%
        summarize(Points = sum(points)) %>%
        mutate(Pct = Points / sum(Points)) %>%
        ungroup() %>%
        ggplot(aes(team, Pct, fill = Pct)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
        scale_fill_viridis_c(guide = F) +
        facet_wrap( ~ Position, scales = "free_x") +
        theme_fvoa()

    } else {

      teams %>%
        filter(!roster %in% c("BN", "IR", "BE"),
               week < 16) %>%
        mutate(Position = case_when(
          position %in% c("CB", "S", "DB") ~ "DB",
          position %in% c("DE", "DT", "DL", "LB") ~ "DL",
          TRUE ~ position) %>%
            fct_relevel("QB", "RB", "WR", "TE",
                        "DST", "K", "DB", "DL")) %>%
        group_by(team, Position) %>%
        summarize(Points = sum(points)) %>%
        mutate(Pct = Points / sum(Points)) %>%
        ungroup() %>%
        ggplot(aes(Position, Pct, fill = Pct)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
        scale_fill_viridis_c(guide = F) +
        facet_wrap( ~ team, scales = "free_x") +
        theme_fvoa()

    }


  } else {

    teams %>%
      dplyr::filter(!Lineup %in% c("BN", "IR"), week < 16, team == team) %>%
      mutate(Position = case_when(
        Position %in% c("CB", "S", "DB") ~ "DB",
        Position %in% c("DE", "DT", "DL", "LB") ~ "DL",
        TRUE ~ Position) %>%
          fct_relevel("QB", "RB", "WR", "TE",
                      "DEF", "K", "DB", "DL")) %>%
      group_by(week, team, Position) %>%
      summarize(Points = sum(Points)) %>%
      mutate(Pct = Points / sum(Points)) %>%
      ungroup() %>%
      ggplot(aes(Position, Pct, fill = Pct)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      scale_fill_viridis_c(guide = F) +
      facet_wrap( ~ week, scales = "free_x") +
      theme_fvoa()

  }

}

plot_shrinkage <- function(fit) {

  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop("Package \"ggtext\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  scores <- as_tibble(fit$data)

  team_avg <- scores %>%
    group_by(team) %>%
    summarize(avg = mean(score), .groups = "drop")

  tidybayes::add_epred_draws(distinct(scores, team), fit) %>%
    tidybayes::median_hdi() %>%
    left_join(team_avg, by = "team") %>%
    ggplot(aes(y = reorder(team, avg))) +
    geom_point(aes(x = avg), color = "#0072B2") +
    geom_point(aes(x = .epred), color = "#009E73") +
    geom_vline(xintercept = 115, linetype = 2) +
    labs(y = NULL,
         x = "Score",
         title = "Visualize shrinkage from <span style='color:#0072B2;'>actual</span> and <span style='color:#009E73;'>fitted</span> scores") +
    theme_fvoa() +
    theme(plot.title = ggtext::element_markdown())

}

plot_opponent_fvoa <- function(model) {

  tmp <- as_tibble(model$data) %>%
    distinct(opponent, position) %>%
    mutate(home = TRUE,
           team = "A",
           player = "A",
           mflID = "A") %>%
    add_epred_draws(model, value = "points", seed = 42) %>%
    median_hdi(.width = c(0.5, 0.89)) %>%
    mutate(opponent = reorder_within(opponent, points, position, fun = median))

  tmp %>%
    ggplot(aes(y = opponent,
               yend = opponent)) +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.89),
                 size = 0.5, color = "#6497b1") +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.5),
                 size = 2, color = "#03396c") +
    geom_point(aes(x = points),
               size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
    scale_y_reordered() +
    # geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    labs(x = "FVOA", y = NULL) +
    facet_wrap(~ position, scales = "free") +
    theme_fvoa() +
    theme(axis.text.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          panel.grid.major.y = element_blank())

}

plot_player_fvoa <- function(model, top = 40) {

  tmp <- as_tibble(model$data) %>%
    distinct(mflID, name, team, position) %>%
    mutate(opponent = "A", home = TRUE) %>%
    add_epred_draws(model, value = "points") %>%
    median_hdi(.width = c(0.5, 0.89)) %>%
    top_n_group(top, points, position) #%>%
  # left_join(distinct(weekly_player_data, name, mflID), by = "mflID")

  tmp %>%
    ggplot(aes(y = reorder(name, points),
               yend = reorder(name, points))) +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.89),
                 size = 0.5, color = "#6497b1") +
    geom_segment(aes(x = .lower, xend = .upper),
                 data = filter(tmp, .width == 0.5),
                 size = 2, color = "#03396c") +
    geom_point(aes(x = points),
               size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
    labs(x = "FVOA", y = NULL) +
    facet_wrap(~ position, scales = "free") +
    theme_fvoa() +
    theme(axis.text.y = element_text(face = "bold"),
          axis.title.x = element_text(face = "bold"),
          panel.grid.major.y = element_blank())

}
