#' @export
theme_fvoa <- function(base_size = 12, base_family = "Helvetica") {
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour="grey50"),
        strip.background = element_rect(color = "black"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        strip.text = element_text(size =12))
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
    geom_smooth(se=F, color = "darkgrey",
                # n = n_distinct(!!x_quo),
                linetype=2, formula = y ~ x, method = "loess") +
    geom_line(alpha = 0.5, size = 1.5) +
    geom_point() +
    # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    labs(y = "FVOA", x = "week", title = "weekly FVOA") +
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
plot_manager_evaluation <- function(team) {

  team %>%
    extract_projections() %>%
    mutate(margin = actual - projected,
           sign = margin >= 0,
           avg = mean(margin, na.rm = T),
           pos_count = sum(sign)) %>%
    ggplot(aes(x = week, y = margin, fill = sign)) +
    geom_bar(stat = "identity") +
    facet_wrap(~reorder(team, - pos_count), ncol = 5) +
    guides(fill = "none") +
    labs(title = "Weekly Projection v Actual Results", y = "Margin") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())
}

# Teams -------------------------------------------------------------------

#' @export
plot_boxplots <- function(scores, score = score, team = team) {
  ggplot(scores, aes(x=reorder(team, -score, fun=mean), y=score, fill=team)) +
    geom_boxplot(coef = 1.25, outlier.alpha = 0.6) +
    stat_summary(fun = mean, geom="point", shape=18, size=3, show.legend = FALSE) +
    guides(fill = "none") +
    labs(y = "score", x = "", title = "Team Boxplots") +
    theme_fvoa() +
    theme(panel.border = element_blank())
}

#' @export
plot_joy_plots <- function(scores, score = score, team = team) {

  requireNamespace("ggridges", quietly = TRUE)

  ggplot(scores, aes(x = score, y = reorder(team, score, FUN = mean), fill = team)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept = mean(score)), alpha = 0.5) +
    labs(x = "Distribution of scores", y = "", title = "Team Density Plots") +
    guides(fill = "none") +
    theme_fvoa()
}

#' @export
plot_h2h_matchup <- function(fit, team1, team2,
                             square = FALSE) {

  sim_scores_subset <- extract_draws(scores, fit) %>%
    ungroup() %>%
    select(sim = .draw, team, score = .prediction) %>%
    filter(team %in% c(team1, team2))

  lo <- min(sim_scores_subset$score) - 10
  hi <- max(sim_scores_subset$score) + 10

  sim_scores_final <- sim_scores_subset %>%
    spread(team, score) %>%
    select(sim, tm1 = !!team1, tm2 = !!team2) %>%
    mutate(winner = tm1 > tm2)

  wp <- sim_scores_final %>%
    summarize(team1 = sum(winner) / n()) %>%
    mutate(team2 = 1 - team1) %>%
    mutate_all(~ paste0(round(.x, 2) * 100, "%"))

  p <- sim_scores_final %>%
    ggplot(aes(tm1, tm2, color = winner)) +
    geom_point(alpha = 0.1) +
    geom_abline(color = "grey30", linetype = 2) +
    guides(color = "none") +
    labs(x = str_glue(team1, " Simulated Scores \n(Win Probability: {wp[[1]]})"),
         y = str_glue(team2, " Simulated Scores \n(Win Probability: {wp[[2]]})")) +
    # y = str_glue(team2, " ({wp[[2]]})")) +
    theme_fvoa()

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

  library(patchwork)

  leverage_week <- unique(sim_standings$leverage_week)

  leverage_plot <- sim_standings %>%
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

  # Core dataset with the basic labels
  label_df <- tibble(
    x = c(15, 15, 77, 101),
    y = c(1.6, 0.35, 0.35, 1),
    label = c("Chance to make playoffs with win ", "Chance to make playoffs with loss ", "Leverage", "X%")
  )


  # the horizontal lines
  seg_df <- tibble(
    x1 = c(0.2, 90, 0.2, 74.8, 75.3, 90, 103),
    x2 = c(0.2, 90, 0.2, 74.8, 75.3, 90, 103),
    y1 = c(1.3, 1.3, rep(.7, 5)),
    y2 = c(1.61, 1.61, rep(.343, 5))

  )

  # vertical lines
  seg2_df <- tibble(
    x1 = c(0.2, 0.2, 75.3),
    x2 = c(90, 74.8, 103),
    y1 = c(1.6, .35, .35),
    y2 = c(1.6, .35, .35)
  )

  legend_plot <- tibble(
    x = 75,
    y = factor("Y"),
    x2 = 90
  ) %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(aes(x = 100), fill = "white", color = "grey", width = 0.4) +
    geom_col(aes(x = x2), width = 0.4, color = "#DC143C", fill = "grey") +
    geom_col(width = 0.4, color = "black", fill = "black") +
    geom_segment(
      data = seg_df,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = c(rep("black", 4), rep("#DC143C", 3)),
      size = 1
    ) +
    geom_segment(
      data = seg2_df,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = c("black", "black", "#DC143C"),
      size = 1
    ) +
    geom_label(
      data = label_df,
      aes(x = x, y = y, label = label),
      hjust = 0, size = 6, fontface = "bold", fill = "white",
      color = c("black", "black", "#DC143C", "#DC143C"),
      label.size = NA,
      # family = "Oswald",
      label.padding = unit(0.05, "lines"),
    ) +
    coord_cartesian(ylim = c(0.7, 1.2), xlim = c(0, 108)) +
    theme_void() +
    theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))

  leverage_plot / legend_plot + plot_layout(heights = c(5, 1))

}



#' @export
plot_quadrant <- function(quadrants, x = c("pf", "pa", "delta")) {

  x <- match.arg(x)

  x_label <- case_when(
    x == "pf" ~ "Points Scored",
    x == "pa" ~ "Points Against",
    x == "delta" ~ "Point Differential"
  )

  quadrants <- select(quadrants,
                      team = starts_with("team"), wp, x_axis = x)

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
             y = (max(quadrants$wp) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Good",
             color = "grey65") +
    annotate("text",
             x = (max(quadrants$x_axis) - x_intercept) / 2 + x_intercept,
             y = (min(quadrants$wp) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Unlucky",
             color = "grey65") +
    annotate("text",
             x = x_intercept - (x_intercept - min(quadrants$x_axis)) / 2,
             y = (max(quadrants$wp) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Lucky",
             color = "grey65") +
    annotate("text",
             x = x_intercept - (x_intercept - min(quadrants$x_axis)) / 2,
             y = (min(quadrants$wp) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Bad",
             color = "grey65") +
    ggrepel::geom_text_repel(aes(label = team)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1),
                       expand = c(0, 0)) +
    # tidyquant::theme_tq() +
    theme_fvoa() +
    labs(y = "Win Percentage",
         x = x_label) +
    # tidyquant::scale_color_tq() +
    guides(color = "none")

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
    ggthemes::theme_tufte()

}

#' @export
plot_wpag <- function(scores, schedule) {

  left_join(rename(scores, team1 = team, score1 = score),
            rename(scores, team2 = team, score2 = score),
            by = "week") %>%
    filter(team1 != team2) %>%
    left_join(mutate(schedule, scheduled = T),
              by = c("week", "team1", "team2")) %>%
    replace_na(list(scheduled = FALSE)) %>%
    mutate(win = score1 > score2,
           scheduled_win = scheduled & win) %>%
    group_by(team1) %>%
    summarize(scheduled = sum(scheduled),
              scheduled_wins = sum(scheduled_win),
              possible = n(),
              possible_wins = sum(win)) %>%
    mutate(scheduled_wp = scheduled_wins / scheduled,
           possible_wp = possible_wins / possible) %>%
    ggplot(aes(scheduled_wp, possible_wp, label = team1)) +
    ggrepel::geom_text_repel() +
    geom_point() +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = c(0, 0)) +
    geom_abline(linetype = 2) +
    annotate("text",
             x = 0.1,
             y = 0.9,
             size = 8,
             label = "Unlucky",
             color = "grey65") +
    annotate("text",
             x = 0.9,
             y = 0.2,
             size = 8,
             label = "Lucky",
             color = "grey65") +
    labs(x = "Win Percentage",
         y = "Win Percentage All Games",
         title = "Comparison of Win Percentage and Win Percentage of All Possible Games") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())

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
    labs(title = "Weekly Evaluation of Model",
         x = "Week (starting with week 2)",
         y = "Percent Correct") +
    theme(panel.background= element_blank(),
          panel.border = element_blank()) +
    guides(fill = "none")
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
              alpha = 0.7, vjust = "outward") +
    geom_line() +
    geom_abline(color = "red") +
    geom_abline(color = "red", intercept = 10) +
    scale_x_continuous(limits = c(50, 100)) +
    scale_y_continuous(limits = c(30, 100)) +
    labs(x = "Tier", y = "Percent Correct",
         title = "Calibration of Weekly Predictions")

}

# Experimental ------------------------------------------------------------



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

# Win/Loss Margin
# Average win/loss score
# Do teams play you harder?: opponents scores vs their average
# Team by team heatmap
