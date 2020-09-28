theme_fvoa <- function(base_size = 12, base_family = "Helvetica") {
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour="grey50"),
        strip.background = element_rect(color = "black"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", size = 0.2),
        strip.text = element_text(size =12))
}

# Plot functions

weekly_plots <- function(scores, x = week, y = score, group = team) {

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
    guides(color=FALSE) +
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    theme_fvoa()
}

boxplots <- function(scores, score = score, team = team) {
  ggplot(scores, aes(x=reorder(team, -score, fun=mean), y=score, fill=team)) +
    geom_boxplot(coef = 1.25, outlier.alpha = 0.6) +
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend=FALSE) +
    guides(fill=F) +
    labs(y = "Score", x = "", title = "Team Boxplots") +
    theme_fvoa() +
    theme(panel.border = element_blank())
}

joy_plots <- function(scores, score = score, team = team) {

  requireNamespace("ggridges", quietly = TRUE)

  ggplot(scores, aes(x = score, y = reorder(team, score, FUN = mean), fill = team)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept = mean(score)), alpha = 0.5) +
    labs(x = "Distribution of Scores", y = "", title = "Team Density Plots") +
    guides(fill=FALSE) +
    theme_fvoa()
}

plot_matchups <- function(all_matchups_df) {

  matchup_df <- all_matchups_df %>%
    rename(winner = team) %>%
    gather(loser, score, -winner) %>%
    mutate(winner = as_factor(winner) %>%
             fct_rev(),
           loser = as_factor(loser))

  matchup_df %>%
    ggplot(aes(reorder(winner, -score, FUN = mean), score)) +
    geom_point(aes(color=loser)) +
    geom_hline(yintercept=50, color="darkgrey") +
    labs(x = "", y="% Chance to Win", color="") +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 1))
}

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
    labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
    guides(color=FALSE) +
    theme_fvoa()
}

matchups_hm <- function(all_matchups_df) {

  hm_df <- all_matchups_df %>%
    rename(winner = team) %>%
    gather(loser, score, -winner) %>%
    mutate(winner = as_factor(winner) %>%
             fct_rev(),
           loser = as_factor(loser))

  hm_df %>%
    ggplot(aes(loser, winner, fill = score)) +
    geom_tile() +
    scale_fill_distiller(palette = "Spectral", direction=1) +
    #scale_fill_gradient2(low=muted("red"), mid="white", high=muted("green"), midpoint=50)+
    #scale_fill_viridis() +
    theme(panel.background=element_rect(fill="white", colour="white")) +
    labs(x = "Team 2", y="Team 1", fill="% Chance", title="Chance Team 1 Beats Team 2")

}

team_evaluation <- function(df) {

  if(!"Proj" %in% names(df)) {
    stop("Dataframe must include projected scores")
  }

  df %>%
    group_by(team) %>%
    mutate(margin = score - Proj,
           sign = margin >= 0,
           avg = mean(margin),
           pos_count = sum(sign)) %>%
    ggplot(aes(x = week, y = margin, fill = sign)) +
    geom_bar(stat = "identity") +
    facet_wrap(~reorder(team, - pos_count), ncol = n_distinct(df$team)/2) +
    guides(fill=FALSE) +
    labs(title = "Weekly Projection v Actual Results", y = "Margin") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())
}
playoff_leverage_plot <- function(scores, schedule, playoff_leverage_df) {

  sims <- max(playoff_leverage_df$sim)

  if("team" %in% names(schedule)) {
    schedule <- schedule %>%
      spread_schedule() %>%
      doublewide_schedule()
  }

  schedule %>%
    filter(week == max(scores$week) + 1) %>%
    mutate(tmp = list(playoff_leverage_df)) %>%
    unnest(tmp) %>%
    mutate(sim_wins = Wins,
           wins_with_loss = pl_wins,
           wins_with_win = pl_wins + 1L,
           wins = case_when(
             team == team1 ~ wins_with_win,
             team == team2 ~ wins_with_loss,
             TRUE ~ sim_wins
           )) %>%
    select(week:team, wins) %>%
    arrange(-wins) %>%
    group_by(team1, sim) %>%
    mutate(row = row_number(),
           playoff = if_else(row <= 4, 1, 0)) %>%
    ungroup() %>%
    group_by(team1, team) %>%
    summarise(Percent = sum(playoff/sims * 100)) %>%
    arrange(team1, -Percent) %>%
    rename(Winner = team1) %>%
    nest(team:Percent) %>%
    left_join(schedule %>%
                filter(week == max(scores$week) + 1) %>%
                select(-week) %>%
                rename(Winner = team1, Loser = team2), by = "Winner") %>%
    select(Winner, Loser, data) %>%
    unnest() %>%
    filter(Winner == team | Loser == team) %>%
    arrange(team) %>%
    mutate(style = if_else(Winner == team, "Win", "Lose")) %>%
    select(team:style) %>%
    spread(style, Percent) %>%
    mutate(delta = round(Win - Lose, 1), Total = 100) %>%
    ggplot(aes(reorder(team, Win), y = Total)) +
    geom_bar(stat = "identity", fill = "white", color = "grey", alpha = 0.4) +
    geom_bar(stat = "identity", aes(y = Win, fill = team), alpha = 0.5) +
    geom_bar(stat = "identity", aes(y = Lose, fill = team)) +
    geom_text(aes(y = Total + 0.5, label = paste0(delta, "%")), color = "grey30", hjust = 0) +
    # geom_text(aes(label = paste0(delta, "%"), group = team), color = "grey30", nudge_y = 5) +
    scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75, 100)) +
    guides(fill = FALSE) +
    labs(x = "",
         y = "Chance to Make Playoffs",
         title = str_glue("Playoff Probability Leverage (week {max(scores$week) + 1})")) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(color = "black"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          strip.text = element_text(size =12))
}

simulation_plot <- function(simulated_season_df, plot = c(Wins, points, Percent)) {
  plots <- tibble(Wins = "Projected Wins by Week",
                      points = "Projected Total Points by Week",
                      Percent = "Projected Chance of Making Playoffs by Week")

  plot_quo <- enquo(plot)

  simulated_season_df %>%
    ggplot(aes(week, !!plot_quo, color = team)) +
    geom_line(size=1.5) +
    geom_point(size = 2) +
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    facet_wrap(~reorder(team, rank, FUN = last), ncol = 5) +
    labs(y = "Wins", x = "Week",
         title = plots %>% pull(!!plot_quo)) +
    guides(color=FALSE) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    theme_fvoa()
}

plot_quadrant <- function(quadrants) {

  quadrants %>%
    ggplot(aes(delta, win_pct)) +
    geom_point() +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = 0) +
    annotate("text",
             x = max(df$delta) / 2,
             y = (max(df$win_pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Good",
             color = "grey65") +
    annotate("text",
             x = max(df$delta) / 2,
             y = (min(df$win_pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Unlucky",
             color = "grey65") +
    annotate("text",
             x = min(df$delta) / 2,
             y = (max(df$win_pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Lucky",
             color = "grey65") +
    annotate("text",
             x = min(df$delta) / 2,
             y = (min(df$win_pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Bad",
             color = "grey65") +
    ggrepel::geom_text_repel(aes(label = team)) +
    scale_y_continuous(labels = scales::percent) +
    tidyquant::theme_tq() +
    labs(y = "Win Percentage",
         x = "Point Differential") +
    tidyquant::scale_color_tq() +
    guides(color = F)

}

plot_sim_matchup <- function(sim_scores, team1, team2,
                             week, square = FALSE) {

  sim_scores_subset <- sim_scores %>%
    filter(team %in% c(team1, team2),
           week == week)

  lo <- min(sim_scores_subset$score) - 10
  hi <- max(sim_scores_subset$score) + 10

  sim_scores_final <- sim_scores_subset %>%
    spread(team, score) %>%
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

# add win bins?
plot_exp_wpct <- function(scores, schedule) {

  # wins or win_pct?
  # pythagorean or other?

  schedule %>%
    filter(Week < 2) %>%
    spread_schedule() %>%
    doublewide_schedule() %>%
    mutate_at(vars(Team1, Team2), as.character) %>%
    inner_join(scores %>%
                 rename(t1_points = Score),
               by = c("Week", "Team1" = "Team")) %>%
    inner_join(scores %>%
                 rename(t2_points = Score),
               by = c("Week", "Team2" = "Team")) %>%
    group_by(Team = Team1) %>%
    summarize(PF = sum(t1_points),
              PA = sum(t2_points),
              wins = sum(t1_points > t2_points),
              games = n()) %>%
    mutate(exp_wpct_pyth = (PF ^ 2.37 / (PF ^ 2.37 + PA ^ 2.37)),
           exp_wins = (PF ^ 2.37 / (PF ^ 2.37 + PA ^ 2.37) * games),
           exp_wpct = 1 / (1 + (PA / PF) ^2),
           wpct = wins / games) %>%
    ggplot(aes(wpct, exp_wpct, label = Team)) +
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
         title = "Real and Expected Win Percentage for each Team") +
    ggthemes::theme_tufte()

}

plot_pos_contribution <- function(teams, team = NULL, season = F, group = c("Team", "Position")) {

  if(season) {

    if(group[1] == "Position") {

      teams %>%
        filter(Lineup != "BN", Week < 16) %>%
        mutate(Position = case_when(
          Position %in% c("CB", "S", "DB") ~ "DB",
          Position %in% c("DE", "DT", "DL", "LB") ~ "DL",
          TRUE ~ Position) %>%
            fct_relevel("QB", "RB", "WR", "TE",
                        "DEF", "K", "DB", "DL")) %>%
        group_by(Team, Position) %>%
        summarize(Points = sum(Points)) %>%
        mutate(Pct = Points / sum(Points)) %>%
        ungroup() %>%
        ggplot(aes(Team, Pct, fill = Pct)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
        scale_fill_viridis_c(guide = F) +
        facet_wrap( ~ Position, scales = "free_x") +
        theme_fvoa()

    } else {

      teams %>%
        filter(Lineup != "BN", Week < 16) %>%
        mutate(Position = case_when(
          Position %in% c("CB", "S", "DB") ~ "DB",
          Position %in% c("DE", "DT", "DL", "LB") ~ "DL",
          TRUE ~ Position) %>%
            fct_relevel("QB", "RB", "WR", "TE",
                        "DEF", "K", "DB", "DL")) %>%
        group_by(Team, Position) %>%
        summarize(Points = sum(Points)) %>%
        mutate(Pct = Points / sum(Points)) %>%
        ungroup() %>%
        ggplot(aes(Position, Pct, fill = Pct)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
        scale_fill_viridis_c(guide = F) +
        facet_wrap( ~ Team, scales = "free_x") +
        theme_fvoa()

    }


  } else {

    teams %>%
      dplyr::filter(Lineup != "BN", Week < 16, Team == team) %>%
      mutate(Position = case_when(
        Position %in% c("CB", "S", "DB") ~ "DB",
        Position %in% c("DE", "DT", "DL", "LB") ~ "DL",
        TRUE ~ Position) %>%
          fct_relevel("QB", "RB", "WR", "TE",
                      "DEF", "K", "DB", "DL")) %>%
      group_by(Week, Team, Position) %>%
      summarize(Points = sum(Points)) %>%
      mutate(Pct = Points / sum(Points)) %>%
      ungroup() %>%
      ggplot(aes(Position, Pct, fill = Pct)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      scale_fill_viridis_c(guide = F) +
      facet_wrap( ~ Week, scales = "free_x") +
      theme_fvoa()

  }

}
