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

weekly_plots <- function(scores, x = Week, y = Score, group = Team) {

  x_quo <- enquo(x)
  y_quo <- enquo(y)
  group_quo <-enquo(group)


  ggplot(scores, aes(!!x_quo, !!y_quo, color = !!group_quo)) +
    geom_line(size = 1.5) +
    geom_point(size = 2) +
    facet_wrap(~reorder(Team, -Score, FUN = mean), ncol=n_distinct(scores$Team)/2) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    labs(y = quo_name(y_quo), x = quo_name(x_quo), title = "Weekly Scores") +
    guides(color=FALSE) +
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    theme_fvoa()
}
boxplots <- function(scores, Score = Score, Team = Team) {
  ggplot(scores, aes(x=reorder(Team, -Score, fun=mean), y=Score, fill=Team)) +
    geom_boxplot(coef = 1.25, outlier.alpha = 0.6) +
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend=FALSE) +
    guides(fill=F) +
    labs(y = "Score", x = "", title = "Team Boxplots") +
    theme_fvoa() +
    theme(panel.border = element_blank())
}
joy_plots <- function(scores, Score = Score, Team = Team) {

  requireNamespace("ggridges", quietly = TRUE)

  ggplot(scores, aes(x = Score, y = reorder(Team, Score, FUN = mean), fill = Team)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept = mean(Score)), alpha = 0.5) +
    labs(x = "Distribution of Scores", y = "", title = "Team Density Plots") +
    guides(fill=FALSE) +
    theme_fvoa()
}
plot_matchups <- function(all_matchups_df) {

  matchup_df <- all_matchups_df %>%
    rename(winner = Team) %>%
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
plot_fvoa <- function(fvoa_df, x = Week, y = FVOA, group = Team) {

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
    rename(winner = Team) %>%
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
    group_by(Team) %>%
    mutate(margin = Score - Proj,
           sign = margin >= 0,
           avg = mean(margin),
           pos_count = sum(sign)) %>%
    ggplot(aes(x = Week, y = margin, fill = sign)) +
    geom_bar(stat = "identity") +
    facet_wrap(~reorder(Team, - pos_count), ncol = n_distinct(df$Team)/2) +
    guides(fill=FALSE) +
    labs(title = "Weekly Projection v Actual Results", y = "Margin") +
    theme_fvoa() +
    theme(panel.grid.major.y = element_blank())
}
playoff_leverage_plot <- function(scores, schedule, playoff_leverage_df) {

  sims <- max(playoff_leverage_df$sim)

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }
  schedule_tmp <- schedule %>%
    mutate_if(is.factor, as.character)
  schedule_rev <- schedule_tmp %>%
    select(Week, Game_id, Team1 = Team2, Team2 = Team1)
  schedule <- bind_rows(schedule_tmp, schedule_rev) %>%
    arrange(Week, Team1)

  schedule %>%
    filter(Week == max(scores$Week) + 1) %>%
    mutate(tmp = list(playoff_leverage_df)) %>%
    unnest(tmp) %>%
    mutate(sim_wins = Wins,
           wins_with_loss = pl_wins,
           wins_with_win = pl_wins + 1L,
           wins = case_when(
             Team == Team1 ~ wins_with_win,
             Team == Team2 ~ wins_with_loss,
             TRUE ~ sim_wins
           )) %>%
    select(Week:Team, wins) %>%
    arrange(-wins) %>%
    group_by(Team1, sim) %>%
    mutate(row = row_number(),
           playoff = if_else(row <= 4, 1, 0)) %>%
    ungroup() %>%
    group_by(Team1, Team) %>%
    summarise(Percent = sum(playoff/sims * 100)) %>%
    arrange(Team1, -Percent) %>%
    rename(Winner = Team1) %>%
    nest(Team:Percent) %>%
    left_join(schedule %>%
                filter(Week == max(scores$Week) + 1) %>%
                select(-Week) %>%
                rename(Winner = Team1, Loser = Team2), by = "Winner") %>%
    select(Winner, Loser, data) %>%
    unnest() %>%
    filter(Winner == Team | Loser == Team) %>%
    arrange(Team) %>%
    mutate(style = if_else(Winner == Team, "Win", "Lose")) %>%
    select(Team:style) %>%
    spread(style, Percent) %>%
    mutate(Delta = round(Win - Lose, 1), Total = 100) %>%
    ggplot(aes(reorder(Team, Win), y = Total)) +
    geom_bar(stat = "identity", fill = "white", color = "grey", alpha = 0.4) +
    geom_bar(stat = "identity", aes(y = Win, fill = Team), alpha = 0.5) +
    geom_bar(stat = "identity", aes(y = Lose, fill = Team)) +
    geom_text(aes(y = Total + 0.5, label = paste0(Delta, "%")), color = "grey30", hjust = 0) +
    # geom_text(aes(label = paste0(Delta, "%"), group = Team), color = "grey30", nudge_y = 5) +
    scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75, 100)) +
    guides(fill = FALSE) +
    labs(x = "",
         y = "Chance to Make Playoffs",
         title = str_glue("Playoff Probability Leverage (Week {max(scores$Week) + 1})")) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.background = element_rect(color = "black"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(color = "grey90", size = 0.2),
          strip.text = element_text(size =12))
}
simulation_plot <- function(simulated_season_df, plot = c(Wins, Points, Percent)) {
  plots <- data_frame(Wins = "Projected Wins by Week",
                      Points = "Projected Total Points by Week",
                      Percent = "Projected Chance of Making Playoffs by Week")

  plot_quo <- enquo(plot)

  simulated_season_df %>%
    ggplot(aes(Week, !!plot_quo, color = Team)) +
    geom_line(size=1.5) +
    geom_point(size = 2) +
    stat_smooth(se=FALSE, method="lm", linetype = 2, size=0.5, color="grey") +
    facet_wrap(~reorder(Team, Rank, FUN = last), ncol = 5) +
    labs(y = "Wins", x = "Week",
         title = plots %>% pull(!!plot_quo)) +
    guides(color=FALSE) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
    theme_fvoa()
}
plot_quadrant <- function(league, league_id, id = NULL) {

  if(league == "yahoo") {

    url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id)

    page <- xml2::read_html(url)

    df <- page %>%
      html_nodes("table") %>%
      .[[2]] %>%
      html_table() %>%
      as_tibble() %>%
      separate(3, into = c("W", "L", "T"), convert = T) %>%
      mutate(Delta = `Pts For` - `Pts Agnst`,
             Pct = W / (W + L + T)) %>%
      select(Team, Delta, Pct)

  } else {

    url <- paste0("http://games.espn.com/ffl/standings?leagueId=", league_id)

    page <- xml2::read_html(url)

    record <- page %>%
      html_nodes("table") %>%
      .[[3]] %>%
      html_table(header = T)

    colnames(record) <- as.character(unlist(record[1,]))

    record <- record %>%
      slice(-1) %>%
      as_tibble() %>%
      mutate_at(vars(W, L, T, PCT, GB), as.numeric)

    points <- page %>%
      html_nodes("table") %>%
      .[[4]] %>%
      html_table(header = T)

    colnames(points) <- as.character(unlist(points[1,]))

    points <- points %>%
      slice(-1) %>%
      as_tibble() %>%
      rename(Team = `TEAM, OWNER(S)`) %>%
      mutate_at(vars(PF, PA), as.numeric) %>%
      mutate(Team = str_extract(Team, "^[^\\(]+") %>% str_trim())

    df <- points %>%
      mutate(Delta = PF - PA) %>%
      left_join(record, by = c("Team" = "TEAM")) %>%
      select(Team, Delta, Pct = PCT)

  }

  if(!is.null(id)) {

    df <- df %>%
      left_join(id, by = "Team") %>%
      select(Team = team, Delta, Pct)

  }

  df %>%
    ggplot(aes(Delta, Pct)) +
    geom_point() +
    geom_hline(yintercept = 0.5) +
    geom_vline(xintercept = 0) +
    annotate("text",
             x = max(df$Delta) / 2,
             y = (max(df$Pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Good",
             color = "grey65") +
    annotate("text",
             x = max(df$Delta) / 2,
             y = (min(df$Pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Unlucky",
             color = "grey65") +
    annotate("text",
             x = min(df$Delta) / 2,
             y = (max(df$Pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Lucky",
             color = "grey65") +
    annotate("text",
             x = min(df$Delta) / 2,
             y = (min(df$Pct) - 0.5) / 2 + 0.5,
             size = 8,
             label = "Bad",
             color = "grey65") +
    ggrepel::geom_text_repel(aes(label = Team)) +
    scale_y_continuous(labels = scales::percent) +
    tidyquant::theme_tq() +
    labs(y = "Win Percentage",
         x = "Point Differential") +
    tidyquant::scale_color_tq() +
    guides(color = F)

}
