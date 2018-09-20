

# Rank position scores, assign starters, remove WR/RB starters, determine top flex,
# margin of best and started, rank each team on management, chart each week

# lineup_df columns: Team, Score, Player, Position, Lineup, Proj, Points

max_points_position <- function(lineup_df, team, week,
                                position, roster_spots) {

  lineup_df %>%
    filter(Team %in% c(team, NA),
           Week == week,
           Position == position) %>%
    top_n(roster_spots, Points) %>%
    mutate(max_points = sum(Points)) %>%
    select(max_points, Player)

}

max_points_flex <- function(best_lineup, lineup_df, team, week,
                            rb_wr = 1, flex = 1) {

  best_lineup <- best_lineup %>%
    filter(Team == team,
           Week == week)

  lineup_df <- lineup_df %>%
    filter(Team %in% c(team, NA),
           Week == week)

  if(rb_wr > 0) {

    chosen_players <- best_lineup %>%
      unnest(Player) %>%
      select(Player)

    best_rb_wr <- lineup_df %>%
      anti_join(chosen_players, by = "Player") %>%
      filter(Position %in% c("RB", "WR")) %>%
      top_n(rb_wr, Points) %>%
      mutate(max_points = sum(Points),
             position = "RB_WR") %>%
      nest(Player, .key = "Player") %>%
      select(Team, Week, position, max_points, Player)

    best_lineup <- bind_rows(best_lineup,
                             best_rb_wr)


  }

  if(flex > 0) {

    chosen_players <- best_lineup %>%
      unnest(Player) %>%
      select(Player)

    best_flex <- lineup_df %>%
      anti_join(chosen_players, by = "Player") %>%
      filter(Position %in% c("RB", "WR", "TE")) %>%
      top_n(flex, Points) %>%
      mutate(max_points = sum(Points),
             position = "Flex") %>%
      nest(Player, .key = "Player") %>%
      select(Team, Week, position, max_points, Player)

    best_lineup <- bind_rows(best_lineup,
                             best_flex)

  }

  best_lineup %>%
    select(-Player) %>%
    summarise(max_points = sum(max_points))


}

evaluate_lineup <- function(lineup_df, qb = 1, rb = 2,
                            wr = 3, te = 1, dst = 1,
                            k = 1, flex = 1, rb_wr = 1,
                            dl = 1, db = 1, fa = NULL,
                            transactions = NULL, plot = FALSE) {

  lineup_df <- lineup_df %>%
    mutate(Position = case_when(
      Position %in% c("DB", "CB", "S") ~ "DB",
      Position %in% c("DL", "DE", "DT", "LB") ~ "DL",
      Position %in% c("DST", "D/ST", "DEF") ~ "DST",
      TRUE ~ Position
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
    filter(!is.na(Team)) %>%
    distinct(Team)
  weeks <- lineup_df %>% distinct(Week)
  positions <- data_frame(qb, rb, wr, te,
                          dst, k, dl, db) %>%
    gather(position, roster_spots) %>%
    filter(roster_spots > 0) %>%
    mutate(position = toupper(position))

  best_lineup <- crossing(teams, weeks, positions) %>%
    mutate(data = list(lineup_df),
           tmp = pmap(list(data, Team, Week,
                           position, roster_spots),
                      max_points_position)) %>%
    unnest(tmp) %>%
    select(Team, Week, position, max_points, Player) %>%
    nest(Player, .key = "Player")

  best_lineup <- crossing(teams, weeks) %>%
    mutate(best_lineup = list(best_lineup),
           lineup_df = list(lineup_df),
           rb_wr = rb_wr,
           flex = flex,
           tmp = pmap(list(best_lineup, lineup_df,
                           Team, Week, rb_wr, flex),
                      max_points_flex)) %>%
    unnest(tmp) %>%
    select(Team, Week, Max = max_points) %>%
    left_join(lineup_df %>%
                select(Team, Week, Score) %>%
                distinct(),
              by = c("Team", "Week"))

  best_lineup_final <- best_lineup %>%
    mutate(Delta = Max - Score,
           sign = if_else(Delta <= 0, "positive", "negative"),
           avg = mean(Delta))

  # best_lineup_final

  if(plot) {
    best_lineup_final %>%
      ggplot(aes(Week, Delta, fill = Delta)) +
      geom_bar(stat = 'identity', color = "black") +
      scale_x_continuous(breaks = 1:max(lineup_df$Week),
                         labels = paste("Week", 1:max(lineup_df$Week)),
                         trans = "reverse") +
      facet_wrap(~reorder(Team, avg), ncol = n_distinct(lineup_df$Team)/2) +
      guides(fill=FALSE) +
      labs(title = "Weekly Manager Evaluation",
           subtitle = "How many points you left on your bench each week",
           x = NULL, y = "Lost Points") +
      theme_fvoa() +
      theme(panel.grid.major.y = element_blank()) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      coord_flip()
  } else return(best_lineup_final)
}
