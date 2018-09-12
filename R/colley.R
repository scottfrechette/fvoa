calculate_colley <- function(schedule, scores) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule_tmp <- schedule %>%
    mutate_if(is.factor, as.character)
  schedule_rev <- schedule_tmp %>%
    select(Week, Game_id, Team1 = Team2, Team2 = Team1)
  schedule <- bind_rows(schedule_tmp, schedule_rev) %>%
    arrange(Week, Team1)

  colley_df <- schedule %>%
    left_join(scores, by = c("Week", "Team1" = "Team")) %>%
    left_join(scores, by = c("Week", "Team2" = "Team")) %>%
    mutate(x = if_else(Score.x > Score.y, "W",
                       if_else(Score.y > Score.x, "L", "")) )%>%
    group_by(Team1, Team2) %>%
    summarise(x = paste(x, collapse = "")) %>%
    ungroup() %>%
    spread(Team2, x) %>%
    mutate_if(is.character, replace_na, "X") %>%
    unite(outcomes, -Team1, sep = "_") %>%
    mutate(W = str_count(outcomes, "W"),
           L = str_count(outcomes, "L"),
           t = W + L,
           C = t + 2,
           b = 1 + (W - L) / 2)

  colley_transform <- function(x) {
    if(x == 0) {
      0
    } else if(x == "X") {
      3
    } else {
      -nchar(x)
    }
  }

  colley_invmat <- colley_df %>%
    select(Team1, outcomes) %>%
    separate(outcomes, colley_df$Team1) %>%
    gather(Team, value, -Team1) %>%
    mutate(x = map_dbl(value, colley_transform)) %>%
    select(-value) %>%
    spread(Team, x) %>%
    select(-Team1) %>%
    solve(tol = 1e-20)

  colley_df %>%
    mutate(Rating = colley_invmat %*% colley_df$b %>%
             as.numeric() %>%
             round(4),
           Rank = min_rank(-Rating)) %>%
    select(Team = Team1, Rating, `Colley Rank` = Rank) %>%
    arrange(`Colley Rank`)
}
