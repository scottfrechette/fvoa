calculate_stats <- function(schedule, scores, league = "Yahoo") {

  if("Type" %in% names(scores)) {
    scores <- scores %>%
      filter(Type == "act")
  }

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule %>%
    left_join(scores %>%
                rename(Team1 = Team,
                       score1 = Score),
              by = c("Week", "Team1")) %>%
    left_join(scores %>%
                rename(Team2 = Team,
                       score2 = Score),
              by = c("Week", "Team2")) %>%
    drop_na() %>%
    gather(x, Team, Team1:Team2) %>%
    mutate(score = if_else(x == "Team1", score1, score2),
           Tie = if_else(score1 == score2, 1, 0),
           Win = if_else(x == "Team1" & score1 - score2 > 0, 1,
                         if_else(x == "Team2" & score2 - score1 > 0, 1, 0)),
           Lose = if_else(Tie == 0 & Win == 0, 1, 0)) %>%
    group_by(Team) %>%
    summarise(Score = sum(score),
              Wins = sum(Win),
              Losses = sum(Lose),
              Tie = sum(Tie)) %>%
    mutate(Percent = round(Wins/(Wins + Losses + Tie), 3),
           `Yahoo Rank` = min_rank(-Percent),
           Percent = format_pct(Percent)) %>%
    arrange(-Wins, -Score) %>%
    unite(Record, c(Wins, Losses, Tie), sep = "-")
}
