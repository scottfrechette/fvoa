calculate_rankings <- function(schedule, scores) {
  calculate_stats(schedule, scores) %>%
    left_join(calculate_fvoa(scores) %>%
                select(-Week),
              by = "Team") %>%
    left_join(calculate_strength_schedule(schedule, scores),
              by = "Team") %>%
    left_join(calculate_colley(schedule, scores),
              by = "Team")
}

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
    summarise(Points = sum(score),
              Wins = sum(Win),
              Losses = sum(Lose),
              Tie = sum(Tie)) %>%
    mutate(Percent = round(Wins/(Wins + Losses + Tie), 3)) %>%
    arrange(-Wins, -Points) %>%
    mutate(`Yahoo Rank` = row_number(),
           Percent = format_pct(Percent)) %>%
    arrange(-Wins, -Points) %>%
    unite(Record, c(Wins, Losses, Tie), sep = "-")
}

calculate_fvoa <- function(scores) {

  all_matchups(scores, type = "prob") %>%
    select(-Team) %>%
    map_df(function(x) {round((mean(100 - x, na.rm = T) - 50) / 0.5, 2)}) %>%
    gather(Team, FVOA) %>%
    arrange(-FVOA) %>%
    mutate(`FVOA Rank` = dense_rank(-FVOA),
           Week = max(scores$Week))

}

calculate_fvoa_season <- function(scores) {

  fvoa <- list()

  for (i in 1:n_distinct(scores$Week)) {

    scores_tmp <- scores %>%
      filter(Week %in% 1:i)
    fvoa_rankings <- calculate_fvoa(scores_tmp)
    fvoa[i] <- list(fvoa_rankings)

  }

  data_frame(fvoa) %>%
    unnest()

}

calculate_strength_schedule <- function(schedule, scores) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

  calculate_fvoa(scores) %>%
    select(Team2 = Team, FVOA) %>%
    right_join(schedule, by = "Team2") %>%
    rename(Team = Team1) %>%
    group_by(Team) %>%
    summarise(SoS = round(mean(FVOA), 2)) %>%
    arrange(SoS) %>%
    mutate("SoS Rank" = min_rank(-SoS))

}

calculate_colley <- function(schedule, scores) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

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
    select(Team = Team1, `Colley Rating` = Rating, `Colley Rank` = Rank) %>%
    arrange(`Colley Rank`)
}

calculate_quadrants <- function(scores, schedule, start = 1, end = NULL) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule) %>%
      doublewide_schedule()
  }

  if(is.null(end)) {
    end = max(scores$Week)
  }

  scores <- scores %>%
    mutate_if(is.character, as.factor)

  schedule %>%
    filter(between(Week, start, end)) %>%
    left_join(scores %>% rename(Score1 = Score),
              by = c("Week", "Team1" = "Team")) %>%
    left_join(scores %>% rename(Score2 = Score),
              by = c("Week", "Team2" = "Team")) %>%
    drop_na() %>%
    mutate(Diff = Score1 - Score2) %>%
    select(Week, Team = Team1, Diff) %>%
    group_by(Team) %>%
    summarise(Delta = sum(Diff),
              Win_Pct = sum(Diff > 0) / length(start:end))

}
