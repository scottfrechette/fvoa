
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
