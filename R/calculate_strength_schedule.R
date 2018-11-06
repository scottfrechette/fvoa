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
