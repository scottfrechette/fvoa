calculate_strength_schedule <- function(schedule, scores) {

  if("Team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule <- schedule %>%
    mutate_if(is.factor, as.character)
  schedule_rev <- schedule %>%
    select(Week, Game_id, Team1 = Team2, Team2 = Team1)
  schedule_both <- bind_rows(schedule, schedule_rev)

  calculate_fvoa(scores) %>%
    select(Team2 = Team, FVOA) %>%
    right_join(schedule_both, by = "Team2") %>%
    rename(Team = Team1) %>%
    group_by(Team) %>%
    summarise(SoS = round(mean(FVOA), 2)) %>%
    arrange(SoS) %>%
    mutate("SoS Rank" = 1:nrow(.))

}
