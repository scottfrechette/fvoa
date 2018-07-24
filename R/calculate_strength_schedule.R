calculate_strength_schedule <- function(schedule, scores) {

  calculate_fvoa(scores) %>%
    select(Team2 = Team, FVOA) %>%
    right_join(schedule, by = "Team2") %>%
    rename(Team = Team1) %>%
    group_by(Team) %>%
    summarise(SoS = round(mean(FVOA), 2)) %>%
    arrange(SoS) %>%
    mutate("SoS Rank" = 1:nrow(.))

}
