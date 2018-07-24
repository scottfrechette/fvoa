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
