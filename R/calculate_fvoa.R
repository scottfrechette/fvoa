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
