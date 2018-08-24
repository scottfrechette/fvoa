calculate_fvoa <- function(scores) {

  all_matchups(df, type = "prob") %>%
    select(-Team) %>%
    map_df(function(x) {round((mean(100 - x, na.rm = T) - 50) / 0.5, 2)}) %>%
    gather(Team, FVOA) %>%
    arrange(-FVOA) %>%
    mutate(`FVOA Rank` = dense_rank(-FVOA),
           Week = max(df$Week))

}

calculate_fvoa_season <- function(scores) {

  fvoa <- list()

  for (i in 1:n_distinct(df$Week)) {

    df_tmp <- df %>%
      filter(Week %in% 1:i)
    fvoa_rankings <- calculate_fvoa(df_tmp)
    fvoa[i] <- list(fvoa_rankings)

  }

  data_frame(fvoa) %>%
    unnest()

}
