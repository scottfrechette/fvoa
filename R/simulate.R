
#' @export
simulate_season <- function(scores,
                            n_sims,
                            season_weeks = 15,
                            .fun = simulate_score,
                            ...,
                            .parallel = TRUE,
                            .progress = FALSE) {

  team_col <- names(select(scores, starts_with("team")))
  scores_tmp <- scores %>%
    extract_scores() %>%
    select(week, team = starts_with("team"), score)
  teams <- unique(scores_tmp$team)
  remaining_weeks <- (max(scores_tmp$week) + 1):season_weeks

  if(.parallel) {

    future::plan(multiprocess)
    options(future.rng.onMisuse = 'ignore')

    out <- tibble(sim = 1:n_sims) %>%
      mutate(data = furrr::future_map(sim,
                                      simulate_single_season,
                                      scores = scores_tmp,
                                      season_weeks = season_weeks,
                                      .fun = .fun,
                                      ...,
                                      .progress = .progress)) %>%
      unnest(data) %>%
      set_names("sim", "week", team_col, "score")

  } else {

    out <- tibble(sim = 1:n_sims) %>%
      mutate(data = map(sim,
                        simulate_single_season,
                        scores = scores_tmp,
                        season_weeks = season_weeks,
                        .fun = .fun,
                        ...)) %>%
      unnest(data) %>%
      set_names("sim", "week", team_col, "score")

  }

  if("player" %in% names(scores)) {

    out <- out %>%
      rename(sim_week = week) %>%
      mutate(league = scores$league[1],
             leagueID = scores$leagueID[1],
             season = scores$season[1],
             week = max(scores$week),
             .before = 1)

  }

  return(out)

}

#' @export
simulate_record <- function(sim_season, schedule, weeks_played) {

  team_col <- names(select(sim_season, starts_with("team")))

  if("league" %in% names(sim_season)) {

    sim_season_tmp <- select(sim_season, sim, week = sim_week,
                             team = starts_with("team"), score)

  } else {

    sim_season_tmp <- select(sim_season, sim, week,
                             team = starts_with("team"), score)

  }

  if("teamID" %in% names(schedule) | "team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule <-  doublewide_schedule(schedule)

  final_record <- schedule %>%
    mutate_if(is.factor, as.character) %>%
    inner_join(sim_season_tmp %>%
                 rename(team1 = team,
                        score1 = score),
               by = c("week", "team1")) %>%
    inner_join(sim_season_tmp %>%
                 rename(team2 = team,
                        score2 = score),
               by = c("week", "team2", "sim")) %>%
    mutate(diff = score1 - score2,
           win = diff > 0,
           pl_win = case_when(
             week == weeks_played + 1 ~ 0,
             diff > 0                 ~ 1,
             TRUE                     ~ 0),
           lose = diff < 0,
           tie = diff == 0) %>%
    group_by(team1, sim) %>%
    summarise(wins = sum(win),
              pl_wins = sum(pl_win),
              losses = sum(lose),
              tie = sum(tie),
              .groups = "drop") %>%
    rename(team = team1) %>%
    arrange(sim, -wins, -losses) %>%
    mutate_if(is.numeric, as.integer)

  out <- sim_season_tmp %>%
    group_by(team, sim) %>%
    summarise(points = sum(score)) %>%
    ungroup() %>%
    arrange(sim, -points) %>%
    right_join(final_record,
               by = c("sim", "team")) %>%
    arrange(sim, -wins, -points) %>%
    set_names(team_col, "sim", "points", "wins",
              "pl_wins", "losses", "tie")

  if("league" %in% names(sim_season)) {

    out <- out %>%
      mutate(league = sim_season$league[1],
             leagueID = sim_season$leagueID[1],
             season = sim_season$season[1],
             week = max(sim_season$week),
             .before = 1)


  }

  return(out)

}

#' @export
simulate_ranking <- function(sim_record, weeks_played) {

  team_col <- names(select(sim_record, starts_with("team")))
  sim_record_tmp <- select(sim_record, team = starts_with("team"), sim:tie)
  teams <- unique(sim_record_tmp$team)

  out <- sim_record_tmp %>%
    nest(data = -sim) %>%
    mutate(playoff = map(data,
                         ~ .x %>%
                           slice(1:4) %>%
                           pull(team))) %>%
    unnest(playoff) %>%
    unnest(data) %>%
    mutate(playoffs = if_else(team == playoff, 1, 0)) %>%
    group_by(team) %>%
    summarise(points = round(mean(points), 1),
              wins = round(mean(wins), 1),
              percent = sum(playoffs)/n_distinct(.$sim) * 100,
              .groups = "drop") %>%
    arrange(-percent, -wins, -points) %>%
    mutate(rank = 1L:length(teams),
           week = weeks_played) %>%
    set_names(team_col, "points", "wins",
              "percent", "rank", "week") %>%
    select(week, everything())

  if("league" %in% names(sim_record)) {

    out <- out %>%
      mutate(league = sim_record$league[1],
             leagueID = sim_record$leagueID[1],
             season = sim_record$season[1],
             week = max(sim_record$week),
             .before = 1)

  }

  return(out)

}


# Helper Functions --------------------------------------------------------

simulate_score <- function(scores,
                           .team,
                           .reps = 1,
                           .min_score = 50,
                           .reg_games = 6,
                           .reg_points = 110) {

  tmp <- scores %>%
    extract_scores() %>%
    select(week, team = starts_with("team"), score) %>%
    filter(team == .team)

  team_scores <- tmp$score

  weighting <- weight_games(team_scores, .reg_games)

  if (max(tmp$week) < .reg_games) {

    team_scores <- c(team_scores, .reg_points)

  }

  sim_scores <- rnorm(
    .reps,
    weighted.mean(team_scores, weighting),
    weighted_sd(team_scores, weighting, method="ML")
  )

  if(!is.null(.min_score)) {

    sim_scores <- if_else(sim_scores < .min_score, as.numeric(.min_score), sim_scores)

  }

  round(sim_scores, 2)

}

simulate_single_season <- function(scores,
                                   season_weeks = 15,
                                   sim = NULL,
                                   .fun = simulate_score,
                                   ...) {

  set.seed(sim)

  remaining_weeks <- (max(scores$week) + 1):season_weeks

  team_col <- names(select(scores, starts_with("team")))
  scores <- select(scores, week, team = starts_with("team"), score)
  teams <- unique(scores$team)

  for (week in remaining_weeks) {

    for (team in teams) {

      simulated_score <- .fun(scores,
                              team,
                              ...)

      scores <- scores %>%
        add_row(week = week,
                team = team,
                score = round(simulated_score, 2))

    }

  }

  set_names(scores, c("week", team_col, "score"))

}
