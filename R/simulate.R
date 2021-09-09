
#' @export
simulate_season_scores <- function(scores,
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

  if(.parallel) {

    future::plan(future::multiprocess)
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
simulate_season_standings <- function(sim_scores, schedule) {

  team_col <- names(select(sim_scores, starts_with("team")))

  if("league" %in% names(sim_scores)) {

    sim_scores_tmp <- select(sim_scores, sim, week = sim_week,
                             team = starts_with("team"), score)

  } else {

    sim_scores_tmp <- select(sim_scores, sim, week,
                             team = starts_with("team"), score)

  }

  if("teamID" %in% names(schedule) | "team" %in% names(schedule)) {
    schedule <- spread_schedule(schedule)
  }

  schedule <-  doublewide_schedule(schedule)

  weeks_played <- sim_scores_tmp %>%
    distinct(week, team, score) %>%
    count(week) %>%
    filter(n == n_distinct(sim_scores_tmp$team)) %>%
    filter(week == max(week)) %>%
    pull(week)
  current_week <- weeks_played + 1

  out <- schedule %>%
    mutate_if(is.factor, as.character) %>%
    inner_join(sim_scores_tmp %>%
                 rename(team1 = team,
                        score1 = score),
               by = c("week", "team1")) %>%
    inner_join(sim_scores_tmp %>%
                 rename(team2 = team,
                        score2 = score),
               by = c("week", "team2", "sim")) %>%
    mutate(diff = score1 - score2,
           win = diff > 0,
           leverage_win = if_else(week == current_week & win, 1, 0),
           lose = diff < 0,
           tie = diff == 0) %>%
    group_by(team1, sim) %>%
    summarise(pf = sum(score1),
              pa = sum(score2),
              wins = sum(win),
              losses = sum(lose),
              tie = sum(tie),
              wp = wins / (wins + losses),
              leverage_win = sum(leverage_win),
              .groups = "drop") %>%
    arrange(sim, -wp, -pf) %>%
    set_names(team_col, "sim", "pf", "pa",
              "wins", "losses", "tie",
              "wp", "leverage_win") %>%
    group_by(sim) %>%
    mutate(playoffs = row_number() <= 4,
           weeks_played = weeks_played) %>%
    ungroup()

  if("league" %in% names(sim_scores)) {

    out <- out %>%
      mutate(league = sim_scores$league[1],
             leagueID = sim_scores$leagueID[1],
             season = sim_scores$season[1],
             week = max(sim_scores$week),
             .before = 1)


  }

  return(out)

}

#' @export
simulate_final_standings <- function(sim_standings) {

  team_col <- names(select(sim_standings, starts_with("team")))
  weeks_played <- unique(sim_standings$weeks_played)
  sim_standings_tmp <- select(sim_standings, team = starts_with("team"), sim:weeks_played)

  out <- sim_standings_tmp %>%
    rename(team = 1) %>%
    group_by(team) %>%
    summarise(pf = round(mean(pf), 1),
              pa = round(mean(pa), 1),
              wins = round(mean(wins), 1),
              losses = round(mean(losses), 1),
              tie = round(mean(tie), 1),
              wp = mean(wp),
              playoffs = mean(playoffs),
              .groups = "drop") %>%
    arrange(-playoffs, -wins, -pf) %>%
    mutate(rank = 1L:n_distinct(sim_standings_tmp$team),
           week = weeks_played) %>%
    set_names(team_col, "pf", "pa",
              "wins", "losses", "ties",
              "wp", "playoffs",
              "rank", "week") %>%
    select(week, everything())

  if("league" %in% names(sim_standings)) {

    out <- out %>%
      mutate(league = sim_standings$league[1],
             leagueID = sim_standings$leagueID[1],
             season = sim_standings$season[1],
             week = max(sim_standings$week),
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
