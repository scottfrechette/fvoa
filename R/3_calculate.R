#' @export
calculate_rankings <- function(schedule, fit) {

  scores <- as_tibble(fit$data)

  calculate_stats(schedule, scores) %>%
    left_join(calculate_fvoa(fit), by = "team") %>%
    left_join(calculate_sos(schedule, fit), by = "team") %>%
    left_join(calculate_sor(schedule, scores), by = "team") %>%
    left_join(calculate_colley(schedule, scores), by = "team")
}

#' @export
calculate_fvoa_season <- function(fit_team_season_df) {

  fit_team_season_df %>%
    mutate(weekly_fvoa = map(model, calculate_fvoa)) %>%
    select(week, weekly_fvoa) %>%
    unnest(weekly_fvoa)

}

#' @export
calculate_ffa_projections <- function(ffa_data,
                                      league = c('espn', 'yahoo'),
                                      ecr = TRUE) {

  if (!requireNamespace("ffanalytics", quietly = TRUE)) {
    stop("Package \"ffanalytics\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  league <- match.arg(league)

  if (league == "espn") {

    tmp <- ffa_data %>%
      ffanalytics::projections_table(league_scoring_rules$sx_scoring) %>%
      ffanalytics::add_player_info()

  } else {

    tmp <- ffa_data %>%
      ffanalytics::projections_table(league_scoring_rules$clt_scoring) %>%
      ffanalytics::add_player_info()

  }

  if (ecr) tmp <- add_ecr(tmp)

  rename(tmp, mflID = id)

}

#' @export
calculate_ffa_src_points <- function(ffa_data,
                                     league = c('espn', 'yahoo')) {

  if (!requireNamespace("ffanalytics", quietly = TRUE)) {
    stop("Package \"ffanalytics\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  league <- match.arg(league)

  if (league == "espn") {

    ffa_data %>%
      ffanalytics:::source_points(league_scoring_rules$sx_scoring) %>%
      ffanalytics::add_player_info() %>%
      rename(mflID = id)

  } else {

    ffa_data %>%
      ffanalytics:::source_points(league_scoring_rules$clt_scoring) %>%
      ffanalytics::add_player_info() %>%
      rename(mflID = id)

  }

}

# Helper Functions --------------------------------------------------------

calculate_stats <- function(schedule, scores) {

  schedule %>%
    left_join(scores, by = c("week", "team")) %>%
    left_join(scores %>%
                rename(opponent = team,
                       opp_score = score),
              by = c("week", "opponent")) %>%
    drop_na() %>%
    mutate(tie = if_else(score == opp_score, 1, 0),
           win = if_else(score > opp_score, 1, 0),
           lose = if_else(score < opp_score, 1, 0)) %>%
    group_by(team) %>%
    summarise(pf = sum(score),
              pa = sum(opp_score),
              wins = sum(win),
              losses = sum(lose),
              tie = sum(tie),
              .groups = "drop") %>%
    mutate(wp = wins / (wins + losses)) %>%
    arrange(-wp, -pf) %>%
    mutate(league_rank = row_number(),
           wp = format_pct(wp, accuracy = 0)) %>%
    unite(record, c(wins, losses, tie), sep = "-")
}

calculate_fvoa <- function(fit) {

  scores <- as_tibble(fit$data)

  tidybayes::add_predicted_draws(tibble(week = max(scores$week),
                                        team = unique(scores$team)),
                                 fit,
                                 seed = 42) %>%
    group_by(team) %>%
    summarize(fvoa_wins = mean(.prediction > 110),
              fvoa_score = mean(.prediction - 110),
              fvoa_sd = sd(.prediction - 110)) %>%
    arrange(-fvoa_score) %>%
    select(team, fvoa = fvoa_score) %>%
    mutate(fvoa_rank = min_rank(-fvoa))

}

calculate_sos <- function(schedule, fit) {

  fvoa <- calculate_fvoa(fit)
  scores <- as_tibble(fit$data)

  season_sos <- schedule %>%
    left_join(select(fvoa, team, fvoa_team = fvoa), by = "team") %>%
    left_join(select(fvoa, opponent = team, fvoa_opp = fvoa), by = "opponent") %>%
    mutate(fvoa_diff = fvoa_team - fvoa_opp) %>%
    group_by(team) %>%
    summarize(sos_favored = sum(fvoa_diff > 0) / n(),
              sos_margin = round(mean(-fvoa_opp), 1)) %>%
    arrange(-sos_margin) %>%
    mutate(sos_rank = min_rank(-sos_margin))

  played_sos <- schedule %>%
    filter(week <= max(scores$week)) %>%
    left_join(select(fvoa, team, fvoa_team = fvoa), by = "team") %>%
    left_join(select(fvoa, opponent = team, fvoa_opp = fvoa), by = "opponent") %>%
    left_join(rename(scores, opponent = team, opp_score = score), by = c("opponent", "week")) %>%
    mutate(fvoa_diff = fvoa_team - fvoa_opp) %>%
    group_by(team) %>%
    summarize(played_sos_favored = sum(fvoa_diff > 0) / n(),
              played_sos_avg_fvoa = round(mean(-fvoa_opp), 1),
              played_sos_margin = round(mean(opp_score - 110), 2),
              played_sos = sum(opp_score > 110) / (sum(opp_score > 110) + sum(opp_score < 110))) %>%
    arrange(-played_sos_margin) %>%
    mutate(played_sos_rank = min_rank(-played_sos_margin))

  remaining_sos <- schedule %>%
    filter(week > max(scores$week)) %>%
    left_join(select(fvoa, team, fvoa_team = fvoa), by = "team") %>%
    left_join(select(fvoa, opponent = team, fvoa_opp = fvoa), by = "opponent") %>%
    mutate(fvoa_diff = fvoa_team - fvoa_opp) %>%
    group_by(team) %>%
    summarize(remain_sos_favored = sum(fvoa_diff > 0) / n(),
              remain_sos_margin = round(mean(-fvoa_opp), 1)) %>%
    arrange(-remain_sos_margin) %>%
    mutate(remain_sos_rank = min_rank(-remain_sos_margin))

  season_sos %>%
    left_join(played_sos, by = "team") %>%
    left_join(remaining_sos, by = "team")

}

calculate_sor <- function(schedule, scores, sims = 1e4) {

  set.seed(42)

  schedule %>%
    left_join(scores, by = c("week", "team")) %>%
    left_join(scores %>%
                rename(opponent = team,
                       opp_score = score),
              by = c("week", "opponent")) %>%
    crossing(tibble(sim = 1:sims,
                    sim_score = rnorm(sims, 110, 10))) %>%
    drop_na() %>%
    mutate(tie = score == opp_score,
           win = score > opp_score,
           lose = score < opp_score,
           sim_tie = sim_score == opp_score,
           sim_win = sim_score > opp_score,
           sim_lose = sim_score < opp_score) %>%
    group_by(team, sim) %>%
    summarize(tie = sum(tie),
              win = sum(win),
              lose = sum(lose),
              sim_tie = sum(sim_tie),
              sim_win = sum(sim_win),
              sim_lose = sum(sim_lose),
              .groups = "drop") %>%
    mutate(wp = win / (win + lose),
           sim_wp = sim_win / (sim_win + sim_lose)) %>%
    count(team, wp, sim_wp) %>%
    filter(sim_wp >= wp) %>%
    group_by(team) %>%
    summarize(sor = sum(n) / sims) %>%
    arrange(sor) %>%
    mutate(sor_rank = min_rank(sor))

}

calculate_colley <- function(schedule, scores) {

  scores <- select(scores, week, team = starts_with("team"), score)

  colley_df <- schedule %>%
    left_join(scores, by = c("week", "team")) %>%
    left_join(rename(scores, opponent = team, opp_score = score),
              by = c("week", "opponent")) %>%
    mutate(x = if_else(score > opp_score, "W",
                       if_else(opp_score > score, "L", "NA"))) %>%
    group_by(team, opponent) %>%
    summarise(x = paste(x, collapse = ""),
              .groups = "drop") %>%
    spread(opponent, x) %>%
    mutate_if(is.character, replace_na, "X") %>%
    unite(outcomes, -team, sep = "_") %>%
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

  colley_df %>%
    select(team, outcomes) %>%
    separate(outcomes, as.character(colley_df$team)) %>%
    gather(opponent, value, -team) %>%
    mutate(x = map_dbl(value, colley_transform)) %>%
    select(-value) %>%
    spread(opponent, x) %>%
    select(-team) %>%
    solve(tol = 1e-20) %*%
    colley_df$b %>%
    as_tibble(rownames = "team",
              .name_repair = ~"colley_rating") %>%
    mutate(colley_rank = min_rank(-colley_rating)) %>%
    arrange(colley_rank)
}

# Experimental ------------------------------------------------------------

calculate_roster_draws <- function(roster,
                                   model) {

  # player_draws <- roster %>%
  #   add_player_data(data = "mflID") %>%
  #   add_player_data(data = "position") %>%
  #   extract_player_draws(model)

  player_draws <- as_tibble(model$data) %>%
    semi_join(add_player_data(roster), by = "mflID") %>%
    filter(week == max(week)) %>%
    distinct(mflID, .keep_all = T) %>%
    extract_player_draws(model)

  roster %>%
    add_player_data(data = "mflID") %>%
    left_join(player_draws, by = "mflID") %>%
    filter(roster != "BE") %>%
    select(team, player, roster, position, sim, pred) %>%
    group_by(team, sim) %>%
    summarize(points = sum(pred)) %>%
    ungroup()

}

calculate_roster_draws1 <- function(roster,
                                    model_norm,
                                    model_pois) {

  # player_draws <- roster %>%
  #   add_player_data(data = "mflID") %>%
  #   add_player_data(data = "position") %>%
  #   extract_player_draws(model)

  player_draws_norm <- as_tibble(model_norm$data) %>%
    semi_join(add_player_data(roster), by = "mflID") %>%
    filter(week == max(week)) %>%
    distinct(mflID, .keep_all = T) %>%
    extract_player_draws(model_norm) %>%
    mutate(pred = as.integer(pred))

  player_draws_pois <- as_tibble(model_pois$data) %>%
    semi_join(add_player_data(roster), by = "mflID") %>%
    filter(week == max(week)) %>%
    distinct(mflID, .keep_all = T) %>%
    extract_player_draws(model_pois)

  bind_rows(
    roster %>%
      add_player_data(data = "mflID") %>%
      left_join(player_draws_norm, by = "mflID"),
    roster %>%
      add_player_data(data = "mflID") %>%
      left_join(player_draws_pois, by = "mflID")
  ) %>%
    filter(roster != "BE") %>%
    select(team, player, roster, position, sim, pred) %>%
    group_by(team, sim) %>%
    summarize(points = sum(pred)) %>%
    ungroup()

}
