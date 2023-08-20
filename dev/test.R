
# Setup -------------------------------------------------------------------

library(tidyverse)
library(ffscrapr)
library(funcyfrech)
library(skimr)

theme_set(fvoa::theme_fvoa())

sx_con <- espn_connect(2020, 299999)

owners <- ff_franchises(sx_con) %>%
  select(teamID = 1, team = 2)

schedule <- get_schedule(season = 2021) %>%
  left_join(owners, by = "teamID") %>%
  left_join(rename(owners, opponent = team, opponentID = teamID), by = "opponentID") %>%
  select(week, team, opponent)

teams <- map_df(1:15, ~get_team(week = ., season = 2021)) %>%
  left_join(owners, by = "teamID") %>%
  select(week, team, score:points)

scores_full <- extract_scores(teams)

scores <- filter(scores_full, week <= 10)

projections <- extract_projections(teams)

fit <- fit_team(scores)

all_teams_full <- crossing(week = 1:13, season = 2019:2021) %>%
  mutate(data = map2(week, season, ~ get_team(week = .x, season = .y))) %>%
  select(season, data) %>%
  unnest(data) %>%
  left_join(owners, by = "teamID") %>%
  select(season, week, team, score:points)

all_teams <- all_teams_full %>%
  unite(team_yr, team, season, remove = F) %>%
  distinct(team_yr, season, week, score)

scores21 <- all_teams %>%
  filter(season == 2021) %>%
  separate(team_yr,c("team", "yr"), sep = "_") %>%
  select(team, week, score)

# Functions ---------------------------------------------------------------

evaluate_fit_matchups <- function(predicted_draws,
                                  prediction,
                                  robust = F,
                                  verbose = F) {

  predicted_draws <- ungroup(predicted_draws)

  out <- left_join(select(predicted_draws, week, team, score, sim = .draw, {{prediction}}),
                   select(predicted_draws, week, opp = team, opp_score = score, sim = .draw, opp_pred = {{prediction}}),
                   by = c("week", "sim")) %>%
    filter(team != opp) %>%
    mutate(pred_margin = {{prediction}} - opp_pred) %>%
    group_by(week, team, opp) %>%
    summarize(score = score[1],
              opp_score = opp_score[1],
              mean = mean(pred_margin),
              median = median(pred_margin),
              sd = sd(pred_margin),
              mad = mad(pred_margin, constant = 1),
              wp = mean(pred_margin > 0),
              margin_obs = mean(score - opp_score),
              margin_pred = mean(pred_margin),
              l50 = quantile(pred_margin, 0.25),
              u50 = quantile(pred_margin, 0.75),
              l80 = quantile(pred_margin, 0.1),
              u80 = quantile(pred_margin, 0.9),
              l95 = quantile(pred_margin, 0.025),
              u95 = quantile(pred_margin, 0.975),
              .groups = "keep") %>%
    mutate(center = mean * (robust == FALSE) + median * (robust == TRUE),
           scale = sd * (robust == FALSE) + mad * (robust == TRUE),
           correct = sign(margin_obs) == sign(margin_pred),
           error = margin_obs - center,
           error_scaled = error / scale,
           within50 = between(margin_obs, l50, u50),
           within80 = between(margin_obs, l80, u80),
           within95 = between(margin_obs, l95, u95)) %>%
    ungroup() %>%
    select(week, team, score,
           opp, opp_score,
           wp, margin_pred, margin_obs,
           correct, error, error_scaled,
           within50, within80, within95)

  if (!verbose) {out <- summarize(out,
                                  accuracy = mean(correct),
                                  mae = mean(abs(error)),
                                  mae_scaled = mean(abs(error_scaled)),
                                  within50 = mean(within50),
                                  within80 = mean(within80),
                                  within95 = mean(within95))}

  return(out)
}



# Simulated point trees ---------------------------------------------------

fit <- fit_model(filter(scores, week <= 6))

sims <- simulate_season_scores(filter(scores, week <= 6), schedule, fit)

sims %>%
  nest(data = -sim) %>%
  slice_sample(n = 100) %>%
  unnest(data) %>%
  select(sim, week, team = team1, score = score1) %>%
  group_by(sim, team) %>%
  mutate(points = cumsum(score)) %>%
  group_by(week, team) %>%
  mutate(avg = mean(points)) %>%
  ggplot(aes(x = week, y = points, color = team, group = sim)) +
  geom_line(alpha = 0.1) +
  geom_line(aes(y = avg)) +
  facet_wrap(~ team) +
  labs(x = "Week", y = "Total Points") +
  theme_fvoa() +
  guides(color = "none")

sims %>%
  nest(data = -sim) %>%
  slice_sample(n = 10) %>%
  mutate(sim = 1:10) %>%
  unnest(data) %>%
  select(sim, week, team = team1, score = score1) %>%
  ggplot(aes(week, score, color = team)) +
  geom_line() +
  facet_wrap(~ sim)

# FantasyPros Data --------------------------------------------------------

url <- "https://www.fantasypros.com/nfl/projections/rb.php?week=4&filters=2750:152:73:1580:286:11:71:4421"


map_df(c(2750, 152, 73, 1580, 286, 11, 71, 441),
       ~ paste0("https://www.fantasypros.com/nfl/projections/rb.php?week=4&filters=", .x) %>%
         read_html() %>%
         html_table() %>%
         .[[1]] %>%
         # slice(-1) %>%
         janitor::row_to_names(2) %>%
         janitor::clean_names())

# ESPN WP -----------------------------------------------------------------

# https://fantasy.espn.com/apis/v3/games/ffl/seasons/2021/segments/0/leagues/299999?view=mMatchup&view=mMatchupScore&view=mRoster&view=mScoreboard&view=mSettings&view=mStatus&view=mTeam&view=modular&view=mNav

conn <- ffscrapr::espn_connect(season = season, league_id = leagueID)

season <- as.integer(season)
week <- as.integer(week)
posID <- slot_name_to_id(pos)
projID <- as.integer(projections)

players = list(
  filterSlotIds = list(value = posID),
  filterStatsForSourceIds = list(value = projID), # 0 = actual, 1 = projected
  offset = jsonlite::unbox(0)
)

if (week == 0) {
  players$filterStatsForExternalIds = list(value = season)
} else {
  players$filterStatsForSplitTypeIds = list(value = week)
}

player_scores <- ffscrapr::espn_getendpoint(conn, view = "kona_player_info",
                                            x_fantasy_filter = jsonlite::toJSON(list("players" = players)))


url_game <- "https://fantasy.espn.com/football/fantasycast?leagueId=299999&matchupPeriodId=3&seasonId=2021&teamId=3"

url_game %>%
  read_html() %>%
  html_nodes("jsx-321219690 fw-bold pa2 pr3 winPercent")

# <div class="jsx-321219690 fw-bold pa2 pr3 winPercent">55%</div>


# Matchups ----------------------------------------------------------------

library(ffscrapr)
library(ffanalytics)
library(nflreadr)
library(rstanarm)
library(tidybayes)

options(mc.cores = parallel::detectCores())

leagueID <- 299999
current_season <- 2021
current_week <- 6

# Player IDs
playerIDs <- ffscrapr::dp_playerids() %>%
  select(playerID = gsis_id, , pfrID = pfr_id, position)

player_table <- ffscrapr::dp_playerids() %>%
  select(player = name, team, position, age, draft_year, gsis_id,
         espnID = espn_id, yahooID = yahoo_id, mflID = mfl_id) %>%
  mutate(espnID = as.integer(espnID)) %>%
  bind_rows(defenseIDs) %>%
  rename(name = player, mfl_id = mflID)

## get_current_roster
current_roster <- get_current_roster(current_week) %>%
  left_join(owners, by = "teamID") %>%
  select(team, playerID:injury)

## get_schedule
schedule <- get_schedule(season = 2021) %>%
  left_join(owners, by = "teamID") %>%
  left_join(rename(owners, opponent = team, opponentID = teamID), by = "opponentID") %>%
  select(week, team, opponent)

## get ffa data
ffa_data <- get_ffa_data(current_week)

## get source projections
ffa_src_points <- calculate_ffa_src_points(ffa_data) %>%
  left_join(select(player_table, mflID = mfl_id, playerID = gsis_id),
            by = "mflID")

schedules <- load_schedules(2020:2021) %>%
  select(season, week, home_team, away_team)

snaps <- load_snap_counts(2020:2021) %>%
  separate(game_id, c("season1", "week", "team1", "opponent"), sep = "_") %>%
  mutate(week = as.integer(week)) %>%
  select(pfrID = pfr_player_id, season, week, offense_snaps, offense_pct)

passing_ngs <- load_nextgen_stats(2020:2021, 'passing')
receiving_ngs <- load_nextgen_stats(2020:2021, 'receiving')
rushing_ngs <- load_nextgen_stats(2020:2021, 'rushing')

depth_charts <- load_depth_charts(2020:2021) %>%
  select(season, week, playerID = gsis_id, depth_team)

game_data <- bind_rows(schedules %>%
                         mutate(team = home_team,
                                opponent = away_team,
                                home = TRUE) %>%
                         select(season, week, team, opponent, home),
                       schedules %>%
                         mutate(team = away_team,
                                opponent = home_team,
                                home = FALSE) %>%
                         select(season, week, team, opponent, home))

weekly_player_data <- load_player_stats(2021:2021) %>%
  select(season, week, playerID = player_id, player = player_name,
         team = recent_team, points = fantasy_points) %>%
  inner_join(playerIDs, by = "playerID") %>%
  filter(position %in% c("QB", "WR", "RB", "TE")) %>%
  left_join(game_data, by = c("season", "week", "team")) %>%
  left_join(depth_charts, by = c("season", "week", "playerID")) %>%
  left_join(snaps, by = c("season", "week", "pfrID")) %>%
  left_join(select(passing_ngs, season, week, playerID = player_gsis_id, attempts),
            by = c("season", "week", "playerID")) %>%
  left_join(select(receiving_ngs, season, week, playerID = player_gsis_id, targets),
            by = c("season", "week", "playerID")) %>%
  left_join(select(rushing_ngs, season, week, playerID = player_gsis_id, rush_attempts),
            by = c("season", "week", "playerID")) %>%
  mutate(across(where(is.integer), ~if_else(is.na(.x), 0L, .x)),
         across(where(is.double), ~if_else(is.na(.x), 0, .x))) %>%
  # work on rules: qb >= 0.5 offense_pct and rb/wr/te >= 0.2 offense_pct
  filter(season == 2021 &
           ((position == "QB" & offense_pct >= 0.5) | offense_pct >= 0.2)) %>%
  left_join(select(player_table, playerID = gsis_id, name, mflID = mfl_id),
            by = "playerID") %>%
  select(season, week, mflID, name, team, position, points) %>%
  mutate(team = if_else(team == "LA", "LAR", team),
         points_count = as.integer(points))

ffa_src_points_clean <- ffa_src_points %>%
  left_join(depth_charts %>%
              filter(week == current_week,
                     season == current_season) %>%
              select(playerID, depth_team),
            by = "playerID") %>%
  group_by(mflID) %>%
  filter(depth_team == min(depth_team) | is.na(depth_team)) %>%
  ungroup() %>%
  filter(!team %in% c("FA", "FA*"),
         position %in% c("QB", "RB", "WR",
                         "TE", "K", "DST"),
         (position != "QB" | depth_team == 1)) %>%
  mutate(season = current_season,
         week = current_week,
         points_count = as.integer(points)) %>%
  unite(name, first_name, last_name, sep = " ") %>%
  select(season, week, mflID, name, team, position, depth_team, points, points_count)


ffa_src_points_norm <- filter(ffa_src_points_clean, position %in% c("DST", "K", "QB"))
ffa_src_points_pois <- filter(ffa_src_points_clean,
                              position %in% c("RB", "WR", "TE") & points_count > 0)

player_data <- bind_rows(
  mutate(weekly_player_data, actual = TRUE),
  ffa_src_points_clean %>%
    anti_join(weekly_player_data,
              by = c("season", "week", "mflID")) %>%
    mutate(actual = FALSE)) %>%
  mutate(team = case_when(
    team == "LA" ~ "LAR",
    team == "GBP" ~ "GB",
    team == "JAC" ~ "JAX",
    team == "KCC" ~ "KC",
    team == "LVR" ~ "LV",
    team == "NEP" ~ "NE",
    team == "NOS" ~ "NO",
    team == "SFO" ~ "SF",
    team == "TBB" ~ "TB",
    TRUE ~ team),
    points_count = as.integer(points)) %>%
  left_join(game_data, by = c("season", "week", "team")) %>%
  mutate(team = if_else(team == "LA", "LAR", team),
         opponent = if_else(team == "LA", "LAR", opponent),
         depth_team = NULL) %>%
  distinct()

qb_data <- filter(player_data, position == "QB")
non_qb_data <- filter(player_data, position %in% c("RB", "WR", "TE"), points >= 0)
rb_data <- filter(player_data, position == "RB", actual, points >= 0)
wr_data <- filter(player_data, position == "WR", actual, points >= 0)
te_data <- filter(player_data, position == "TE", actual, points >= 0)

## fit_player
# Population: player, position, team, opponent, home, week, season, injury
# Groups: team, player, opponent, position?
# Group terms: week, home, injury

fit_pospop_posteam_posopp_id <- stan_glmer(points_count ~ 1 +
                                             position +
                                             (position | team) +
                                             (position | opponent) +
                                             (1 | mflID),
                                           data = player_data,
                                           family = poisson,
                                           chains = 4,
                                           iter = 3500,
                                           warmup = 1000,
                                           seed = 42)

fit_pospop_posteam_posopp_home_id <- stan_glmer(points_count ~ 1 +
                                                  home +
                                                  position +
                                                  (home + position | team) +
                                                  (home + position | opponent) +
                                                  (1 | mflID),
                                                data = player_data,
                                                family = poisson,
                                                chains = 4,
                                                iter = 3500,
                                                warmup = 1000,
                                                seed = 42)

fit_opppos_posteamid <- stan_glmer(poipoints_countnts ~ 1 +
                                     (1 | opponent) +
                                     (1 | opponent:position) +
                                     (1 | position) +
                                     (1 | position:team) +
                                     (1 | position:team:mflID),
                                   data = player_data,
                                   family = poisson,
                                   chains = 4,
                                   iter = 3500,
                                   warmup = 1000,
                                   seed = 42)



# redo with DST, K, and QB as normal and RB, WR, TE as poisson
fit_ffa <- stan_glmer(points ~ 1 + position + (1 | mflID),
                      data = ffa_src_points_count,
                      family = poisson,
                      chains = 4,
                      iter = 4000,
                      # warmup = 1500,
                      seed = 42)

# QB/K/DST
fit_ffa_norm <- stan_glmer(points_count ~ 1 + position + (1 | mflID),
                           data = ffa_src_points_norm,
                           family = gaussian,
                           chains = 4,
                           iter = 4000,
                           # warmup = 1500,
                           seed = 42)

# RB/WR/TE
fit_ffa_pois <- stan_glmer(points_count ~ 1 + position + (1 | mflID),
                           data = ffa_src_points_pois,
                           family = poisson,
                           chains = 4,
                           iter = 4000,
                           # warmup = 1500,
                           seed = 42)

fit_qb <- stan_glmer(points ~ 1 + (1 | mflID),
                     prior_intercept = normal(17, 10),
                     data = qb_data,
                     family = gaussian,
                     chains = 4,
                     iter = 4000,
                     # warmup = 1500,
                     seed = 42)

fit_non_qb <- stan_glmer(points_count ~ 1 + position +  (1 | mflID),
                         # prior_intercept = normal(7, 10),
                         # prior_intercept = cauchy(6, 10),
                         data = non_qb_data,
                         family = poisson,
                         chains = 4,
                         iter = 4000,
                         # warmup = 1500,
                         seed = 42)

fit_rb <- stan_glmer(points_count ~ 1 + (1 | mflID),
                     prior_intercept = normal(8, 10),
                     data = rb_data,
                     family = neg_binomial_2,
                     chains = 4,
                     iter = 4000,
                     # warmup = 1500,
                     seed = 42)

fit_wr <- stan_glmer(points_count ~ 1 + (1 | mflID),
                     prior_intercept = normal(6, 10),
                     data = wr_data,
                     family = neg_binomial_2,
                     chains = 4,
                     iter = 4000,
                     # warmup = 1500,
                     seed = 42)

fit_te <- stan_glmer(points_count ~ 1 + (1 | mflID),
                     prior_intercept = normal(4, 10),
                     data = te_data,
                     family = neg_binomial_2,
                     chains = 4,
                     iter = 4000,
                     # warmup = 1500,
                     seed = 42)

# loo_opppos_posteamid <- loo(fit_opppos_posteamid, cores = 7)
# loo_compare(loo_pospop_posteam_posopp_id, loo_pospop_posteam_id)

# models:
# fit_pospop_id
# fit_posgroup_id
# fit_pospop_posteam_id
# fit_opppos_posteamid
# fit_pospop_posteam_posopp_id
# fit_poppop_posteam_popopp_home_id

mdl <- fit_qb

pp_check(mdl)

current_roster_draws <- current_roster %>%
  # filter(playerID != 4240657) %>%
  calculate_roster_draws(mdl) #%>% mutate(points = points + 15)

current_roster_draws <- current_roster %>%
  calculate_roster_draws1(fit_ffa_norm, fit_ffa_pois)

compare_teams_player(schedule, drop_na(current_roster_draws), current_week = week)

plot_player_fvoa(mdl)
plot_opponent_fvoa(mdl)

## extract_player_draws
# player_draws <- extract_player_draws(player_data, player_fit)

tmp <- left_join(qb_data %>%
                   distinct(name, mflID, position) %>%
                   # add_row(mflID = "A", name = "Average") %>%
                   add_epred_draws(fit_qb, value = "points") %>%
                   group_by(name) %>%
                   median_hdi(points, .width = c(0.5, 0.89)) %>%
                   select(name:.width),
                 qb_data %>%
                   group_by(name) %>%
                   mutate(avg = mean(points),
                          games = n()) %>%
                   ungroup() %>%
                   select(name, week, games, avg, actual = points),
                 by = "name")

tmp %>%
  ggplot(aes(y = reorder(name, points),
             yend = reorder(name, points))) +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.89),
               size = 0.5, color = "#6497b1") +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.5),
               size = 2, color = "#03396c") +
  geom_point(aes(x = points),
             size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
  geom_point(aes(x = actual), color = 'red') +
  geom_point(aes(x = avg, size = games)) +
  geom_vline(xintercept = mean(tmp$points), color = 'red', linetype = 2) +
  labs(x = "FVOA", y = NULL) +
  theme_fvoa() +
  theme(axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.grid.major.y = element_blank())


tmp <- left_join(non_qb_data %>%
                   distinct(name, mflID, position) %>%
                   add_row(mflID = "A", name = "Average RB", position = "RB") %>%
                   add_row(mflID = "A", name = "Average WR", position = "WR") %>%
                   add_row(mflID = "A", name = "Average TE", position = "TE") %>%
                   add_predicted_draws(fit_non_qb, value = "points") %>%
                   group_by(name, position) %>%
                   median_hdi(points, .width = c(0.5, 0.89)) %>%
                   select(name:.width),
                 non_qb_data %>%
                   group_by(name) %>%
                   mutate(avg = mean(points),
                          games = n()) %>%
                   ungroup() %>%
                   select(name, week, games, avg, actual = points),
                 by = "name") %>%
  group_by(position) %>%
  mutate(pos_avg = mean(actual, na.rm = T)) %>%
  ungroup() %>% semi_join(distinct(tmp, name, position, avg) %>% top_n_group(25, avg, position), by = c("name", "position"))

tmp %>%
  ggplot(aes(y = reorder(name, points),
             yend = reorder(name, points))) +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.89),
               size = 0.5, color = "#6497b1") +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.5),
               size = 2, color = "#03396c") +
  geom_point(aes(x = points),
             size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
  geom_point(aes(x = actual), color = 'red') +
  geom_point(aes(x = avg, size = games)) +
  geom_vline(aes(xintercept = pos_avg), color = 'red', linetype = 2) +
  labs(x = "FVOA", y = NULL) +
  facet_wrap(~ position, scales = "free") +
  theme_fvoa() +
  theme(axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

as_tibble(fit_non_qb$data) %>%
  semi_join(add_player_data(current_roster), by = "mflID") %>%
  distinct(mflID, .keep_all = T) %>%
  extract_player_draws(fit_non_qb)

roster_draws <- bind_rows(
  current_roster %>%
    add_player_data(data = "mflID") %>%
    inner_join(as_tibble(fit_qb$data) %>%
                 semi_join(add_player_data(current_roster), by = "mflID") %>%
                 distinct(mflID, .keep_all = T) %>%
                 extract_player_draws(fit_qb),
               by = "mflID"),
  current_roster %>%
    add_player_data(data = "mflID") %>%
    inner_join(as_tibble(fit_non_qb$data) %>%
                 semi_join(add_player_data(current_roster), by = "mflID") %>%
                 distinct(mflID, .keep_all = T) %>%
                 extract_player_draws(fit_non_qb),
               by = "mflID")) %>%
  filter(roster != "BE") %>%
  mutate(pred = case_when(
    injured ~ 0,
    !mflID %in% ffa_src_points_clean$mflID ~ 0,
    TRUE ~ pred
  )) %>%
  select(team, player, roster, position, sim, pred) %>%
  group_by(team, sim) %>%
  summarize(points = sum(pred)) %>%
  ungroup() %>% mutate(points = as.integer(points + 15))

compare_teams_player(schedule, roster_draws, 6)

# Combined evaluation -----------------------------------------------------

# BRMS --------------------------------------------------------------------

library(tidybayes)
library(bayesplot)
library(performance)
library(brms)
library(broom.mixed)

rstan::rstan_options(auto_write = TRUE)

fit1 <- brm(score ~ 0 + Intercept + s(week, by = team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = "dev/fit1",
            control = list(adapt_delta = 0.99))

fit2 <- brm(score ~ 0 + Intercept + s(week, by = team) + (1 | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit2',
            control = list(adapt_delta = 0.99))

fit3 <- brm(score ~ 0 + intercept + (1 | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit3',
            control = list(adapt_delta = 0.99))

fit4 <- brm(score ~ s(week, by = team) + (1 | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit4',
            control = list(adapt_delta = 0.99))

fit5 <- brm(score ~ s(week, by = team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit5',
            control = list(adapt_delta = 0.99))

fit6 <- brm(score ~ (week | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit6',
            control = list(adapt_delta = 0.99))

fit7 <- brm(score ~ week + I(week^2) + (week + I(week^2) | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit7',
            control = list(adapt_delta = 0.99))

fit8 <- brm(score ~ week + I(week^2) + I(week^3) + (week + I(week^2) + I(week^3) | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = b)),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit8',
            control = list(adapt_delta = 0.99))

fit9 <- brm(score ~ week + I(week^2) + (week + I(week^2) | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = 'Intercept')),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit9',
            control = list(adapt_delta = 0.99))

fit10 <- brm(score ~ 0 + Intercept + week + (week | team),
             data = scores,
             family = gaussian,
             prior = c(prior(student_t(10, 115, 10), class = 'Intercept')),
             seed = 42,
             iter = 3750,
             warmup = 1250,
             chains = 4,
             cores = 4,
             file = 'dev/fit10',
             control = list(adapt_delta = 0.99))

fit11 <- brm(score ~ 0 + Intercept + week + (week | team),
             data = scores,
             family = gaussian,
             prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                       prior(normal(0, 12), class = 'b'),
                       prior(exponential(0.04166667), class = 'sd'), # 1 / 24
                       prior(exponential(0.04166667), class = 'sigma'),
                       prior(lkj(4), class = 'cor')),
             seed = 42,
             iter = 3750,
             warmup = 1250,
             chains = 4,
             cores = 4,
             file = 'dev/fit11',
             control = list(adapt_delta = 0.99))

fit12 <- brm(score ~ 0 + Intercept + week + I(week^2) + (week + I(week^2) | team),
            data = scores,
            family = gaussian,
            prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                      prior(normal(0, 12), class = 'b'),
                      prior(exponential(0.04166667), class = 'sd'),
                      prior(exponential(0.04166667), class = 'sigma'),
                      prior(lkj(4), class = 'cor')),
            seed = 42,
            iter = 3750,
            warmup = 1250,
            chains = 4,
            cores = 4,
            file = 'dev/fit12',
            file_refit = "on_change",
            control = list(adapt_delta = 0.99))

fit13 <- update(fit12,
                score ~ 0 + Intercept + poly(week, 2) + (poly(week, 2) | team),
                newdata = scores,
                family = gaussian,
                # prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                #           prior(normal(0, 12), class = 'b'),
                #           prior(exponential(0.04166667), class = 'sd'), # 1 / 24
                #           prior(exponential(0.04166667), class = 'sigma'),
                #           prior(lkj(4), class = 'cor')),
                seed = 42,
                iter = 3750,
                warmup = 1250,
                chains = 4,
                cores = 4,
                file = 'dev/fit13',
                control = list(adapt_delta = 0.99))

fit14 <- update(fit12,
                score ~ 0 + Intercept + poly(week, 3) + (poly(week, 3) | team),
                newdata= scores,
                family = gaussian,
                # prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                #           prior(normal(0, 12), class = 'b'),
                #           prior(exponential(0.04166667), class = 'sd'), # 1 / 24
                #           prior(exponential(0.04166667), class = 'sigma'),
                #           prior(lkj(4), class = 'cor')),
                seed = 42,
                iter = 3750,
                warmup = 1250,
                chains = 4,
                cores = 4,
                file = 'dev/fit14',
                control = list(adapt_delta = 0.99))

fit15 <- update(fit12,
                score ~ 0 + Intercept + week + I(week^2) + I(week^3) + (week + I(week^2) + I(week^3) | team),
                newdata= scores,
                family = gaussian,
                # prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                #           prior(normal(0, 12), class = 'b'),
                #           prior(exponential(0.04166667), class = 'sd'), # 1 / 24
                #           prior(exponential(0.04166667), class = 'sigma'),
                #           prior(lkj(4), class = 'cor')),
                seed = 42,
                iter = 3750,
                warmup = 1250,
                chains = 4,
                cores = 4,
                file = 'dev/fit15',
                control = list(adapt_delta = 0.99))

# fit21 <- update(fit12, newdata = scores21, cores = 4)

fit_loo1 <- loo(fit1)
fit_loo2 <- loo(fit2)
fit_loo3 <- loo(fit3)
fit_loo4 <- loo(fit4)
fit_loo5 <- loo(fit5)
fit_loo6 <- loo(fit6)
fit_loo7 <- loo(fit7)
fit_loo8 <- loo(fit8)
fit_loo9 <- loo(fit9)
fit_loo10 <- loo(fit10)
fit_loo11 <- loo(fit11)
fit_loo12 <- loo(fit12)
fit_loo13 <- loo(fit13)
fit_loo14 <- loo(fit14)
fit_loo15 <- loo(fit15)

loo_compare(fit_loo1, fit_loo2, fit_loo3, fit_loo4, fit_loo5,
            fit_loo6, fit_loo7, fit_loo8, fit_loo9, fit_loo10,
            fit_loo11, fit_loo12, fit_loo13, fit_loo14, fit_loo15)

round(model_weights(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12, fit13, fit14, fit15), 3)

performance::compare_performance(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10, fit11, fit12, fit13, fit14, fit15)

mdl <- fit_arma1
s <- train21

pp_check(mdl, ndraws = 50)
prior_summary(mdl)

epred_draws(mdl, s, value = 'pred') %>%
  median_hdi() %>%
  ggplot(aes(x = week, y = score)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F, method = 'lm') +
  geom_point(aes(y = pred), color = 'red') +
  geom_line(aes(y = pred), color = 'red') +
  geom_segment(aes(xend = week, y = .lower, yend = .upper)) +
  geom_smooth(aes(y = pred), se = F, method = 'lm', color = 'red') +
  facet_wrap(~ team)

epred_draws(mdl, s, value = 'pred') %>%
  ggplot(aes(x = week, y = score)) +
  geom_point(data = scores) +
  geom_line(data = scores) +
  geom_smooth(se = F, method = 'lm', data = s) +
  stat_pointinterval(aes(y = pred)) +
  facet_wrap(~ team)

predicted_draws(mdl, s, value = 'pred') %>%
  ggplot(aes(x = week, y = score)) +
  geom_point(data = s) +
  geom_line(data = s) +
  geom_smooth(se = F, method = 'lm', data = s) +
  stat_pointinterval(aes(y = pred)) +
  facet_wrap(~ team)

  crossing(week = 14:15,
         team = unique(s$team)) %>%
  add_predicted_draws(mdl, value = 'pred') %>%
  ggplot(aes(week, pred)) +
  stat_pointinterval() +
  geom_line(data = filter(scores_full, week > 10), aes(y = score), color = 'red') +
  facet_wrap(~ team)

ranef(mdl)$team[, , 1] %>%
  as_tibble(rownames = 'team') %>%
  arrange(-Estimate)

ranef(mdl)$team %>%
  as_tibble(rownames = 'team') %>%
  select(team, contains("Estimate")) %>%
  rename(intercept = 2, week = 3, week2 = 4) %>%
  mutate(fvoa = intercept + week * max(s$week) + week2 * max(s$week)^2) %>%
  # rename(intercept = 2, week = 3) %>%
  # mutate(fvoa = intercept + week * max(s$week)) %>%
  arrange(-fvoa)

a <- left_join(s %>%
                 add_epred_draws(mdl, value = 'pred') %>%
                 ungroup() %>%
                 select(.draw, week, team, score, pred),
               tibble(week = 1:max(s$week), team = "t") %>%
                 add_epred_draws(mdl, value = 'avg', allow_new_levels = T) %>%
                 ungroup() %>%
                 select(.draw, week, avg),
               by = c(".draw", "week"))

b <- a %>%
  mutate(margin = pred - avg) %>%
  group_by(week, team) %>%
  summarize(score = score[1],
            pred = mean(pred),
            avg = mean(avg),
            fvoa = mean(margin),
            .groups = "drop")

b %>%
  ggplot(aes(week, fvoa)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  facet_wrap(~ team)

b %>%
  gather(key, value, score, pred, avg) %>%
  ggplot(aes(week, value, color = key)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = F, method = 'lm') +
  geom_hline(yintercept = 115, color = 'red', linetype = 2) +
  facet_wrap(~ team)

# Historic Model ----------------------------------------------------------


all_teams_full <- crossing(week = 1:13, season = 2019:2021) %>%
  mutate(data = map2(week, season, ~ get_team(week = .x, season = .y))) %>%
  select(season, data) %>%
  unnest(data) %>%
  left_join(owners, by = "teamID") %>%
  select(season, week, team, score:points)

all_teams <- all_teams_full %>%
  unite(team_yr, team, season, remove = F) %>%
  distinct(team_yr, season, week, score)

fit_hist1 <- brm(score ~ 0 + Intercept + season + week + (week | team_yr),
                 data = all_teams,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                           prior(normal(0, 12), class = 'b'),
                           prior(exponential(.04), class = 'sd'),
                           prior(exponential(.04), class = 'sigma'),
                           prior(lkj(4), class = 'cor')),
                 seed = 42,
                 iter = 3750,
                 warmup = 1250,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_hist1',
                 control = list(adapt_delta = 0.99))

fit_hist2 <- brm(score ~ 0 + Intercept + season + week + I(week^2) + (week + I(week^2) | team_yr),
                 data = all_teams,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                           prior(normal(0, 12), class = 'b'),
                           prior(exponential(.04), class = 'sd'),
                           prior(exponential(.04), class = 'sigma'),
                           prior(lkj(4), class = 'cor')),
                 seed = 42,
                 iter = 3750,
                 warmup = 1250,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_hist2',
                 control = list(adapt_delta = 0.99))

fit_hist3 <- brm(score ~ 0 + Intercept + season + poly(week, 3) + (poly(week, 3) | team_yr),
                 data = all_teams,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                           prior(normal(0, 12), class = 'b'),
                           prior(exponential(.04), class = 'sd'),
                           prior(exponential(.04), class = 'sigma'),
                           prior(lkj(4), class = 'cor')),
                 seed = 42,
                 iter = 3750,
                 warmup = 1250,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_hist3',
                 control = list(adapt_delta = 0.99))

fit_hist4 <- brm(score ~ 0 + Intercept + season + poly(week, 2) + (poly(week, 2) | team_yr),
                 data = all_teams,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                           prior(normal(0, 12), class = 'b'),
                           prior(exponential(.04), class = 'sd'),
                           prior(exponential(.04), class = 'sigma'),
                           prior(lkj(4), class = 'cor')),
                 seed = 42,
                 iter = 3750,
                 warmup = 1250,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_hist4',
                 control = list(adapt_delta = 0.99))

fit_hist5 <- brm(score ~ 0 + Intercept + season + week + I(week^2) + I(week^3) + (week + I(week^2) + I(week^3) | team_yr),
                 data = all_teams,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b', coef = 'Intercept'),
                           prior(normal(0, 12), class = 'b'),
                           prior(exponential(.04), class = 'sd'),
                           prior(exponential(.04), class = 'sigma'),
                           prior(lkj(4), class = 'cor')),
                 seed = 42,
                 iter = 3750,
                 warmup = 1250,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_hist5',
                 control = list(adapt_delta = 0.99))

model_performance(fit_hist1, fit_hist2, fit_hist3, fit_hist4)

pp_check(fit_hist1, ndraws = 50)
tidy(fit_hist1)
tidy(fit_hist1, effects = 'ran_vals') %>%
  filter(str_detect(level, "2021"), term == "(Intercept)")

ranef(fit_hist3)$team_yr %>%
  as_tibble(rownames = 'team') %>%
  separate(team, c("team", "season"), sep = "_") %>%
  # filter(season == 2021) %>%
  select(season, team, contains("Estimate")) %>%
  # rename(intercept = 2, week = 3, week2 = 4) %>%
  # mutate(fvoa = intercept + week * max(s$week) + week2 * max(s$week)^2) %>%
  # rename(intercept = 2, week = 3) %>%
  # mutate(fvoa = intercept + week * 13) %>%
  rename(intercept = 3, week1 = 4, week2 = 5, week3 = 6) %>%
  crossing(week = 1:13) %>%
  mutate(fvoa = intercept + week1 * week + week2 * week^2 + week3 * week^3) %>%
  arrange(-fvoa)

a <- left_join(all_teams %>%
                 add_epred_draws(fit_hist3, value = 'pred') %>%
                 ungroup() %>%
                 select(.draw, week, team_yr, score, pred),
               tibble(week = 1:13, season = 2021, team = "t") %>%
                 add_epred_draws(fit_hist3, value = 'avg', allow_new_levels = T) %>%
                 ungroup() %>%
                 select(.draw, week, avg),
               by = c(".draw", "week"))

b <- a %>%
  mutate(margin = pred - avg) %>%
  group_by(week, team_yr) %>%
  summarize(score = score[1],
            pred = mean(pred),
            avg = mean(avg),
            fvoa = mean(margin),
            .groups = "drop")


# Weekly Test -------------------------------------------------------------

train21 <- filter(scores21, week < max(week))
train_hist <- filter(all_teams, week < max(week))

# no week & no random intercept
fit_21 <- fit_team(train21)
train_hist_wt <- left_join(train_hist,
                           weight_games(1:max(train_hist$week)),
                           by = "week")
fit_hist <- rstanarm::stan_glm(score ~ 0 + season + team_yr,
                               data = train_hist_wt,
                               weights = weight,
                               prior = rstanarm::student_t(10, 115, 10),
                               seed = 42,
                               iter = 3750,
                               warmup = 1250,
                               chains = 4,
                               cores = 4)

pred_21 <- scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(fit_21, value = "pred") %>%
  ungroup() %>%
  select(team, week, score, sim = .draw, pred)

pred_hist <- all_teams %>%
  filter(season == max(season) & week == max(week)) %>%
  add_predicted_draws(fit_hist, value = "pred") %>%
  ungroup() %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)

# no week
fit_21_no_week <- update(fit3, newdata = train21, cores = 4)
fit_hist_no_week <- update(fit_hist1,
                           newdata = train_hist,
                           formula = score ~ 0 + Intercept + season + (1 | team_yr),
                           seed = 42,
                           iter = 3750,
                           warmup = 1250,
                           chains = 4,
                           cores = 4,
                           control = list(adapt_delta = 0.99))

pred_21_no_week <- scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(fit_21_no_week, value = "pred") %>%
  ungroup() %>%
  select(team, week, score, sim = .draw, pred)

pred_hist <- all_teams %>%
  filter(season == max(season) & week == max(week)) %>%
  add_predicted_draws(fit_hist, value = "pred") %>%
  ungroup() %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)


# week
fit_21_week <- update(fit11, newdata = train21, cores = 4)
fit_hist_week <- update(fit_hist1, newdata = train_hist, cores = 4)


pred_21_week <- scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(fit_21_week, value = "pred") %>%
  ungroup() %>%
  select(team, week, score, sim = .draw, pred)

pred_hist_week <- all_teams %>%
  filter(season == max(season) & week == max(week)) %>%
  add_predicted_draws(fit_hist_week, value = "pred") %>%
  ungroup() %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)


# week i2
fit_21_week_i2 <- update(fit12, newdata = train21, cores = 4)
fit_hist_week_i2 <- update(fit_hist2, newdata = train_hist, cores = 4)

pred_21_week_i2 <- scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(fit_21_week_i2, value = "pred") %>%
  ungroup() %>%
  select(team, week, score, sim = .draw, pred)

pred_hist_week_i2 <- all_teams %>%
  filter(season == max(season) & week == max(week)) %>%
  add_predicted_draws(fit_hist_week_i2, value = "pred") %>%
  ungroup() %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)

# week i3
fit_21_week_i3 <- update(fit15, newdata = train21, cores = 4)
fit_hist_week_i3 <- update(fit_hist5, newdata = train_hist, cores = 4)

pred_21_week_i3 <- scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(fit_21_week_i3, value = "pred") %>%
  ungroup() %>%
  select(team, week, score, sim = .draw, pred)

pred_hist_week_i3 <- all_teams %>%
  filter(season == max(season) & week == max(week)) %>%
  add_predicted_draws(fit_hist_week_i3, value = "pred") %>%
  ungroup() %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)

# week poly2
fit_21_week_poly2 <- update(fit13, newdata = train21, cores = 4)
fit_hist_week_poly2 <- update(fit_hist4, newdata = train_hist, cores = 4)

pred_21_week_poly2 <- scores21 %>%
  # filter(week == max(week)) %>%
  add_predicted_draws(fit_21_week_poly2, value = "pred") %>%
  ungroup() %>%
  filter(week == max(week)) %>%
  select(team, week, score, sim = .draw, pred)

pred_hist_week_poly2 <- all_teams %>%
  filter(season == max(season)) %>%
  add_predicted_draws(fit_hist_week_poly2, value = "pred") %>%
  ungroup() %>%
  filter(week == max(week)) %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)

# week poly3
fit_21_week_poly3 <- update(fit14, newdata = train21, cores = 4)
fit_hist_week_poly3 <- update(fit_hist3, newdata = train_hist, cores = 4)

pred_21_week_poly3 <- scores21 %>%
  # filter(week == max(week)) %>%
  add_predicted_draws(fit_21_week_poly3, value = "pred") %>%
  ungroup() %>%
  filter(week == max(week)) %>%
  select(team, week, score, sim = .draw, pred)

pred_hist_week_poly3 <- all_teams %>%
  filter(season == max(season)) %>%
  add_predicted_draws(fit_hist_week_poly3, value = "pred") %>%
  ungroup() %>%
  filter(week == max(week)) %>%
  separate(team_yr, c("team", "season"), sep = "_") %>%
  select(team, week, score, sim = .draw, pred)

mdl_comp <- model_performance(fit_21, fit_hist,
                              fit_21_no_week, fit_hist_no_week,
                              fit_21_week, fit_hist_week,
                              fit_21_week_i2, fit_hist_week_i2,
                              fit_21_week_i3, fit_hist_week_i3,
                              fit_21_week_poly2, fit_hist_week_poly2,
                              fit_21_week_poly3, fit_hist_week_poly3,
                              verbose = F)

save(scores21, all_teams, train21, train_hist,
     fit_21, fit_hist,
     fit_21_no_week, fit_hist_no_week,
     fit_21_week, fit_hist_week,
     fit_21_week_i2, fit_hist_week_i2,
     fit_21_week_i3, fit_hist_week_i3,
     fit_21_week_poly2, fit_hist_week_poly2,
     fit_21_week_poly3, fit_hist_week_poly3,
     mdl_comp,
     file = "dev/week_test.rda")

evaluate_fit <- function(fit) {

fit %>%
    left_join(rename(fit, opp = team, opp_score = score, opp_pred = pred),
              by = c("week", "sim")) %>%
    filter(team != opp) %>%
    mutate(margin = score - opp_score,
           pred_margin = pred - opp_pred,
           error = margin - pred_margin) %>%
    group_by(team, score, opp, opp_score, margin) %>%
    summarize(wp = mean(pred_margin > 0),
              pred_margin = mean(pred_margin),
              error = mean(error),
              .groups = "drop") %>%
    mutate(correct = sign(margin) == sign(pred_margin)) %>%
    summarize(accuracy = mean(correct),
              mae = mean(abs(error)))


}


# Weekly Interaction ------------------------------------------------------

fit_int1 <- rstanarm::stan_glm(score ~ 0 + week * team,
                               data = train21,
                               prior = rstanarm::student_t(10, 115, 10),
                               seed = 42,
                               iter = 3750,
                               warmup = 1250,
                               chains = 4,
                               cores = 4)

fit_int2 <- rstanarm::stan_glm(score ~ 0 + poly(week, 2) * team,
                               data = train21,
                               prior = rstanarm::student_t(10, 115, 10),
                               seed = 42,
                               iter = 3750,
                               warmup = 1250,
                               chains = 4,
                               cores = 4)

fit_int3 <- rstanarm::stan_glm(score ~ 0 + poly(week, 3) * team,
                               data = train21,
                               prior = rstanarm::student_t(10, 115, 10),
                               seed = 42,
                               iter = 3750,
                               warmup = 1250,
                               chains = 4,
                               cores = 4)

fit_int4 <- brm(score ~ 0 + week * team,
                data = train21,
                family = gaussian,
                prior = c(prior(student_t(10, 115, 10), class = 'b'),
                          prior(normal(0, 12), class = 'b', coef = "week"),
                          # prior(exponential(0.04166667), class = 'sd'),
                          prior(exponential(0.04166667), class = 'sigma')),
                seed = 42,
                iter = 3750,
                warmup = 1250,
                chains = 4,
                cores = 4,
                control = list(adapt_delta = 0.99))

fit_int5 <- update(fit_int4,
                   formula = score ~ 0 + poly(week, 2) * team,
                   newdata= train21)

fit_int6 <- update(fit_int4,
                   formula = score ~ 0 + poly(week, 3) * team,
                   newdata= train21)

mdl <- fit_int6

scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(mdl, value = "pred") %>%
  rename(sim = .draw) %>%
  ungroup() %>%
  evaluate_fit()

scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(mdl, value = "pred") %>%
  summarize_predicted_draws(score, pred)


# ARMA --------------------------------------------------------------------

fit_arma1 <- brm(score ~ 0 + team + arma(week, team),
                 data = train21,
                 family = gaussian,
                 prior = c(prior(student_t(10, 115, 10), class = 'b'),
                           prior(normal(0, 2), class = 'ma'),
                           prior(normal(0, 2), class = 'ar'),
                           prior(exponential(0.04166667), class = 'sigma')),
                 seed = 42,
                 iter = 4000,
                 warmup = 1500,
                 chains = 4,
                 cores = 4,
                 file = 'dev/fit_arma1',
                 control = list(adapt_delta = 0.999,
                                max_treedepth = 15))



mdl <- fit_arma1

scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(mdl, value = "pred") %>%
  # rename(sim = .draw) %>%
  ungroup() %>%
  evaluate_fit_matchups(pred)

scores21 %>%
  filter(week == max(week)) %>%
  add_predicted_draws(mdl, value = "pred") %>%
  summarize_predicted_draws(score, pred)

tst <- tibble(week = 2:14) %>%
  mutate(train = map(week, ~filter(scores21, week < .x)),
         test = map(week, ~filter(scores21, week == .x)),
         model = map(train, ~update(fit_arma1, newdata = .x, cores = 4)),
         draws = map2(test, model, ~add_predicted_draws(.x, .y, value = "pred")),
         accuracy = map(draws, ~evaluate_fit_matchups(.x, pred)))

# Normal-Normal -----------------------------------------------------------

ema <- function (x, window = NULL, ratio = NULL) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  c(stats::filter(x = x * ratio, filter = 1 - ratio, method = "recursive", init = x[1]))

}

emsd <- function(x, window = NULL, ratio = NULL, initial = 15) {

  if (is.null(window) & is.null(ratio)) stop("Please select either window or ratio")

  if (!is.null(window)) ratio <- 2 / (window + 1)

  n <- length(x)

  out <- sapply(
    1:n,
    function(i, x, ratio) {
      y <- x[1:i]
      m <- length(y)
      weights <- (1 - ratio)^((m - 1):0)
      ewma <- sum(weights * y) / sum(weights)
      bias <- sum(weights)^2 / (sum(weights)^2 - sum(weights^2))
      ewmsd <- sqrt(bias * sum(weights * (y - ewma)^2) / sum(weights))
    },
    x = x,
    ratio = ratio
  )

  out[1] <- initial

  out
}

scores_full %>%
  group_by(team) %>%
  mutate(avg = ema(score, 4),
         sd = emsd(score, 4),
         post = pmap(list(avg, sd, week),
                     ~ estimate_normal(110, 25, ..1, ..2, ..3))) %>%
  ungroup() %>%
  hoist(post, post_mean = "mean", post_sd = "sd") %>%
  mutate(fvoa = post_mean - 110) %>%
  filter(week == 14) %>%
  arrange(-post_mean)

# Season Simulations ------------------------------------------------------

library(tidyverse)
library(funcyfrech)
library(skimr)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(bayesAB)
rename <- dplyr::rename

theme_set(fvoa::theme_fvoa())

sim_season <- function(score,
                       seed = 42,
                       last_n = 3,
                       change_prob = 0.25,
                       score_adj = 25) {

  set.seed(seed)

  score_tmp <- rep(NA, 15)
  score_tmp[1:length(score)] <- score

  for (i in (length(score) + 1):15) {

    score_last_n <- tail(score_tmp[!is.na(score_tmp)], last_n)

    dist <- estimate_normal(110, 25, mean(score_last_n), sd(score_last_n), length(score_last_n))
    pred <- rnorm(1, dist$mean, dist$sd) +
      rbinom(n = 1, size = 1, prob = change_prob) *
      rnorm(1, 0, score_adj)
    score_tmp[i] <- pred

  }

  tibble(week = 1:15, score = score_tmp)

}

schedule <- tibble::tribble(
  ~season, ~week,     ~team, ~opponent,
  2022L,    1L, "Barrett",   "David",
  2022L,    1L,   "Bobby",   "Scott",
  2022L,    1L,  "German",   "PFinn",
  2022L,    1L,  "Justin",    "Diaz",
  2022L,    1L,    "Josh",    "Eric",
  2022L,    1L,   "David", "Barrett",
  2022L,    1L,   "Scott",   "Bobby",
  2022L,    1L,   "PFinn",  "German",
  2022L,    1L,    "Diaz",  "Justin",
  2022L,    1L,    "Eric",    "Josh",
  2022L,    2L, "Barrett",  "Justin",
  2022L,    2L,   "Bobby",    "Diaz",
  2022L,    2L,   "Scott",    "Josh",
  2022L,    2L,  "German",    "Eric",
  2022L,    2L,   "PFinn",   "David",
  2022L,    2L,  "Justin", "Barrett",
  2022L,    2L,    "Diaz",   "Bobby",
  2022L,    2L,    "Josh",   "Scott",
  2022L,    2L,    "Eric",  "German",
  2022L,    2L,   "David",   "PFinn",
  2022L,    3L, "Barrett",   "Bobby",
  2022L,    3L,   "Scott",  "German",
  2022L,    3L,  "Justin",   "David",
  2022L,    3L,   "PFinn",    "Eric",
  2022L,    3L,    "Josh",    "Diaz",
  2022L,    3L,   "Bobby", "Barrett",
  2022L,    3L,  "German",   "Scott",
  2022L,    3L,   "David",  "Justin",
  2022L,    3L,    "Eric",   "PFinn",
  2022L,    3L,    "Diaz",    "Josh",
  2022L,    4L, "Barrett",    "Josh",
  2022L,    4L,   "Bobby",  "Justin",
  2022L,    4L,   "Scott",   "PFinn",
  2022L,    4L,  "German",    "Diaz",
  2022L,    4L,   "David",    "Eric",
  2022L,    4L,    "Josh", "Barrett",
  2022L,    4L,  "Justin",   "Bobby",
  2022L,    4L,   "PFinn",   "Scott",
  2022L,    4L,    "Diaz",  "German",
  2022L,    4L,    "Eric",   "David",
  2022L,    5L, "Barrett",  "German",
  2022L,    5L,   "Bobby",   "David",
  2022L,    5L,   "Scott",    "Eric",
  2022L,    5L,  "Justin",    "Josh",
  2022L,    5L,   "PFinn",    "Diaz",
  2022L,    5L,  "German", "Barrett",
  2022L,    5L,   "David",   "Bobby",
  2022L,    5L,    "Eric",   "Scott",
  2022L,    5L,    "Josh",  "Justin",
  2022L,    5L,    "Diaz",   "PFinn",
  2022L,    6L, "Barrett",   "PFinn",
  2022L,    6L,   "Bobby",    "Josh",
  2022L,    6L,   "Scott",   "David",
  2022L,    6L,  "German",  "Justin",
  2022L,    6L,    "Diaz",    "Eric",
  2022L,    6L,   "PFinn", "Barrett",
  2022L,    6L,    "Josh",   "Bobby",
  2022L,    6L,   "David",   "Scott",
  2022L,    6L,  "Justin",  "German",
  2022L,    6L,    "Eric",    "Diaz",
  2022L,    7L, "Barrett",    "Eric",
  2022L,    7L,   "Bobby",  "German",
  2022L,    7L,   "Scott",    "Diaz",
  2022L,    7L,  "Justin",   "PFinn",
  2022L,    7L,    "Josh",   "David",
  2022L,    7L,    "Eric", "Barrett",
  2022L,    7L,  "German",   "Bobby",
  2022L,    7L,    "Diaz",   "Scott",
  2022L,    7L,   "PFinn",  "Justin",
  2022L,    7L,   "David",    "Josh",
  2022L,    8L, "Barrett",   "Scott",
  2022L,    8L,   "Bobby",   "PFinn",
  2022L,    8L,  "German",    "Josh",
  2022L,    8L,  "Justin",    "Eric",
  2022L,    8L,   "David",    "Diaz",
  2022L,    8L,   "Scott", "Barrett",
  2022L,    8L,   "PFinn",   "Bobby",
  2022L,    8L,    "Josh",  "German",
  2022L,    8L,    "Eric",  "Justin",
  2022L,    8L,    "Diaz",   "David",
  2022L,    9L, "Barrett",    "Diaz",
  2022L,    9L,   "Bobby",    "Eric",
  2022L,    9L,   "Scott",  "Justin",
  2022L,    9L,  "German",   "David",
  2022L,    9L,   "PFinn",    "Josh",
  2022L,    9L,    "Diaz", "Barrett",
  2022L,    9L,    "Eric",   "Bobby",
  2022L,    9L,  "Justin",   "Scott",
  2022L,    9L,   "David",  "German",
  2022L,    9L,    "Josh",   "PFinn",
  2022L,   10L, "Barrett",   "David",
  2022L,   10L,   "Bobby",   "Scott",
  2022L,   10L,  "German",   "PFinn",
  2022L,   10L,  "Justin",    "Diaz",
  2022L,   10L,    "Josh",    "Eric",
  2022L,   10L,   "David", "Barrett",
  2022L,   10L,   "Scott",   "Bobby",
  2022L,   10L,   "PFinn",  "German",
  2022L,   10L,    "Diaz",  "Justin",
  2022L,   10L,    "Eric",    "Josh",
  2022L,   11L, "Barrett",  "Justin",
  2022L,   11L,   "Bobby",    "Diaz",
  2022L,   11L,   "Scott",    "Josh",
  2022L,   11L,  "German",    "Eric",
  2022L,   11L,   "PFinn",   "David",
  2022L,   11L,  "Justin", "Barrett",
  2022L,   11L,    "Diaz",   "Bobby",
  2022L,   11L,    "Josh",   "Scott",
  2022L,   11L,    "Eric",  "German",
  2022L,   11L,   "David",   "PFinn",
  2022L,   12L, "Barrett",   "Bobby",
  2022L,   12L,   "Scott",  "German",
  2022L,   12L,  "Justin",   "David",
  2022L,   12L,   "PFinn",    "Eric",
  2022L,   12L,    "Josh",    "Diaz",
  2022L,   12L,   "Bobby", "Barrett",
  2022L,   12L,  "German",   "Scott",
  2022L,   12L,   "David",  "Justin",
  2022L,   12L,    "Eric",   "PFinn",
  2022L,   12L,    "Diaz",    "Josh",
  2022L,   13L, "Barrett",    "Josh",
  2022L,   13L,   "Bobby",  "Justin",
  2022L,   13L,   "Scott",   "PFinn",
  2022L,   13L,  "German",    "Diaz",
  2022L,   13L,   "David",    "Eric",
  2022L,   13L,    "Josh", "Barrett",
  2022L,   13L,  "Justin",   "Bobby",
  2022L,   13L,   "PFinn",   "Scott",
  2022L,   13L,    "Diaz",  "German",
  2022L,   13L,    "Eric",   "David",
  2022L,   14L, "Barrett",  "German",
  2022L,   14L,   "Bobby",   "David",
  2022L,   14L,   "Scott",    "Eric",
  2022L,   14L,  "Justin",    "Josh",
  2022L,   14L,   "PFinn",    "Diaz",
  2022L,   14L,  "German", "Barrett",
  2022L,   14L,   "David",   "Bobby",
  2022L,   14L,    "Eric",   "Scott",
  2022L,   14L,    "Josh",  "Justin",
  2022L,   14L,    "Diaz",   "PFinn",
  2022L,   15L, "Barrett",   "PFinn",
  2022L,   15L,   "Bobby",    "Josh",
  2022L,   15L,   "Scott",   "David",
  2022L,   15L,  "German",  "Justin",
  2022L,   15L,    "Diaz",    "Eric",
  2022L,   15L,   "PFinn", "Barrett",
  2022L,   15L,    "Josh",   "Bobby",
  2022L,   15L,   "David",   "Scott",
  2022L,   15L,  "Justin",  "German",
  2022L,   15L,    "Eric",    "Diaz"
)

clt_full <- tibble::tribble(
  ~week,     ~team, ~score,
  1L, "Barrett",  100.7,
  1L,   "Bobby", 118.13,
  1L,   "Scott",  124.3,
  1L,  "German", 105.57,
  1L,  "Justin", 117.73,
  1L,   "PFinn",  105.6,
  1L,    "Josh",    144,
  1L,   "David", 129.07,
  1L,    "Diaz",    130,
  1L,    "Eric",  94.37,
  2L, "Barrett",  142.5,
  2L,   "Bobby", 126.23,
  2L,   "Scott", 140.07,
  2L,  "German",   66.9,
  2L,  "Justin",   93.1,
  2L,   "PFinn",  133.2,
  2L,    "Josh",  97.33,
  2L,   "David", 133.93,
  2L,    "Diaz", 115.53,
  2L,    "Eric", 114.23,
  3L, "Barrett", 103.33,
  3L,   "Bobby",  89.37,
  3L,   "Scott", 128.33,
  3L,  "German", 109.87,
  3L,  "Justin",     98,
  3L,   "PFinn", 107.97,
  3L,    "Josh", 103.03,
  3L,   "David",  86.63,
  3L,    "Diaz",  112.3,
  3L,    "Eric",  67.63,
  4L, "Barrett", 106.87,
  4L,   "Bobby",  128.1,
  4L,   "Scott",  125.5,
  4L,  "German", 107.77,
  4L,  "Justin",  79.07,
  4L,   "PFinn",  108.8,
  4L,    "Josh",  101.1,
  4L,   "David", 138.03,
  4L,    "Diaz", 129.23,
  4L,    "Eric",  133.2,
  5L, "Barrett", 111.87,
  5L,   "Bobby", 108.43,
  5L,   "Scott", 171.73,
  5L,  "German",  87.93,
  5L,  "Justin",  80.43,
  5L,   "PFinn",    119,
  5L,    "Josh", 112.43,
  5L,   "David", 117.97,
  5L,    "Diaz",  138.3,
  5L,    "Eric",  78.33,
  6L, "Barrett",  73.17,
  6L,   "Bobby",  98.83,
  6L,   "Scott", 139.47,
  6L,  "German",    103,
  6L,  "Justin",    126,
  6L,   "PFinn",   88.6,
  6L,    "Josh",  97.67,
  6L,   "David",   95.3,
  6L,    "Diaz",  68.03,
  6L,    "Eric", 102.27,
  7L, "Barrett",   89.1,
  7L,   "Bobby",  103.5,
  7L,   "Scott",    113,
  7L,  "German", 108.13,
  7L,  "Justin", 130.73,
  7L,   "PFinn",   87.4,
  7L,    "Josh",  126.8,
  7L,   "David",  81.77,
  7L,    "Diaz",  96.07,
  7L,    "Eric",   79.1,
  8L, "Barrett",  128.8,
  8L,   "Bobby", 179.17,
  8L,   "Scott",  78.17,
  8L,  "German", 103.93,
  8L,  "Justin",  99.87,
  8L,   "PFinn", 119.63,
  8L,    "Josh", 168.77,
  8L,   "David", 128.43,
  8L,    "Diaz", 149.53,
  8L,    "Eric", 137.73,
  9L, "Barrett",   78.9,
  9L,   "Bobby", 127.46,
  9L,   "Scott", 102.93,
  9L,  "German", 125.77,
  9L,  "Justin", 125.93,
  9L,   "PFinn",  96.63,
  9L,    "Josh", 127.27,
  9L,   "David", 123.87,
  9L,    "Diaz",  98.47,
  9L,    "Eric",  114.8,
  10L, "Barrett", 103.53,
  10L,   "Bobby", 112.17,
  10L,   "Scott",  110.3,
  10L,  "German",  101.1,
  10L,  "Justin", 121.77,
  10L,   "PFinn",   58.5,
  10L,    "Josh",  96.63,
  10L,   "David", 109.47,
  10L,    "Diaz",   90.3,
  10L,    "Eric", 132.47,
  11L, "Barrett", 103.73,
  11L,   "Bobby", 110.44,
  11L,   "Scott", 119.37,
  11L,  "German", 126.33,
  11L,  "Justin", 136.23,
  11L,   "PFinn",  97.97,
  11L,    "Josh",  114.9,
  11L,   "David", 117.73,
  11L,    "Diaz",  81.83,
  11L,    "Eric",  116.2,
  12L, "Barrett",  125.4,
  12L,   "Bobby", 120.27,
  12L,   "Scott", 151.73,
  12L,  "German",  111.2,
  12L,  "Justin",  124.5,
  12L,   "PFinn",  95.47,
  12L,    "Josh",  95.47,
  12L,   "David",    118,
  12L,    "Diaz", 101.87,
  12L,    "Eric",  117.1,
  13L, "Barrett", 114.87,
  13L,   "Bobby", 119.63,
  13L,   "Scott", 116.13,
  13L,  "German", 118.33,
  13L,  "Justin", 112.37,
  13L,   "PFinn",  91.57,
  13L,    "Josh",  90.93,
  13L,   "David",  86.37,
  13L,    "Diaz", 106.07,
  13L,    "Eric", 122.17,
  14L, "Barrett",  91.23,
  14L,   "Bobby",  142.6,
  14L,   "Scott",  119.4,
  14L,  "German",  73.97,
  14L,  "Justin", 144.77,
  14L,   "PFinn",  92.67,
  14L,    "Josh",  95.43,
  14L,   "David",  127.3,
  14L,    "Diaz",  99.63,
  14L,    "Eric",  97.87,
  15L, "Barrett",  147.5,
  15L,   "Bobby",  96.27,
  15L,   "Scott", 135.33,
  15L,  "German", 123.37,
  15L,  "Justin",    108,
  15L,   "PFinn", 145.83,
  15L,    "Josh",  138.3,
  15L,   "David",  93.97,
  15L,    "Diaz",  84.23,
  15L,    "Eric", 120.43
)

wk <- 2
clt <- filter(clt_full, week <= wk)

c <- mutate(clt, score_c = score - 110)

m <- stan_glmer(data = c,
                score ~ week + (week | team),
                # score ~ 0 + week + (week | team),
                # score ~ week + I(week^2) + (week + I(week^2) | team),
                # score ~ poly(week, 2) + (poly(week, 2) | team),
                # score ~ poly(week, 3) + (poly(week, 3) | team),
                # score ~ 1 + (week | team),
                # score ~ 0 + week + I(week^2) + (week + I(week^2) | team),
                prior = normal(0, 25),
                prior_intercept = student_t(10, 110, 35),
                prior_aux = exponential(rate = 1, autoscale = T),
                prior_covariance = decov(1, 1, 1, 1),
                seed = 42,
                iter = 3750,
                warmup = 1250,
                cores = 4,
                chains = 4)

tidy(m, effects = 'fixed', robust = T, conf.int = T)
tidy(m, effects = 'ran_pars', robust = T, conf.int = T)
arrange(tidy(m, effects = 'ran_vals', robust = T, conf.int = T), term, -estimate)
tidyMCMC(m, robust = T, conf.int = T) %>% View()

tmp <- add_epred_draws(tibble(week = wk, team = unique(c$team)), m, seed = 42) %>%
  mutate(fvoa = .epred - 110) %>%
  median_hdi(fvoa, .width = c(.89, .5)) %>%
  arrange(-fvoa) %>%
  mutate(label = str_glue("{team} ({round(fvoa, 1)})")) %>%
  arrange(-fvoa)

tmp %>%
  ggplot(aes(y = reorder(team, fvoa),
             yend = reorder(team, fvoa))) +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.89),
               size = 0.5, color = "#6497b1") +
  geom_segment(aes(x = .lower, xend = .upper),
               data = filter(tmp, .width == 0.5),
               size = 2, color = "#03396c") +
  geom_point(aes(x = fvoa),
             size = 4, fill = "#d1e1ec", color = "#011f4b", shape = 21) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(x = "FVOA", y = NULL) +
  theme_fvoa() +
  theme(axis.text.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.grid.major.y = element_blank()) +
  geom_text(aes(label = label, x = fvoa),
            vjust = -1, alpha = 0.5, size = 3.5) +
  theme(axis.text.y = element_blank(),
        panel.border = element_blank())

p <- tibble(week = wk + 1, team = unique(c$team)) %>%
  add_predicted_draws(m, ndraws = 1000) %>%
  ungroup() %>%
  select(sim = .draw, week, team, score = .prediction)

sims <- crossing(sim = 1:1000, c) %>%
  bind_rows(p) %>%
  nest(.by = c(team, sim)) %>%
  mutate(pred = map2(data, sim, ~sim_season(.x$score, .y, last_n = 3))) %>%
  select(-data) %>%
  unnest(pred)

crossing(sim = 1:1000, schedule) %>%
  left_join(rename(sims, score1 = score), by = c('team', 'sim', 'week')) %>%
  left_join(rename(sims, opponent = team, score2 = score), by = c('opponent', 'sim', 'week')) %>%
  simulate_season_standings() %>%
  simulate_final_standings()

sims %>%
  nest(.by = sim) %>%
  sample_n(100) %>%
  unnest(data) %>%
  ggplot(aes(week, score)) +
  geom_line(alpha = 0.05, aes(group = sim)) +
  geom_smooth(se = F) +
  # geom_line(data = filter(clt_full, week >= wk + 1)) +
  geom_hline(yintercept = 110, color = 'red', linetype = 2) +
  # geom_smooth(data = c, se = F, method = 'lm', formula = 'y ~ x') +
  scale_x_continuous(breaks = 1:15) +
  facet_wrap(~ team, scales = "free_x")

c %>%
  ggplot(aes(week, score)) +
  geom_line() +
  geom_smooth(se = F, method = 'lm', formula = 'y ~ x') +
  stat_lineribbon(data = filter(sims, week >= wk + 1)) +
  geom_hline(yintercept = 110, color = 'red', linetype = 2) +
  scale_x_continuous(breaks = 1:15) +
  facet_wrap(~ team, scales = "free_x")

q <- crossing(week = (wk + 1):15, team = unique(c$team)) %>%
  add_predicted_draws(m, ndraws = 10000) %>%
  ungroup() %>%
  # mutate(.prediction = .prediction + 110) %>%
  select(sim = .draw, week, team, score = .prediction)

qq <- bind_rows(crossing(sim = 1:10000, c), q)

crossing(sim = 1:10000, schedule) %>%
  left_join(rename(qq, score1 = score), by = c('team', 'sim', 'week')) %>%
  left_join(rename(qq, opponent = team, score2 = score), by = c('opponent', 'sim', 'week')) %>%
  simulate_season_standings() %>%
  simulate_final_standings()

c %>%
  ggplot(aes(week, score)) +
  geom_line() +
  geom_smooth(se = F, method = 'lm', formula = 'y ~ x') +
  stat_lineribbon(data = q) +
  geom_hline(yintercept = 110, color = 'red', linetype = 2) +
  scale_x_continuous(breaks = 1:15) +
  facet_wrap(~ team, scales = "free_x")
