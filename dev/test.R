
# Setup -------------------------------------------------------------------

library(tidyverse)
library(ffscrapr)
library(funcyfrech)
library(skimr)

theme_set(fvoa::theme_fvoa())

sx_con <- espn_connect(2021, 299999)

owners <- ff_franchises(sx_con) %>%
  select(teamID = 1, team = 2)

schedule <- get_schedule(season = 2020) %>%
  left_join(owners, by = "teamID") %>%
  left_join(rename(owners, opponent = team, opponentID = teamID), by = "opponentID") %>%
  select(week, team, opponent)

teams <- map_df(1:15, ~get_team(week = ., season = 2020)) %>%
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
            file = "data/fit1",
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
            file = 'data/fit2',
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
            file = 'data/fit3',
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
            file = 'data/fit4',
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
            file = 'data/fit5',
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
            file = 'data/fit6',
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
            file = 'data/fit7',
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
            file = 'data/fit8',
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
            file = 'data/fit9',
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
             file = 'data/fit10',
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
             file = 'data/fit11',
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
            file = 'data/fit12',
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
                file = 'data/fit13',
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
                file = 'data/fit14',
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
                file = 'data/fit15',
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

mdl <- fit14
s <- scores

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

crossing(week = 11:15,
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
                 file = 'data/fit_hist1',
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
                 file = 'data/fit_hist2',
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
                 file = 'data/fit_hist3',
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
                 file = 'data/fit_hist4',
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
                 file = 'data/fit_hist5',
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
     file = "data/week_test.rda")

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


# Functions ---------------------------------------------------------------

scores %>%
  add_predicted_draws(fit, value = "pred") %>%
  summarize(mae = mean(abs(score - pred)),
            l50 = quantile(pred, 0.25),
            u50 = quantile(pred, 0.75),
            l80 = quantile(pred, 0.1),
            u80 = quantile(pred, 0.9),
            l95 = quantile(pred, 0.025),
            u95 = quantile(pred, 0.975)) %>%
  mutate(within50 = between(score, l50, u50),
         within80 = between(score, l80, u80),
         within95 = between(score, l95, u95)) %>%
  ungroup() %>%
  mutate(sd = sd(score),
         mae_scaled = mae / sd) %>%
  summarize(mae = mean(mae),
            mae_scaled = mean(mae_scaled),
            within50 = mean(within50),
            within80 = mean(within80),
            within95 = mean(within95))

summarize_predicted_draws <- function(predicted_draws,
                                      truth = score,
                                      prediction = pred) {

  predicted_draws %>%
    summarize(mae = mean(abs({{truth}} - {{prediction}})),
              l50 = quantile({{prediction}}, 0.25),
              u50 = quantile({{prediction}}, 0.75),
              l80 = quantile({{prediction}}, 0.1),
              u80 = quantile({{prediction}}, 0.9),
              l95 = quantile({{prediction}}, 0.025),
              u95 = quantile({{prediction}}, 0.975),
              .groups = "keep") %>%
    mutate(within50 = between({{truth}}, l50, u50),
           within80 = between({{truth}}, l80, u80),
           within95 = between({{truth}}, l95, u95)) %>%
    ungroup() %>%
    mutate(sd = sd({{truth}}),
           mae_scaled = mae / sd) %>%
    summarize(mae = mean(mae),
              mae_scaled = mean(mae_scaled),
              within50 = mean(within50),
              within80 = mean(within80),
              within95 = mean(within95))

}
