
# Main Functions ----------------------------------------------------------

#' @export
get_schedule <- function(league = c("espn", "yahoo"),
                         leagueID = NULL,
                         season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if(league == "yahoo") {

    if(is.null(leagueID)) leagueID <- 102347

    map_df(1:15, ~ get_yahoo_schedule(leagueID, .x, season))

  } else if (league == "espn") {

    if (!requireNamespace("ffscrapr", quietly = TRUE)) {
      stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(is.null(leagueID)) leagueID <- 299999

    ffscrapr::espn_connect(season, leagueID) %>%
      ffscrapr::ff_schedule() %>%
      select(week, teamID = 2, team_score = 3, opponentID = 5, opponent_score = 6)

  }
}

#' @export
get_team <- function(week,
                     league = c("espn", "yahoo"),
                     leagueID = NULL,
                     season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if (league == "yahoo") {

    if(is.null(leagueID)) leagueID <- 102347

    get_yahoo_teamIDs(leagueID) %>%
      select(teamID) %>%
      pull() %>%
      map_df(~ get_yahoo_team(week, .x, leagueID, season))

  } else if (league == "espn") {

    if (!requireNamespace("ffscrapr", quietly = TRUE)) {
      stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(is.null(leagueID)) leagueID <- 299999

    ffscrapr::espn_connect(season, leagueID) %>%
      ffscrapr::ff_starters(week = week) %>%
      select(week, teamID = 2, #team = 3,
             score = 4, playerID = 8, player = 9, position = 10,
             roster = 5, projected = 7, points = 6)

  } else {

    print("Wrong league")

  }

}

#' @export
get_win_prob <- function(week,
                         league = "yahoo",
                         leagueID = NULL,
                         season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if(is.null(leagueID)) leagueID <- 102347

  get_yahoo_teamIDs(leagueID) %>%
    crossing(week) %>%
    mutate(prob = pmap_chr(list(week, teamID, leagueID), get_yahoo_winprob),
           wp = str_extract(prob, "[:digit:]+"),
           wp = as.numeric(wp)/100) %>%
    select(teamID, wp)

}

#' @export
get_players <- function(week,
                        league = c("espn", "yahoo"),
                        leagueID = NULL,
                        season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  player_table <- ffscrapr::dp_playerids() %>%
    select(player = name, team, position, age, draft_year,
           espnID = espn_id, yahooID = yahoo_id, mflID = mfl_id) %>%
    mutate(espnID = as.integer(espnID)) %>%
    bind_rows(defenseIDs)

  if (league == "yahoo") {

    if(is.null(leagueID)) leagueID <- 102347

    yahoo_players <- crossing(position = c("QB", "RB", "WR", "TE",
                                           "K", "DEF", "DB", "DL"),
                              page = seq(0, 75, by = 25)) %>%
      mutate(data = map2(position, page,
                         ~ get_yahoo_players(week, .x, .y, leagueID))) %>%
      unnest(data) %>%
      select(yahooID = playerID, player, position, teamID) %>%
      mutate(position = if_else(position == "DEF", "DST", position),
             teamID = if_else(str_detect(teamID, "^[A-Z] [()]|^0$"), "0", teamID))

    # add in players without ID
    tmp <- yahoo_players %>%
      inner_join(select(player_table, yahooID, mflID), by = "yahooID")

    out <- bind_rows(tmp,
                     yahoo_players %>%
                       anti_join(tmp, by = 'yahooID') %>%
                       left_join(select(player_table, player, mflID), by = "player"))

  } else if (league == "espn") {

    if (!requireNamespace("ffscrapr", quietly = TRUE)) {
      stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(is.null(leagueID)) leagueID <- 299999

    conn <- ffscrapr::espn_connect(season = season, league_id = leagueID)

    espn_players <- ffscrapr::espn_players(season = season) %>%
      filter(pos %in% c("QB", "RB", "WR", "TE", "K", "DST"),
             team != "FA") %>%
      select(espnID = 1, player = 2, position = 3) %>%
      left_join(ffscrapr::ff_rosters(conn, week = week) %>%
                  select(teamID = 1, espnID = 3),
                by = "espnID") %>%
      replace_na(list(teamID = 0L))

    # add in players without ID
    tmp <- espn_players %>%
      inner_join(select(player_table, espnID, mflID), by = "espnID")

    out <- bind_rows(tmp,
                     espn_players %>%
                       anti_join(tmp, by = 'espnID') %>%
                       left_join(select(player_table, player, mflID, ffprosID), by = "player"))

  } else {

    print("Wrong league")

  }

  mutate(out,
         teamID = as.integer(teamID))#,
  # mflID = as.integer(mflID))

}

#' @export
get_current_roster <- function(week,
                               league = c("espn", "yahoo"),
                               leagueID = NULL,
                               season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if (league == "espn") {

    if (!requireNamespace("ffscrapr", quietly = TRUE)) {
      stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if(is.null(leagueID)) leagueID <- 299999

    conn <- ffscrapr::espn_connect(season, leagueID)

    ffscrapr::espn_getendpoint(conn, view = "mMatchup", scoringPeriodId = week) %>%
      pluck("content", "teams") %>%
      tibble() %>%
      hoist(1, "teamID" = "id", "roster") %>%
      hoist(roster, "entries") %>%
      select(-roster) %>%
      unnest(entries) %>%
      hoist(entries, "playerID" = "playerId", "roster" = "lineupSlotId", "injuryStatus", "status", "playerPoolEntry") %>%
      mutate(roster = case_when(
        roster == 0 ~ "QB",
        roster == 2 ~ "RB",
        roster == 3 ~ "RB/WR",
        roster == 4 ~ "WR",
        roster == 6 ~ "TE",
        roster == 16 ~ "DST",
        roster == 17 ~ "K",
        roster == 20 ~ "BE",
        roster == 23 ~ "FLEX",
        TRUE ~ "Other"
      )) %>%
      select(-entries) %>%
      hoist(playerPoolEntry, "player_projection" = "appliedStatTotal", "player") %>%
      select(-playerPoolEntry) %>%
      hoist(player, "fullName", "injured", "injury" = "injuryStatus") %>%
      #remove player_projection, injuryStatus, and status
      select(teamID, playerID, player = fullName, roster, injured, injury)

  } else {

    print("Yahoo not ready")

  }

}

#' @export
get_ffa_data <- function(week = week,
                         src = c('CBS',
                                 'ESPN',
                                 #'FantasyData', #paywall
                                 'FantasyPros',
                                 'FantasySharks',
                                 'FFToday', #errored
                                 'FleaFlicker',
                                 'NumberFire',
                                 # 'Yahoo',
                                 'FantasyFootballNerd', #not supported yet
                                 'RTSports',
                                 'Walterfootball',
                                 'NFL'), #
                         pos = c("QB", "RB", "WR", "TE",
                                 "K", "DST", "DL", "DB"),
                         season = as.numeric(format(Sys.Date(),'%Y'))) {

  if (!requireNamespace("ffanalytics", quietly = TRUE)) {
    stop("Package \"ffanalytics\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  ffanalytics::scrape_data(src = src,
                           pos = pos,
                           season = season,
                           week = week)

}

# Helper Functions --------------------------------------------------------

#' @export
extract_scores <- function(team) {

  team %>%
    select(week, starts_with("team"), score) %>%
    distinct()

}

#' @export
extract_projections <- function(team) {

  team %>%
    dplyr::filter(!roster %in% c("BN", "IR", "BE")) %>%
    dplyr::select(week, team, points, projected) %>%
    dplyr::group_by(week, team) %>%
    dplyr::summarize(projected = sum(projected, na.rm = T),
                     actual = sum(points, na.rm = T),
                     .groups = "drop")

}

valid_teamID <- function(leagueID, teamID) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/",
                leagueID, "/teams")

  page <- xml2::read_html(url)

  exists <- page %>%
    rvest::html_nodes(str_glue(".team-{teamID}")) %>%
    length() == 1

  if(exists) {

    page %>%
      rvest::html_nodes(str_glue(".team-{teamID}")) %>%
      rvest::html_text() %>%
      as_tibble() %>%
      mutate(Team = word(value, sep = "-")) %>%
      pull(Team)

  } else {
    NA
  }

}

get_yahoo_teamIDs <- function(leagueID, teamID = 1:20) {

  tibble(teamID) %>%
    mutate(team = map(teamID, ~ valid_teamID(leagueID, .x))) %>%
    unnest(team) %>%
    filter(!is.na(team))

}

get_yahoo_schedule <- function(leagueID, week,
                               season = as.numeric(format(Sys.Date(),'%Y'))) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID, "?matchup_week=", week, "&module=matchups&lhst=matchups")

  teamIDs <- rvest::read_html(url) %>%
    rvest::html_nodes("#matchupweek .F-link") %>%
    rvest::html_attr("href") %>%
    str_extract("\\d*$") %>%
    as_tibble() %>%
    mutate(odd = if_else(!row_number() %% 2, "opponentID", "teamID"),
           gameID = (row_number() + 1) %/% 2) %>%
    spread(odd, value) %>%
    select(teamID, opponentID)

  tmp <- rvest::read_html(url) %>%
    rvest::html_nodes("#matchupweek") %>%
    # rvest::html_nodes("#matchupweek .List-rich") %>%
    rvest::html_text() %>%
    str_remove_all(., "\\n|   ") %>%
    as_tibble() %>%
    separate_rows(value, sep = "View Matchup") %>%
    # filter(value != "") %>%
    slice_tail(n = 5) %>%
    mutate(value = str_squish(value)) %>%
    separate(value, c("team1", "team2"), sep = " vs ") %>%
    mutate(gameID = row_number(),
           week = week,
           team1 = str_remove(team1, "\\| "),
           team2 = str_remove(team2, "\\| ")) %>%
    # extract(team1, c("team_projected", "team_score", "team", "team_record"),
    #         "(\\d+\\.\\d+)  ([0-9.]+) +(.*)  ([0-9-)]+)") %>%
    extract(team1, c("team", "team_record", "team_place", "team_score"),
            "(.*) ([0-9-)]+) ([0-9]+[a-z]{2}) (\\d+\\.\\d+)") %>%
      extract(team2, c("opponent_score", "opponent", "opponent_record", "opponent_place"),
              "(\\d+\\.\\d+) (.*) ([0-9-)]+) ([0-9]+[a-z]{2})") %>%
    # extract(team2, c("opponent_projected", "opponent_score", "opponent", "opponent_record"),
    #         "(\\d+\\.\\d+)  ([0-9.]+) +(.*)  ([0-9-)]+)") %>%
    bind_cols(teamIDs) %>%
    select(week, gameID, teamID, team_score, opponentID, opponent_score)

  bind_rows(tmp,
            rename(tmp, opponentID = 3, opponent_score = 4, teamID = 5, team_score = 6))

}

get_yahoo_team <- function(week, teamID,
                           leagueID = 102347,
                           season = as.numeric(format(Sys.Date(),'%Y'))) {

  team_url <- paste0("https://football.fantasysports.yahoo.com/", season,
                     "/f1/", leagueID,
                     "/matchup?week=", week, "&mid1=", teamID)

  page <- xml2::read_html(team_url)

  starters <- page %>%
    rvest::html_nodes("#statTable1") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    select(1:5) %>%
    as_tibble() %>%
    filter(Pos != "Total") %>%
    mutate(`Fan Pts` = as.numeric(`Fan Pts`)) %>%
    replace_na(list(`Fan Pts` = 0))

  bench <- page %>%
    rvest::html_nodes("#statTable2") %>%
    rvest::html_table(fill = T) %>%
    .[[1]] %>%
    select(1:5) %>%
    as_tibble() %>%
    filter(Pos != "Total") %>%
    mutate(Proj = as.numeric(Proj),
           `Fan Pts` = as.numeric(`Fan Pts`)) %>%
    replace_na(list(`Fan Pts` = 0))

  bind_rows(starters,
            bench) %>%
    rename(points = `Fan Pts`,
           projected = Proj,
           roster = Pos) %>%
    separate(Player, c("notes", "player"), "Notes\\s+|Note\\s+") %>%
    separate(player, c("player", "result"), "Final|Bye") %>%
    separate(player, c("player", "position"), " - ") %>%
    mutate(teamID = teamID,
           week = week,
           player = str_replace(player, "\\s[:alpha:]+$", ""),
           position = str_extract(position, "[:alpha:]+"),
           score = sum(starters$`Fan Pts`, na.rm = T),
           across(c(week, teamID), as.integer)) %>%
    drop_na(player) %>%
    select(week, teamID, score, player, #playerID,
           position, roster, projected, points)

}

get_yahoo_winprob <- function(week, teamID,
                              leagueID = 102347) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID,
                "/matchup?week=", week, "&mid1=", teamID)

  page <- xml2::read_html(url)

  page %>%
    rvest::html_nodes(".Pend-med") %>%
    rvest::html_text() %>%
    .[[2]]

}

get_yahoo_players <- function(week, position, page,
                              leagueID = 102347) {

  url <- str_glue("https://football.fantasysports.yahoo.com/f1/{leagueID}/players?status=ALL&pos={position}&cut_type=9&stat1=S_PW_{week}&myteam=0&sort=AR&sdir=1&count={page}")

  page <- rvest::read_html(url)

  if(position %in% c("K", "DL", "DB")) {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             map(get_yahoo_player_team) %>%
             unlist())

  } else if (position == "DEF") {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_attr("href") %>%
             str_extract("[a-z-]*/$") %>%
             str_remove("/"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             map(get_yahoo_player_team) %>%
             unlist())

  } else {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             # .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             # .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link playernote"]') %>%
             rvest::html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             # .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Alt Ta-start Nowrap Bdrend"]') %>%
             map(get_yahoo_player_team) %>%
             unlist())

  }

}

get_yahoo_player_team <- function(x) {

  team <- rvest::html_text(x)

  if (str_detect(team, "^[A-Z] [()]|^FA$")) {

    return("0")

  } else {

    x %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr('href') %>%
      str_extract("\\d*$")

  }

}

slot_name_to_id <- function(x) {
  dplyr::case_when(
    x == "QB" ~ 0L,
    x == "TQB" ~ 1L, # team quarterback
    x == "RB" ~ 2L,
    x == "RB/WR" ~ 3L,
    x == "WR" ~ 4L,
    x == "WR/TE" ~ 5L,
    x == "TE" ~ 6L,
    x == "OP" ~ 7L, # offensive player
    x == "DT" ~ 8L,
    x == "DE" ~ 9L,
    x == "LB" ~ 10L,
    x == "DL" ~ 11L,
    x == "CB" ~ 12L,
    x == "S" ~ 13L,
    x == "DB" ~ 14L,
    x == "DP" ~ 15L, # defensive player
    x == "DST" ~ 16L,
    x == "K" ~ 17L,
    x == "P" ~ 18L,
    x == "HC" ~ 19L, # head coach
    x == "FLEX" ~ 23L,
    x == "EDR" ~ 24L,
    TRUE ~ NA_integer_
  )
}

get_espn_players <- function(week,
                             leagueID = 299999,
                             season = as.numeric(format(Sys.Date(),'%Y')),
                             pos = c("QB", "TQB", "RB", "RB/WR", "WR",
                                     "WR/TE", "TE", "OP",
                                     "DT", "DE", "LB", "DL", "CB",
                                     "S", "DB", "DP", "DST",
                                     "K", "P", "HC", "FLEX", "EDR"),
                             projections = TRUE) {

  if (!requireNamespace("ffscrapr", quietly = TRUE)) {
    stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

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

  out <- player_scores$content %>%
    as_tibble() %>%
    select(players) %>%
    hoist(players, espnID = "id", teamID = "onTeamId", "player", "ratings", "status", "rosterLocked",
          "tradeLocked", #"keeperValue", "keeperValueFuture",
          "lineupLocked") %>%
    select(-players) %>%
    unnest(ratings) %>%
    hoist(player, "fullName", "active", "droppable", "injured", "injuryStatus",
          "proTeamId", "ownership", "stats", "eligibleSlots") %>%
    select(-player) %>%
    hoist(ownership, "averageDraftPosition", #"auctionValueAverage",
          "percentOwned", "percentChange", "percentStarted") %>%
    select(-ownership)

  if (!all(is.na(out$ratings))) {

    out <- hoist(out, ratings, "positionalRanking", "totalRanking", "totalRating")

  }

  return(out)
}

get_espn_standings <- function(season, leaugeID) {

  conn <- ffscrapr::espn_connect(season = season, league_id = leagueID)

  left_join(
    ffscrapr::espn_getendpoint(conn, view = "mTeam", scoringPeriodId = week) %>%
      pluck("content", "teams") %>%
      tibble() %>%
      hoist(1, "teamID" = "id",
            "draft_proj_rank" = "draftDayProjectedRank",
            "current_proj_rank" = "currentProjectedRank",
            "rank" = "playoffSeed", "record") %>%
      hoist(record, "overall") %>%
      hoist(overall, "pf" = "pointsFor", "pa" = "pointsAgainst", "wins", "ties", "losses") %>%
      select(-., -record, -overall),
    ffscrapr::espn_getendpoint(conn, view = "mStandings", scoringPeriodId = week) %>%
      pluck("content", "teams") %>%
      tibble() %>%
      hoist(1, "teamID" = "id", "currentSimulationResults") %>%
      hoist(currentSimulationResults,
            "sim_rank" = "rank",
            "sim_playoff_pct" = "playoffPct",
            "modeRecord") %>%
      hoist(modeRecord, "sim_wins" = "wins", "sim_ties" = "ties", "sim_losses" = "losses") %>%
      select(-currentSimulationResults, -modeRecord),
    by = "teamID"
  ) %>%
    select(teamID, pf:losses, rank,
           draft_proj_rank, current_proj_rank,
           sim_wins:sim_losses, sim_rank:sim_playoff_pct) %>%
    arrange(rank)

}

get_mfl_ids <- function(season = as.numeric(format(Sys.Date(),'%Y'))) {

  httr::GET(str_glue("https://api.myfantasyleague.com/{season}/export?TYPE=players&L=&APIKEY=&DETAILS=1&SINCE=&PLAYERS=&JSON=1")) %>%
    httr::content() %>%
    `[[`("players") %>%
    `[[`("player") %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(position %in% c("QB", "RB", "WR", "TE", "PK",
                                  "Def", "DE", "DT", "LB", "CB", "S")) %>%
    tidyr::extract(name, c("last_name", "first_name"), "(.+),\\s(.+)") %>%
    tidyr::unite(player, first_name, last_name, sep = " ") %>%
    dplyr::mutate(position = dplyr::recode(position, Def = "DST", PK = "K"),
                  birthdate = as.Date(as.POSIXct(as.numeric(birthdate),
                                                 origin = "1970-01-01")),
                  age = as.integer(lubridate::year(Sys.time()) - lubridate::year(birthdate)),
                  exp = season - as.integer(draft_year)) %>%
    dplyr::select(id, player, position, team, exp)

}

add_player_data <- function(df,
                            league = c("espn", "yahoo"),
                            data = c("mflID", "position", "all")) {

  if (!requireNamespace("ffscrapr", quietly = TRUE)) {
    stop("Package \"ffscrapr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  league <- match.arg(league)
  data <- match.arg(data)

  player_table <- ffscrapr::dp_playerids() %>%
    mutate(position = if_else(position == "PK", "K", position))

  if (data == "mflID") {

    if (league == "espn") {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(espnID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        select(player, playerID = espnID, mflID)

    } else {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(yahooID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        select(player, playerID = yahooID, mflID)

    }

  } else if (data == "position") {

    if (league == "espn") {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(espnID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        select(player, playerID = espnID, position)

    } else {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(yahooID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        select(player, playerID = yahooID, position)

    }

  } else {

    if (league == "espn") {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(espnID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        rename(playerID = espnID)

    } else {

      player_table <- player_table %>%
        rename(player = name, mflID = mfl_id) %>%
        mutate(yahooID = as.integer(espn_id)) %>%
        bind_rows(defenseIDs) %>%
        rename(playerID = yahooID)

    }

  }

  if ("player" %in% names(df)) player_table$player <- NULL

  left_join(df, player_table, by = "playerID") %>%
    mutate(mflID = if_else(mflID == "15432", "15258", mflID))

}


# Testing -----------------------------------------------------------------

# library(tidyverse)
#
# df_stats <- read_html('https://www.fantasypros.com/nfl/stats/qb.php?year=2021&week=1&scoring=Standard&roster=consensus&range=week') %>%
#   html_table(header = F) %>%
#   .[[1]]
#
#
# cols <- paste(as.character(df_stats[1,]),
#               as.character(df_stats[2,]),
#               sep = "_") %>%
#   gsub("^_|MISC_", "", .)
#
# df_stats %>%
#   slice(-1, -2) %>%
#   rename_with(~tolower(cols)) %>%
#   type.convert(as.is = T) %>%
#   glimpse()
#
#
# b <- read_html('https://www.fantasypros.com/nfl/stats/rb.php?year=2021&week=1&scoring=Standard&roster=consensus&range=week') %>%
#   html_table(header = F) %>%
#   .[[1]]
#
#
# cols <- paste(as.character(b[1,]),
#               as.character(b[2,]),
#               sep = "_") %>%
#   gsub("^_|MISC_", "", .)
#
# b %>%
#   slice(-1, -2) %>%
#   set_names(cols) %>%
#   janitor::clean_names() %>%
#   type.convert(as.is = T)


## parameters
# position
# year
# season or weekly
# scoring - standard, ppr, half ppr
# roster - consensus, yahoo, espn
# range



# read_csv('https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_fpecr_latest.csv',
#          show_col_types = F) %>%
#   filter(pos %in% c("RB"),
#          ecr_type == "bp",
#          ecr <= 75) %>%
#   arrange(ecr) %>%
#   mutate(rank = row_number()) %>%
#   ggplot(aes(y = rank, color = pos)) +
#   geom_point(aes(x = ecr)) +
#   scale_y_reverse(breaks = seq(from = 0, to = 80, by = 10)) +
#   scale_x_continuous(breaks = seq(from = 0, to = 80, by = 10),
#                      limits = c(-3, 90)) +
#   geom_text(aes(label = player, x = best), size = 3, hjust = 1.15) +
#   geom_segment(aes(yend = rank, x = best, xend = worst)) +
#   theme_minimal() +
#   theme(legend.position = c(.1, .1),
#         panel.grid.minor.x = element_blank()) +
#   guides(color = guide_legend(nrow = 1)) +
#   labs(color = NULL)
