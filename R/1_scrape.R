
# Main Functions ----------------------------------------------------------

#' @export
scrape_schedule <- function(league = c("espn", "yahoo"),
                            leagueID = NULL,
                            season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if(league == "yahoo") {

    if(is.null(leagueID)) leagueID <- 160225

    map_df(1:15, ~ scrape_yahoo_schedule(leagueID, .x, season))

  } else if (league == "espn") {

    if(is.null(leagueID)) leagueID <- 299999

    ffscrapr::espn_connect(season, leagueID) %>%
      ffscrapr::ff_schedule() %>%
      select(week, teamID = 2, team_score = 3, opponentID = 5, opponent_score = 6)

  }
}

#' @export
scrape_team <- function(week,
                        league = c("espn", "yahoo"),
                        leagueID = NULL,
                        season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if (league == "yahoo") {

    if(is.null(leagueID)) leagueID <- 160225

    scrape_yahoo_teamIDs(leagueID) %>%
      select(teamID) %>%
      pull() %>%
      map_df(~ scrape_yahoo_team(week, .x, leagueID, season))

  } else if (league == "espn") {

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
scrape_win_prob <- function(week,
                            league = "yahoo",
                            leagueID = NULL,
                            season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- match.arg(league)

  if(is.null(leagueID)) leagueID <- 160225

  scrape_yahoo_teamIDs(leagueID) %>%
    crossing(week) %>%
    mutate(prob = pmap_chr(list(week, teamID, leagueID), scrape_yahoo_winprob),
           wp = str_extract(prob, "[:digit:]+"),
           wp = as.numeric(wp)/100) %>%
    select(teamID, wp)

}

#' @export
scrape_player_projections <- function(week,
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

    if(is.null(leagueID)) leagueID <- 160225

    yahoo_players <- crossing(position = c("QB", "RB", "WR", "TE",
                                           "K", "DEF", "DB", "DL"),
                              page = seq(0, 75, by = 25)) %>%
      mutate(data = map2(position, page,
                         ~ scrape_yahoo_players(week, .x, .y, leagueID))) %>%
      unnest(data) %>%
      select(yahooID = playerID, player, position, teamID) %>%
      mutate(position = if_else(position == "DEF", "DST", position),
             teamID = if_else(str_detect(teamID, "^[A-Z] [()]|^0$"), "FA", teamID))

    out <- yahoo_players %>%
      inner_join(select(player_table, yahooID, mflID), by = "yahooID")

  } else if (league == "espn") {

    if(is.null(leagueID)) leagueID <- 299999

    conn <- ffscrapr::espn_connect(season = season, league_id = leagueID)

    espn_players <- ffscrapr::espn_players(season = season) %>%
      filter(pos %in% c("QB", "RB", "WR", "TE", "K", "DST"),
             team != "FA") %>%
      select(espnID = 1, player = 2, position = 3) %>%
      left_join(ffscrapr::ff_rosters(conn, week = week) %>%
                  select(teamID = 1, espnID = 3),
                by = "espnID") %>%
      replace_na(list(teamID = "FA"))

    out <- espn_players %>%
      inner_join(select(player_table, espnID, mflID), by = "espnID")

  } else {

    print("Wrong league")

  }

  mutate(out,
         teamID = as.integer(teamID),
         mflID = as.integer(mflID))

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

  team_col <- names(select(team, starts_with("team")))

  team %>%
    dplyr::filter(!roster %in% c("BN", "IR", "BE")) %>%
    dplyr::select(week, team = starts_with("team"), score, projected) %>%
    dplyr::group_by(week, team) %>%
    dplyr::summarize(projected = sum(projected, na.rm = T),
                     actual = score[1],
                     .groups = "drop") %>%
    purrr::set_names("week", team_col, "projected", "actual")

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

scrape_yahoo_teamIDs <- function(leagueID, teamID = 1:20) {

  tibble(teamID) %>%
    mutate(team = map(teamID, ~ valid_teamID(leagueID, .x))) %>%
    unnest(team) %>%
    filter(!is.na(team))

}

scrape_yahoo_schedule <- function(leagueID, week,
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
    rvest::html_nodes("#matchupweek .List-rich") %>%
    rvest::html_text() %>%
    str_remove_all(., "\\n|   ") %>%
    as_tibble() %>%
    separate_rows(value, sep = "View Matchup") %>%
    filter(value != "") %>%
    separate(value, c("team1", "team2"), sep = "vs") %>%
    mutate(gameID = row_number(),
           week = week) %>%
    extract(team1, c("team_score", "team_projected", "team", "team_record"),
            "(\\d+\\.\\d+)  ([0-9.]+) +(.*)  ([0-9-)]+)") %>%
    extract(team2, c("opponent_score", "opponent_projected", "opponent", "opponent_record"),
            "(\\d+\\.\\d+)  ([0-9.]+) +(.*)  ([0-9-)]+)") %>%
    bind_cols(teamIDs) %>%
    select(week, teamID, team_score, opponentID, opponent_score)

  bind_rows(tmp,
            rename(tmp, opponentID = 2, opponent_score = 3, teamID = 4, team_score = 5))

}

scrape_yahoo_team <- function(week, teamID,
                              leagueID = 160225,
                              season = as.numeric(format(Sys.Date(),'%Y'))) {

  team_url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID,
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

scrape_yahoo_winprob <- function(week, teamID,
                                 leagueID = 160225) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID,
                "/matchup?week=", week, "&mid1=", teamID)

  page <- xml2::read_html(url)

  page %>%
    rvest::html_nodes(".Pend-med") %>%
    rvest::html_text() %>%
    .[[2]]

}

scrape_yahoo_players <- function(week, position, page,
                                 leagueID = 160225) {

  url <- str_glue("https://football.fantasysports.yahoo.com/f1/{leagueID}/players?status=ALL&pos={position}&cut_type=9&stat1=S_PW_{week}&myteam=0&sort=AR&sdir=1&count={page}")

  page <- rvest::read_html(url)

  if(position %in% c("K", "DL", "DB")) {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             map(scrape_yahoo_player_team) %>%
             unlist())

  } else if (position == "DEF") {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_attr("href") %>%
             str_extract("[a-z-]*/$") %>%
             str_remove("/"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Ta-start Nowrap Bdrend"]') %>%
             map(scrape_yahoo_player_team) %>%
             unlist())

  } else {

    tibble(player = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_text(),
           playerID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Nowrap name F-link"]') %>%
             rvest::html_attr("href") %>%
             str_extract("\\d*$"),
           teamID = page %>%
             rvest::html_nodes("table") %>%
             .[[2]] %>%
             rvest::html_nodes(xpath = '//*[@class="Alt Ta-start Nowrap Bdrend"]') %>%
             map(scrape_yahoo_player_team) %>%
             unlist())

  }

}

scrape_yahoo_player_team <- function(x) {

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

scrape_espn_players <- function(week,
                                leagueID = 299999,
                                season = as.numeric(format(Sys.Date(),'%Y')),
                                pos = c("QB", "TQB", "RB", "RB/WR", "WR",
                                        "WR/TE", "TE", "OP",
                                        "DT", "DE", "LB", "DL", "CB",
                                        "S", "DB", "DP", "DST",
                                        "K", "P", "HC", "FLEX", "EDR"),
                                projections = TRUE) {

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

scrape_mfl_ids <- function(season = as.numeric(format(Sys.Date(),'%Y'))) {

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
