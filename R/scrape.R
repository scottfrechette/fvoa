
# Main Functions ----------------------------------------------------------

#' @export
scrape_schedule <- function(league, leagueID,
                            season = as.numeric(format(Sys.Date(),'%Y'))) {

  league <- tolower(league)

  stopifnot(league %in% c("espn", "yahoo"),
            is.numeric(leagueID)
  )

  if(league == "yahoo") {

    map_df(1:17, ~ scrape_yahoo_schedule(leagueID, season, .x)) %>%
      mutate(season = as.integer(season)) %>%
      select(league, leagueID, season, everything())

  } else if (league == "espn") {

    if(season == as.numeric(format(Sys.Date(),'%Y'))) {

      url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{leagueID}?view=mMatchupScore&view=mRoster")
      scores <- jsonlite::fromJSON(url) %>%
        .$schedule

      df <- bind_cols(gameID = scores$id,
                      week = scores$matchupPeriodId,
                      scores$home %>%
                        select(team1 = teamId, team1_points = totalPoints),
                      scores$away %>%
                        select(team2 = teamId, team2_points = totalPoints),
                      playoff = scores$playoffTierType)

    } else {

      url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{leagueID}?season={season}&view=mMatchupScore")
      scores <- jsonlite::fromJSON(url) %>%
        .$schedule %>%
        .[[1]] %>%
        tidy_espn_cols()

      df <- bind_cols(gameID = scores$id,
                      week = scores$matchupPeriodId,
                      scores$home[[1]] %>%
                        select(team1 = teamId, team1_points = totalPoints),
                      scores$away[[1]] %>%
                        select(team2 = teamId, team2_points = totalPoints),
                      playoff = scores$playoffTierType)

    }

    df %>%
      mutate(league = 'espn',
             leagueID = leagueID,
             season = season,
             game_type = if_else(playoff == "NONE", "regular", "playoff")) %>%
      select(league, leagueID, season, week, gameID, team1, team2) %>%
      as_tibble() %>%
      mutate(across(c(-league), .fns = as.integer))
  }
}

#' @export
scrape_team <- function(league, leagueID, week, season = 2020) {

  if (league == "yahoo") {

    yahoo_teamIDs(leagueID) %>%
      select(teamID) %>%
      pull() %>%
      map_df(~ scrape_yahoo_team(leagueID, week, season, .x))

  } else if (league == "espn") {

    scrape_espn_team(leagueID, week, season)

  } else {

    print("Wrong league")

  }

}

#' @export
scrape_win_prob <- function(leagueID, week, season = 2020, league = "yahoo"){

  yahoo_teamIDs(leagueID) %>%
    crossing(week) %>%
    mutate(league = 'yahoo',
           leagueID = leagueID,
           prob = pmap_chr(list(week, leagueID, teamID), yahoo_winprob),
           # type = str_extract(prob, "[:alpha:]*"),
           wp = str_extract(prob, "[:digit:]+"),
           wp = as.numeric(wp)/100) %>%
    select(league, leagueID, week, teamID, wp)

}

#' @export
scrape_player_projections <- function(league, leagueID, week, season = 2020) {

  name_removals <- "\\.| I$| II$| III$| IV$| V$| Jr.$| Sr.$"

  player_table_tmp <- scrape_player_ids(season) %>%
    select(id, player, position) %>%
    mutate(player = str_remove_all(player, name_removals),
           position = case_when(
             position %in% c("CB", "S")  ~ "DB",
             position %in% c("DT", "DE") ~ "DL",
             TRUE                        ~ position
           ))

  if (league == "yahoo") {

    tmp <- crossing(position = c("QB", "RB", "WR", "TE",
                                 "K", "DEF", "DB", "DL"),
                    page = seq(0, 75, by = 25)) %>%
      mutate(data = map2(position, page,
                         ~ scrape_yahoo_players(479084, week, .x, .y))) %>%
      unnest(data) %>%
      select(-page) %>%
      mutate(league = "yahoo",
             leagueID = leagueID,
             season = season,
             week = week,
             player = str_remove_all(player, name_removals),
             position = if_else(position == "DEF", "DST", position),
             teamID = if_else(str_detect(teamID, "^[A-Z] [()]"), "FA", teamID))

    out <- bind_rows(
      tmp %>%
        filter(!position %in% c("DB", "DL", "DST")) %>%
        inner_join(player_table_tmp, by = c("player", "position")),
      tmp %>%
        filter(position %in% c("DB", "DL")) %>%
        inner_join(player_table_tmp,
                   by = c("player", "position")),
      tmp %>%
        filter(position == "DST") %>%
        inner_join(player_table_tmp %>%
                     filter(position == "DST") %>%
                     mutate(player = str_remove(player, " [A-z]*$")),
                   by = c("player", "position"))
    ) %>%
      select(league, leagueID, season, week,
             teamID, playerID, player, position) %>%
      distinct()

  } else if (league == "espn") {

    espn_players <- map_df(c("QB", "RB", "WR", "TE",
                             "K", "DST"),
                           ~ scrape_espn_players(leagueID = leagueID, season = season,
                                                 week = week, pos = .x,
                                                 projections = TRUE))

    out <- bind_rows(
      espn_players %>%
        filter(position != "DST",
               nfl_team > 0) %>%
        mutate(player = str_remove_all(player, name_removals)) %>%
        inner_join(player_table_tmp,
                   by = c("player", "position")),
      espn_players %>%
        filter(position == "DST") %>%
        mutate(player = str_remove(player, " D/ST")) %>%
        inner_join(player_table_tmp %>%
                     mutate(player = str_extract(player, "\\w*$")),
                   by = c("player", "position"))
    ) %>%
      mutate(league = "yahoo",
             leagueID = leagueID,
             season = season,
             week = week) %>%
      select(league, leagueID, season, week,
             teamID, playerID, player, position)

  } else {

    print("Wrong league")

  }

  out %>%
    mutate(leagueID = as.integer(leagueID),
           season = as.integer(season),
           week = as.integer(week),
           teamID = as.integer(teamID))

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
    dplyr::filter(!roster %in% c("BN", "IR")) %>%
    dplyr::select(week, team = starts_with("team"), score, proj_pts) %>%
    dplyr::group_by(week, team) %>%
    dplyr::summarize(proj = sum(proj_pts),
              act = score[1],
              .groups = "drop") %>%
    purrr::set_names("week", team_col, "proj", "act")

}

ffespn_api <- function(path, query = NULL, headers = NULL) {
  # url
  baseurl <- "http://fantasy.espn.com/"
  path <- paste0("apis/v3/games/ffl/", path)
  url <- httr::modify_url(baseurl, path = path, query = query)

  # get data
  if (is.null(headers)) {
    resp <- httr::GET(url)
  } else {
    resp <- httr::GET(url, headers)
  }

  # get content
  page <- httr::content(resp, "text", encoding = "utf-8")

  # check errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "ESPN API request failed [%s]\n[%s]",
        httr::status_code(resp), url
      ),
      call. = FALSE
    )
  }

  # check data type
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return html", call. = FALSE)
  }

  # parse content
  x <- jsonlite::fromJSON(page, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)

  x
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

yahoo_teamIDs <- function(leagueID, teamID = 1:20) {

  tibble(teamID) %>%
    mutate(team = map(teamID, ~ valid_teamID(leagueID, .x))) %>%
    unnest(team) %>%
    filter(!is.na(team))

}

espn_teamIDs <- function(leagueID, season) {
  url <- "http://fantasy.espn.com/apis/v3/games/ffl/seasons/2020/segments/0/leagues/299999?view=settings"

  jsonlite::fromJSON(url) %>%
    .$teams %>%
    tidy_espn_cols() %>%
    unite(team, location, nickname, sep = " ") %>%
    select(teamID = id, team)
}

scrape_yahoo_schedule <- function(leagueID, season, week) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID, "?matchup_week=", week)

  page <- xml2::read_html(url)

  page %>%
    rvest::html_nodes("#matchupweek .F-link") %>%
    rvest::html_attr('href') %>%
    str_extract("\\d*$") %>%
    as_tibble() %>%
    mutate(week = week,
           gameID = ceiling(row_number()/2)) %>%
    select(week, gameID, team = value) %>%
    spread_schedule() %>%
    mutate(league = 'yahoo',
           leagueID = leagueID,
           season = season) %>%
    mutate(across(c(-league), .fns = as.integer)) %>%
    select(league, leagueID, season, week,
           gameID, team1, team2)

}

scrape_espn_team <- function(leagueID = 299999, week = 1, season = 2020) {

  season_filter <- season
  week_filter <- week

  if(season == as.numeric(format(Sys.Date(),'%Y'))) {

    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{leagueID}?view=mMatchup&view=mMatchupScore&scoringPeriodId={week}")

    json_data <- jsonlite::fromJSON(url)

    teams <- json_data %>%
      .$teams %>%
      tidy_espn_cols()

    team_roster <- teams$roster[[1]]$entries

    schedule <- json_data %>%
      .$schedule

    scores <- bind_rows(
      bind_cols(week = schedule$matchupPeriodId,
                schedule$home %>%
                  select(teamID = teamId, score = totalPoints)),
      bind_cols(week = schedule$matchupPeriodId,
                schedule$away %>%
                  select(teamID = teamId, score = totalPoints))
    ) %>%
      as_tibble() %>%
      filter(week == week_filter)

  } else {

    url <- str_glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{leagueID}?season={season}&view=mMatchup&view=mMatchupScore&scoringPeriodId={week}")


    teams <- jsonlite::fromJSON(url) %>%
      .$teams %>%
      .[[1]] %>%
      tidy_espn_cols()

    team_roster <- teams$roster[[1]]$entries

    schedule <- json_data %>%
      .$schedule %>%
      .[[1]]

    scores <- bind_rows(
      bind_cols(week = schedule$matchupPeriodId,
                schedule$home %>%
                  select(teamID = teamId, score = totalPoints)),
      bind_cols(week = schedule$matchupPeriodId,
                schedule$away %>%
                  select(teamID = teamId, score = totalPoints))
    ) %>%
      as_tibble() %>%
      filter(week == week_filter)

  }

  if(season >= 2017) {

    tmp <- teams %>%
      select(teamID = id) %>%
      left_join(scores, by = "teamID") %>%
      mutate(
        season = as.integer(season),
        team_n = row_number(),
        player_data = map(team_n,
                          ~ team_roster[[.x]][["playerPoolEntry"]][["player"]] %>%
                            as_tibble() %>%
                            select(playerID = id, player = fullName,
                                   position = defaultPositionId, proTeamId,
                                   injured, injury_status = injuryStatus)),
        roster = map(team_n,
                     ~team_roster[[.x]][["lineupSlotId"]]))

  } else {

    tmp <- teams %>%
      unite(team, location, nickname, sep = " ") %>%
      mutate(team = str_trim(team)) %>%
      select(teamID = id, team) %>%
      left_join(scores, by = "teamID") %>%
      mutate(
        season = season,
        team_n = row_number(),
        player_data = map(team_n,
                          ~ team_roster[[.x]][["playerPoolEntry"]][["player"]] %>%
                            as_tibble() %>%
                            select(playerID = id, player = fullName,
                                   position = defaultPositionId, proTeamId,
                                   injured)),
        roster = map(team_n,
                     ~team_roster[[.x]][["lineupSlotId"]]))

  }

  tmp %>%
    unnest(c(player_data, roster)) %>%
    group_by(teamID) %>%
    mutate(
      position = case_when(
        position == 1 ~ "QB",
        position == 2 ~ "RB",
        position == 3 ~ "WR",
        position == 4 ~ "TE",
        position == 5 ~ "K",
        position == 16 ~ "DST"),
      roster = case_when(
        roster == 0 ~ "QB",
        roster == 2 ~ "RB",
        roster == 3 ~ "RB/WR",
        roster == 4 ~ "WR",
        roster == 6 ~ "TE",
        roster == 16 ~ "DST",
        roster == 17 ~ "K",
        roster == 20 ~ "BN",
        roster == 23 ~ "FLEX"
      ),
      data = map2(team_n, row_number(),
                  ~ team_roster[[.x]][["playerPoolEntry"]][["player"]][["stats"]][[.y]] %>%
                    tidy_espn_cols %>%
                    filter(seasonId == season_filter,
                           scoringPeriodId == week_filter) %>%
                    mutate(type = if_else(statSourceId == 1, "proj_pts", "act_pts")) %>%
                    select(seasonId, scoringPeriodId, type, points = appliedTotal) %>%
                    spread(type, points)
      )
    ) %>%
    ungroup() %>%
    unnest(c(data)) %>%
    filter(scoringPeriodId == week) %>%
    mutate(league = 'espn', leagueID = as.integer(leagueID),
           season = as.integer(season)) %>%
    select(league, leagueID, season, week = scoringPeriodId,
           teamID, score, playerID:position,
           roster, proj_pts, act_pts, injured, injury_status)
}

scrape_yahoo_team <- function(leagueID, week, season, teamID) {

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
    rename(act_pts = `Fan Pts`,
           proj_pts = Proj,
           roster = Pos) %>%
    separate(Player, c("notes", "player"), "Notes\\s+|Note\\s+") %>%
    separate(player, c("player", "result"), "Final|Bye") %>%
    separate(player, c("player", "position"), " - ") %>%
    mutate(league = 'yahoo',
           leagueID = as.integer(leagueID),
           season = season,
           teamID = teamID,
           week = week,
           player = str_replace(player, "\\s[:alpha:]+$", ""),
           position = str_extract(position, "[:alpha:]+"),
           score = sum(starters$`Fan Pts`, na.rm = T),
           across(c(season, week, teamID), as.integer)) %>%
    drop_na(player) %>%
    select(league, leagueID, season, week, teamID, score,
           player, position, roster, proj_pts, act_pts)

}

yahoo_winprob <- function(week, leagueID, teamID) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/", leagueID,
                "/matchup?week=", week, "&mid1=", teamID)

  page <- xml2::read_html(url)

  page %>%
    rvest::html_nodes(".Pend-med") %>%
    rvest::html_text() %>%
    .[[2]]

}

scrape_player_ids <- function(season) {

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
                  exp = 2020 - as.integer(draft_year)) %>%
    dplyr::select(id, player, position, team, exp)

}

scrape_espn_players <- function(leagueID = 299999, season = 2020, week,
                                pos = slot_names,
                                projections = TRUE) {
  pos <- match.arg(pos)
  stopifnot(is.numeric(week), is.numeric(season), is.character(pos))
  stopifnot(is_scalar(week), is_scalar(season), is_scalar(pos))
  stopifnot(week >= 0L, week <= 17L)

  season <- as.integer(season)
  week <- as.integer(week)
  posID <- slot_name_to_id(pos)
  projID <- as.integer(projections)

  path <- sprintf("seasons/%s/segments/0/leagues/%s", season, leagueID)

  query <- list("view" = "kona_player_info")

  players = list(
    filterSlotIds = list(value = posID),
    filterStatsForSourceIds = list(value = projID), # 0 = actual, 1 = projected
    offset = jsonlite::unbox(0)
  )

  if (week == 0) {
    players$filterStatsForExternalIds = list(value = season)
  } else {
    players$filterStatsForSplitTypeIds = list(value = c(week))
  }

  x_fantasy_filter <- list("players" = players)
  headers <- httr::add_headers(.headers = c(
    "X-Fantasy-Filter" = jsonlite::toJSON(x_fantasy_filter),
    "X-Fantasy-Source" = "kona",
    "Accept" =  "application/json",
    "Referer" =  "http://fantasy.espn.com/football/players/projections",
    "DNT" = "1",
    "Connection" =  "keep-alive",
    "X-Fantasy-Platform" = "kona-PROD-669a217c5628b670cf386bd4cebe972bf88022eb"
  ))

  x <- ffespn_api(path, query, headers)

  if (identical(length(x$players), 0L)) {
    stop("results are empty. no players found", call. = FALSE)
  }

  tidy_projections(x)
}

scrape_yahoo_players <- function(leagueID, week, position, page) {

  url <- str_glue("https://football.fantasysports.yahoo.com/f1/{leagueID}/players?status=ALL&pos={position}&cut_type=9&stat1=S_PW_{week}&myteam=0&sort=AR&sdir=1&count={page}")

  page <- xml2::read_html(url)

  if(position %in% c("K", "DEF", "DL", "DB")) {

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
             map(yahoo_player_team) %>%
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
             map(yahoo_player_team) %>%
             unlist())

  }

}

yahoo_player_team <- function(x) {

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


tidy_espn_cols <- function(df) {

  df %>%
    map_if(is.data.frame, list) %>%
    as_tibble()

}

spread_schedule <- function(schedule, week = week,
                            gameID = gameID) {

  long_to_wide(schedule, team, week, gameID)

}

gather_schedule <- function(schedule, teamID = teamID,
                            team1 = team1, team2 = team2) {

  wide_to_long(schedule, teamID, team1, team2)

}

doublewide_schedule <- function(schedule) {

  if("teamID" %in% names(schedule)) {

    schedule <- spread_schedule(schedule)

  }

  other_cols <- setdiff(names(schedule), c("team1", "team2"))

  schedule_tmp <- schedule %>%
    mutate_if(is.factor, as.character)
  schedule_rev <- schedule_tmp %>%
    select(other_cols, team1 = team2, team2 = team1)
  bind_rows(schedule_tmp, schedule_rev) %>%
    arrange(week, team1)

}
