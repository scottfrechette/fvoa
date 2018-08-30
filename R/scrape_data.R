
# Main Functions ----------------------------------------------------------

scrape_schedule <- function(league, league_id) {

  league <- tolower(league)

  stopifnot(league %in% c("espn", "yahoo"),
            is.numeric(league_id),
            format %in% c("wide", "long")
  )

  if(league == "yahoo") {

    scrape_week <- function(league_id, week, format) {

      url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id, "?week=", week)

      page <- xml2::read_html(url)

      schedule <- page %>%
        rvest::html_nodes("#matchupweek .F-link") %>%
        rvest::html_text() %>%
        as_tibble() %>%
        mutate(Game_id = ceiling(row_number()/2)) %>%
        select(Game_id, Team = value)

    }

    data_frame(league_id = league_id,
               Week = 1:17, format = format) %>%
      mutate(weekly_schedule = pmap(list(league_id, Week, format), scrape_week)) %>%
      unnest() %>%
      select(-league_id, -format) %>%
      mutate_at(vars(contains("Team")), factor)

  } else if (league == "espn") {

    paste("not ready yet")

  }

}

scrape_team <- function(weeks, league, league_id, season = 2018) {

  league <- tolower(league)

  stopifnot(week %in% 1:17,
            league %in% c("espn", "yahoo"),
            is.numeric(league_id),
            is.numeric(season)
  )

  if(league == "yahoo") {

    yahoo_teamIDs(league_id) %>%
      crossing(Week = 1:weeks) %>%
      mutate(league = league,
             league_id = league_id,
             team = pmap(list(Week, team_id, league, league_id), scrape_weekly_team)) %>%
      select(-league, -league_id, -Team)

  } else {


  }

}

scrape_win_prob <- function(weeks, league, league_id, season = 2018){

  league <- tolower(league)

  stopifnot(weeks %in% 1:17,
            league %in% c("espn", "yahoo"),
            is.numeric(league_id),
            is.numeric(season)
  )

  if(league == "yahoo") {

    yahoo_winprob <- function(week, league_id, team_id) {

      url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id,
                    "/matchup?week=", week, "&mid1=", team_id)

      page <- xml2::read_html(url)

      page %>%
        html_nodes(".Pend-med") %>%
        html_text() %>%
        .[[2]]

    }

    yahoo_teamIDs(league_id) %>%
      crossing(Week = 1:weeks) %>%
      mutate(league = league,
             league_id = league_id,
             prob = pmap_chr(list(Week, league_id, team_id), yahoo_winprob),
             type = str_extract(prob, "[:alpha:]*"),
             win_prob = str_extract(prob, "[:digit:]+%")) %>%
      select(Week, team_id, Team, type, win_prob)

  } else {

  }


}

# Helper Functions --------------------------------------------------------

scrape_weekly_team <- function(week, team_id, league, league_id, season = 2018) {

  league <- tolower(league)

  stopifnot(week %in% 1:17,
            team_id %in% 1:20,
            league %in% c("espn", "yahoo"),
            is.numeric(league_id),
            is.numeric(season)
  )

  if(league == "yahoo") {

    url <- paste0("https://football.fantasysports.yahoo.com/f1/", league_id,
                  "/matchup?week=", week, "&mid1=", team_id)

    page <- xml2::read_html(url)

    name <- page %>%
      rvest::html_nodes(".Ta-end .F-link") %>%
      .[[1]] %>%
      rvest::html_text()

    starters <- page %>%
      rvest::html_nodes("#statTable1") %>%
      rvest::html_table() %>%
      flatten_dfc() %>%
      select(1:5) %>%
      mutate(`Fan Pts` = as.numeric(`Fan Pts`)) %>%
      filter(Pos != "Total")

    bench <- page %>%
      rvest::html_nodes("#statTable2") %>%
      rvest::html_table(fill = T) %>%
      flatten_dfc() %>%
      select(1:5) %>%
      mutate(Proj = as.numeric(Proj),
             `Fan Pts` = as.numeric(`Fan Pts`)) %>%
      replace_na(list(`Fan Pts` = 0)) %>%
      filter(Pos != "Total")

    bind_rows(starters,
              bench) %>%
      mutate(Team = name) %>%
      rename(Points = `Fan Pts`,
             Lineup = Pos) %>%
      separate(Player, c("notes", "Player"), "Notes\\s+|Note\\s+") %>%
      separate(Player, c("Player", "result"), "Final|Bye") %>%
      separate(Player, c("Player", "Position"), " - ") %>%
      mutate(Player = str_replace(Player, "\\s[:alpha:]+$", ""),
             Position = str_extract(Position, "[:alpha:]+"),
             Score = sum(starters$`Fan Pts`)) %>%
      drop_na(Player) %>%
      select(Team:Score, Player:Position, Lineup, Proj, Points) %>%
      ungroup

  } else if (league == "espn") {

    url <- paste0("http://games.espn.com/ffl/boxscorequick?leagueId=", league_id,
                  "&teamId=", team_id, "&scoringPeriodId=", week, "&seasonId=",
                  season, "&view=scoringperiod&version=quick")

    page <- xml2::read_html(url)

    name <- page %>%
      rvest::html_nodes("#teamInfos div:nth-child(1) div .bodyCopy div b") %>%
      rvest::html_text()

    starters <- page %>%
      rvest::html_nodes("#playertable_0") %>%
      rvest::html_table(fill = T) %>%
      flatten_dfc() %>%
      select(1, 2, 5) %>%
      slice(-c(1:3)) %>%
      set_names("Lineup", "Player", "Points") %>%
      mutate(Points = as.numeric(Points),
             Player = str_replace_all(Player, "\\s+Q$|\\s+IR$", ""),
             Position = str_extract(Player, "[:graph:]+$")) %>%
      separate(Player, c("Player", "Team_Pos"), ",") %>%
      mutate(Player = str_replace_all(Player, " D/ST", "")) %>%
      select(Player, Position, Lineup, Points)

    bench <- page %>%
      rvest::html_nodes("#playertable_1") %>%
      rvest::html_table() %>%
      flatten_dfc() %>%
      select(1, 2, 5) %>%
      slice(-1) %>%
      set_names("Lineup", "Player", "Points") %>%
      mutate(Points = as.numeric(Points),
             Player = str_replace_all(Player, "\\s+Q$|\\s+IR$", ""),
             Position = str_extract(Player, "[:graph:]+$")) %>%
      separate(Player, c("Player", "Team_Pos"), ",") %>%
      mutate(Player = str_replace_all(Player, " D/ST", "")) %>%
      select(Player, Position, Lineup, Points)

    bind_rows(starters, bench) %>%
      replace_na(list(Points = 0)) %>%
      mutate(Team = name,
             Score = sum(starters$Points, na.rm = T)) %>%
      select(Team, Score, everything())

  }
}

extract_weekly_scores <- function(weekly_team_df) {

  if("Points" %in% names(weekly_team_df)) {

    weekly_team_df %>%
      filter(!Lineup %in% c("BN", "Bench")) %>%
      group_by(Week, Team) %>%
      summarise(Proj = sum(Proj),
                Score = sum(Points)) %>%
      distinct()

  } else {

    weekly_team_df %>%
      distinct(Week, Team, Score)

  }

}

spread_schedule <- function(schedule, Week = Week,
                            Game_id = Game_id) {

  long_to_wide(schedule, Team, Week, Game_id)

}

gather_schedule <- function(schedule, Team = Team,
                            Team1 = Team1, Team2 = Team2) {

  wide_to_long(schedule, Team, Team1, Team2)

  }


valid_teamID <- function(id, league_id) {

  url <- paste0("https://football.fantasysports.yahoo.com/f1/",
                league_id, "/teams")

  page <- xml2::read_html(url)

  exists <- page %>%
    html_nodes(str_glue(".team-{id}")) %>%
    length() == 1

  if(exists) {

    page %>%
      html_nodes(str_glue(".team-{id}")) %>%
      html_text() %>%
      as_tibble() %>%
      mutate(Team = word(value, sep = "-")) %>%
      pull(Team)

  } else {
    NA
  }

}

yahoo_teamIDs <- function(league_id, id = 1:20) {

  data_frame(team_id = id) %>%
    mutate(league_id = league_id,
           Team = map2_chr(team_id, league_id, valid_teamID)) %>%
    filter(!is.na(Team)) %>%
    select(team_id, Team)

}
