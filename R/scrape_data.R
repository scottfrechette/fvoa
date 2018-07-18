scrape_schedule <- function(league, league_id, format = "wide") {

  league <- stringr::str_to_lower(league)

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
        mutate(game_id = ceiling(row_number()/2))

      if(format == "wide") {

        schedule %>%
          group_by(game_id) %>%
          summarise(teams = paste(value, collapse = ",")) %>%
          separate(teams, into = c("team1", "team2"), sep = ",")

      } else if (format == "long") {

        schedule %>%
          select(game_id, team = value)

      }

    }

    data_frame(league_id = league_id,
               week = 1:17, format = format) %>%
      mutate(weekly_schedule = pmap(list(league_id, week, format), scrape_week)) %>%
      unnest() %>%
      select(-league_id, -format) %>%
      mutate_at(vars(contains("team")), factor)

  } else if (league == "espn") {

    paste("not ready yet")

  }

}

scrape_team <- function(week, team_id, league, league_id, season = 2018) {

  league <- stringr::str_to_lower(league)

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

extract_weekly_scores <- function(df) {

  if("Points" %in% names(df)) {
    df %>%
      filter(!Lineup %in% c("BN", "Bench")) %>%
      group_by(Week, Team) %>%
      summarise(Proj = sum(Proj),
                Score = sum(Points)) %>%
      distinct()

  } else {
    df %>%
      distinct(Week, Team, Score)
  }
}
