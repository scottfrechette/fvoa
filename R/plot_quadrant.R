
plot_quadrant <- function(league, league_id, id = NULL) {

  if(league == "yahoo") {

    print("Yahoo not added yet")

  } else {

    url <- paste0("http://games.espn.com/ffl/standings?leagueId=", league_id)

    page <- xml2::read_html(url)

    record <- page %>%
      html_nodes("table") %>%
      .[[3]] %>%
      html_table(header = T)

    colnames(record) <- as.character(unlist(record[1,]))

    record <- record %>%
      slice(-1) %>%
      as_tibble() %>%
      mutate_at(vars(W, L, T, PCT, GB), as.numeric)

    points <- page %>%
      html_nodes("table") %>%
      .[[4]] %>%
      html_table(header = T)

    colnames(points) <- as.character(unlist(points[1,]))

    points <- points %>%
      slice(-1) %>%
      as_tibble() %>%
      rename(Owner = `TEAM, OWNER(S)`) %>%
      mutate_at(vars(PF, PA), as.numeric) %>%
      mutate(Owner = str_extract(Owner, "^[^\\(]+") %>% str_trim())

    df <- points %>%
      mutate(Delta = PF - PA) %>%
      left_join(record, by = c("Owner" = "TEAM")) %>%
      select(Owner, Delta, Pct = PCT)

    if(!is.null(id)) {

      df <- df %>%
        left_join(id, by = c("Owner" = "Team")) %>%
        select(Team, Delta, Pct)

    }

    df %>%
      ggplot(aes(Delta, Pct)) +
      geom_point() +
      geom_hline(yintercept = 0.5) +
      geom_vline(xintercept = 0) +
      annotate("text", x = 50, y = .65, size = 8,
               label = "Good", color = "grey65") +
      annotate("text", x = 50, y = .35, size = 8,
               label = "Unlucky", color = "grey65") +
      annotate("text", x = -75, y = .65, size = 8,
               label = "Lucky", color = "grey65") +
      annotate("text", x = -75, y = .35, size = 8,
               label = "Bad", color = "grey65") +
      ggrepel::geom_text_repel(aes(label = Owner)) +
      scale_y_continuous(labels = scales::percent) +
      tidyquant::theme_tq() +
      labs(y = "Win Percentage",
           x = "Point Differential") +
      tidyquant::scale_color_tq() +
      guides(color = F)

  }

}
