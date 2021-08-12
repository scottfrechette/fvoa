
# Main Function -----------------------------------------------------------

#' @export
fit_model <- function(scores) {

  scores <- left_join(scores,
                      weight_games(1:max(scores$week)),
                      by = "week")

  rstanarm::stan_glm(score ~ 0 + team,
                     data = scores,
                     weights = weight,
                     prior = rstanarm::normal(115, 20),
                     seed = 42,
                     iter = 3750,
                     warmup = 1250,
                     chains = 4,
                     cores = 4,
                     refresh = 0)

}

# Helpers -----------------------------------------------------------------

extract_draws <- function(scores, model, ndraws = NULL) {

  tidybayes::add_predicted_draws(distinct(scores, team), model, ndraws)

}

summarize_draws <- function(draws, .width = 0.95) {

  tidybayes::median_hdi(draws, .width = .width) %>%
    select(team, median = .prediction,
           lower = .lower, upper = .upper) %>%
    arrange(-median)

}
