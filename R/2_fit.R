
# Main Function -----------------------------------------------------------

#' @export
fit_model <- function(scores,
                      prior_mean = 115,
                      prior_sd = 20) {

  scores <- left_join(scores,
                      weight_games(1:max(scores$week)),
                      by = "week")

  rstanarm::stan_glm(score ~ 0 + team,
                     data = scores,
                     weights = weight,
                     prior = rstanarm::student_t(10, prior_mean, prior_sd),
                     # prior = rstanarm::normal(prior_mean, prior_sd),
                     seed = 42,
                     iter = 3750,
                     warmup = 1250,
                     chains = 4,
                     cores = 4)

}

# Helpers -----------------------------------------------------------------

weight_games <- function(x, alpha = 0.15) {

  weekly_weights <- replicate(length(x), NA)
  uniform_weights <- 1

  if(length(x) %% 2 == 0) { #even

    index <- length(x) / 2
    for (i in 1:length(x)) {

      weekly_weights[i] <- ifelse(i <= index,
                                  uniform_weights - ((index + 1) - i) * alpha,
                                  uniform_weights + (i - (index )) * alpha)

    }

  } else { #odd
    index <- (length(x) + 1) / 2

    for (i in 1:length(x)) {

      if (i != index) {

        weekly_weights[i] <- ifelse(i < index,
                                    uniform_weights - (index - i) * alpha,
                                    uniform_weights + (i - index) * alpha)

      } else {

        weekly_weights[i] <- uniform_weights

      }
    }
  }

  weekly_weights <- if_else(weekly_weights < 0, 0, weekly_weights)

  tibble(week = x,
         weight = weekly_weights)
}

extract_draws <- function(scores, model, ndraws = NULL) {

  tidybayes::add_predicted_draws(distinct(scores, team),
                                 model,
                                 ndraws,
                                 seed = 42)

}

summarize_draws <- function(draws, .width = 0.95) {

  tidybayes::median_hdi(draws, .width = .width) %>%
    select(team, median = .prediction,
           lower = .lower, upper = .upper) %>%
    arrange(-median)

}
