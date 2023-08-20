
# Main Function -----------------------------------------------------------

#' @export
fit_team <- function(scores) {

  rstanarm::stan_glmer(data = scores,
                       score ~ week + (week | team),
                       prior = normal(0, 25),
                       prior_intercept = student_t(10, 110, 35),
                       prior_aux = exponential(rate = 1, autoscale = T),
                       prior_covariance = decov(1, 1, 1, 1),
                       seed = 42,
                       iter = 3750,
                       warmup = 1250,
                       cores = 4,
                       chains = 4,
                       refresh = 0)

}

#' @export
fit_team_season <- function(scores) {

  tibble(week = 1:max(scores$week)) %>%
    mutate(scores_filtered = map(week, ~filter(scores, week <= .x)),
           model = map(scores_filtered, fit_team))

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

extract_team_draws <- function(team_data, fit, ndraws = NULL) {

  tibble(week = max(fit$data$week),
         team = unique(fit$data$team)) %>%
    tidybayes::add_predicted_draws(fit,
                                   ndraws,
                                   seed = 42)

}

summarize_draws <- function(draws, .width = 0.95) {

  tidybayes::median_hdi(draws, .width = .width) %>%
    select(team, median = .prediction,
           lower = .lower, upper = .upper) %>%
    arrange(-median)

}


# Experimental ------------------------------------------------------------

fit_player <- function(player_data) {

  stan_glmer(points ~ position +
               (1 | mflID) +
               (position | team) +
               (position | opponent),
             data = player_data,
             family = poisson,
             chains = 4,
             iter = 3500,
             warmup = 1000,
             seed = 42)

}

extract_player_draws <- function(player_data, model, ndraws = NULL) {

  player_data %>%
    distinct(mflID, position, team) %>%
    # distinct(mflID, position, team, opponent, home) %>%
    tidybayes::add_predicted_draws(model,
                                   ndraws,
                                   seed = 42) %>%
    ungroup() %>%
    select(mflID, position, sim = .draw, pred = .prediction)

}
