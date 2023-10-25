
# Main Function -----------------------------------------------------------

#' @export
fit_team <- function(scores) {

  scores <- left_join(scores,
                      weight_games(1:max(scores$week)),
                      by = 'week')

  rstanarm::stan_glmer(data = scores,
                       score ~ week + (week | team),
                       # score ~ 1 + (1 | team),
                       # score ~ 0 +  team,
                       # score ~ 0 + week + (week | team),
                       # weights = weight,
                       # prior = rstanarm::student_t(nrow(scores) - 1, 110, 10),
                       prior = rstanarm::normal(0, 2),
                       prior_intercept = rstanarm::student_t(nrow(scores) - 1, 110, 10),
                       prior_aux = rstanarm::exponential(rate = 1 / 23),
                       prior_covariance = rstanarm::decov(1, 1, 1, 1),
                       seed = 42,
                       iter = 3750,
                       warmup = 1250,
                       cores = 1,
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

extract_team_draws <- function(fit, ndraws = NULL) {

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
