#'
#' generate_pred
#'
#' This function will take in two dataframes (cohort data and coefficients) and calculate functions for all patients with available data
#'
#' @param data Data for which predictions will be generated
#' @param coefficients Dataframe containing model coefficients
#' @param model_type Type of model (linear, logistic, survival, quantile)
#' @param id Name of the variable containing unique identifier. Default is "id"
#' @param covariate Name of the variable in "coefficients" data containing covariate name. Default is "covariate"
#' @param value Name of the variable in the "coefficients" data containing coefficient value. Default is "value"
#' @param outcometime For survival models, the desired timepoint for prediction, in whatever unit was used for time to event in creating model. Default is 5 years.
#' @param starttime For survival models, the starting timepoint for prediction, in whatever unit was used for time to event in creating model. Default is 0 years (time of surgery).
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # add example here
generate_pred <- function(data,
                          coefficients,
                          model_type = c("linear", "logistic", "survival", "quantile"),
                          id = "id",
                          covariate = "covariate",
                          value = "value",
                          outcometime = 5,
                          starttime = 0) {

  # model_type can be passed in any case
  model_type <- tolower(model_type)

  # Confirm variables exist in coefficients data
  if(!(covariate %in% names(coefficients))) {
    stop(glue("'{covariate}' does not exist in the coefficients data"), call. = FALSE)
  } else if(!(value %in% names(coefficients))) {
    stop(glue("'{value}' does not exist in the coefficients data"), call. = FALSE)
  }

  # Rename variables if necessary
  coefficients <-
    coefficients %>%
    rename(covariate_name = .env$covariate, coef_value = .env$value)

  # Set up model coefficients data
  coefficients_pred <-
    coefficients %>%
    # Drop AUC, C-index, Scaling Parameter, Model N
    filter(!(.data$covariate_name %in% c("C-index", "AUC", "Scaling Parameter", "Model N")))

  # Save out list of covariate names for data check
  covariates <-
    coefficients_pred %>%
    pull(.data$covariate_name) %>%
    # Exclude intercept as covariate
    dplyr::setdiff(c("Intercept"))

  # For survival data, pull out scaling parameter too
  if(model_type == "survival") {
    scaling <-
      coefficients %>%
      filter(.data$covariate_name == "Scaling Parameter") %>%
      pull(.data$coef_value)
  } else if(model_type != "survival") {
    scaling <- 1
    # Otherwise there is an error in "case_when" statement when calculating predictions
  }

  # Data Checks

  # Confirm covariates exist in dataframe
  if(!all(covariates %in% names(data))) {

    stop("Check that the covariates exist in the dataframe provided.", call. = FALSE)

    # Confirm that id variable exists in dataframe
  } else if(!(id %in% names(data))) {

    stop(glue("Check that '{id}' exists in the dataframe provided."), call. = FALSE)

    # Check that model type is linear, logistic or survival
  } else if(!(model_type %in% c("linear", "logistic", "survival", "quantile"))) {

    stop("Check that the model type is linear, logistic, survival or quantile.", call. = FALSE)

    # Check that coefficient value is numeric
  } else if(!is.numeric(coefficients_pred$coef_value)) {

    stop(glue("'{value}' must be a numeric variable (the value of the coefficient)"),
         call. = FALSE)

  }

  # Set up patient data
  data_pred <-
    data %>%
    # Keep only covariates + id variable
    select(.env$id, dplyr::all_of(covariates)) %>%
    # Keep only complete cases (will merge with main data later)
    filter(complete.cases(.)) %>%
    # Create variable for intercept
    mutate(Intercept = 1)

  # Save out the risk formula for the model
  model_formula <-
    coefficients_pred %>%
    mutate(
      covar_formula = glue("{.data$covariate_name} * {.data$coef_value}")
    ) %>%
    pull(.data$covar_formula) %>%
    paste0(collapse = " + ")

  # For all patients with data available, calculate linear predictor and predictions
  data_pred <-
    data_pred %>%
    mutate(
      # Linear predictor - for linear/quantile, this is all that will be kept
      pred_xb = eval(parse(text = model_formula)),
      # Event probability - for logistic/survival, otherwise NA for linear/quantile
      event_pr =
        case_when(
          # Logistic model - this code gives event probability, also calculate non-event probability
          model_type == "logistic" ~ exp(.data$pred_xb) / (1 + exp(.data$pred_xb)),
          # Survival model - updated code to give event probability, will calculate non-event/survival separately
          model_type == "survival" ~
            1 - ((1 + (exp(-1 * .data$pred_xb) * .env$starttime) ^ (1 / scaling)) / (1 + (exp(-1 * .data$pred_xb) * .env$outcometime) ^ (1 / scaling)))
        ),
      # Non-event/survival probability - not needed for linear/quantile
      nonevent_pr = 1 - .data$event_pr
    )

  # Merge this data in with original patient dataset, so we are returning all
  # patients, even though some patients may not have a prediction
  data_final <-
    data %>%
    dplyr::left_join(
      data_pred %>%
        select(.env$id, .data$pred_xb, .data$event_pr, .data$nonevent_pr) %>%
        # Drop variables if all NA (event_pr/nonevent_pr for linear/quantile models)
        select(where(~ !all(is.na(.x)))),
      by = .env$id
    )

  # Return final dataframe
  return(data_final)

}
