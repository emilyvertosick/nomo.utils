#'
#' generate_pred
#'
#' This function will take in two dataframes (cohort data and coefficients) and calculate functions for all patients with available data
#' This function generates the following variables:
#' `pred_xb`: For linear and quantile regression, this is the predicted value. For logistic and survival analysis, this is the linear predictor.
#' `event_pr`: For logistic and survival analysis, this is the predicted probability of the event (at the specified timepoint, for survival models). For linear and quantile regression, this is not generated.
#' `nonevent_pr`: For logistic and survival analysis, this is the predicted probability of not having the event (survival probability at the specified timepoint, for survival models). For linear and quantile regression, this is not generated.
#'
#' @param data Data for which predictions will be generated
#' @param coefficients Dataframe containing model coefficients
#' @param model_type Type of model (linear, logistic, survival, quantile)
#' @param id Name of the variable containing unique identifier. Default is "id"
#' @param covariate Name of the variable in "coefficients" data containing covariate name. Default is "covariate"
#' @param value Name of the variable in the "coefficients" data containing coefficient value. Default is "value"
#' @param pred_xb Name for the linear predictor variable in the returned dataframe. Default is "pred_xb"
#' @param event_pr Name for the event probability variable in the returned dataframe. Default is "event_pr"
#' @param nonevent_pr Name for the non-event probability variable in the returned dataframe. Default is "nonevent_pr"
#' @param starttime For survival models, the starting timepoint for prediction, in whatever unit was used for time to event in creating model. Default is 0 years (time of surgery).
#' @param outcometime For survival models, the desired timepoint for prediction, in whatever unit was used for time to event in creating model. Default is 5 years.
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
                          pred_xb = "pred_xb",
                          event_pr = "event_pr",
                          nonevent_pr = "nonevent_pr",
                          starttime = 0,
                          outcometime = 5) {

  # model_type can be passed in any case
  model_type <- tolower(model_type)

  # Confirm variables exist in coefficients data
  if(!(covariate %in% names(coefficients))) {
    stop(glue("'{covariate}' does not exist in the coefficients data"), call. = FALSE)
  } else if(!(value %in% names(coefficients))) {
    stop(glue("'{value}' does not exist in the coefficients data"), call. = FALSE)
  }

  # Confirm predicted final variables don't already exist in dataframe
  if(length(intersect(c(pred_xb, event_pr, nonevent_pr), names(data))) != 0) {
    stop(glue("Variables named ",
              "{glue::glue_collapse(
              intersect(c(pred_xb, event_pr, nonevent_pr), names(data)),
              sep = ', ',
              last = ' and ')} ",
              "already exist in this dataset. Please choose different names for the output variables."),
         call. = FALSE)
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

  # Do any coefficients have an NA value? If so, remove and print a warning
  if (any(is.na(coefficients_pred$coef_value))) {

    # Identify covariates with NA values
    covar_na_list <-
      coefficients_pred %>%
      filter(is.na(.data$coef_value)) %>%
      pull(.data$covariate_name)

    # Print a warning
    warning(glue("The following covariates have an NA value for the model coefficient: ",
                     glue::glue_collapse(covar_na_list, sep = ", ", last = "and"),
                 ". Predictions will be calculated excluding these covariates."
                 ),
            call. = FALSE)

    # Save out coefficients_pred without NA values
    coefficients_pred <-
      coefficients_pred %>%
      filter(!is.na(.data$coef_value))

  }

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
    ) %>%
    # Keep only necessary variables
    select(dplyr::all_of(c(id, "pred_xb", "event_pr", "nonevent_pr")))

  # Assign names per options passed to function
  names(data_pred) <- c(id, pred_xb, event_pr, nonevent_pr)

  # Merge this data in with original patient dataset, so we are returning all
  # patients, even though some patients may not have a prediction
  data_final <-
    # suppress "by = " message - we are specifying "by" here so no need for message
    suppressMessages(
      data %>%
        dplyr::left_join(
          data_pred %>%
            # Drop variables if all NA (event_pr/nonevent_pr for linear/quantile models)
            select(where(~ !all(is.na(.x)))),
          by = id
        )
    )

  # Return final dataframe
  return(data_final)

}

