
#' generate_coefs
#'
#' This function takes a dataframe, a list of covariates and a model type to
#' create a model and save out the coefficients for that model
#'
#' @param data Data that will be used to create model
#' @param outcome The outcome for the model
#' @param covars A vector of covariate names. If NULL, a null model (no covariates) will be created.
#' @param id Name of the variable containing unique identifier
#' @param model_type Type of model (linear, logistic, survival, quantile)
#' @param quantile_tau Vector of desired centiles for quantile model
#' @param model_weights Name of variable containing model weights
#' @param labels Labels corresponding to covariates in model
#'
#' @return dataframe
#' @export
#'
#' @examples
#' # add example here
generate_coefs <- function(data,
                           outcome,
                           covars = NULL,
                           id = "id",
                           model_type = c("linear", "logistic", "survival", "quantile"),
                           quantile_tau = c(0.25, 0.5, 0.75), # TODO: How to pass vector?
                           model_weights = NULL,
                           labels = NULL) {

  # model_type can be passed in any case
  model_type <- tolower(model_type)

  # Input checks

  # If no covariates, use "1" to create null model
  # Also set labels to NULL to avoid any issues
  if (rlang::is_empty(covars) == TRUE) {

    covars <- c(1)
    labels <- NULL

    # If only passed a blank string or vector of blank strings, give an error

  } else if (rlang::is_empty(covars) == FALSE & all(covars == "")) {

    stop("Do not pass a vector of empty strings as covariates. If no covariates will be used, the 'covars' option should be NULL.",
         call. = FALSE)

    # Otherwise, confirm covariates exist in dataframe
  } else if (rlang::is_empty(covars) == FALSE & !(all(covars %in% names(data)))) {

    stop("Check that the covariates exist in the dataframe provided.",
         call. = FALSE)
  }

  # Confirm outcome exists in dataframe
  if (!all(outcome %in% names(data))) {

    stop("Check that the outcome exists in the dataframe provided.",
         call. = FALSE)

    # Check that id variable exists in dataframe
  } else if (!(id %in% names(data))) {

    stop(glue("Check that {id} exists in the dataframe provided."),
         call. = FALSE)

    # Check that model type is linear, logistic or survival
  } else if (!(model_type %in% c("linear", "logistic", "survival", "quantile"))) {

    stop("Check that the model type is linear, logistic, survival or quantile.",
         call. = FALSE)
  }

  # Confirm correct number of outcome variables
  if(model_type == "survival" & length(outcome) != 2) {

    stop("Exactly two outcome variables (time, outcome) should be provided for survival models.",
         call. = FALSE)

  } else if(model_type %in% c("linear", "logistic", "quantile") & length(outcome) != 1) {

    stop("Exactly one outcome should be provided for linear, logistic and quantile models.",
         call. = FALSE)

    # Confirm weighting variable exists
  } else if(!is.null(model_weights)) {

    if(!(model_weights %in% names(data))) {
      stop("Check that the weighting variable exists in the dataframe provided.",
           call. = FALSE)
    }

    # Confirm label length is same as covars
  } else if(!is.null(labels)) {

    if(length(covars) != length(labels)) {
      stop("covars and labels must be the same length",
           call. = FALSE)

    }
  }

  # Data setup
  model_data <-
    data %>%
    # Keep only outcome, covariates and weights
    select(dplyr::one_of(id), dplyr::all_of(outcome), dplyr::all_of(covars), dplyr::one_of(model_weights)) %>%
    # Keep only those patients with complete cases
    filter(complete.cases(.))

  # Confirm enough observations (at least 10)
  if(nrow(model_data) < 10) {
    warning("There are less than 10 observations, no model will be created.",
            call. = FALSE)
    regression_coefs <- tibble(status = "No model: <10 observations")
    return(regression_coefs)
  }

  # If there is only one outcome level (no events or no non-events), no error but no model
  if(length(model_data %>% pull(outcome[1]) %>% unique()) == 1) {
    warning(glue("There is only one level of the outcome variable ",
                 "({outcome[1]} = {model_data %>% pull(outcome[1]) %>% unique()}), ",
                 "no model will be created."),
            call. = FALSE)
    regression_coefs <-
      tibble(status = glue("No model: one outcome level ({outcome[1]} = {model_data %>% pull(outcome[1]) %>% unique()})"))
    return(regression_coefs)

  }

  # LINEAR MODELS #
  if(model_type == "linear") {

    # Create formula
    regression_formula <-
      as.character(glue("{outcome} ~ ", glue_collapse(covars, sep = " + ")))

    # Create model
    regression_model <-
      lm(
        as.formula(regression_formula),
        data = model_data,
        weights = model_weights
      )

    # For linear, only need to save out modelN
    regression_stats <-
      tibble(covariate = "Model N", value = nobs(regression_model))

    # LOGISTIC MODELS #

  } else if(model_type == "logistic") {

    # Confirm only two levels of outcome variable
    if(length(model_data %>% pull(outcome[1]) %>% unique()) != 2) {
      stop("The outcome must have two and only two levels",
           call. = FALSE)
    }

    # Confirm enough events or non-events
    min_event_n <-
      min(model_data %>% pull(outcome) %>% table(), na.rm = TRUE)
    if(min_event_n < length(covars)*10) {
      warning(glue("Only {min_event_n} events for {length(covars)} covariates"),
              call. = FALSE)
      regression_coefs <- tibble(status = glue("No model: {min_event_n} events for {length(covars)} covariates"))
      return(regression_coefs)
    }

    # Create formula
    regression_formula <-
      as.character(glue("{outcome} ~ ", glue_collapse(covars, sep = " + ")))

    # Create model
    regression_model <-
      glm(
        as.formula(regression_formula),
        data = model_data,
        weights = model_weights,
        family = "binomial"
      )
    # The warning is because outcome*weight does not give an integer.

    # Predict to calculate AUC
    regression_pred <-
      model_data %>%
      mutate(
        regression_xb =
          predict(regression_model)
        # linear predictor given by default
      )

    # Save out AUC and model N
    regression_roc <-
      pROC::roc(
        formula = as.formula(glue("{outcome} ~ regression_xb")),
        data = regression_pred
      )

    regression_stats <-
      tibble(
        covariate = c("AUC", "Model N"),
        value = c(regression_roc$auc[1], nobs(regression_model))
      )

    # SURVIVAL MODELS #

  } else if(model_type == "survival") {

    # Confirm only two levels of outcome variable
    if(length(model_data %>% pull(outcome[2]) %>% unique()) != 2) {
      stop("The outcome must have two and only two levels",
           call. = FALSE)
    }

    # Confirm enough events or non-events
    # event_n <-
    #   model_data %>%
    #   # TODO: Okay to use "get" here or is there a better way? .env$outcome[2]??
    #   dplyr::summarize(sum = sum(get(outcome[2]), na.rm = TRUE)) %>%
    #   pull()

    min_event_n <-
      min(model_data %>% pull(.env$outcome[[2]]) %>% table(), na.rm = TRUE)

    if(min_event_n < length(covars)*10) {
      warning(glue("Only {min_event_n} events for {length(covars)} covariates"),
              call. = FALSE)
      regression_coefs <- tibble(status = glue("No model: {min_event_n} events for {length(covars)} covariates"))
      return(regression_coefs)
    }

    # Create formula
    regression_formula <-
      as.character(
        glue("survival::Surv({outcome[1]}, {outcome[2]}) ~ ",
             glue_collapse(covars, sep = " + ")))

    # Create model
    regression_model <-
      survival::survreg(
        as.formula(regression_formula),
        data = model_data,
        weights = model_weights,
        dist = "loglogistic"
      )

    # 7/7/2022: If the dataset is very large then calculating c-index with Hmisc
    # is extremely slow - updating code to use concordance from survival package
    # which allows us to skip several steps and this will run faster

    regression_concordance <-
      survival::concordance(regression_model) %>%
      purrr::pluck("concordance")

    # Calculate c-index
    regression_stats <-
      tibble(
        covariate = "C-index",
        value = regression_concordance
      ) %>%
      dplyr::bind_rows(
        tibble(
          covariate = "Model N",
          value = summary(regression_model)$n
        )
      )

    # QUANTILE MODELS #

  } else if (model_type == "quantile") {

    # Create formula
    regression_formula <-
      as.character(glue("{outcome} ~ ", glue_collapse(covars, sep = " + ")))

    # Create model
    regression_model <-
      quantreg::rq(
        as.formula(regression_formula),
        data = model_data,
        weights = model_weights,
        tau = quantile_tau
      )

    # For quantile, only need to save out modelN
    # 1 row per tau, so that it carries through if reshaping is necessary
    regression_stats <-
      tidyr::expand_grid(
        tau = quantile_tau,
        covariate = "Model N",
        value = nrow(model_data)
      )

  }

  # Save out coefficients using broom::tidy and merge in any other statistics needed
  # TODO: Is there a better way to deal with "no visible binding" note?
  term <- estimate <- NULL
  regression_coefs <-
    broom::tidy(regression_model) %>%
    # Convert scaling parameter from log scale
    mutate(
      estimate =
        case_when(
          .data$term == "Log(scale)" ~ exp(.data$estimate),
          TRUE ~ .data$estimate
        )
    ) %>%
    # Fix name for intercept and scaling parameter, if necessary
    mutate(
      term =
        case_when(
          .data$term == "(Intercept)" ~ "Intercept",
          .data$term == "Log(scale)" ~ "Scaling Parameter",
          TRUE ~ .data$term
        )
    ) %>%
    # Rename and keep variables as necessary
    select(covariate = term, value = estimate, dplyr::starts_with("tau")) %>%
    # Merge in other statistics (N, AUC, C index)
    dplyr::bind_rows(regression_stats)

  # Add variable labels, if specified
  if(rlang::is_empty(covars) == FALSE & !is.null(labels)) {
    regression_coefs <-
      regression_coefs %>%
      dplyr::left_join(
        tibble(
          covariate = c(covars, "Intercept", "Model N", "AUC", "C-index", "Scaling Parameter"),
          label = c(labels, "Intercept", "Model N", "AUC", "C-index", "Scaling Parameter"),
        ),
        by = c("covariate")
      )
  }

  # Return coefficients
  return(regression_coefs)

}
