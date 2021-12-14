
# Create data for test

mtcars_id <-
  mtcars %>%
  dplyr::mutate(
    id = 1:n(),
    model_weights = ifelse(drat >= 3 & drat <= 4, 0.75, 1)
  )

# Create coefficients to pass to generate_pred for tests

mtcars_linear <-
  generate_coefs(
    data = mtcars_id,
    outcome = "mpg",
    covars = c("wt", "cyl"),
    id = "id",
    model_type = "linear"
  )

mtcars_logistic <-
  generate_coefs(
    data = mtcars_id,
    outcome = "am",
    covars  = c("cyl"),
    id = "id",
    model_type = "logistic"
  )

mtcars_survival <-
  generate_coefs(
    data = mtcars_id,
    outcome = c("qsec", "am"),
    covars = c("cyl"),
    id = "id",
    model_type = "survival"
  )

mtcars_quantile <-
  generate_coefs(
    data = mtcars_id,
    outcome = c("mpg"),
    covars = c("qsec"),
    id = "id",
    model_type = "quantile",
    quantile_tau = c(0.5)
  )

test_that("function works correctly for all model types", {

  # linear
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear,
      model_type = "linear",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    NA
  )

  # logistic
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_logistic,
      model_type = "logistic",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    NA
  )

  # survival, with default options
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_survival,
      model_type = "survival",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    NA
  )

  # survival, changing time options
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_survival,
      model_type = "survival",
      id = "id",
      covariate = "covariate",
      value = "value",
      starttime = 10,
      outcometime = 15
    ),
    NA
  )

  # quantile
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_quantile,
      model_type = "quantile",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    NA
  )

})

test_that("error if correct variables don't exist in coefficients data", {

  # covariate variable
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear,
      model_type = "linear",
      id = "id",
      covariate = "covar",
      value = "value"
    ),
    "*"
  )

  # value variable
  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear,
      model_type = "linear",
      id = "id",
      covariate = "covariate",
      value = "coef_value"
    ),
    "*"
  )

})

test_that("error if covariates do not exist in cohort data", {

  expect_error(
    generate_pred(
      data = mtcars_id %>% rename(wt2 = wt),
      coefficients = mtcars_linear,
      model_type = "linear",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    "*"
  )

})

test_that("error if id variable does not exist in data", {

  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear,
      model_type = "linear",
      id = "patient_id",
      covariate = "covariate",
      value = "value"
    ),
    "*"
  )

})

test_that("error if value variable is not numeric", {

  expect_error(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear %>% mutate(value = as.character(value)),
      model_type = "linear",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    "*"
  )

})

test_that("warning if any NA values for coefficients", {

  expect_warning(
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_linear %>%
        mutate(value = case_when(covariate != "cyl" ~ value)),
      model_type = "linear",
      id = "id",
      covariate = "covariate",
      value = "value"
    ),
    "*"
  )

})

test_that("predictions are different if different outcome times given", {

  mtcars_outcome <-
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_survival,
      model_type = "survival",
      id = "id",
      covariate = "covariate",
      value = "value",
      outcometime = 10
    ) %>%
    select(id, event_pr10 = event_pr) %>%
    dplyr::left_join(
      generate_pred(
        data = mtcars_id,
        coefficients = mtcars_survival,
        model_type = "survival",
        id = "id",
        covariate = "covariate",
        value = "value",
        outcometime = 15
      ) %>%
        select(id, event_pr15 = event_pr),
      by = "id"
    )

  expect_false(all(mtcars_outcome$event_pr10 == mtcars_outcome$event_pr15, na.rm = TRUE))

})

test_that("predictions are different if different start times given", {

  mtcars_start <-
    generate_pred(
      data = mtcars_id,
      coefficients = mtcars_survival,
      model_type = "survival",
      id = "id",
      covariate = "covariate",
      value = "value",
      starttime = 5,
      outcometime = 15
    ) %>%
    select(id, event_pr5 = event_pr) %>%
    dplyr::left_join(
      generate_pred(
        data = mtcars_id,
        coefficients = mtcars_survival,
        model_type = "survival",
        id = "id",
        covariate = "covariate",
        value = "value",
        starttime = 10,
        outcometime = 15
      ) %>%
        select(id, event_pr10 = event_pr),
      by = "id"
    )

  expect_false(all(mtcars_start$event_pr5 == mtcars_start$event_pr10, na.rm = TRUE))

})
