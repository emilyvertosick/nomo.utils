
mtcars_id <-
  mtcars %>%
  mutate(
    id = 1:n(),
    model_weights = ifelse(drat >= 3 & drat <= 4, 0.75, 1)
  )

test_that("function works correctly for all model types", {

  # linear
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear"
    ),
    NA
  )

  # logistic
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "am",
      covars  = c("cyl"),
      id = "id",
      model_type = "logistic"
    ),
    NA
  )

  # survival
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("qsec", "am"),
      covars = c("cyl"),
      id = "id",
      model_type = "survival"
    ),
    NA
  )

  # quantile
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("mpg"),
      covars = c("qsec"),
      id = "id",
      model_type = "quantile",
      quantile_tau = c(0.5)
    ),
    NA
  )

})

test_that("function works correctly for all model types when no covariates specified", {

  # linear
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = NULL,
      id = "id",
      model_type = "linear",
      labels = NULL
    ),
    NA
  )

  # logistic
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "am",
      id = "id",
      model_type = "logistic",
      labels = c("")
    ),
    NA
  )

  # survival
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("qsec", "am"),
      covars = character(0),
      id = "id",
      model_type = "survival"
    ),
    NA
  )

  # quantile
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("mpg"),
      covars = NULL,
      id = "id",
      model_type = "quantile",
      quantile_tau = c(0.5)
    ),
    NA
  )

})

test_that("non-required options work correctly", {

  # model weights
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear",
      model_weights = "model_weights"
    ),
    NA
  )

  # labels
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear",
      labels = c("WT", "cylinder")
    ),
    NA
  )


})

test_that("error if input does not exist", {

  # outcome
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg2",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear"
    ),
    "*"
  )

  # covariate
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = c("wt2", "cyl"),
      id = "id",
      model_type = "linear"
    ),
    "*"
  )

  # id
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg2",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear"
    ),
    "*"
  )

  # model_weights
  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "mpg",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear",
      model_weights = "weight2"
    ),
    "*"
  )

})

test_that("error if passing blank string as covariates", {

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "am",
      covars = c("", ""),
      id = "id",
      model_type = "logistic",
      labels = c("")
    ),
    "*"
  )

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "am",
      covars = "",
      id = "id",
      model_type = "logistic",
      labels = c("")
    ),
    "*"
  )

})

test_that("survival outcome must have time and event variables", {

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("qsec"),
      covars = c("cyl"),
      id = "id",
      model_type = "survival"
    ),
    "*"
  )

})

test_that("labels and covars must be the same length", {

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("qsec", "am"),
      covars = c("cyl"),
      id = "id",
      model_type = "survival",
      labels = c("cylinder", "test")
    ),
    "*"
  )

})

# warning if only one outcome level
test_that("warning if only one outcome level", {

  expect_warning(
    generate_coefs(
      data = mtcars_id %>% mutate(gear = 4),
      outcome = "gear",
      covars = c("cyl"),
      id = "id",
      model_type = "logistic"
    ),
    "*"
  )

})

# survival and logistic outcomes must have 2 levels
test_that("survival and logistic outcomes have 2 levels", {

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = "gear",
      covars  = c("cyl"),
      id = "id",
      model_type = "logistic"
    ),
    "*"
  )

  expect_error(
    generate_coefs(
      data = mtcars_id,
      outcome = c("qsec", "gear"),
      covars = c("cyl"),
      id = "id",
      model_type = "survival"
    ),
    "*"
  )


})

# no models with < 10 observations
test_that("no models with < 10 observations", {

  expect_warning(
    generate_coefs(
      data = mtcars_id %>% dplyr::slice(1:9),
      outcome = "mpg",
      covars = c("wt", "cyl"),
      id = "id",
      model_type = "linear"
    ),
    "*"
  )

})

test_that("no models with more variables than necessary events", {

  expect_warning(
    generate_coefs(
      data = mtcars_id %>% dplyr::slice(1:20),
      outcome = "am",
      covars  = c("cyl", "mpg", "wt"),
      id = "id",
      model_type = "logistic"
    ),
    "*"
  )

})

