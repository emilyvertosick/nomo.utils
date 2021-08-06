#' @keywords internal
#' @importFrom dplyr %>% mutate select rename filter pull case_when n
#' @importFrom glue glue glue_collapse
#' @importFrom tibble enframe tibble
#' @importFrom stats as.formula complete.cases glm lm nobs predict
#' @importFrom rlang .data .env
#'
"_PACKAGE"

# allowing for the use of the dot when piping
utils::globalVariables(".")

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
