#' recipes: A package for computing and preprocessing design matrices.
#'
#' The `recipes` package can be used to create design matrices for modeling and
#' to conduct preprocessing of variables. It is meant to be a more extensive
#' framework that R's formula method. Some differences between simple formula
#' methods and recipes are that
#' \enumerate{
#' \item Variables can have arbitrary roles in the analysis beyond predictors
#'  and outcomes.
#' \item A recipe consists of one or more steps that define actions on the
#'  variables.
#' \item Recipes can be defined sequentially using pipes as well as being
#'  modifiable and extensible.
#' }
#'
#'
#' @section Basic Functions:
#'
#'   The three main functions are [recipe()], [prep()], and [bake()].
#'
#'   [recipe()] defines the operations on the data and the associated roles.
#'   Once the preprocessing steps are defined, any parameters are estimated
#'   using [prep()]. Once the data are ready for transformation, the [bake()]
#'   function applies the operations.
#'
#' @section Step Functions:
#'
#'   These functions are used to add new actions to the recipe and have the
#'   naming convention `"step_action"`. For example, [step_center()] centers the
#'   data to have a zero mean and [step_dummy()] is used to create dummy
#'   variables.
#' @docType package
#' @keywords internal
#' @name recipes
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import timeDate
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom dplyr do
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom glue glue
#' @importFrom gower gower_topn
#' @importFrom ipred ipredbagg
#' @importFrom lifecycle deprecated
#' @importFrom lubridate am
#' @importFrom lubridate decimal_date
#' @importFrom lubridate hour
#' @importFrom lubridate is.Date
#' @importFrom lubridate minute
#' @importFrom lubridate month
#' @importFrom lubridate quarter
#' @importFrom lubridate second
#' @importFrom lubridate semester
#' @importFrom lubridate wday
#' @importFrom lubridate week
#' @importFrom lubridate yday
#' @importFrom lubridate mday
#' @importFrom lubridate year
#' @importFrom Matrix Matrix
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom stats cov
#' @importFrom stats cov.wt
#' @importFrom stats lm
#' @importFrom stats mahalanobis
#' @importFrom stats median
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats na.pass
#' @importFrom stats optimize
#' @importFrom stats poly
#' @importFrom stats prcomp
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom stats terms
#' @importFrom stats var
#' @importFrom tibble add_column
#' @importFrom tibble as_tibble
#' @importFrom tibble is_tibble
#' @importFrom tibble tibble
#' @importFrom tidyselect everything
#' @importFrom utils install.packages
#' @importFrom utils object.size
#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_cbind
#' @importFrom vctrs vec_detect_complete
#' @importFrom vctrs vec_slice
## usethis namespace: end
NULL
