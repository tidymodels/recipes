% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update-role-requirements.R
\name{update_role_requirements}
\alias{update_role_requirements}
\title{Update role specific requirements}
\usage{
update_role_requirements(recipe, role, ..., bake = NULL)
}
\arguments{
\item{recipe}{A recipe.}

\item{role}{A string representing the role that you'd like to modify the
requirements of. This must be a role that already exists in the recipe.}

\item{...}{These dots are for future extensions and must be empty.}

\item{bake}{At \code{bake()} time, should a check be done to ensure that all
columns of this role that were supplied to \code{recipe()} also be present in
the \code{new_data} supplied to \code{bake()}?

Must be a single \code{TRUE} or \code{FALSE}. The default, \code{NULL}, won't modify this
requirement.

The following represents the default bake time requirements of specific
types of roles:
\itemize{
\item \code{"outcome"}: Not required at bake time. Can't be changed.
\item \code{"predictor"}: Required at bake time. Can't be changed.
\item \code{"case_weights"}: Not required at bake time by default.
\item \code{NA}: Required at bake time by default.
\item Custom roles: Required at bake time by default.
}}
}
\description{
\code{update_role_requirements()} allows you to fine tune
requirements of the various roles you might come across in recipes (see
\code{\link[=update_role]{update_role()}} for general information about roles). Role requirements can
only be altered for roles that exist in the \emph{original} data supplied to
\code{\link[=recipe]{recipe()}}, they are not applied to columns computed by steps.

Like \code{update_role()}, \code{update_role_requirements()} is applied to the recipe
\emph{immediately}, unlike the \verb{step_*()} functions which do most of their work at
\code{\link[=prep]{prep()}} time.
}
\examples{
df <- tibble(y = c(1, 2, 3), x = c(4, 5, 6), var = c("a", "b", "c"))

# Let's assume that you have a `var` column that isn't used in the recipe.
# We typically recommend that you remove this column before passing the
# `data` to `recipe()`, but for now let's pass it through and assign it an
# `"id"` role.
rec <- recipe(y ~ ., df) |>
  update_role(var, new_role = "id") |>
  step_center(x)

prepped <- prep(rec, df)

# Now assume you have some "new data" and you are ready to `bake()` it
# to prepare it for prediction purposes. Here, you might not have `var`
# available as a column because it isn't important to your model.
new_data <- df[c("y", "x")]

# By default `var` is required at `bake()` time because we don't know if
# you actually use it in the recipe or not
try(bake(prepped, new_data))

# You can turn off this check by using `update_role_requirements()` and
# setting `bake = FALSE` for the `"id"` role. We recommend doing this on
# the original unprepped recipe, but it will also work on a prepped recipe.
rec <- update_role_requirements(rec, "id", bake = FALSE)
prepped <- prep(rec, df)

# Now you can `bake()` on `new_data` even though `var` is missing
bake(prepped, new_data)
}
