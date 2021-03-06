% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing.r
\name{check_missing}
\alias{check_missing}
\title{Check for Missing Values}
\usage{
check_missing(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("missing")
)
}
\arguments{
\item{recipe}{A recipe object. The check will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are checked in the check See \code{\link[=selections]{selections()}}
for more details.}

\item{role}{Not used by this check since no new variables are
created.}

\item{trained}{A logical for whether the selectors in \code{...}
have been resolved by \code{\link[=prep]{prep()}}.}

\item{columns}{A character string of variable names that will
be populated (eventually) by the terms argument.}

\item{skip}{A logical. Should the check be skipped when the
recipe is baked by \code{\link[=bake.recipe]{bake.recipe()}}? While all operations are baked
when \code{\link[=prep.recipe]{prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect
the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of any existing steps.
}
\description{
\code{check_missing} creates a \emph{specification} of a recipe
operation that will check if variables contain missing values.
}
\details{
This check will break the \code{bake} function if any of the checked
columns does contain \code{NA} values. If the check passes, nothing is changed
to the data.

When you \code{\link[=tidy]{tidy()}} this check, a tibble with column \code{terms} (the
selectors or variables selected) is returned.
}
\examples{
library(modeldata)
data(credit_data)
is.na(credit_data) \%>\% colSums()

# If the test passes, `new_data` is returned unaltered
recipe(credit_data) \%>\%
  check_missing(Age, Expenses) \%>\%
  prep() \%>\%
  bake(credit_data)

# If your training set doesn't pass, prep() will stop with an error

\dontrun{
recipe(credit_data)  \%>\%
  check_missing(Income) \%>\%
  prep()
}

# If `new_data` contain missing values, the check will stop bake()

train_data <- credit_data \%>\% dplyr::filter(Income > 150)
test_data  <- credit_data \%>\% dplyr::filter(Income <= 150 | is.na(Income))

rp <- recipe(train_data) \%>\%
  check_missing(Income) \%>\%
  prep()

bake(rp, train_data)
\dontrun{
bake(rp, test_data)
}
}
