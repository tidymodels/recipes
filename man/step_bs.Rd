% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bs.R
\name{step_bs}
\alias{step_bs}
\title{B-Spline Basis Functions}
\usage{
step_bs(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  deg_free = NULL,
  degree = 3,
  objects = NULL,
  options = list(),
  skip = FALSE,
  id = rand_id("bs")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose which
variables are affected by the step. See \code{\link[=selections]{selections()}}
for more details.}

\item{role}{For model terms created by this step, what analysis
role should they be assigned?. By default, the function assumes
that the new columns created from the original variables will be
used as predictors in a model.}

\item{trained}{A logical to indicate if the quantities for
preprocessing have been estimated.}

\item{deg_free}{The degrees of freedom for the spline. As the
degrees of freedom for a spline increase, more flexible and
complex curves can be generated. When a single degree of freedom is used,
the result is a rescaled version of the original data.}

\item{degree}{Degree of polynomial spline (integer).}

\item{objects}{A list of \code{\link[splines:bs]{splines::bs()}} objects
created once the step has been trained.}

\item{options}{A list of options for \code{\link[splines:bs]{splines::bs()}}
which should not include \code{x}, \code{degree}, or \code{df}.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[=bake.recipe]{bake.recipe()}}? While all operations are baked
when \code{\link[=prep.recipe]{prep.recipe()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect
the computations for subsequent operations}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of any existing steps.
}
\description{
\code{step_bs} creates a \emph{specification} of a recipe step
that will create new columns that are basis expansions of
variables using B-splines.
}
\details{
\code{step_bs} can create new features from a single variable
that enable fitting routines to model this variable in a
nonlinear manner. The extent of the possible nonlinearity is
determined by the \code{df}, \code{degree}, or \code{knot} arguments of
\code{\link[splines:bs]{splines::bs()}}. The original variables are removed
from the data and new columns are added. The naming convention
for the new variables is \code{varname_bs_1} and so on.

When you \code{\link[=tidy]{tidy()}} this step, a tibble with column \code{terms} (the
columns that will be affected) is returned.
}
\examples{
library(modeldata)
data(biomass)

biomass_tr <- biomass[biomass$dataset == "Training",]
biomass_te <- biomass[biomass$dataset == "Testing",]

rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass_tr)

with_splines <- rec \%>\%
  step_bs(carbon, hydrogen)
with_splines <- prep(with_splines, training = biomass_tr)

expanded <- bake(with_splines, biomass_te)
expanded
}
\seealso{
\code{\link[=step_poly]{step_poly()}} \code{\link[=recipe]{recipe()}} \code{\link[=step_ns]{step_ns()}}
\code{\link[=prep.recipe]{prep.recipe()}} \code{\link[=bake.recipe]{bake.recipe()}}
}
\concept{basis_expansion}
\concept{preprocessing}
\keyword{datagen}
