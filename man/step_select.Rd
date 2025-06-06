% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select.R
\name{step_select}
\alias{step_select}
\title{Select variables using dplyr}
\usage{
step_select(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("select")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose variables for this step.
See \code{\link[=selections]{selections()}} for more details.}

\item{role}{For model terms selected by this step, what analysis role should
they be assigned?}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by
\code{\link[=bake]{bake()}}? While all operations are baked when \code{\link[=prep]{prep()}} is run, some
operations may not be able to be conducted on new data (e.g. processing the
outcome variable(s)). Care should be taken when using \code{skip = TRUE} as it
may affect the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of any existing operations.
}
\description{
\code{step_select()} creates a \emph{specification} of a recipe step that will select
variables using \code{\link[dplyr:select]{dplyr::select()}}.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

Due to how \code{step_select()} works with \code{workflows::workflow()}, we no longer
recommend the usage of this step.If you are using \code{step_select()} to remove
variables with \code{-} then you can flip it around and use \code{\link[=step_rm]{step_rm()}} instead.
All other uses of \code{step_select()} could be replaced by a call to
\code{\link[dplyr:select]{dplyr::select()}} on the data before it is passed to \code{recipe()}.
}
\details{
When an object in the user's global environment is referenced in the
expression defining the new variable(s), it is a good idea to use
quasiquotation (e.g. \verb{!!}) to embed the value of the object in the expression
(to be portable between sessions). See the examples.

This step can potentially remove columns from the data set. This may
cause issues for subsequent steps in your recipe if the missing columns are
specifically referenced by name. To avoid this, see the advice in the
\emph{Tips for saving recipes and filtering columns} section of \link{selections}.
}
\section{Tidying}{
When you \code{\link[=tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms} and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{id}{character, id of this step}
}
}

\section{Sparse data}{


This step can be applied to \link{sparse_data} such that it is preserved. Nothing
needs to be done for this to happen as it is done automatically.
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
library(dplyr)

iris_tbl <- as_tibble(iris)
iris_train <- slice(iris_tbl, 1:75)
iris_test <- slice(iris_tbl, 76:150)

dplyr_train <- select(iris_train, Species, starts_with("Sepal"))
dplyr_test <- select(iris_test, Species, starts_with("Sepal"))

rec <- recipe(~., data = iris_train) |>
  step_select(Species, starts_with("Sepal")) |>
  prep(training = iris_train)

rec_train <- bake(rec, new_data = NULL)
all.equal(dplyr_train, rec_train)

rec_test <- bake(rec, iris_test)
all.equal(dplyr_test, rec_test)

# Local variables
sepal_vars <- c("Sepal.Width", "Sepal.Length")

qq_rec <-
  recipe(~., data = iris_train) |>
  # fine for interactive usage
  step_select(Species, all_of(sepal_vars)) |>
  # best approach for saving a recipe to disk
  step_select(Species, all_of(!!sepal_vars))

# Note that `sepal_vars` is inlined in the second approach
qq_rec
}
\seealso{
Other variable filter steps: 
\code{\link{step_corr}()},
\code{\link{step_filter_missing}()},
\code{\link{step_lincomb}()},
\code{\link{step_nzv}()},
\code{\link{step_rm}()},
\code{\link{step_zv}()}

Other dplyr steps: 
\code{\link{step_arrange}()},
\code{\link{step_filter}()},
\code{\link{step_mutate}()},
\code{\link{step_mutate_at}()},
\code{\link{step_rename}()},
\code{\link{step_rename_at}()},
\code{\link{step_sample}()},
\code{\link{step_slice}()}
}
\concept{dplyr steps}
\concept{variable filter steps}
