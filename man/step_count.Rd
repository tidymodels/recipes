% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count.R
\name{step_count}
\alias{step_count}
\title{Create counts of patterns using regular expressions}
\usage{
step_count(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  pattern = ".",
  normalize = FALSE,
  options = list(),
  result = make.names(pattern),
  input = NULL,
  sparse = "auto",
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("count")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{A single selector function to choose which variable will be
searched for the regex pattern. The selector should resolve to a single
variable. See \code{\link[=selections]{selections()}} for more details.}

\item{role}{For model terms created by this step, what analysis role should
they be assigned? By default, the new columns created by this step from the
original variables will be used as \emph{predictors} in a model.}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{pattern}{A character string containing a regular expression (or
character string for \code{fixed = TRUE}) to be matched in the given character
vector. Coerced by \code{as.character} to a character string if possible.}

\item{normalize}{A logical; should the integer counts be divided by the total
number of characters in the string?.}

\item{options}{A list of options to \code{\link[=gregexpr]{gregexpr()}} that should not include \code{x}
or \code{pattern}.}

\item{result}{A single character value for the name of the new variable. It
should be a valid column name.}

\item{input}{A single character value for the name of the variable being
searched. This is \code{NULL} until computed by \code{\link[=prep]{prep()}}.}

\item{sparse}{A single string. Should the columns produced be sparse vectors.
Can take the values \code{"yes"}, \code{"no"}, and \code{"auto"}. If \code{sparse = "auto"}
then workflows can determine the best option. Defaults to \code{"auto"}.}

\item{keep_original_cols}{A logical to keep the original variables in the
output. Defaults to \code{TRUE}.}

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
\code{step_count()} creates a \emph{specification} of a recipe step that will create a
variable that counts instances of a regular expression pattern in text.
}
\section{Tidying}{
When you \code{\link[=tidy.recipe]{tidy()}} this step, a tibble is returned with
columns \code{terms}, \code{result} , and \code{id}:

\describe{
\item{terms}{character, the selectors or variables selected}
\item{result}{character, the new column names}
\item{id}{character, id of this step}
}
}

\section{Sparse data}{


This step produces sparse columns if \code{sparse = "yes"} is being set. The
default value \code{"auto"} won't trigger production fo sparse columns if a recipe
is \code{\link[=prep]{prep()}}ed, but allows for a workflow to toggle to \code{"yes"} or \code{"no"}
depending on whether the model supports \link{sparse_data} and if the model is
is expected to run faster with the data.

The mechanism for determining how much sparsity is produced isn't perfect,
and there will be times when you want to manually overwrite by setting
\code{sparse = "yes"} or \code{sparse = "no"}.
}

\section{Case weights}{


The underlying operation does not allow for case weights.
}

\examples{
\dontshow{if (rlang::is_installed("modeldata")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data(covers, package = "modeldata")

rec <- recipe(~description, covers) |>
  step_count(description, pattern = "(rock|stony)", result = "rocks") |>
  step_count(description, pattern = "famil", normalize = TRUE)

rec2 <- prep(rec, training = covers)
rec2

count_values <- bake(rec2, new_data = covers)
count_values

tidy(rec, number = 1)
tidy(rec2, number = 1)
\dontshow{\}) # examplesIf}
}
\seealso{
Other dummy variable and encoding steps: 
\code{\link{step_bin2factor}()},
\code{\link{step_date}()},
\code{\link{step_dummy}()},
\code{\link{step_dummy_extract}()},
\code{\link{step_dummy_multi_choice}()},
\code{\link{step_factor2string}()},
\code{\link{step_holiday}()},
\code{\link{step_indicate_na}()},
\code{\link{step_integer}()},
\code{\link{step_novel}()},
\code{\link{step_num2factor}()},
\code{\link{step_ordinalscore}()},
\code{\link{step_other}()},
\code{\link{step_regex}()},
\code{\link{step_relevel}()},
\code{\link{step_string2factor}()},
\code{\link{step_time}()},
\code{\link{step_unknown}()},
\code{\link{step_unorder}()}
}
\concept{dummy variable and encoding steps}
