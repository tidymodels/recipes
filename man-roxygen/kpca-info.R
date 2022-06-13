#' @details
#' Kernel principal component analysis (kPCA) is an extension of a PCA analysis
#' that conducts the calculations in a broader dimensionality defined by a
#' kernel function. For example, if a quadratic kernel function were used,
#' each variable would be represented by its original values as well as its
#' square. This nonlinear mapping is used during the PCA analysis and can
#' potentially help find better representations of the original data.
#'
#' This step requires the \pkg{kernlab} package.
#' If not installed, the step will stop with a prompt about installing
#' the package.
#'
#' As with ordinary PCA, it is important to center and scale the variables
#' prior to computing PCA components ([step_normalize()] can be used for
#' this purpose).
#'
#' The argument `num_comp` controls the number of components that will be
#' retained; the original variables that are used to derive the components are
#' removed from the data when `keep_original_cols = FALSE`. The new components
#' will have names that begin with `prefix` and a sequence of numbers. The
#' variable names are padded with zeros. For example, if `num_comp < 10`, the
#' new names will be `kPC1` - `kPC9`. If `num_comp = 101`, the names would be
#' `kPC001` - `kPC101`.
#'
#' # tidy() results
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with column
#' `terms` (the selectors or variables selected) is returned.
#'
#' @references Scholkopf, B., Smola, A., and Muller, K. (1997).
#'  Kernel principal component analysis. *Lecture Notes in
#'  Computer Science*, 1327, 583-588.
#'
#' Karatzoglou, K., Smola, A., Hornik, K., and Zeileis, A. (2004).
#'  kernlab - An S4 package for kernel methods in R. *Journal
#'  of Statistical Software*, 11(1), 1-20.





