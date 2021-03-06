% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depth.R, R/ica.R, R/isomap.R, R/kpca.R,
%   R/kpca_poly.R, R/kpca_rbf.R, R/nnmf.R, R/pls.R, R/window.R
\name{required_pkgs.step_depth}
\alias{required_pkgs.step_depth}
\alias{required_pkgs.step_ica}
\alias{required_pkgs.step_isomap}
\alias{required_pkgs.step_kpca}
\alias{required_pkgs.step_kpca_poly}
\alias{required_pkgs.step_kpca_rbf}
\alias{required_pkgs.step_nnmf}
\alias{required_pkgs.step_pls}
\alias{required_pkgs.step_window}
\title{S3 methods for tracking which additional packages are needed for steps.}
\usage{
\method{required_pkgs}{step_depth}(x, ...)

\method{required_pkgs}{step_ica}(x, ...)

\method{required_pkgs}{step_isomap}(x, ...)

\method{required_pkgs}{step_kpca}(x, ...)

\method{required_pkgs}{step_kpca_poly}(x, ...)

\method{required_pkgs}{step_kpca_rbf}(x, ...)

\method{required_pkgs}{step_nnmf}(x, ...)

\method{required_pkgs}{step_pls}(x, ...)

\method{required_pkgs}{step_window}(x, ...)
}
\arguments{
\item{x}{A recipe step}
}
\value{
A character vector
}
\description{
S3 methods for tracking which additional packages are needed for steps.
}
\keyword{internal}
