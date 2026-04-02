# S3 methods for tracking which additional packages are needed for steps.

S3 methods for tracking which additional packages are needed for steps.

## Usage

``` r
# S3 method for class 'step_classdist_shrunken'
required_pkgs(x, ...)

# S3 method for class 'step_depth'
required_pkgs(x, ...)

# S3 method for class 'step_ica'
required_pkgs(x, ...)

# S3 method for class 'step_isomap'
required_pkgs(x, ...)

# S3 method for class 'step_kpca'
required_pkgs(x, ...)

# S3 method for class 'step_kpca_poly'
required_pkgs(x, ...)

# S3 method for class 'step_kpca_rbf'
required_pkgs(x, ...)

# S3 method for class 'step_mutate'
required_pkgs(x, ...)

# S3 method for class 'step_nnmf'
required_pkgs(x, ...)

# S3 method for class 'step_nnmf_sparse'
required_pkgs(x, ...)

# S3 method for class 'step_pls'
required_pkgs(x, ...)

# S3 method for class 'step_poly_bernstein'
required_pkgs(x, ...)

# S3 method for class 'recipe'
required_pkgs(x, infra = TRUE, ...)

# S3 method for class 'step'
required_pkgs(x, ...)

# S3 method for class 'check'
required_pkgs(x, ...)

# S3 method for class 'step_spline_b'
required_pkgs(x, ...)

# S3 method for class 'step_spline_convex'
required_pkgs(x, ...)

# S3 method for class 'step_spline_monotone'
required_pkgs(x, ...)

# S3 method for class 'step_spline_natural'
required_pkgs(x, ...)

# S3 method for class 'step_spline_nonnegative'
required_pkgs(x, ...)

# S3 method for class 'step_window'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe or recipe step

- infra:

  Should recipes itself be included in the result?

## Value

A character vector
