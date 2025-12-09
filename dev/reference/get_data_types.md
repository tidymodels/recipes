# Get types for use in recipes

The `.get_data_types()` generic is used internally to supply types to
columns used in recipes. These functions underlie the work that the user
sees in
[selections](https://recipes.tidymodels.org/dev/reference/selections.md).

## Usage

``` r
.get_data_types(x)

# Default S3 method
.get_data_types(x)

# S3 method for class 'character'
.get_data_types(x)

# S3 method for class 'ordered'
.get_data_types(x)

# S3 method for class 'factor'
.get_data_types(x)

# S3 method for class 'integer'
.get_data_types(x)

# S3 method for class 'numeric'
.get_data_types(x)

# S3 method for class 'double'
.get_data_types(x)

# S3 method for class 'Surv'
.get_data_types(x)

# S3 method for class 'logical'
.get_data_types(x)

# S3 method for class 'Date'
.get_data_types(x)

# S3 method for class 'POSIXct'
.get_data_types(x)

# S3 method for class 'list'
.get_data_types(x)

# S3 method for class 'textrecipes_tokenlist'
.get_data_types(x)

# S3 method for class 'hardhat_case_weights'
.get_data_types(x)
```

## Arguments

- x:

  An object

## Details

This function acts as an extended recipes-specific version of
[`class()`](https://rdrr.io/r/base/class.html). By ignoring differences
in similar types ("double" and "numeric") and allowing each element to
have multiple types ("factor" returns "factor", "unordered", and
"nominal", and "character" returns "string", "unordered", and "nominal")
we are able to create more natural selectors such as
[`all_nominal()`](https://recipes.tidymodels.org/dev/reference/has_role.md),
[`all_string()`](https://recipes.tidymodels.org/dev/reference/has_role.md)
and
[`all_integer()`](https://recipes.tidymodels.org/dev/reference/has_role.md).

The following list shows the data types for different classes, as
defined by recipes. If an object has a class not supported by
`.get_data_types()`, it will get data type "other".

- character: string, unordered, and nominal

- ordered: ordered, and nominal

- factor: factor, unordered, and nominal

- integer: integer, and numeric

- numeric: double, and numeric

- double: double, and numeric

- Surv: surv

- logical: logical

- Date: date

- POSIXct: datetime

- list: list

- textrecipes_tokenlist: tokenlist

- hardhat_case_weights: case_weights

## See also

[developer_functions](https://recipes.tidymodels.org/dev/reference/developer_functions.md)

## Examples

``` r
data(Sacramento, package = "modeldata")
lapply(Sacramento, .get_data_types)
#> $city
#> [1] "factor"    "unordered" "nominal"  
#> 
#> $zip
#> [1] "factor"    "unordered" "nominal"  
#> 
#> $beds
#> [1] "integer" "numeric"
#> 
#> $baths
#> [1] "double"  "numeric"
#> 
#> $sqft
#> [1] "integer" "numeric"
#> 
#> $type
#> [1] "factor"    "unordered" "nominal"  
#> 
#> $price
#> [1] "integer" "numeric"
#> 
#> $latitude
#> [1] "double"  "numeric"
#> 
#> $longitude
#> [1] "double"  "numeric"
#> 
```
