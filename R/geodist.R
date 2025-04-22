#' Distance between two locations
#'
#' `step_geodist()` creates a *specification* of a recipe step that will
#' calculate the distance between points on a map to a reference location.
#'
#' @inheritParams step_classdist
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param lon,lat Selector functions to choose which variables are used by the
#'   step. See [selections()] for more details.
#' @param ref_lon,ref_lat Single numeric values for the location of the
#'   reference point.
#' @param is_lat_lon A logical: Are coordinates in latitude and longitude? If
#'   `TRUE` the Haversine formula is used and the returned result is meters. If
#'   `FALSE` the Pythagorean formula is used. Default is `TRUE` and for recipes
#'   created from previous versions of recipes, a value of `FALSE` is used.
#' @param log A logical: should the distance be transformed by the natural log
#'   function?
#' @param name A single character value to use for the new predictor column. If
#'   a column exists with this name, an error is issued.
#' @template step-return
#' @family multivariate transformation steps
#' @references https://en.wikipedia.org/wiki/Haversine_formula
#' @export
#' @details
#'
#' `step_geodist` uses the Pythagorean theorem to calculate Euclidean distances
#' if `is_lat_lon` is FALSE. If `is_lat_lon` is TRUE, the Haversine formula is
#' used to calculate the great-circle distance in meters.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `latitude`, `longitude`, `ref_latitude`, `ref_longitude`,
#' `is_lat_lon`, `name` , and `id`:
#'
#' \describe{
#'   \item{latitude}{character, name of latitude variable}
#'   \item{longitude}{character, name of longitude variable}
#'   \item{ref_latitude}{numeric, location of latitude reference point}
#'   \item{ref_longitude}{numeric, location of longitude reference point}
#'   \item{is_lat_lon}{character, the summary function name}
#'   \item{name}{character, name of resulting variable}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(Smithsonian, package = "modeldata")
#'
#' # How close are the museums to Union Station?
#' near_station <- recipe(~., data = Smithsonian) |>
#'   update_role(name, new_role = "location") |>
#'   step_geodist(
#'     lat = latitude, lon = longitude, log = FALSE,
#'     ref_lat = 38.8986312, ref_lon = -77.0062457,
#'     is_lat_lon = TRUE
#'   ) |>
#'   prep(training = Smithsonian)
#'
#' bake(near_station, new_data = NULL) |>
#'   arrange(geo_dist)
#'
#' tidy(near_station, number = 1)
step_geodist <- function(
  recipe,
  lat = NULL,
  lon = NULL,
  role = "predictor",
  trained = FALSE,
  ref_lat = NULL,
  ref_lon = NULL,
  is_lat_lon = TRUE,
  log = FALSE,
  name = "geo_dist",
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("geodist")
) {
  check_bool(is_lat_lon)
  if (is_lat_lon) {
    check_number_decimal(ref_lat, min = -90, max = 90)
    check_number_decimal(ref_lon, min = -180, max = 180)
  } else {
    check_number_decimal(ref_lat)
    check_number_decimal(ref_lon)
  }
  check_bool(log)
  check_string(name)

  add_step(
    recipe,
    step_geodist_new(
      lon = enquos(lon),
      lat = enquos(lat),
      role = role,
      trained = trained,
      ref_lon = ref_lon,
      ref_lat = ref_lat,
      is_lat_lon = is_lat_lon,
      log = log,
      name = name,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  )
}

step_geodist_new <-
  function(
    lon,
    lat,
    role,
    trained,
    ref_lon,
    ref_lat,
    is_lat_lon,
    log,
    name,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "geodist",
      lon = lon,
      lat = lat,
      role = role,
      trained = trained,
      ref_lon = ref_lon,
      ref_lat = ref_lat,
      is_lat_lon = is_lat_lon,
      log = log,
      name = name,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

# for backward compatibility
check_is_lat_lon <- function(x) {
  if (!"is_lat_lon" %in% names(x)) {
    x$is_lat_lon <- FALSE
  }
  x
}

#' @export
prep.step_geodist <- function(x, training, info = NULL, ...) {
  lon_name <- recipes_eval_select(x$lon, training, info)
  lat_name <- recipes_eval_select(x$lat, training, info)
  check_type(training[, c(lon_name, lat_name)], types = c("double", "integer"))

  x <- check_is_lat_lon(x)

  if (length(lon_name) > 1) {
    cli::cli_abort(
      c(
        x = "The {.arg lon} selector should select at most a single variable.",
        i = "The following {length(lon_name)} were selected: \\
          {.and {.var {lon_name}}}."
      )
    )
  }
  check_type(training[, lon_name], types = c("double", "integer"))

  if (length(lat_name) > 1) {
    cli::cli_abort(
      c(
        x = "The {.arg lat} selector should select at most a single variable.",
        i = "The following {length(lat_name)} were selected: \\
          {.and {.var {lat_name}}}."
      )
    )
  }
  check_type(training[, lat_name], types = c("double", "integer"))

  step_geodist_new(
    lon = x$lon,
    lat = x$lat,
    role = x$role,
    trained = TRUE,
    ref_lon = x$ref_lon,
    ref_lat = x$ref_lat,
    is_lat_lon = x$is_lat_lon,
    log = x$log,
    name = x$name,
    columns = c(lat_name, lon_name),
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

# TODO case weights for distances
geo_dist_calc_xy <- function(x_1, y_1, x_2, y_2) {
  sqrt((x_1 - x_2)^2L + (y_1 - y_2)^2L)
}

# earth_radius = 6371e3 in meters
geo_dist_calc_lat_lon <- function(
  x_1,
  y_1,
  x_2,
  y_2,
  earth_radius = 6371e3,
  call = caller_env()
) {
  if (any(abs(x_1) > 180.0)) {
    cli::cli_abort(
      "All {.var lon} values should be between -180 and 180.",
      call = call
    )
  }

  if (any(abs(y_1) > 90.0)) {
    cli::cli_abort(
      "All {.var lat} values should be between -90 and 90.",
      call = call
    )
  }

  to_rad <- pi / 180.0

  y_1 <- y_1 * to_rad
  y_2 <- y_2 * to_rad

  x_1 <- x_1 * to_rad
  x_2 <- x_2 * to_rad

  delta_lat <- (y_2 - y_1) / 2.0
  delta_lon <- (x_2 - x_1) / 2.0

  a <- sin(delta_lat)^2L + cos(y_1) * cos(y_2) * sin(delta_lon)^2L
  c <- 2.0 * atan2(sqrt(a), sqrt(1 - a))

  earth_radius * c
}

#' @export
bake.step_geodist <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  object <- check_is_lat_lon(object)

  if (object$is_lat_lon) {
    dist_vals <-
      geo_dist_calc_lat_lon(
        new_data[[col_names[2]]], # lon
        new_data[[col_names[1]]], # lat
        object$ref_lon,
        object$ref_lat
      )
  } else {
    dist_vals <-
      geo_dist_calc_xy(
        new_data[[col_names[2]]], # lon
        new_data[[col_names[1]]], # lat
        object$ref_lon,
        object$ref_lat
      )
  }

  if (object$log) {
    dist_vals <- log(dist_vals)
  }

  geo_data <- tibble(dist_vals)
  names(geo_data) <- object$name

  geo_data <- check_name(geo_data, new_data, object, newname = object$name)

  new_data <- vec_cbind(new_data, geo_data, .name_repair = "minimal")
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

#' @export
print.step_geodist <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- paste(
      "Geographical distances from",
      format(x$ref_lat, digits = 10),
      "x",
      format(x$ref_lon, digits = 10),
      "using "
    )
    untr_obj <- c(x$lat, x$lon)

    if (
      all(
        vapply(
          untr_obj,
          function(obj) quo_is_null(obj),
          FUN.VALUE = logical(1)
        )
      )
    ) {
      untr_obj <- NULL
    }

    print_step(x$columns, untr_obj, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_geodist <- function(x, ...) {
  x <- check_is_lat_lon(x)

  if (length(x$columns) == 0) {
    res <- tibble(
      latitude = character(),
      longitude = character(),
      ref_latitude = double(),
      ref_longitude = double(),
      is_lat_lon = logical(),
      name = character()
    )
  } else if (is_trained(x)) {
    res <- tibble(
      latitude = x$columns[1],
      longitude = x$columns[2],
      ref_latitude = x$ref_lat,
      ref_longitude = x$ref_lon,
      is_lat_lon = x$is_lat_lon,
      name = x$name
    )
  } else {
    res <- tibble(
      latitude = sel2char(x$lat),
      longitude = sel2char(x$lon),
      ref_latitude = x$ref_lat,
      ref_longitude = x$ref_lon,
      is_lat_lon = x$is_lat_lon,
      name = x$name
    )
  }

  res$id <- x$id
  res
}
