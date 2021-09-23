#' Distance between two locations
#'
#' `step_geodist` creates a *specification* of a
#'  recipe step that will calculate the distance between
#'  points on a map to a reference location.
#'
#' @inheritParams step_pca
#' @inheritParams step_center
#' @param lon,lat Selector functions to choose which variables are
#'  used by the step. See [selections()] for more details.
#' @param ref_lon,ref_lat Single numeric values for the location
#'  of the reference point.
#' @param is_lat_lon A logical: Are coordinates in latitude and longitude? If
#'  `TRUE` the Haversine formula is used and the returned result is meters. If
#'  `FALSE` the Pythagorean formula is used. Default is `TRUE` and for recipes
#'  created from previous versions of recipes, a value of `FALSE` is used.
#' @param log A logical: should the distance be transformed by
#'  the natural log function?
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param name A single character value to use for the new
#'  predictor column. If a column exists with this name, an error is
#'  issued.
#' @template step-return
#' @details When you [`tidy()`] this step, a tibble with columns echoing the
#'  values of `lat`, `lon`, `ref_lat`, `ref_lon`, `is_lat_lon`, `name`, and `id`
#'  is returned.
#' @family multivariate transformation steps
#' @references https://en.wikipedia.org/wiki/Haversine_formula
#' @export
#' @details `step_geodist` uses the Pythagorean theorem to calculate Euclidean
#'  distances if `is_lat_lon` is FALSE. If `is_lat_lon` is TRUE, the Haversine
#'  formula is used to calculate the great-circle distance in meters.
#'
#' @examples
#'
#' library(modeldata)
#' data(Smithsonian)
#'
#' # How close are the museums to Union Station?
#' near_station <- recipe( ~ ., data = Smithsonian) %>%
#'   update_role(name, new_role = "location") %>%
#'   step_geodist(lat = latitude, lon = longitude, log = FALSE,
#'                ref_lat = 38.8986312, ref_lon = -77.0062457,
#'                is_lat_lon = TRUE) %>%
#'   prep(training = Smithsonian)
#'
#' bake(near_station, new_data = NULL) %>%
#'   arrange(geo_dist)
#'
#' tidy(near_station, number = 1)
step_geodist <- function(recipe,
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
                         skip = FALSE,
                         id = rand_id("geodist")) {
  if (length(ref_lon) != 1 || !is.numeric(ref_lon))
    rlang::abort("`ref_lon` should be a single numeric value.")
  if (length(ref_lat) != 1 || !is.numeric(ref_lat))
    rlang::abort("`ref_lat` should be a single numeric value.")
  if (length(is_lat_lon) != 1 || !is.logical(is_lat_lon))
    rlang::abort("`is_lat_lon` should be a single logical value.")
  if (length(log) != 1 || !is.logical(log))
    rlang::abort("`log` should be a single logical value.")
  if (length(name) != 1 || !is.character(name))
    rlang::abort("`name` should be a single character value.")

  if (is_lat_lon && abs(ref_lat) > 90.0)
    rlang::abort("`ref_lat` should be between -90 and 90")
  if (is_lat_lon && abs(ref_lon) > 180.0)
    rlang::abort("`ref_lon` should be between -180 and 180")


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
      skip = skip,
      id = id
    )
  )
}

step_geodist_new <-
  function(lon, lat, role, trained, ref_lon, ref_lat, is_lat_lon,
           log, name, columns, skip, id) {
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
      skip = skip,
      id = id
    )
  }

# for backward compatibility
check_is_lat_lon <- function(x) {
  if (!'is_lat_lon' %in% names(x)) {
    x$is_lat_lon <- FALSE
  }
  x
}


#' @export
prep.step_geodist <- function(x, training, info = NULL, ...) {
  lon_name <- recipes_eval_select(x$lon, training, info)
  lat_name <- recipes_eval_select(x$lat, training, info)

  x <- check_is_lat_lon(x)

  if (length(lon_name) > 1)
    rlang::abort("`lon` should resolve to a single column name.")
  check_type(training[, lon_name])

  if (length(lat_name) > 1)
    rlang::abort("`lat` should resolve to a single column name.")
  check_type(training[, lat_name])


  if (any(names(training) == x$name))
    rlang::abort("'", x$name, "' is already used in the data.")

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
    skip = x$skip,
    id = x$id
  )
}

geo_dist_calc_xy <- function(x_1, y_1, x_2, y_2) {
  sqrt((x_1 - x_2) ^ 2L + (y_1 - y_2) ^ 2L)
}


# earth_radius = 6371e3 in meters
geo_dist_calc_lat_lon <- function(x_1, y_1, x_2, y_2, earth_radius = 6371e3) {

  if(any(abs(x_1) > 180.0)) {
    rlang::abort("All `lon` values should be between -180 and 180")
  }

  if(any(abs(y_1) > 90.0)) {
    rlang::abort("All `lat` values should be between -90 and 90")
  }

  to_rad <- pi / 180.0

  y_1 <- y_1 * to_rad
  y_2 <- y_2 * to_rad

  x_1 <- x_1 * to_rad
  x_2 <- x_2 * to_rad

  delta_lat <- (y_2 - y_1) / 2.0
  delta_lon <- (x_2 - x_1) / 2.0

  a <- sin(delta_lat) ^ 2L + cos(y_1) * cos(y_2) * sin(delta_lon) ^ 2L
  c <- 2.0 * atan2(sqrt(a), sqrt(1-a))

  earth_radius * c

}


#' @export
bake.step_geodist <- function(object, new_data, ...) {

  object <- check_is_lat_lon(object)

  if(object$is_lat_lon) {
    dist_vals <-
      geo_dist_calc_lat_lon(new_data[[object$columns[2]]], # lon
                            new_data[[object$columns[1]]], # lat
                            object$ref_lon,
                            object$ref_lat)
  } else {
    dist_vals <-
      geo_dist_calc_xy(new_data[[object$columns[2]]], # lon
                       new_data[[object$columns[1]]], # lat
                       object$ref_lon,
                       object$ref_lat)
  }

  if (object$log) {
    new_data[, object$name] <- log(dist_vals)
  } else {
    new_data[, object$name] <- dist_vals
  }

  new_data
}

print.step_geodist <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("Geographical distances from",
        format(x$ref_lat, digits = 10), "x",
        format(x$ref_lon, digits = 10), "\n")
    invisible(x)
  }



#' @rdname tidy.recipe
#' @export
tidy.step_geodist <- function(x, ...) {

  x <- check_is_lat_lon(x)

  if (is_trained(x)) {
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
