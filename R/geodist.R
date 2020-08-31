#' Distance between two locations
#'
#' `step_geodist` creates a *specification* of a
#'  recipe step that will calculate the distance between
#'  points on a map to a reference location.
#'
#' @inheritParams step_center
#' @param lon,lat Selector functions to choose which variables are
#'  affected by the step. See selections() for more details.
#' @param ref_lon,ref_lat Single numeric values for the location
#'  of the reference point.
#' @param role or model term created by this step, what analysis
#'  role should be assigned?. By default, the function assumes
#'  that resulting distance will be used as a predictor in a model.
#' @param log A logical: should the distance be transformed by
#'  the natural log function?
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param name A single character value to use for the new
#'  predictor column. If a column exists with this name, an error is
#'  issued.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns echoing the values of `lat`,
#'  `lon`, `ref_lat`, `ref_lon`, `name`, and `id`.
#' @keywords datagen
#' @concept preprocessing
#' @export
#' @details `step_geodist` will create a
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
#'                ref_lat = 38.8986312, ref_lon = -77.0062457) %>%
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
                         log = FALSE,
                         name = "geo_dist",
                         columns = NULL,
                         skip = FALSE,
                         id = rand_id("geodist")) {
  if (length(ref_lon) != 1 || !is.numeric(ref_lon))
    rlang::abort("`ref_lon` should be a single numeric value.")
  if (length(ref_lat) != 1 || !is.numeric(ref_lat))
    rlang::abort("`ref_lat` should be a single numeric value.")
  if (length(log) != 1 || !is.logical(log))
    rlang::abort("`log` should be a single logical value.")
  if (length(name) != 1 || !is.character(name))
    rlang::abort("`name` should be a single character value.")

  add_step(
    recipe,
    step_geodist_new(
      lon = enquos(lon),
      lat = enquos(lat),
      role = role,
      trained = trained,
      ref_lon = ref_lon,
      ref_lat = ref_lat,
      log = log,
      name = name,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

step_geodist_new <-
  function(lon, lat, role, trained, ref_lon, ref_lat,
           log, name, columns, skip, id) {
    step(
      subclass = "geodist",
      lon = lon,
      lat = lat,
      role = role,
      trained = trained,
      ref_lon = ref_lon,
      ref_lat = ref_lat,
      log = log,
      name = name,
      columns = columns,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_geodist <- function(x, training, info = NULL, ...) {
  lon_name <- terms_select(x$lon, info = info)
  if (length(lon_name) > 1)
    rlang::abort("`lon` should resolve to a single column name.")
  check_type(training[, lon_name])
  lat_name <- terms_select(x$lat, info = info)
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
    log = x$log,
    name = x$name,
    columns = c(lat_name, lon_name),
    skip = x$skip,
    id = x$id
  )
}

geo_dist_calc <- function(x, a, b)
  apply(x, 1, function(x, a, b) sqrt((x[1] - a) ^ 2 + (x[2] - b) ^ 2),
        a = a, b = b)

#' @export
bake.step_geodist <- function(object, new_data, ...) {
  dist_vals <-
    geo_dist_calc(new_data[, object$columns], object$ref_lat, object$ref_lon)
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



#' @rdname step_geodist
#' @param x A `step_geodist` object.
#' @export
tidy.step_geodist <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      latitude = x$columns[1],
      longitude = x$columns[2],
      ref_latitude = x$ref_lat,
      ref_longitude = x$ref_lon,
      name = x$name
    )
  } else {
    res <- tibble(
      latitude = sel2char(x$lat),
      longitude = sel2char(x$lon),
      ref_latitude = x$ref_lat,
      ref_longitude = x$ref_lon,
      name = x$name
    )
  }
  res$id <- x$id
  res
}
