#' Compute the geodesic distance between two coordinate locations
#'
#' @param long1 Numerical argument -- the longitude of the first coordinate location
#' @param lat1 Numerical argument -- the latitude of the first coordinate location
#' @param long2 Numerical argument -- the longitude of the second coordinate location
#' @param lat2 Numerical argument -- the latitude of the second coordinate location
#' @param units The geodesic distance will be computed in terms of these units -- Defaults to km
#' @return Returns the geodesic distance between two coordinate locations



get.geo.dist = function(long1, lat1, long2, lat2, units = "km") {
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
  }
  return(distance)
}
