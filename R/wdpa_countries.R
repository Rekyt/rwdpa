# Functions to get countries information

#' List of all protected areas by country
#'
#' Return the list and the characteristics of protected areas in the world.
#' @inheritParams wdpa_test
#' @param with_geometry boolean if geometry should be returned (for the moment
#'                      GeoJSON return is not supported)
#' @param page integer indicating which page you want to return
#' @param per_page integer indicating the number of results you want to return
#' @import httr
#' @export
wdpa_countries = function(key = NULL, with_geometry = FALSE, page = 1,
                          per_page = 25) {

    raw_query = GET(wdpa_base(), path = "/v3/countries",
                    query = list(token = check_key(key),
                                 with_geometry = with_geometry,
                                 page = page,
                                 per_page = per_page))

    # Converts error in query result into R errors
    stop_for_status(raw_query)

    if (with_geometry) {
        stop("GeoJSON file not yet supported")
    } else {
        query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                                  type = "UTF-8"))
    }


    return(query_result)
}


#' Get a country list of Protected Areas
#'
#' Returns a list of protected area in a given country
#' @inheritParams wdpa_test
#' @param country_iso3 a character vector giving the ISO3 code of a country
#' @param with_geometry boolean if geometry should be returned (for the moment
#'                      GeoJSON return is not supported)
#' @import httr
#' @export
wdpa_country = function(key = NULL, country_iso3, with_geometry = FALSE) {

    is_ISO3(country_iso3)

    country_path = paste0("/v3/countries/", country_iso3)

    raw_query = GET(wdpa_base(), path = country_path,
                    query = list(token = check_key(key),
                                 with_geometry = with_geometry))

    # Converts error in query result into R errors
    stop_for_status(raw_query)

    query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                              type = "UTF-8"))

    return(query_result)
}
