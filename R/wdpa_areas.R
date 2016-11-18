# Functions to retrieve protected areas directly

#' List of all protected areas in Protected Areas
#'
#' Return the list and the characteristics of protected areas in the world.
#' @inheritParams wdpa_test
#' @inheritParams wdpa_countries
#' @import httr
#' @export
wdpa_areas = function(key = NULL, with_geometry = FALSE, page = 1,
                      per_page = 25) {

    raw_query = GET(wdpa_base(), path = "/v3/protected_areas",
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


#' Query a single Protected Area
#'
#' Return the characteristics of given protected area from its ID
#' @inheritParams wdpa_test
#' @param wdpa_id a numeric vector giving the ID of the protected area
#' @param with_geometry boolean if geometry should be returned (for the moment
#'                      GeoJSON return is not supported)
#' @import httr
#' @export
wdpa_area = function(key = NULL, wdpa_id, with_geometry = FALSE) {

    area_path = paste0("/v3/protected_areas/", wdpa_id)

    raw_query = GET(wdpa_base(), path = area_path,
                    query = list(token = check_key(key),
                                 with_geometry = with_geometry))

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

#' Search protected areas based on attributes
#'
#' @inheritParams wdpa_test
#' @param marine boolean if \code{TRUE} returns all marine protected areas,
#'               if \code{FALSE} returns all the terrestrial ones, \code{NULL}
#'               by default returning all types of protected areas
#' @param country 3 characters string if set returns all protected areas from
#'                the given countries, by default unset and return PAs from
#'                all countries
#' @param designation If set, returns all protected areas designated with the
#'                    given `id`, paginated. By default, this is unset:
#'                    protected areas with all designations are returned.
#' @param jurisdiction If set, returns all protected areas with the given
#'                     jurisdiciton `id`, paginated. By default, this is unset:
#'                     protected areas with all jurisdictions are returned.
#' @param governance If set, returns all protected areas with the given
#'                   governance `id`, paginated. By default, this is unset:
#'                   protected areas with all governances are returned.
#' @param iucn_category If set, returns all protected areas with the given IUCN
#'                      category `id`, paginated. By default, this is unset:
#'                      protected areas with all IUCN categories are returned.
#' @param page Controls the returned page. Defaults to 1.
#' @param per_page Controls how many protected areas are returned per page.
#'                 Defaults to 25. For performance reasons, the maximum value
#'                 is 50.
#' @import httr
#' @export
wdpa_search = function(key = NULL, marine = NULL, country = NULL,
                       designation = NULL, jurisdiction = NULL,
                       governance = NULL, iucn_category = NULL, page = 1,
                       per_page = 25) {


    query_arguments = list("token"         = check_key(key),
                           "marine"        = marine,
                           "country"       = country,
                           "designation"   = designation,
                           "jurisdiction"  = jurisdiction,
                           "governance"    = governance,
                           "iucn_category" = iucn_category,
                           "page"          = page,
                           "per_page"      = per_page)

    # Take out all 'NULL' (= unset) arguments
    query_arguments = Filter(Negate(is.null), query_arguments)

    raw_query = GET(wdpa_base(), path = "/v3/protected_areas/search",
                    query = query_arguments)

    # Converts error in query result into R errors
    stop_for_status(raw_query)

    query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                                  type = "UTF-8"))

    return(query_result)
}
