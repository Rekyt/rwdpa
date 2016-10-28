# Functions to get countries information

#' Function to get list of countries and
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
    stop_for_status(content(raw_query))

    if (with_geometry) {
        stop("GeoJSON file not yet supported")
    } else {
        query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                                  type = "UTF-8"))
    }


    return(query_result)
}
