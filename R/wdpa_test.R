# Function to test if token works

#' Function to test if key is valid
#' @import httr
#' @export
wdpa_test = function(key = NULL, ...) {
    # Query
    raw_query = GET(wdpa_base(), path = "test",
                    query = list(token = check_key(key)))

    # Converts error in query result into R errors
    stop_for_status(content(raw_query))

    # Parse Query result
    query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                              type = "UTF-8"))

    return(query_result)
}

# Function to get key from Scott Chamberlain 'rredlist' package
check_key <- function(x){
    tmp <- if (is.null(x)) Sys.getenv("WDPA_KEY", "") else x
    if (tmp == "") {
        getOption("WDPA_KEY", stop("You need an API key for WDPA data",
                                           call. = FALSE))
    } else {
        tmp
    }
}

# Function with the base URL of WDPA
wdpa_base = function() "https://api.protectedplanet.net/"
