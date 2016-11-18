# Function to test if token works

#' Test if API key is valid
#'
#' Check if your provided API key works with protected planet. Returns
#' \code{TRUE} if key is valid, returns \code{FALSE} otherwise.
#' @param key your API key as provided by Protected Planet
#' @import httr
#' @export
wdpa_test = function(key = NULL) {
    # Query
    raw_query = GET(wdpa_base(), path = "test",
                    query = list(token = check_key(key)))

    # Converts error in query result into R errors
    stop_for_status(raw_query)

    # Parse Query result
    query_result = jsonlite::fromJSON(content(raw_query, as = "text",
                                              type = "UTF-8"))

    if (query_result$status == "Success!") {
        result = TRUE
    } else {
        result = FALSE
    }

    return(result)
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
