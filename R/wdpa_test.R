# Function to test if token works

#' Function to test if key is valid
#' @export
wdpa_test = function(key = NULL, ...) {
    raw_query = httr::GET(wdpa_base(), path = "test",
                          query = list(token = check_key(key)))

    jsonlite::fromJSON(httr::content(raw_query, as = "text", type = "UTF-8"))
}

# Function to get key from Scott Chamberlain 'rredlist' package
check_key <- function(x){
    tmp <- if (is.null(x)) Sys.getenv("WDPA_KEY", "") else x
    if (tmp == "") {
        getOption("WDPA_KEY", stop("need an API key for WDPA data",
                                           call. = FALSE))
    } else {
        tmp
    }
}

wdpa_base = function() "https://api.protectedplanet.net/"
