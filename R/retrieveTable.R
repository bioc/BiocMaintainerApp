#' @title Retrieve Maintainer Information
#'
#' @aliases get_maintainer_data
#'
#' @description Retrieves maintainer information from the package maintainer
#' validation app
#'
#' @details Retrieves maintainer information from the package maintainer
#' validation app
#'
#' @param url Url to retrieve data from whose data can be parsed with jsonlite::fromJSON
#'
#' @return data.frame of maintainer information
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- get_maintainer_data()
#'
#' @export
get_maintainer_data <- function(url =
                                    "https://pkgmaintainers.bioconductor.org/download-maintainer-db"){

    stopifnot(length(url)==1L)
               
    df <- jsonlite::fromJSON(url)
    
    if ("is_email_valid" %in% names(df)) {
        df$is_email_valid <- as.logical(df$is_email_valid)
    }
    
    if ("consent_date" %in% names(df)) {
        df$consent_date <- as.Date(df$consent_date)  
        one_year_ago <- Sys.Date() - 365
        df$needs_consent <- df$consent_date < one_year_ago
    } else {
        df$needs_consent <- NA  
    }    
    df            
}
