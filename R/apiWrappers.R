url_base <- "https://pkgmaintainers.bioconductor.org/"

#' @title Get Maintainer Information by Package Name
#'
#' @aliases getInfoByPackage
#'
#' @description Retrieves maintainer information by package name
#'
#' @details Retrieves maintainer information by package name
#'
#' @param packageName current Bioconductor package
#'
#' @return data.frame of maintainer information including name, package, email,
#' consent_date, email_status, is_email_valid
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- getInfoByPackage("BiocFileCache")
#'
#' @export
getInfoByPackage <- function(packageName){
    stopifnot(length(packageName)==1L, is.character(packageName))
    data_url <- paste0(url_base, "info/package/", packageName)
    as.data.frame(jsonlite::fromJSON(data_url))
}

#' @title Get Maintainer Information by Maintainer Name
#'
#' @aliases getInfoByName
#'
#' @description Retrieves maintainer information by maintainer name
#'
#' @details Retrieves maintainer information by maintainer name
#'
#' @param name current Bioconductor package maintainer name
#'
#' @return data.frame of maintainer information including name, package, email,
#' consent_date, email_status, is_email_valid
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- getInfoByName("Lori Shepherd")
#'   tbl <- getInfoByName("Hervé Pagès")
#'
#' @export
getInfoByName <- function(name){
    stopifnot(length(name)==1L, is.character(name))
    name_enc <- utils::URLencode(name, reserved = TRUE)
    data_url <- paste0(url_base, "info/name/", name_enc)
    as.data.frame(jsonlite::fromJSON(data_url))
}

#' @title Get Maintainer Information by Maintainer Email
#'
#' @aliases getInfoByEmail
#'
#' @description Retrieves maintainer information by maintainer email
#'
#' @details Retrieves maintainer information by maintainer email
#'
#' @param email current Bioconductor package maintainer email
#'
#' @return data.frame of maintainer information including name, package, email,
#' consent_date, email_status, is_email_valid
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- getInfoByEmail("maintainer@bioconductor.org")
#'
#' @export
getInfoByEmail <- function(email){
    stopifnot(length(email)==1L, is.character(email))
    email_enc <- utils::URLencode(email, reserved = TRUE)
    data_url <- paste0(url_base, "info/email/", email_enc)
    as.data.frame(jsonlite::fromJSON(data_url))
}

#' @title Check if Maintainer Email is Valid
#'
#' @aliases isEmailValid
#'
#' @description Retrieves if maintainer email is valid
#' 
#' @details Retrieves if maintainer email is valid. 
#'
#' @param email current Bioconductor package maintainer email
#'
#' @return list. All lists have attribute 'valid' which is TRUE/FALSE. If valid
#' is FALSE, the list will also contain an attribute 'data' with additional
#' diagnostic columns in the database. This includes: email, name, package,
#' consent_date, email_status, is_email_valid, bounce_type, bounce_subtype,
#' smtp_status, diagnostic_code. If no 'data' attribute is included, the email
#' is not found in the database.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- isEmailValid("maintainer@bioconductor.org")
#'
#' @export
isEmailValid <- function(email){
    stopifnot(length(email)==1L, is.character(email))
    email_enc <- utils::URLencode(email, reserved = TRUE)
    data_url <- paste0(url_base, "info/valid/", email_enc)
    jsonlite::fromJSON(data_url)
}

#' @title List All Invalid Emails 
#'
#' @aliases listInvalid
#'
#' @description Retrieves all invalid emails
#' 
#' @details Retrieves all invalid emails 
#'
#' @return data.frame of maintainer information for anyone with a currently
#' identified invalid email. This includes: email, name, package,
#' email_status, is_email_valid, bounce_type, bounce_subtype, smtp_status,
#' diagnostic_code. An email is designated invalid if the validation email
#' failed to send either for a bounce or because it is on the AWS suppression
#' list.
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- listInvalid()
#'
#' @export
listInvalid <- function(){
    data_url <- paste0(url_base, "list/invalid")
    jsonlite::fromJSON(data_url)
}

#' @title List All Maintainer that Need Consent
#'
#' @aliases listNeedsConsent
#'
#' @description Retrieves all maintainers that need to consent to Bioconductor policies.
#' 
#' @details Retrieves all maintainers that need to consent to Bioconductor
#' policies. Bioconductor requires maintainers consent to Bioconductor policies
#' once a year. 
#'
#' @return character vector that lists all maintainer emails that need consent.
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- listNeedsConsent()
#'
#' @export
listNeedsConsent <- function(){
    data_url <- paste0(url_base, "list/needs-consent")
    as.character(jsonlite::fromJSON(data_url))
}

#' @title List All 'Bad' Emails
#'
#' @aliases listAllBadEmails
#'
#' @description Retrieves all 'bad' emails.
#' 
#' @details Retrieves all 'bad' emails. A 'bad' email is either invalid or has
#' not consented within the last year.
#'
#' @return character vector that lists all maintainer emails.
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- listAllBadEmails()
#'
#' @export
listAllBadEmails <- function(){
    data_url <- paste0(url_base, "list/bademails")
    as.character(jsonlite::fromJSON(data_url))
}

#' @title List All Emails on Suppression List
#'
#' @aliases listEmailsOnSuppressionList
#'
#' @description Retrieves all emails on AWS Suppression List
#' 
#' @details Retrieves all emails on AWS Suppression List
#'
#' @return list. First attribute is 'awssuppression' which is TRUE. Second
#' attribute is 'data' which is a data.frame that includes the email and name.
#'
#' @importFrom jsonlite fromJSON
#'
#' @author Lori Shepherd
#' 
#' @examples
#'   tbl <- listEmailsOnSuppressionList()
#'
#' @export
listEmailsOnSuppressionList <- function(){
    data_url <- paste0(url_base, "list/suppressionList")
    jsonlite::fromJSON(data_url)
}
