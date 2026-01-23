url_base <- "https://pkgmaintainers.bioconductor.org/"


## get '/info/package/:pkg' do
##   content_type :json
##   return Core.get_package_info(params[:pkg])
## end

getInfoByPackage <- function(packageName){
    stopifnot(length(packageName)==1L, is.character(packageName))
    data_url <- paste0(url_base, "info/package/", packageName)
    jsonlite::fromJSON(data_url)
}

## get %r{/info/name/(.+)} do |name|
##   content_type :json
##   return Core.get_name_info(name)
## end

getInfoByName <- function(name){
    stopifnot(length(name)==1L, is.character(name))
    name_enc <- utils::URLencode(name, reserved = TRUE)
    data_url <- paste0(url_base, "info/name/", name_enc)
    jsonlite::fromJSON(data_url)
}

## get '/info/email/:email' do
##   content_type :json
##   return Core.get_email_info(params[:email])
## end

getInfoByEmail <- function(email){
    stopifnot(length(email)==1L, is.character(email))
    email_enc <- utils::URLencode(email, reserved = TRUE)
    data_url <- paste0(url_base, "info/email/", email_enc)
    jsonlite::fromJSON(data_url)
}

## get '/info/valid/:email' do
##   content_type :json
##   return Core.is_email_valid(params[:email])
## end

isEmailValid <- function(email){
    stopifnot(length(email)==1L, is.character(email))
    email_enc <- utils::URLencode(email, reserved = TRUE)
    data_url <- paste0(url_base, "info/valid/", email_enc)
    jsonlite::fromJSON(data_url)
}

## get '/list/invalid/?' do
##   content_type :json
##   return Core.list_invalid()
## end


listInValid <- function(){
    data_url <- paste0(url_base, "list/invalid")
    jsonlite::fromJSON(data_url)
}


## get '/list/needs-consent/?' do
##   content_type :json
##   return Core.list_needs_consent()
## end

listNeedsConsent <- function(){
    data_url <- paste0(url_base, "list/needs-consent")
    jsonlite::fromJSON(data_url)
}

## get '/list/bademails/?' do
##   content_type :json
##   return Core.list_bad_emails()
## end

listAllBadEmails <- function(){
    data_url <- paste0(url_base, "list/bademails")
    jsonlite::fromJSON(data_url)
}

## get '/list/suppressionList/?' do
##   content_type :json
##   return Core.list_suppression_list()
## end

listEmailsOnAWSsuppression <- function(){
    data_url <- paste0(url_base, "list/suppressionList")
    jsonlite::fromJSON(data_url)
}
