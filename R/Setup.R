key = NULL


##' Authentication
##'
##' This function check if you are authorized or not
##'
##'
##'
##' @return if you are authentication
##' @author Yassin Abdelhady
##' @export
##'
##' @examples
##' auth_me()

auth_me <- function(){
  key <- Sys.getenv("GUARDIAN_API_KEY")
  assertthat::assert_that(!missing(key), msg = "Sign Up for an API key here https://open-platform.theguardian.com/access then use the function guardian_key('') to set your API Key ")
  print("You have your API Key set")
}

##' The Guardian API key
##'
##' This function retrive your API key
##'
##'
##'
##' @return show your API key
##' @author Yassin Abdelhady
##' @export
##'
##' @examples
##' show_my_key()


show_my_key <- function(){
  key <- Sys.getenv("GUARDIAN_API_KEY")
  assertthat::assert_that(!missing(key), msg = "Sign Up for an API key her then use the function guardian_key('') to set your API Key ")
  print(key)
}


##' Set the Guardian API key
##'
##' This function sets your API key
##'
##'
##'
##' @return set your API key
##' @author Yassin Abdelhady
##' @export
##'
##' @param key character string containing your API key.
##'
##' @examples
##' guardian_key("your_api_key_here")

# api_key <- paste("api-key=",rjson::fromJSON(file ="../credentials/theguardiankey.json")$api_key,sep="")
guardian_key <- function(key){
  assertthat::assert_that(!missing(key), msg = "Missing key")
  Sys.setenv(GUARDIAN_API_KEY = paste0("api-key=",key))
}


.get_key <- function() {
  key <- Sys.getenv("GUARDIAN_API_KEY")
  assertthat::assert_that(nchar(key) > 1, msg = "Missing key, see `guardian_key`")
  return(key)
}



