# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Sys.setenv(link_base = "https://content.guardianapis.com/search?")


auth_me <- function(){
  key <- Sys.getenv("GUARDIAN_API_KEY")
  assertthat::assert_that(!missing(key), msg = "Sign Up for an API key here https://open-platform.theguardian.com/access then use the function guardian_key('') to set your API Key ")
  print("You have your API Key set")
}

show_my_key <- function(){
  key <- Sys.getenv("GUARDIAN_API_KEY")
  assertthat::assert_that(!missing(key), msg = "Sign Up for an API key her then use the function guardian_key('') to set your API Key ")
  print(key)
}




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



