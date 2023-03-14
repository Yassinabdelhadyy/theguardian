Sys.setenv(link_base = "https://content.guardianapis.com/search?")
base_link<-"https://content.guardianapis.com/search?"
##' The Guardian call
##'
##' This function retrieve the data from the guardian API
##'
##'
##'
##' @return the data from the guardian
##' @author Yassin Abdelhady
##' @export
##'
##'
##' @param q A character string containing the query term.
##' @param orderby A character string indicating how the results should be ordered.
##' @param page_numbers An integer vector indicating the page numbers to retrieve.
##' @param page_size An integer indicating the number of results per page.
##' @param to_date A character string indicating the latest date for articles to retrieve.
##' @param from_date A character string indicating the earliest date for articles to retrieve.
##' @param loop A logical indicating whether to make multiple API calls to retrieve all available results.
##'
##'
##' @examples
##'
##' \dontrun{
##'   guardian_call()
##' }


guardian_call <- function(q=NULL,orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){
  api_key<-Sys.getenv("GUARDIAN_API_KEY")
  orderby <- paste0("order-by=",orderby,"&")
  page_size <- paste0("page-size=",page_size,"&")
  from_date <- paste0("from-date=",from_date,"&")
  to_date <- paste0("to-date=",to_date,"&")
  query <- ifelse(!is.null(q),paste0("q='",q,"'&"),"")

  if(loop==FALSE){
    page <- paste0("page=",page_numbers,"&")
    theguardian <- paste0(base_link,query,to_date,from_date,orderby,page,page_size,api_key)
    #reading the html page of the result
    text <- rvest::read_html(theguardian)|>
      rvest::html_element("body")|>
      rvest::html_element("p")|>
      rvest::html_text2()
    #transforming the page from text to json format
    js <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
    #adding the result into a data frame
    df <- data.frame(js$response)
    #removing results. from the column names
    colnames(df) <- stringr::str_replace_all(colnames(df),"results.","")
    df <- df|>
      tidyr::separate(webPublicationDate,sep="T",c("PublicationDate","PublicationTime"))|>
      dplyr::mutate(PublicationDate = as.Date(PublicationDate),PublicationTime= substr(PublicationTime,0,5))

    return(df)

  }else if (loop==TRUE){

    df2 <- data.frame()
    for (page_num in 1:page_numbers){
      page_nm <- paste0("page=",page_num,"&")
      #adding the link with the api key
      theguardian <- paste0(base_link,query,to_date,from_date,orderby,page,page_size,api_key)
      #reading the html page of the result
      text <- rvest::read_html(theguardian)|>
        rvest::html_element("body")|>
        rvest::html_element("p")|>
        rvest::html_text2()
      #transforming the page from text to json format
      js <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
      #adding the result into a data frame
      df <- data.frame(js$response)
      #removing results. from the column names
      colnames(df) <-stringr::str_replace_all(colnames(df),"results.","")
      df <- df|>
        separate(webPublicationDate,sep="T",c("PublicationDate","PublicationTime"))|>
        dplyr::mutate(PublicationDate = as.Date(PublicationDate),PublicationTime= substr(PublicationTime,0,5))
      #appending the result to a dataframe
      df2 <- rbind(df2,df)
    }
    return(df2)
  }
}


##' The Guardian call
##'
##' This function retrieve the data from the guardian API plus the body and the tags of the article
##'
##'
##'
##' @return the full data from the guardian
##' @author Yassin Abdelhady
##' @export
##'
##'
##' @param q A character string containing the query term.
##' @param orderby A character string indicating how the results should be ordered.
##' @param page_numbers An integer vector indicating the page numbers to retrieve.
##' @param page_size An integer indicating the number of results per page.
##' @param to_date A character string indicating the latest date for articles to retrieve.
##' @param from_date A character string indicating the earliest date for articles to retrieve.
##' @param loop A logical indicating whether to make multiple API calls to retrieve all available results.
##'
##'
##'
##' @examples
##'
##' \dontrun{
##'   full_guardian_call()
##' }



full_guardian_call <- function(q=NULL,orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){
  df <- guardian_call(q,orderby,page_numbers,page_size,to_date,from_date,loop)
  articals <-data.frame()

  for(link in 1:nrow(df)){
    #article link
    article_link <- df$webUrl[link]
    #reading html page of the article
    html_artical <- rvest::read_html(article_link)
    #article subtitle
    subtitle<-html_artical|>
      rvest::html_element("p")|>
      rvest::html_text2()
    #article author
    author<-html_artical|>
      rvest::html_element("address")|>
      rvest::html_text2()
    #article body
    body<-html_artical|>
      rvest::html_element(".dcr-i7zira")|>
      rvest::html_text2()
    #tags of the article
    tags<-html_artical|>
      rvest::html_element(".dcr-1nx1rmt")|>
      rvest::html_text2()
    tags<-stringr::str_replace_all(tags,"\\n",", ")

    #appending all the results into the data frame
    articals <- rbind(articals,data.frame(webUrl = article_link,author = author,SubTitle = subtitle,article_text = body,tags= tags))
  }
  df2 <- df|>
    dplyr::left_join(articals,by = "webUrl")

  return(df2)
}

