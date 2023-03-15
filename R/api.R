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
##' @param search_term A character string containing the query term.
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
##'   guardian_call(search_term = "football")
##' }


guardian_call <- function(search_term=NULL,orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){
  api_key<-Sys.getenv("GUARDIAN_API_KEY")
  orderby <- paste0("order-by=",orderby,"&")
  page_size <- paste0("page-size=",page_size,"&")
  from_date <- paste0("from-date=",from_date,"&")
  to_date <- paste0("to-date=",to_date,"&")
  query <- ifelse(!is.null(search_term),paste0("q='",search_term,"'&"),"")

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
##' @param search_term A character string containing the query term.
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
##'   full_guardian_call(search_term = "football")
##' }



full_guardian_call <- function(search_term=NULL,orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){
  df <- guardian_call(search_term,orderby,page_numbers,page_size,to_date,from_date,loop)
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


##' The Guardian call
##'
##' This function retrieve the data from the guardian API using a specific date
##'
##'
##'
##' @return the data from the guardian
##' @author Yassin Abdelhady
##' @export
##'
##'
##' @param date A character string indicating the date for articles to retrieve.DEFAULT today date
##'
##'
##' @examples
##'
##' \dontrun{
##'   guardian_call_by_date(date = "2023-03-10")
##' }


guardian_call_by_date <- function(date = Sys.Date()){
  # fetching the API Key
  api_key<-Sys.getenv("GUARDIAN_API_KEY")
  # getting the date from the function
  from_date <- paste0("from-date=",date,"&")
  to_date <- paste0("to-date=",date,"&")
  # creating the first link to get API output details
  theguardian <- paste0(base_link,to_date,from_date,api_key)

  text <- rvest::read_html(theguardian)|>
    rvest::html_element("body")|>
    rvest::html_element("p")|>
    rvest::html_text2()
  #transforming the page from text to json format
  js <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
  # getting the total results given by the API
  results <- js$response$total
  # creating a vectors for the page lenghth and page size to be able to loop later
  if (results>200){
  page_size <- c(rep(200,floor(results/200)),results-((floor(results/200))*200))
  pages <- length(page_size)
  }else if(results<200){
    page_size <-c(results)
    pages <- length(page_size)

  }
  # empty data frame to append the results of the loop
  df2 <- data.frame()
  # looping over the pages and each page the size of the results
  for (page in 1:pages){
    page_sz <- paste0("page-size=",page_size[page],"&")
    page_nm <- paste0("page=",page,"&")
    theguardian <- paste0(base_link,to_date,from_date,page_sz,page_nm,api_key)
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
    df2 <- rbind(df2,df)
  }
    return(df2)
}


##' The Guardian call
##'
##' This function retrieve the data from the guardian API using a specific date plus the body and the tags of the article
##'
##'
##'
##' @return the full data from the guardian
##' @author Yassin Abdelhady
##' @export
##'
##'
##' @param date A character string indicating the date for articles to retrieve.DEFAULT today date
##'
##'
##'
##' @examples
##'
##' \dontrun{
##'   full_guardian_call_by_date(date="2023-03-10")
##' }



full_guardian_call_by_date <- function(date = Sys.Date()){
  # reusing the guardian_call_by_date() to fetch the API data and search each link to get the article body and auther and tags
  df <- guardian_call_by_date(date)
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
