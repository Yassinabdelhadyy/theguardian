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


guardian_call <- function(orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){

  base_link<-Sys.getenv("link_base")
  api_key<-Sys.getenv("GUARDIAN_API_KEY")
  orderby <- paste0("order-by=",orderby,"&")
  page_size <- paste0("page-size=",page_size,"&")
  from_date <- paste0("from-date=",from_date,"&")
  to_date <- paste0("to-date=",to_date,"&")


  if(loop==FALSE){
    page <- paste0("page=",page_numbers,"&")
    theguardian <- paste0(base_link,to_date,from_date,orderby,page,page_size,api_key)
    #reading the html page of the result
    text <- rvest::read_html(theguardian)%>%html_element("body")%>%html_element("p")%>%html_text2()
    #transforming the page from text to json format
    js <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
    #adding the result into a data frame
    df <- data.frame(js$response)
    #removing results. from the column names
    colnames(df) <-str_replace_all(colnames(df),"results.","")
    df <- df%>%separate(webPublicationDate,sep="T",c("PublicationDate","PublicationTime"))%>%mutate(PublicationDate = as.Date(PublicationDate),PublicationTime= substr(PublicationTime,0,5))

    return(df)

  }else if (loop==TRUE){

    df2 <- data.frame()
    for (page_num in 1:page_numbers){
      page_nm <- paste0("page=",page_num,"&")
      #adding the link with the api key
      theguardian <- paste0(base_link,to_date,from_date,orderby,page_nm,page_size,api_key)
      #reading the html page of the result
      text <- rvest::read_html(theguardian)%>%rvest::html_element("body")%>%rvest::html_element("p")%>%rvest::html_text2()
      #transforming the page from text to json format
      js <- jsonlite::fromJSON(text, simplifyDataFrame = TRUE)
      #adding the result into a data frame
      df <- data.frame(js$response)
      #removing results. from the column names
      colnames(df) <-str_replace_all(colnames(df),"results.","")
      df <- df%>%separate(webPublicationDate,sep="T",c("PublicationDate","PublicationTime"))%>%mutate(PublicationDate = as.Date(PublicationDate),PublicationTime= substr(PublicationTime,0,5))
      #appending the result to a dataframe
      df2 <- rbind(df2,df)
    }
    return(df2)
  }
}


full_guardian_call <- function(orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE){
  df <- guardian_call(orderby,page_numbers,page_size,to_date,from_date,loop)
  articals <-data.frame()

  for(link in 1:nrow(df)){
    #article link
    article_link <- df$webUrl[link]
    #reading html page of the article
    html_artical <- rvest::read_html(article_link)
    #article subtitle
    subtitle<-html_artical%>%rvest::html_element("p")%>%rvest::html_text2()
    #article author
    author<-html_artical%>%rvest::html_element("address")%>%rvest::html_text2()
    #article body
    body<-html_artical%>%rvest::html_element(".dcr-i7zira")%>%rvest::html_text2()
    #tags of the article
    tags<-html_artical%>%rvest::html_element(".dcr-1nx1rmt")%>%rvest::html_text2()
    tags<-str_replace_all(tags,"\\n",", ")

    #appending all the results into the data frame
    articals <- rbind(articals,data.frame(webUrl = article_link,author = author,SubTitle = subtitle,article_text = body,tags= tags))
  }
  df2 <- df%>%left_join(.,articals,by = "webUrl")

  return(df2)
}

