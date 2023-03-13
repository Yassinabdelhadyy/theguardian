\name{guardian_call}
\alias{guardian_call}
\title{Reqest data from the guardian}
\usage{
guardian_call()
}
\arguments{
\item{orderby}{character value}
\item{page_numbers}{numeric value}
\item{page_size}{numeric value}
\item{to_date}{date value "month "}
\item{orderby}{character value}



orderby = "newest" ,page_numbers = 1 ,page_size=10 ,to_date = Sys.Date(), from_date=Sys.Date()-7,loop=FALSE
}
\description{
retrive the data from the guardian
}
\examples{
guardian_call()
}
\author{
Yassin Abdelhady
}
