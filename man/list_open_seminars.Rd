% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data_on_seminars_and_people.R
\name{list_open_seminars}
\alias{list_open_seminars}
\title{Get list of all open seminars}
\usage{
list_open_seminars(credentials)
}
\arguments{
\item{credentials}{(object of class "IScredentials") credentials
created by credentials()}
}
\value{
a string vector of seminar names
}
\description{
\code{list_open_seminars()} returns a vector
of names of open seminars.
}
\note{
It is expected that open and only open seminars have empty
note POZNAMKA.

was \code{get_list_of_open_seminars()}
}
\examples{
\dontrun{
open_seminars <- list_open_seminars(cred)
}

}
