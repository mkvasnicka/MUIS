% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data_on_seminars_and_people.R
\name{get_teachers}
\alias{get_teachers}
\title{Get seminar teachers}
\usage{
get_teachers(credentials, seminars = NULL)
}
\arguments{
\item{credentials}{(object of class "IScredentials") credentials
created by credentials()}

\item{seminars}{(optional, vector of seminar names)
if given, it gets the name, and UČO of the teachers of the given
seminars; otherwise, it returns it for all teachers of all open seminars}
}
\value{
a tibble with columns seminar, uco, first_name, and last_name
}
\description{
\code{get_teachers()} returns a tibble of
seminar teachers.
}
\note{
was \code{get_seminar_teachers()}
}
\examples{
\dontrun{
get_teachers(cred)
get_teachers(cred, c("01", "02"))
}

}
