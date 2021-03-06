% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{get_classification}
\alias{get_classification}
\title{Get Data of students clasified}
\usage{
get_classification(n = 0)
}
\arguments{
\item{n}{Integer indicating a number to conduct sampling. If not indicated, all the data of students will be retrieved.}
}
\value{
A data frame with information of students and a field useful to get dropped or not dropped information.
}
\description{
Load data of students. Filter only enrolled and situation(P), then classify among dropped and not dropeed.
}
\examples{

}
