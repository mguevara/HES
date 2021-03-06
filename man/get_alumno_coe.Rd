% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{get_alumno_coe}
\alias{get_alumno_coe}
\title{Get Alumno coe}
\usage{
get_alumno_coe(g = ches1217, classification, data_postulaciones)
}
\arguments{
\item{g}{Graph containing a network of kind Higher Education Space HES. Default value is ches1227, CHilean HES for 2012-2017 dataset.}

\item{classification}{Dataframe with students' information previously obtained with function get_classification().}

\item{data_postulaciones}{Dataframe with students' information previously obtained with function get_data_postulaciones()}
}
\value{
Data useful to perform regressions and analysis. It includes information of Coherence of network HES and information of dropped and not-dropped.
}
\description{
Alumnos merged with postulaciones merged with coherence.
}
\examples{

}
