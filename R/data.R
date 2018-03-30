#' Biomass Data
#'
#' Ghugare et al (2014) contains a data set where different biomass fuels are
#' characterized by the amount of certain molecules (carbon, hydrogen, oxygen,
#' nitrogen, and sulfur) and the corresponding higher heating value (HHV).
#' These data are from their Table S.2 of the Supplementary Materials
#'
#' @name biomass
#' @aliases biomass
#' @docType data
#' @return \item{biomass}{a data frame}
#'
#' @source Ghugare, S. B., Tiwary, S., Elangovan, V., and Tambe, S. S. (2013).
#' Prediction of Higher Heating Value of Solid Biomass Fuels Using Artificial
#' Intelligence Formalisms. *BioEnergy Research*, 1-12.
#'
#' @keywords datasets
#' @examples
#' data(biomass)
#' str(biomass)
NULL

#' OkCupid Data
#'
#' These are a sample of columns of users of OkCupid dating website. The data
#' are from Kim and Escobedo-Land (2015). Permission to use this data set was
#' explicitly granted by OkCupid.
#'
#' @name okc
#' @aliases okc
#' @docType data
#' @return \item{okc}{a data frame}
#'
#' @source Kim, A. Y., and A. Escobedo-Land. 2015. "OkCupid Data for
#'   Introductory Statistics and Data Science Courses." *Journal of
#'   Statistics Education: An International Journal on the Teaching and
#'   Learning of Statistics*.
#'
#' @keywords datasets
#' @examples
#' data(okc)
#' str(okc)
NULL


#' Credit Data
#'
#' These data are from the website of Dr. Lluís A. Belanche Muñoz by way of a
#' github repository of Dr. Gaston Sanchez. One data point is a missing outcome
#' was removed from the original data.
#'
#' @name credit_data
#' @aliases credit_data
#' @docType data
#' @return \item{credit_data}{a data frame}
#'
#' @source https://github.com/gastonstat/CreditScoring,
#' http://bit.ly/2kkBFrk
#'
#' @keywords datasets
#' @examples
#' data(credit_data)
#' str(credit_data)
NULL



#' Raw Cover Type Data
#'
#' These data are raw data describing different types of forest cover-types
#'   from the UCI Machine Learning Database (see link below). There is one
#'   column in the data that has a few difference pieces of textual
#'   information (of variable lengths).
#'
#' @name covers
#' @aliases covers
#' @docType data
#' @return \item{covers}{a data frame}
#'
#' @source https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info
#'
#' @keywords datasets
#' @examples
#' data(covers)
#' str(covers)
NULL
