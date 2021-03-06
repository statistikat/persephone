% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotResiduals.R
\name{plotResiduals}
\alias{plotResiduals}
\title{Several interactive plots in connection with residuals for a persephone
object}
\usage{
plotResiduals(
  x,
  which = c("res", "acf", "acf2", "pacf", "sreshist", "nqq"),
  main = NULL,
  interactive = TRUE,
  ...
)
}
\arguments{
\item{x}{an object of class \link{persephone}.}

\item{which}{character (\code{"res"},\code{"acf"},\code{"acf2"}, \code{"pacf"},\code{"sreshist"},
\code{"nqq"}) selecting the preferred type of plot, see Details. A numeric
value (\code{1:6}) corresponding to one of these characters is also accepted.}

\item{main}{plot title}

\item{interactive}{If the return value would be a \code{ggplot} object, wrap it in
\link[plotly:ggplotly]{plotly::ggplotly} before returning.}

\item{...}{other plotting parameters to affect the plot. Not currently used.}
}
\value{
Returns an object of class \code{dygraphs}, \code{ggplot} or \code{plotly}
}
\description{
Produces either a dygraph (see the
\href{https://rstudio.github.io/dygraphs/}{online documentation} for more detail)
or a \code{ggplot}/\code{plotly} object for objects of class \link{persephone}.
}
\details{
The following options are available for the parameter \code{which}.
\itemize{
\item \code{res}: residuals (1)
\item \code{acf}: autocorrelations of the residuals (2)
\item \code{pacf}: partial autocorrelations of the residuals (3)
\item \code{acf2}: autocorrelations of the squared residuals (4)
\item \code{sreshist}: histogram of standardized residuals including normal curve (5)
\item \code{nqq}: normal q-q plot of standardized residuals (6)
}
}
\examples{
data(AirPassengers, package = "datasets")
# Generate a persephone object, in this case an x13Single object
obj <- perX13(AirPassengers, "RSA1")

obj$run()
# Plot the residuals after run
plotResiduals(obj)
# Plot the autocorrelations of the residuals
plotResiduals(obj, which="acf")


}
