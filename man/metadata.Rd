\name{metadata}
\alias{metadata}
\docType{data}
\title{
Sample \code{metadata} for NCRNWater package
}
\description{
A \code{data frame} of example water quality metadata in the proper format for ingestion into \code{NCRNWater::importNCRNWater()}.
}
\usage{data("metadata")}
\format{
  A data frame with 830 observations on the following 22 variables.
  \describe{
    \item{\code{Network}}{chr; 4-letter code representing a USNPS Inventory & Monitoring Network, e.g., NCRN}
    \item{\code{ParkCode}}{chr; 4-letter code representing a US National Park, e.g., YELL}
    \item{\code{ShortName}}{chr; short name of a US National Park, e.g., Yellowstone}
    \item{\code{LongName}}{chr; long name of a US National Park, e.g., Yellowstone National Park}
    \item{\code{SiteCode}}{chr; code to identify a sampling site, a concatenation of $Network, $ParkCode, and a 4-letter site identifier}
    \item{\code{SiteCodeWQX}}{chr; code to identify sites for Water Quality Exchange WQX, a concatenation of $Network, $ParkCode, and a 4-letter site identifier}
    \item{\code{SiteName}}{chr; long name of a sampling site, e.g., Yellowstone River}
    \item{\code{Lat}}{num; double; sampling site latitude in decimal degrees North}
    \item{\code{Long}}{num; double; sampling site longitude in decimal degrees West}
    \item{\code{Type}}{chr; the sampling site's habitat type}
    \item{\code{CharacteristicName}}{chr; short-form of the name of a sampling parameter}
    \item{\code{DisplayName}}{chr; long-form of the name of a sampling parameter, displayed in figures}
    \item{\code{DataName}}{chr; alternate long-form of the name of a sampling parameter}
    \item{\code{Category}}{chr; alternate short-form of the name of a sampling parameter}
    \item{\code{CategoryDisplay}}{chr; alternate long-form of the name of a sampling parameter, displayed in figures}
    \item{\code{Units}}{chr; a sampling parameter's unit of measure}
    \item{\code{LowerPoint}}{num; a lower threshold for a sampling parameter}
    \item{\code{UpperPoint}}{num; an upper threshold for a sampling parameter}
    \item{\code{DataType}}{chr; a sampling parameter's data type}
    \item{\code{LowerDescription}}{chr; a sentence explaining $LowerPoint}
    \item{\code{UpperDescription}}{chr; a sentence explaining $UpperPoint}
    \item{\code{AssessmentDetails}}{chr; citations for $LowerPoint and/or $UpperPoint}
  }
}
\source{
US National Park Service, National Capital Region Network Inventory & Monitoring, Washington, DC, USA
}
\examples{
data(metadata)
}
\keyword{datasets}
