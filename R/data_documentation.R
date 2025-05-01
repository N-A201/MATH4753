#' MTBE Groundwater Contamination Dataset
#'
#' Environmental and land use data for wells tested for Methyl Tertiary-Butyl Ether (MTBE).
#'
#' @name mtbe_data
#' @docType data
#' @format A data frame with 109 rows and 16 variables:
#' \describe{
#'   \item{pH}{Water pH level}
#'   \item{SpConduct}{Specific conductance (microS/cm)}
#'   \item{DissOxy}{Dissolved oxygen (mg/L)}
#'   \item{RoadsPct}{Percent of watershed covered by roads}
#'   \item{IndPct}{Percent of industrial land use}
#'   \item{UrbanPct}{Percent of urban land use}
#'   \item{DevPct}{Percent of developed land}
#'   \item{WellClass}{Well class (e.g., Private)}
#'   \item{Aquifier}{Type of aquifer (e.g., Bedrock)}
#'   \item{Depth}{Depth of the well in meters}
#'   \item{SafeYld}{Safe yield of the well (if available)}
#'   \item{Distance}{Distance to nearest urban feature (meters)}
#'   \item{MTBE-Detect}{Detection flag for MTBE presence}
#'   \item{MTBE-Level}{Measured MTBE concentration (micrograms per liter)}
#'   \item{HouseDen}{Housing density (houses per square kilometer)}
#'   \item{PopDen}{Population density (people per square kilometer)}
#' }
#'
#' @source MTBE study dataset
"mtbe_data"

#' DDT Concentration in River Fish
#'
#' Measurements of DDT contamination in fish from various rivers.
#'
#' @name ddt
#' @docType data
#' @format A data frame with 108 rows and 6 variables:
#' \describe{
#'   \item{RIVER}{River code (e.g., "FCM")}
#'   \item{MILE}{River mile where sample was taken}
#'   \item{SPECIES}{Species of fish (e.g., "CCATFISH")}
#'   \item{LENGTH}{Fish length in centimeters}
#'   \item{WEIGHT}{Fish weight in grams}
#'   \item{DDT}{Measured DDT concentration in parts per million (ppm)}
#' }
#'
#' @source Environmental field sampling study
"ddt"
