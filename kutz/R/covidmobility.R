#' Data about changes in mobility during COVID-19 as a response to government imposed restrictions: a multiple regression analysis for the top 10 populous states of the US by Flor et al (2020)
#'
#' format A data frame with 790 rows and 16 variables:
#' \describe{
#'  \item{date}{Date the data was collected}
#'  \item{state}{Factor state in which the data was collected}
#'  \item{fips}{int Federal information processing standards code, a standard geographic identifier, to make it easier to combine this data with other data sets}
#'  \item{admin_level}{int administrative level the data was collected 1 = state level}
#'  \item{samples}{int The number of samples observed in the specified region}
#'  \item{m50}{num denoting the median of the max-distance mobility for all samples in the specified region}
#'  \item{m50_index}{int denoting the percent of normal m50 in the region, with normal m50 defined during 2020-02-17 to 2020-03-07}
#'  \item{state.abb}{Factor denoting the state abbreviations}
#'  \item{actual.cases}{int denoting the number of COVID-19 cases reported in the state at the specified date}
#'  \item{actuals.deaths}{int denoting the number of deaths recorded in the state at the specified date}
#'  \item{metrics.caseDensity}{num the density of cases in the state}
#'  \item{metrics.infectionRate}{num denoting the infection rate in the state}
#'  \item{riskLevels.overall}{num denoting the overall COVID-19 risk level in the state}
#'  \item{cdcTransmissionLevel}{num denoting the CDC based state COVID-19 transmission level}
#'  \item{state.area}{num denoting the size of the state}
#'  \item{restriction}{Factor denoting whether the state had a COVID-19 restriction or not by the  date specified 1 = yes, 0 = no}
#' }
#' @source This data was obtained from various source including:
#' mobility data from https://github.com/descarteslabs/DL-COVID-19
#' populous state data https://worldpopulationreview.com/states
#' data on covid cases, deaths, and risk levels, vaccination rates among other variables which might have an effect on mobility indices from https://covidactnow.org/?s=45673318
#' movement restrictions from https://ballotpedia.org/States_that_issued_lockdown_and_stay-athome_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020
"covidmobility"
