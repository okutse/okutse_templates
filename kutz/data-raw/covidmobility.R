#' Preparing datasets for the first paper
#'title: "Data preparation and attempted replication of:Changes in Mobility during COVID-19 as a Response to Government Imposed Restrictions: A Multiple Regression Analysis for the Top Five Populous U.S States"
#' subtitle: "By Flor et al (2020)"
#' author: "Amos Okutse"
#' date: "  `r format(Sys.time(), '%d %B, %Y')` "

## ---- echo=FALSE, include= FALSE--------------------------------------------------------------------
# function to install missing packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.rstudio.com/')
  sapply(pkg, require, character.only = TRUE)
}
packages =c( "tidyverse","knitr", "kableExtra","skimr", "MatchIt", "RItools","optmatch", "ggplot2", "tufte", "tufterhandout", "plotly", "snowfall", "rstan", "gridExtra", "knitr", "gtsummary", "data.table", "GGally", "MASS", "broom", "boot", "foreach", "doParallel", "glmnet", "tidymodels" , "usemodels", "magrittr", "modelr")
ipak(packages)

#'
#' In this replication, interest is in examining the assoiation between covid-19 restrictions and mobility among the top 5 populous states in the US. The paper uses a linear regression model with the outcome as a mobility index denoting the median of the maximum distance individuals within a given region typically move, the count of covid deaths, the count of covid cases as well as whether a restriction was in place.
#'
#' The mobility data are downloaded from https://github.com/descarteslabs/DL-COVID-19. The secondary data for the covid deaths were from the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE), Worldometer, and lastly from the Institute of Health Metrics and Evaluation (IHME) and were respectively combined into further columns of Cases and
#' Deaths for each date in the time period between *March 10, 2020, and May 28, 2020*.
#'
#' We start by getting the mobility indices for all states in the US from the repo above. We will then subset the data to have the top 10 populous states based on this classification https://worldpopulationreview.com/states. We also do filter for dates within the above range. Note that some mobility data for certain dates was excluded as mentioned in the repo. Data for 2020-04-20 and 2020-05-29 within our date range. The mobility data can be filtered to focus either on the state level or the county level. For this analysis, we focus on the state level and thus also subset to records at the state level (administration level 1). This is so we are able to merge the data to the covid data downloaded in the next step which is at the state level.
#'
## ---------------------------------------------------------------------------------------------------
## download the data from the website
mi <- data.table::fread("https://raw.githubusercontent.com/descarteslabs/DL-COVID-19/master/DL-us-mobility-daterow.csv", header = TRUE)

## subset to 10 most populous states and filter to the dates focused on in analysis
dense_states <- c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
mi_dense <- mi %>% dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::filter((admin1 %in% dense_states) & (date >= "2020-03-10" & date <= "2020-05-28") & (admin_level == 1)) %>%
  dplyr::rename("state" = admin1) %>%
  dplyr::select(date, admin_level, state, fips, samples, m50, m50_index)

## save data as a csv
## write.csv(mi_dense, file = "../Mobility and COVID Restrictions/mobility_subset.csv", row.names = FALSE)

#'
#' Since the data from the JHU CSSE website is a bit trickier to retrieve, and incomplete in some way, we'll get the data on covid cases, deaths, and risk levels, vaccination rates among other variables which might have an effect on mobility indices from https://covidactnow.org/?s=45673318. These data are at the state level. Other covid data can be obtained from the `covidcast` and `COVID19` packages. I have not explored this packages here.
#'
## ---------------------------------------------------------------------------------------------------
## covid states data
states_covid <- data.table::fread("https://api.covidactnow.org/v2/states.timeseries.csv?apiKey=7ef2ad2d78e54bb6ba883019f0618359")
names(states_covid)

#'
## ---------------------------------------------------------------------------------------------------
states_covid <- states_covid %>%
  dplyr::select(date, state, fips, actuals.cases, actuals.deaths, metrics.caseDensity, metrics.infectionRate, riskLevels.overall, cdcTransmissionLevel) %>%
  rename("state.abb" = state) %>%
  mutate(date = as.Date(date, format = "%Y-%m%-%d"))

## create state details for easy merging
state_details <- data.frame(state = state.name,
                            state.abb = state.abb,
                            state.area = state.area)


## merge the states data set with the details above and filter to the states of interest in analysis
states_covid <- merge(states_covid, state_details, by = "state.abb")
states_covid <- states_covid %>%
  filter((state %in% dense_states) & (date >= "2020-03-10" & date <= "2020-05-28"))

## merge the states data with the mobility index data on date, state, and fips
comb_df <- merge(mi_dense, states_covid, by = c("date", "state", "fips"))
dim(comb_df)
names(comb_df)

#'
#' The study mentions using a logic to infer the time at which a state implemented covid restrictions when creating the restriction variable."Additionally, to construct a variable for the effect or contribution of government-imposed restrictions, a logical comparison was implemented to make a binary coded variable that would take assigned values of 0 (No Restriction present) or 1."
#' Here, we'll get the information on movement restrictions from https://ballotpedia.org/States_that_issued_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020
#' and create a variable for COVID-restrictions since we are interested in the state level effects of these restrictions. 1 corresponds to a movement restriction and 0 otherwise.
#'
## ---------------------------------------------------------------------------------------------------
## create the restriction variable. for any state that has a date equal or above the date of movement restriction, assign it to 1 otherwise 0 for all states with dates not meeting the criteria
comb_df$restriction <- ifelse(comb_df$state == "California" & comb_df$date >= "2020-03-19" |
                                comb_df$state == "Texas" & comb_df$date >= "2020-04-02" |
                                comb_df$state == "Florida" & comb_df$date >= "2020-04-02" |
                                comb_df$state == "New York" & comb_df$date >= "2020-03-20" |
                                comb_df$state == "Pennsylvania" & comb_df$date >= "2020-04-01"|
                                comb_df$state == "Illinois" & comb_df$date >= "2020-03-21" |
                                comb_df$state == "Ohio" & comb_df$date >= "2020-03-23" |
                                comb_df$state == "Georgia" & comb_df$date >= "2020-04-03" |
                                comb_df$state == "North Carolina" & comb_df$date >= "2020-03-30"|
                                comb_df$state == "Michigan" & comb_df$date >= "2020-03-24", 1, 0)
# format character to factor
comb_df <- comb_df %>% mutate_if(is.character, as.factor)
## we can write the final file to csv
# write.csv(comb_df, file = "../Mobility and COVID Restrictions/comb_df.csv", row.names = FALSE)

#' here we save the data set to the data_raw folder for use later



#' The final dataset has additional data on states and has some more variables that could be potential confounders. Given that these data are not an exact match to those presented in the paper, we do not expect to have the same exact figures but rather effects in the same direction as what was reported in the paper. Possible extensions here could include fitting an OLS model with cluster robust standard errors accounting for clustering by state instead of the fixed effects model they used. Extensions could also consider increasing the sample size by adding in more states. I have included an additional 5 states to add up to a total of 10 most dense states in the US with their areas which could also be thought of as a confounder.
#'
#' Here, we'll try out a replication of the overall state-wide model reported in Table 3. We'll use the `m50` mobility index as in the paper as our outcome variable. We've not tested the regression assumptions in this case and have not considered any transformations of the variables. In the paper, restrictions were significantly associated with a decline in the mobility index, a similar finding that we observe in this attempted replication (see Table \@ref(tab:table-three-rep)). While in the paper being in Pennsylvania and New York were not significantly associated with mobility, we see here that this only hold for residents in Pennyslvania. The number of covid cases is surprisingly not associated with a decline in mobility as was presented in the paper even though the increase here is also not significant. Factors that can explain this disparities is the fact that the data used here is not the same as the data used in the paper and different data preprocessing steps have been used. Overall though, we still see that restrictions are associated with a significant reduction in mobility, which is a major take away from the paper. For our case, using the variable `m50_index` as the outcome yields comparable findings.
#'
## ----table-three-rep--------------------------------------------------------------------------------
## Table 3: combined state regression model
five_states <- c("California", "Texas", "Florida", "New York", "Pennsylvania")
df <- filter(comb_df, state %in% five_states)
overall.model  <- lm(m50 ~ restriction + actuals.cases + state, data = df)

tbl = summary(overall.model)
as.data.frame(tbl$coefficients) %>%
  kable(format = "latex",
        label = "table-three-rep",
        caption = "Final combined state linear model (summary output).",
        digits = 7) %>%
  kable_styling(latex_options = "hold_position")

## ---------------------------------------------------------------------------------------------------
covidmobility = comb_df

usethis::use_data(covidmobility, overwrite = TRUE)
