library(tidyverse)
library(reshape2)
library(here)
library(lubridate)
#devtools::install_github('randyzwitch/RSiteCatalyst')
library(RSiteCatalyst)

#read in password.R at here()
source(here("password.R"))

#### GET DATA ####

#use sys.date() to get the current year
# Get the current date
current_date <- Sys.Date()

# Extract the year from the current date
current_year <- year(current_date)

# INSTRUCTIONS TO RUN FIRST TIME:
# Specify URL where file is stored and destination to download
# Uncomment url and download file when you need to download first time or redownload
# and update WDI file again.
# Change working directory to where you want to download WDI.

url <- "http://databank.worldbank.org/data/download/WDI_csv.zip"
username <- Sys.info()["user"]

destfile <- file.path(paste0("C:/Users/", username, "/Downloads","wdi.zip"))
download.file(url, destfile)

#### CLEAN DATA ####
data <- read.table(unz(destfile, "WDIData.csv"),header=T, quote="\"", sep=",")
topics <- read.table(unz(destfile, "WDISeries.csv"),header=T, quote="\"", sep=",") 
countrymeta <- read.table(unz(destfile, "WDICountry.csv"),header=T, quote="\"", sep=",")
seriesmeta <- read.table(unz(destfile, "WDISeries.csv"),header=T, quote="\"", sep=",")


# Reshape to long, clean up indicator names, drop all missing "values"
datal <- melt(data, id.vars = c("Country.Name", "Country.Code", "Indicator.Name", "Indicator.Code")) 
datal <- datal %>%
  rename("Year" = "variable") %>% 
  drop_na("value")

# Convert year variable as numeric
datal$Year <- str_replace(datal$Year, "X", "")
datal$Year <- as.numeric(as.character(datal$Year))

# Merge relevant metadata
countrymeta <- countrymeta %>%
  rename("Country.Code" = "Country.Code") %>%
  select("Country.Code", "Region", "Income.Group", "Lending.category")

seriesmeta <- seriesmeta %>%
  rename("Indicator.Code" = "Series.Code") %>%
#keep unique indicator codes
  filter(!duplicated(Indicator.Code)) %>%
    mutate(datatopic = ifelse(Topic == "Public sector",  "Public Sector",  Topic),
         datatopic = ifelse(datatopic == "Private sector", "Private Sector", datatopic))

datal <- datal %>%
  merge(countrymeta, by = "Country.Code", all.x = T) %>%
  merge(seriesmeta, by = "Indicator.Code", all.x = T) %>%
  filter(Region != "")

## There are 6 indicators for which only regional values are available. What to do about those?
#Net official flows from UN agencies, UNEP (current US$) 
#Number of people pushed below the $1.90 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure 
#Number of people pushed below the $3.20 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure 
#Number of people spending more than 10% of household consumption or income on out-of-pocket health care expenditure 
#Number of people spending more than 25% of household consumption or income on out-of-pocket health care expenditure 
#Proportion of population pushed below the $3.20 ($ 2011 PPP) poverty line by out-of-pocket health care expenditure (%) 


#### CREATE CRITERIA INDICATORS ####
totalspan <- max(datal$Year) - min(datal$Year) + 1
totalcountries <- length(unique(datal$Country.Code))

# Make LMIC country list
lmics <- unique(countrymeta[which(
  countrymeta$Income.Group == 'Low income' |
    countrymeta$Income.Group == 'Lower middle income' |
    countrymeta$Income.Group == 'Upper middle income'),]$Country.Code)


#wdic <- merge(wdic, countryobs, by = "Indicator.Code", all = T)
wdic2000 <- datal %>%
  filter(Year >= 2000) %>%
  group_by(Indicator.Code) %>%
  summarise(total_obs = n(),
            n_country  = n_distinct(Country.Code)) %>%
  mutate(nonmiss_tot2000 = round(100 * total_obs/(max(n_country)*(1+current_year-2000)), 2)) %>%
  select(Indicator.Code, nonmiss_tot2000)

# Weighted averages
wdic <- datal %>%
  group_by(Indicator.Code, Country.Code) %>%
  mutate(percountry_obs = n(),
         percountry_maxyear = max(Year),
         percountry_minyear = min(Year),
         percountry_meanyear = mean(Year)) %>%
  ungroup() %>%
  group_by(Indicator.Code) %>% 
  summarise(total_obs = n(),
            yearmean = mean(Year),
            yearmedian = median(Year),
            n_country  = n_distinct(Country.Code),          # Number of countries covered
            n_years    = n_distinct(Year),                  # Number of years covered
            countryobs_avg = mean(percountry_obs),          # Average number of obs per country
            countryobs_max = max(percountry_obs),           # Max number of obs per country
            yearlatest_mean = mean(percountry_maxyear),     # Mean latest year per country
            yearlatest_median = median(percountry_maxyear), # Median latest year per country
            yearlatest = max(percountry_maxyear),           # Latest year
            yearfirst_mean = mean(percountry_minyear),      # Mean first year per country
            yearfirst_median = median(percountry_minyear),  # Median first year per country
            yearfirst = min(percountry_minyear),            # First year
            yearmean_mean = mean(percountry_meanyear),
            yearmean_median = median(percountry_meanyear),
            n_lmic     = sum(unique(Country.Code) %in% lmics)) %>%
  mutate(span_years = yearlatest - yearfirst + 1,
         cov_years  = round(100 * ifelse(span_years == 0, 0, 
                                         (n_years - 1) / span_years), 2)
  ) %>%
  mutate(nonmiss = round(100 * total_obs/(n_country*span_years), 2),
         nonmiss_tot = round(100 * total_obs/(max(n_country)*max(span_years)), 2),
  ) %>%
  merge(wdic2000, by = "Indicator.Code")


wdiy <- datal %>%
  group_by(Indicator.Code, Year) %>%
  summarise(percountry_obs = n())

wdii <- datal %>%
  group_by(Country.Code) %>%
  summarise(percountry_indicators = n_distinct(Indicator.Code))

wdiit <- datal %>%
  group_by(Country.Code, datatopic) %>%
  summarise(percountry_indicators = n_distinct(Indicator.Code))

wdit <- datal %>%
  group_by(datatopic) %>%
  summarise(pertheme_indicators = n_distinct(Indicator.Code))

wdiyc <- datal %>%
  group_by(Year, datatopic) %>%
  summarise(peryear_indicators = n_distinct(Indicator.Code))

wditt <- datal %>%
  group_by(datatopic) %>%
  summarise(pertheme_indicators = n_distinct(Indicator.Code))


# Find what percentage of n_countries are lmic
tmp <- datal %>% 
  select(Indicator.Code, Country.Code) %>%
  distinct() %>%
  group_by(Indicator.Code) %>%
  summarise(p_lmic = round(100 * (
    sum(ifelse(Country.Code %in% lmics, 1, 0)) / length(lmics)), 2))

# Merge represtativeness with main
wdic <- merge(wdic, tmp, by = 'Indicator.Code')

#############################################//Part II. Adobe Analytics//#########################################################
# References: 
# https://www.rdocumentation.org/packages/WDI/versions/2.6.0/source
# https://randyzwitch.com/rsitecatalyst/

# Adobe analytics API login info
SCAuth(adobe_user, adobe_password)


# Get indicator list
dat <- wdic %>%
  select("Indicator.Code")

# get the Adobe analyitics data for 1599 indicators
pageName = paste("en:wb:datamain:/indicator/", dat$Indicator.Code, sep="")
pageviews_visits = QueueTrended("wbgglobalprod", current_date - years(1), current_date, 
                                date.granularity = "year",
                                c("uniquevisitors", "pageviews", "visits"), 
                                "page", selected = pageName)

WDI_Adobe <- data.frame(
  Indicator.Code = dat$Indicator.Code,
  uniquevisitors = pageviews_visits$uniquevisitors,
  pageviews = pageviews_visits$pageviews,
  visitors = pageviews_visits$visits
) %>%
  group_by(Indicator.Code) %>% 
  summarise(uniquevisitors = sum(uniquevisitors),
            pageviews = sum(pageviews),
            visitors = sum(visitors))
  

wdic <- merge(wdic, WDI_Adobe, by = "Indicator.Code")

wdic <- merge(wdic, seriesmeta, by = "Indicator.Code", all.x = T)
wdiy <- merge(wdiy, seriesmeta, by = "Indicator.Code", all.x = T)

write.csv(wdic, here("data","wdic.csv"))
write.csv(wdiy, here("data","wdiy.csv"))
write.csv(wdii, here("data","wdii.csv"))
write.csv(wdiit, here("data","wdiit.csv"))
write.csv(wdit, here("data","wdit.csv"))
write.csv(wdiyc, here("data","wdiyc.csv"))
write.csv(wditt, here("data","wditt.csv"))
