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

url <- "https://databank.worldbank.org/data/download/WDI_CSV.zip"
username <- Sys.info()["user"]

destfile <- file.path(paste0("C:/Users/", username, "/Downloads/","wdi.zip"))
download.file(url, destfile)

#### CLEAN DATA ####
data <- read.table(unz(destfile, "WDICSV.csv"),header=T, quote="\"", sep=",")
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


# flag if missing metadata info

indicators_metascore_df <- seriesmeta %>% #count number of words in each field
  mutate(Aggregationmethod_score=if_else(is.na(Aggregation.method),as.integer(0),sapply(strsplit(Aggregation.method, '\\W+'), length)) ,
         Developmentrelevance_score=if_else(is.na(Development.relevance),as.integer(0),sapply(strsplit(Development.relevance, '\\W+'), length)) ,
         License_Type_score=if_else(is.na(License.Type),as.integer(0),sapply(strsplit(License.Type, '\\W+'), length)) ,
         Limitationsandexceptions_score=if_else(is.na(Limitations.and.exceptions),as.integer(0),sapply(strsplit(Limitations.and.exceptions, '\\W+'), length)) ,
         Longdefinition_score=if_else(is.na(Long.definition),as.integer(0),sapply(strsplit(Long.definition, '\\W+'), length)) ,
         Periodicity_score=if_else(is.na(Periodicity),as.integer(0),sapply(strsplit(Periodicity, '\\W+'), length)) ,
         Source_score=if_else(is.na(Source),as.integer(0),sapply(strsplit(Source, '\\W+'), length)) ,
         Statisticalconceptandmethodology_score=if_else(is.na(Statistical.concept.and.methodology),as.integer(0),sapply(strsplit(Statistical.concept.and.methodology, '\\W+'), length)) ,
         Shortdefinition_score=if_else(is.na(Short.definition),as.integer(0),sapply(strsplit(Short.definition, '\\W+'), length)) ,
         Generalcomments_score=if_else(is.na(General.comments),as.integer(0),sapply(strsplit(General.comments, '\\W+'), length)) ,
         Notesfromoriginalsource_score=if_else(is.na(Notes.from.original.source),as.integer(0),sapply(strsplit(Notes.from.original.source, '\\W+'), length)) ,
         #sum up the various metadata components
         metadata_length=Developmentrelevance_score+Aggregationmethod_score+License_Type_score+Limitationsandexceptions_score+Longdefinition_score+Periodicity_score+Source_score+Statisticalconceptandmethodology_score+Shortdefinition_score+Generalcomments_score+Notesfromoriginalsource_score,
  ) %>%
  mutate(#produce score on any availability
    Developmentrelevance_min_score=if_else(Developmentrelevance_score>0, 1,    0),
    Statisticalconceptandmethodology_min_score=if_else(Statisticalconceptandmethodology_score>0,1,0),
    License_Type_min_score=if_else(License_Type_score>0,1,0),
    definition_min_score=if_else((Longdefinition_score+Shortdefinition_score>0),1,0),
    Source_min_score=if_else(Source_score>0,1,0),  
    #overall score
    metadata_availability=Developmentrelevance_min_score+Statisticalconceptandmethodology_min_score+License_Type_min_score+definition_min_score+Source_min_score,
    metadata_fail=case_when(
      Developmentrelevance_score<5 ~ "Development relevance missing",
      Statisticalconceptandmethodology_score<5 ~ "Statistical concept and methodology missing",
      License_Type_score<1 ~ "License Type missing",
      Longdefinition_score+Shortdefinition_score<5 ~ "Definition missing",
      Source_score<1 ~ "Source missing",
      TRUE ~ "No missing metadata"
    )) 

indicators_metascore_short_df <- indicators_metascore_df %>%
  select(Indicator.Code, Development.relevance, Statistical.concept.and.methodology, License.Type, Short.definition, Long.definition, Source,metadata_availability, metadata_fail,metadata_length)


### WDI Scoring of Indicators

scen <- read.csv("data/scenarios.csv")
colnames(scen)[1] <- "scenario"

score_table <- wdic %>%
  left_join(indicators_metascore_short_df) %>%
  mutate(ncountry_score = n_country/217*100,
         plmic_score = p_lmic,
         yearlatest_score = (yearlatest - 2010)/(current_year-2010)*100,
         yearlatestmedian_score = (yearlatest_median - 2001)/(current_year-2001)*100,
         spanyears_score = span_years/(current_year-1960)*100,
         nonmiss_score = nonmiss,
         uniquevisitors_score = ntile(uniquevisitors, 100),
         metadata_availability_fscore=metadata_availability*20,
         metadata_length_fscore=ntile(metadata_length, 100),
         frontier_score = (ncountry_score + plmic_score + yearlatest_score + yearlatestmedian_score + spanyears_score + nonmiss_score + uniquevisitors_score+metadata_availability_fscore+metadata_length_fscore)/9
  ) %>%
  mutate(
    country_score=case_when(
      n_country < scen$n_country[1] ~ 1,
      between(n_country,scen$n_country[1],scen$n_country[2])  ~ 2,
      between(n_country,scen$n_country[2],scen$n_country[3]) ~ 3,
      n_country >= scen$n_country[3] ~ 4
    )   ,
    p_lmic_score=case_when(
      p_lmic < scen$p_lmic[1] ~ 1,
      between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 2,
      between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 3,
      p_lmic >= scen$p_lmic[3] ~ 4
    )   ,
    yearlatest_median_score=case_when(
      yearlatest_median < scen$yearlatest_median[1] ~ 1,
      between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 2,
      between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 3,
      yearlatest_median >= scen$yearlatest_median[3] ~ 4
    )   ,
    yearlatest_score=case_when(
      yearlatest < scen$yearlatest[1] ~ 1,
      between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 2,
      between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 3,
      yearlatest >= scen$yearlatest[3] ~ 4
    )   ,
    span_years_score=case_when(
      span_years < scen$span_years[1] ~ 1,
      between(span_years,scen$span_years[1],scen$span_years[2])  ~ 2,
      between(span_years,scen$span_years[2],scen$span_years[3]) ~ 3,
      span_years >= scen$span_years[3] ~ 4
    )   ,
    uniquevisitors_score=case_when(
      uniquevisitors < scen$uniquevisitors[1] ~ 1,
      between(uniquevisitors,scen$uniquevisitors[1],scen$uniquevisitors[2])  ~ 2,
      between(uniquevisitors,scen$uniquevisitors[2],scen$uniquevisitors[3]) ~ 3,
      uniquevisitors >= scen$uniquevisitors[3] ~ 4
    )   ,
    nonmiss_score=case_when(
      nonmiss < scen$nonmiss[1] ~ 1,
      between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 2,
      between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 3,
      nonmiss >= scen$nonmiss[3] ~ 4
    ),         
    metadata_availability_score=case_when(
      metadata_availability < scen$metadata_availability[1] ~ 1,
      between(metadata_availability,scen$metadata_availability[1],scen$metadata_availability[2])  ~ 2,
      between(metadata_availability,scen$metadata_availability[2],scen$metadata_availability[3]) ~ 3,
      metadata_availability >= scen$metadata_availability[3] ~ 4
    ) ,   
    metadata_length_score=case_when(
      metadata_length < scen$metadata_length[1] ~ 1,
      between(metadata_length,scen$metadata_length[1],scen$metadata_length[2])  ~ 2,
      between(metadata_length,scen$metadata_length[2],scen$metadata_length[3]) ~ 3,
      metadata_length >= scen$metadata_length[3] ~ 4
    )            
  ) %>%
  mutate( #do a Distance to Threshold approach, where the frontiers are set by the thresholds
    country_score_hybrid=case_when(
      n_country < scen$n_country[4] ~ 0,
      between(n_country,scen$n_country[4],scen$n_country[1])  ~ (n_country-scen$n_country[4])/(scen$n_country[1]-scen$n_country[4]),
      between(n_country,scen$n_country[1],scen$n_country[2])  ~ 1+(n_country-scen$n_country[1])/(scen$n_country[2]-scen$n_country[1]),
      between(n_country,scen$n_country[2],scen$n_country[3]) ~ 2+(n_country-scen$n_country[2])/(scen$n_country[3]-scen$n_country[2]),
      between(n_country,scen$n_country[3],scen$n_country[5]) ~ 3 + (n_country-scen$n_country[3])/(scen$n_country[5]-scen$n_country[3]),
      n_country > scen$n_country[5] ~ 4,
    )   ,
    p_lmic_score_hybrid=case_when(
      p_lmic < scen$p_lmic[4] ~ 0,
      between(p_lmic,scen$p_lmic[4],scen$p_lmic[1])  ~ (p_lmic-scen$p_lmic[4])/(scen$p_lmic[1]-scen$p_lmic[4]),
      between(p_lmic,scen$p_lmic[1],scen$p_lmic[2])  ~ 1+(p_lmic-scen$p_lmic[1])/(scen$p_lmic[2]-scen$p_lmic[1]),
      between(p_lmic,scen$p_lmic[2],scen$p_lmic[3]) ~ 2+(p_lmic-scen$p_lmic[2])/(scen$p_lmic[3]-scen$p_lmic[2]),
      between(p_lmic,scen$p_lmic[3],scen$p_lmic[5]) ~ 3 + (p_lmic-scen$p_lmic[3])/(scen$p_lmic[5]-scen$p_lmic[3]),
      p_lmic > scen$p_lmic[5] ~ 4,
    )   ,
    yearlatest_median_score_hybrid=case_when(
      yearlatest_median < scen$yearlatest_median[4] ~ 0,
      between(yearlatest_median,scen$yearlatest_median[4],scen$yearlatest_median[1])  ~ (yearlatest_median-scen$yearlatest_median[4])/(scen$yearlatest_median[1]-scen$yearlatest_median[4]),
      between(yearlatest_median,scen$yearlatest_median[1],scen$yearlatest_median[2])  ~ 1+(yearlatest_median-scen$yearlatest_median[1])/(scen$yearlatest_median[2]-scen$yearlatest_median[1]),
      between(yearlatest_median,scen$yearlatest_median[2],scen$yearlatest_median[3]) ~ 2+(yearlatest_median-scen$yearlatest_median[2])/(scen$yearlatest_median[3]-scen$yearlatest_median[2]),
      between(yearlatest_median,scen$yearlatest_median[3],scen$yearlatest_median[5]) ~ 3 + (yearlatest_median-scen$yearlatest_median[3])/(scen$yearlatest_median[5]-scen$yearlatest_median[3]),
      yearlatest_median > scen$yearlatest_median[5] ~ 4,
    )   ,
    yearlatest_score_hybrid=case_when(
      yearlatest < scen$yearlatest[4] ~ 0,
      between(yearlatest,scen$yearlatest[4],scen$yearlatest[1])  ~ (yearlatest-scen$yearlatest[4])/(scen$yearlatest[1]-scen$yearlatest[4]),
      between(yearlatest,scen$yearlatest[1],scen$yearlatest[2])  ~ 1+(yearlatest-scen$yearlatest[1])/(scen$yearlatest[2]-scen$yearlatest[1]),
      between(yearlatest,scen$yearlatest[2],scen$yearlatest[3]) ~ 2+(yearlatest-scen$yearlatest[2])/(scen$yearlatest[3]-scen$yearlatest[2]),
      between(yearlatest,scen$yearlatest[3],scen$yearlatest[5]) ~ 3 + (yearlatest-scen$yearlatest[3])/(scen$yearlatest[5]-scen$yearlatest[3]),
      yearlatest > scen$yearlatest[5] ~ 4,
    )   ,
    span_years_score_hybrid=case_when(
      span_years < scen$span_years[4] ~ 0,
      between(span_years,scen$span_years[4],scen$span_years[1])  ~ (span_years-scen$span_years[4])/(scen$span_years[1]-scen$span_years[4]),
      between(span_years,scen$span_years[1],scen$span_years[2])  ~ 1+(span_years-scen$span_years[1])/(scen$span_years[2]-scen$span_years[1]),
      between(span_years,scen$span_years[2],scen$span_years[3]) ~ 2+(span_years-scen$span_years[2])/(scen$span_years[3]-scen$span_years[2]),
      between(span_years,scen$span_years[3],scen$span_years[5]) ~ 3 + (span_years-scen$span_years[3])/(scen$span_years[5]-scen$span_years[3]),
      span_years > scen$span_years[5] ~ 4,
    )   ,
    uniquevisitors_score_hybrid=case_when(
      uniquevisitors < scen$uniquevisitors[4] ~ 0,
      between(uniquevisitors,scen$uniquevisitors[4],scen$uniquevisitors[1])  ~ (uniquevisitors-scen$uniquevisitors[4])/(scen$uniquevisitors[1]-scen$uniquevisitors[4]),
      between(uniquevisitors,scen$uniquevisitors[1],scen$uniquevisitors[2])  ~ 1+(uniquevisitors-scen$uniquevisitors[1])/(scen$uniquevisitors[2]-scen$uniquevisitors[1]),
      between(uniquevisitors,scen$uniquevisitors[2],scen$uniquevisitors[3]) ~ 2+(uniquevisitors-scen$uniquevisitors[2])/(scen$uniquevisitors[3]-scen$uniquevisitors[2]),
      between(uniquevisitors,scen$uniquevisitors[3],scen$uniquevisitors[5]) ~ 3 + (uniquevisitors-scen$uniquevisitors[3])/(scen$uniquevisitors[5]-scen$uniquevisitors[3]),
      uniquevisitors > scen$uniquevisitors[5] ~ 4,
    )   ,
    nonmiss_score_hybrid=case_when(
      nonmiss < scen$nonmiss[4] ~ 0,
      between(nonmiss,scen$nonmiss[4],scen$nonmiss[1])  ~ (nonmiss-scen$nonmiss[4])/(scen$nonmiss[1]-scen$nonmiss[4]),
      between(nonmiss,scen$nonmiss[1],scen$nonmiss[2])  ~ 1+(nonmiss-scen$nonmiss[1])/(scen$nonmiss[2]-scen$nonmiss[1]),
      between(nonmiss,scen$nonmiss[2],scen$nonmiss[3]) ~ 2+(nonmiss-scen$nonmiss[2])/(scen$nonmiss[3]-scen$nonmiss[2]),
      between(nonmiss,scen$nonmiss[3],scen$nonmiss[5]) ~ 3 + (nonmiss-scen$nonmiss[3])/(scen$nonmiss[5]-scen$nonmiss[3]),
      nonmiss > scen$nonmiss[5] ~ 4,
    ) ,
    metadata_availability_score_hybrid=case_when(
      nonmiss < scen$metadata_availability[4] ~ 0,
      between(metadata_availability,scen$metadata_availability[4],scen$metadata_availability[1])  ~ (metadata_availability-scen$metadata_availability[4])/(scen$metadata_availability[1]-scen$metadata_availability[4]),
      between(metadata_availability,scen$metadata_availability[1],scen$metadata_availability[2])  ~ 1+(metadata_availability-scen$metadata_availability[1])/(scen$metadata_availability[2]-scen$metadata_availability[1]),
      between(metadata_availability,scen$metadata_availability[2],scen$metadata_availability[3]) ~ 2+(metadata_availability-scen$metadata_availability[2])/(scen$metadata_availability[3]-scen$metadata_availability[2]),
      between(metadata_availability,scen$metadata_availability[3],scen$metadata_availability[5]) ~ 3 + (metadata_availability-scen$metadata_availability[3])/(scen$metadata_availability[5]-scen$metadata_availability[3]),
      metadata_availability > scen$metadata_availability[5] ~ 4,
    )   ,
    metadata_length_score_hybrid=case_when(
      metadata_length < scen$metadata_length[4] ~ 0,
      between(metadata_length,scen$metadata_length[4],scen$metadata_length[1])  ~ (metadata_length-scen$metadata_length[4])/(scen$metadata_length[1]-scen$metadata_length[4]),
      between(metadata_length,scen$metadata_length[1],scen$metadata_length[2])  ~ 1+(metadata_length-scen$metadata_length[1])/(scen$metadata_length[2]-scen$metadata_length[1]),
      between(metadata_length,scen$metadata_length[2],scen$metadata_length[3]) ~ 2+(metadata_length-scen$metadata_length[2])/(scen$metadata_length[3]-scen$metadata_length[2]),
      between(metadata_length,scen$metadata_length[3],scen$metadata_length[5]) ~ 3 + (metadata_length-scen$metadata_length[3])/(scen$metadata_length[5]-scen$metadata_length[3]),
      metadata_length > scen$metadata_length[5] ~ 4,
    )   
  ) %>%    
  mutate(loose=if_else( #calculate whether indicator would fall in loose, median, or stringent as well
    (n_country < scen$n_country[1] |
       p_lmic     < scen$p_lmic[1] |
       yearlatest_median  < scen$yearlatest_median[1] |
       yearlatest  < scen$yearlatest[1] |
       nonmiss  < scen$nonmiss[1] |
       (yearlatest < 2015 & span_years < scen$span_years[1]) |
       uniquevisitors     < scen$uniquevisitors[1] |
       metadata_availability  < scen$metadata_availability[1] |
       metadata_length  < scen$metadata_length[1]),"Yes", "No"),
    
    median=if_else(
      (n_country < scen$n_country[2] |
         p_lmic     < scen$p_lmic[2] |
         yearlatest_median  < scen$yearlatest_median[2] |
         yearlatest  < scen$yearlatest[2] |
         nonmiss  < scen$nonmiss[2] |
         (yearlatest < 2015 & span_years < scen$span_years[2]) |
         uniquevisitors     < scen$uniquevisitors[2] |
         metadata_availability  < scen$metadata_availability[2] |
         metadata_length  < scen$metadata_length[2]),"Yes", "No"),
    stringent=if_else(
      (n_country < scen$n_country[3] |
         p_lmic     < scen$p_lmic[3] |
         yearlatest_median  < scen$yearlatest_median[3] |
         yearlatest  < scen$yearlatest[3] |
         nonmiss  < scen$nonmiss[3] |
         (yearlatest < 2015 & span_years < scen$span_years[3]) |
         uniquevisitors     < scen$uniquevisitors[3] |
         metadata_availability  < scen$metadata_availability[3] |
         metadata_length  < scen$metadata_length[3]),"Yes", "No"),
  ) %>%      
  
  mutate(
    threshold_score=(country_score+p_lmic_score+yearlatest_median_score+yearlatest_score+ span_years_score+ uniquevisitors_score+ nonmiss_score +metadata_availability_score + metadata_length_score )/9,
    
    hybrid_score=(country_score_hybrid+p_lmic_score_hybrid+yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid+ uniquevisitors_score_hybrid+ nonmiss_score_hybrid +metadata_availability_score_hybrid+metadata_length_score_hybrid )/9,
  ) %>%
  mutate(
    geographic_score= (country_score+p_lmic_score)/2,
    temporal_score=(yearlatest_median_score+yearlatest_score+ span_years_score)/3,
    completeness_score=nonmiss_score,
    usage_score=uniquevisitors_score,
    metadata_score=(metadata_availability_score+metadata_length_score)/2,
    threshold_score_wgtd=(geographic_score+temporal_score+completeness_score+usage_score+metadata_score)/5
  ) %>%   
  mutate(
    geographic_score_hybrid= (country_score_hybrid+p_lmic_score_hybrid)/2,
    temporal_score_hybrid=(yearlatest_median_score_hybrid+yearlatest_score_hybrid+ span_years_score_hybrid)/3,
    completeness_score_hybrid=nonmiss_score_hybrid,
    usage_score_hybrid=uniquevisitors_score_hybrid,
    metadata_score_hybrid=(metadata_availability_score_hybrid+metadata_length_score_hybrid)/2,
    hybrid_score_wgtd=(geographic_score_hybrid+temporal_score_hybrid+completeness_score_hybrid+usage_score_hybrid+metadata_score_hybrid)/5,
    hybrid_score_wgtd2=(geographic_score_hybrid+temporal_score_hybrid+usage_score_hybrid+metadata_score_hybrid)/4
  ) 




# save

write.csv(wdic, here("data","wdic.csv"))
write.csv(wdiy, here("data","wdiy.csv"))
write.csv(wdii, here("data","wdii.csv"))
write.csv(wdiit, here("data","wdiit.csv"))
write.csv(wdiyc, here("data","wdiyc.csv"))
write.csv(wditt, here("data","wditt.csv"))
write.csv(score_table, here("data","indicators_scoretable.csv"))
write.csv(indicators_metascore_short_df, here("data","indicators_metascore.csv"))
