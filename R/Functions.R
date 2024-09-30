library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(countrycode)
library(knitr)
infantmortality <- read.csv(here("original","infantmortality.csv"), header=TRUE)
maternalmortality <- read.csv(here("original","maternalmortality.csv"),header=TRUE)
neonatalmortality <- read.csv(here("original","neonatalmortality.csv"),header=TRUE)
under5mortality <- read.csv(here("original","under5mortality.csv"),header=TRUE)

##create a function that creates dataset for all
head(infantmortality)
head(maternalmortality)
head(neonatalmortality)
head(under5mortality)

list_mortality <- list(infantmortality, maternalmortality, neonatalmortality, under5mortality)
lapply(list_mortality, FUN = summary)##how do you check quality exactly here??##

restructured <- function(data,output_data,values_col) {
  # Reading the CSV file
  raw_mortality_data <- read.csv(here("original", data), header = TRUE, na.strings = c(""))
  # Selecting only certain columns
  mortality_data <- raw_mortality_data %>%
    select(Country.Name, matches("^X20"))
  data <- mortality_data  %>%
    select(Country.Name, starts_with("X")) %>%
    pivot_longer(
      cols = starts_with("X20"),
      names_to = "year",
      values_to = values_col
    ) %>%
    mutate(year = str_remove(year, "^X")) %>%
    mutate(year=as.numeric(year))
  # Saving the cleaned data to CSV
  write.csv(data, here("data", output_data), row.names = FALSE)
  
  # Returning the long-format data frame
  return(data)
}

maternal_cleaned <- restructured("maternalmortality.csv","cleaned_mat.csv","MatMor")
infant_cleaned <- restructured("infantmortality.csv","cleaned_inf.csv","InfMor")
neonatal_cleaned <- restructured("neonatalmortality.csv","cleaned_neo.csv", "NeoMor")
under5_cleaned <- restructured("under5mortality.csv","cleaned_U5.csv","Under5Mor")
list_mortality <- list(maternal_cleaned, infant_cleaned, neonatal_cleaned, under5_cleaned)


allmortality <- reduce(list_mortality, full_join, by = c("Country.Name", "year"))

head(allmortality)
# install.packages("countrycode")

allmortality$ISO <- countrycode(allmortality$Country.Name,
                                origin = "country.name",
                                destination = "iso3c")
allmortality <- allmortality%>%
  select(ISO,year,MatMor,InfMor,NeoMor,Under5Mor)%>%
  mutate(year=as.numeric(year))
#select function works as well with minus
write.csv(allmortality,here("data","allmortality.csv"), row.names = FALSE)

###disaster dataset###
disaster <- read.csv(here("original", "disaster.csv"), header = TRUE)
disaster$year <- disaster$Year
disaster <- disaster%>%select(-c(Year))
filtered <- disaster%>%
  filter(year>=2000 & year<=2019)%>%
  filter(Disaster.Type=="Drought" |Disaster.Type=="Earthquake")%>%
  mutate(drought = if_else(Disaster.Type == "Drought", 1, 0))%>%
  mutate(earthquake=if_else(Disaster.Type=="Earthquake",1,0))
filtered

new <- filtered%>%
  group_by(year,ISO)%>%
  summarize(drought=max(drought,na.rm=TRUE),
            earthquake=max(earthquake,na.rm = TRUE),
            .groups = 'drop')
conflict <- read.csv(here("original","conflictdata.csv"), header=TRUE)
conflict <- conflict%>%
  group_by(year,ISO)%>%
  summarize(total_best = sum(best, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(conflict_flag = if_else(total_best >= 25, 1, 0))%>%
  arrange(ISO)%>%
  mutate(year=year+1)
write.csv(conflict,here("data","conflict.csv"), row.names = FALSE)
