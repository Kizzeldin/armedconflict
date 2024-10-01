library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(here)
library(stringr)
library(countrycode)
library(knitr)
rawdat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)
selected <- rawdat%>%
  select(Country.Name, X2000:X2019)
library(stringr)
renamed <- selected%>%
  rename_with(~ str_replace(., "^X", ""), starts_with("X"))


restructured <- rawdat %>%
  select(Country.Name, X2000:X2019)%>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "MatMor"
  ) %>%
  mutate(year = str_remove(year, "^X"))%>%
  mutate(year=as.numeric(year))

restructured%>%
  group_by(Country.Name)






