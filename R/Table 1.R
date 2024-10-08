library(tidyverse)
library(dplyr)
library(knitr)
library(xtable)
library(here)
library(readr)
library(table1)
library(Hmisc)
##read data##
finaldat <- read.csv(here("data", "finaldata.csv"), header = TRUE)

anyconf <- finaldat%>%
  group_by(ISO)%>%
  mutate(any_conflict = ifelse(sum(conflict_flag[year >= 2000 & year <= 2019]) > 0, 1, 0))%>%
  ungroup()

anyconf$any_conflict <- 
  group_by(ISO)%>%
  factor(anyconf$any_conflict, 
         levels=c(1,0),
         labels=c("Any Conflict(2000-2019)",
                  "No Conflict(2000-2019)")

collapsed_data <- anyconf %>%
  filter(year >= 2000 & year <= 2019) %>% 
  group_by(ISO) %>%
  summarise(
    mean_gdp= mean(gdp1000, na.rm = TRUE),
    mean_popdens= mean(popdens, na.rm = TRUE),
    mean_urban= mean(urban, na.rm = TRUE),
    mean_agedep= mean(agedep, na.rm = TRUE),
    mean_maleedu= mean(male_edu, na.rm = TRUE),
    mean_temp= mean(temp, na.rm = TRUE),
    mean_rainfall= mean(rainfall1000, na.rm = TRUE),
    mean_MatMor = mean(MatMor, na.rm = TRUE),
    mean_InfMor = mean(InfMor, na.rm = TRUE),
    mean_NeoMor= mean(NeoMor, na.rm = TRUE),
    mean_Under5Mor= mean(Under5Mor, na.rm = TRUE),
    mean_drought= mean(drought, na.rm = TRUE),
    mean_earthquake= mean(earthquake, na.rm = TRUE),
    any_conflict = max(any_conflict),  # Assume any conflict is relevant for the summary
    .groups = 'drop'  # Avoids warning messages
  )

collapsed_data$any_conflict <- factor(collapsed_data$any_conflict, 
                                      levels = c(1, 0),
                                      labels = c("Any Conflict (2000-2019)", 
                                                 "No Conflict (2000-2019)"))
table1(~ mean_MatMor+ mean_InfMor+mean_NeoMor+ mean_Under5Mor+ mean_earthquake+ mean_drought+ mean_temp+ mean_rainfall+
         mean_popdens+mean_urban+mean_agedep+ mean_maleedu+mean_gdp| 
         any_conflict,render.continuous=c(.="Median [Min, Max]"), render.missing= NULL, data=collapsed_data,topclass="Rtable1-zebra" )
label(collapsed_data$mean_InfMor) <- "Infant Mortality Rate (per 1000 live births)"
label(collapsed_data$mean_MatMor) <- "Maternal Mortality Rate (per 1000 live births)"
label(collapsed_data$mean_NeoMor) <- "Neonatal Mortality Rate (per 1000 live births)"
label(collapsed_data$mean_Under5Mor) <- "Under5 Mortality Rate (per 1000 live births)"
label(collapsed_data$mean_drought) <- "Drought Occurences"
label(collapsed_data$mean_earthquake) <- "Earthquake Occurences"
label(collapsed_data$mean_rainfall) <- "Rainfall (mm)"
label(collapsed_data$mean_gdp) <- "GDP"
label(collapsed_data$mean_temp) <- "Temperature"
label(collapsed_data$mean_maleedu) <- "Male Education"
label(collapsed_data$mean_urban) <- "Urban"
label(collapsed_data$mean_popdens) <- "Population density"
label(collapsed_data$mean_agedep) <- "Age Dependency Ratio"

library(ggplot2)
# Histogram to see normal distributions, all are skewed plots 
ggplot(collapsed_data, aes(x = mean_MatMor)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7)
ggplot(collapsed_data, aes(x = mean_InfMor)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7)
ggplot(collapsed_data, aes(x = mean_NeoMor)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7)
ggplot(collapsed_data, aes(x = mean_Under5Mor)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7)

