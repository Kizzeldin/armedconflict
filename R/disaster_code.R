library(tidyverse)
library(here)
library(texreg)
library(multgee)
library(table1)
library(dplyr)
library(tidyr)
library(usethis) 
library(countrycode)
disaster <- read.csv(here("original", "disaster.csv"), header = TRUE)
disaster$year <- disaster$Year
disaster <- disaster%>%select(-c(Year))
filtered <- disaster%>%
  filter(year>=2000 & year<=2019)%>%
  filter(Disaster.Type=="Drought" |Disaster.Type=="Earthquake")%>%
  mutate(drought = if_else(Disaster.Type == "Drought", 1, 0))%>%
  mutate(earthquake=if_else(Disaster.Type=="Earthquake",1,0))%>%
  mutate(year=as.numeric(year))

filtered
new <- filtered%>%
  group_by(year,ISO)%>%
  summarize(drought=max(drought,na.rm=TRUE),
            earthquake=max(earthquake,na.rm = TRUE),
            .groups = 'drop')


write.csv(new, here("data","cleaned_disaster.csv"), row.names = FALSE)