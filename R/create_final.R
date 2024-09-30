covariates <- read.csv(here("original", "covariates.csv"), header = TRUE)

library(knitr)

source(here("R", "Functions.R"))
source(here("R", "disaster_code.R"))
source(here("R", "read_csv.R"))
#put all data frames into list
alllist <- list(allmortality, conflict, new)

#merge all data frames in list
alllist |> reduce(full_join, by = c('ISO', 'year')) -> finaldata0

finaldata <- covariates %>%
  left_join(finaldata0, by = c('ISO', 'year'))

finaldata <- finaldata |>
  mutate(conflict_flag = replace_na(conflict_flag, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         total_best = replace_na(total_best, 0))

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)

