library(here)
library(dplyr)
library(boot)
###load in the data
finaldata <- read.csv(here("data", "finaldata.csv"))
data2017 <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(InfMor)) 
##create median differences for inf mort
Infmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(InfMor) & conflict_flag == 1) |>
  dplyr::select(ISO, InfMor)
Infmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(InfMor) & conflict_flag == 0) |>
  dplyr::select(ISO, InfMor)
bootinfmedian<- median(data2017[data2017$conflict_flag == 1,]$InfMor) -
  median(data2017[data2017$conflict_flag == 0,]$InfMor)

##create median differences for neo mort

Neomor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(NeoMor) & conflict_flag == 1) |>
  dplyr::select(ISO, NeoMor)
Neomor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(NeoMor) & conflict_flag == 0) |>
  dplyr::select(ISO, NeoMor)
bootneomedian<- median(data2017[data2017$conflict_flag == 1,]$NeoMor) -
  median(data2017[data2017$conflict_flag == 0,]$NeoMor)

##create median differences for under5 mort

Under5mor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(Under5Mor) & conflict_flag == 1) |>
  dplyr::select(ISO, NeoMor)
Under5mor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(Under5Mor) & conflict_flag == 0) |>
  dplyr::select(ISO, NeoMor)
bootunmedian<- median(data2017[data2017$conflict_flag == 1,]$Under5Mor) -
  median(data2017[data2017$conflict_flag == 0,]$Under5Mor)


##median difference for bootstaps
getmeddiff <- function(data, indices, var_name) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data[[var_name]], sample_data$conflict_flag, 
                       FUN = function(x) median(x, na.rm = TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}
###conduct bootstrap for function
variables <- c("InfMor", "Under5Mor", "NeoMor")
var_boot <- function(var_name) {
  set.seed(2024)
  boot(data = data2017, 
       statistic = function(data, indices) getmeddiff(data, indices, var_name), 
       strata = data2017$conflict_flag, 
       R = 1000)
}

boot_final <- lapply(variables, function(var_name) {
  var_boot(var_name)
})

boot_final ###median differences for each category
names(boot_final) <- variables

boot_all <- lapply(boot_final, function(boot_out) {
  boot.ci(boot.out = boot_out, conf = 0.95, type = c("basic", "perc", "bca"))
})
names(boot_all) <- variables


print(boot_all) ###all confidence intervals
##there is sufficient evidence to suggest that there is 
###median differences
bootneomedian
bootinfmedian
bootunmedian