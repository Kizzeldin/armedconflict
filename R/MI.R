library(dplyr)
library(plm)
library(here)
library(texreg)
library(htmltools)
library(tidyverse)
library(knitr)
library(xtable)
library(table1)
library(Hmisc)
library(mice)
library(stargazer)
final1 <- read.csv(here("data","finaldata.csv"), header=TRUE)
##added loggdp to the dataset for imputation method
final2 <- final1%>%
  mutate(log(gdp1000))
midata <- final2 |>
  mutate(ISOnum = as.numeric(as.factor(final2$ISO))) |>
  select(-country_name,-region,-ISO,-total_best)
##we are gonna impute the following variables:urban, male_edu, temp, rainfall1000,matmor,infmor,neomor, un5mor, loggdp 
md.pattern(final2)
##dry run##
mice0  <- mice(midata, seed = 100, m = 10, maxit = 0, print = F)
mice.multi.out1 <- mice(midata, seed = 100, m = 10, maxit = 20, method = meth, print = F)
meth <- mice.multi.out1$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "MatMor", "InfMor", "NeoMor", "Under5Mor", "log(gdp1000)", "popdens")] <- "2l.lmer"
meth
pred <- mice.multi.out1$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "MatMor", "InfMor", "NeoMor", "Under5Mor", "log(gdp1000)", "popdens"), "ISOnum"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,method = meth, predictorMatrix = pred, print = F)
plot(mice.multi.out)

complete.data.multi2 <- complete(mice.multi.out, "all")
head(complete.data.multi2$`1`, n=20)
view(complete.data.multi2$`1`)


fit.mult <- with(mice.multi.out,
                 matmormod <- lm(MatMor ~ as.factor(conflict_flag) + log(gdp1000) +as.factor(ISOnum)+as.factor(year)+OECD + popdens + urban + 
                                   agedep + male_edu + temp + rainfall1000 + earthquake + drought))
matmormi <- pool(fit.mult)

fit.mult2 <- with(mice.multi.out,
                  under5mormod <- lm(Under5Mor ~ as.factor(conflict_flag) + log(gdp1000)+ log(gdp1000)+as.factor(ISOnum) + OECD + popdens + urban + 
                                       agedep + male_edu + temp + rainfall1000 + earthquake + drought))
undermormi <- pool(fit.mult2)
fit.mult3 <- with(mice.multi.out,
                  infmormod <- lm(InfMor ~ as.factor(conflict_flag) + log(gdp1000) +log(gdp1000)+as.factor(ISOnum)+ OECD + popdens + urban + 
                                    agedep + male_edu + temp + rainfall1000 + earthquake + drought))
infmormi <- pool(fit.mult3)
fit.mult4 <- with(mice.multi.out,
                  neomormod <- lm(NeoMor ~ as.factor(conflict_flag) + log(gdp1000) + log(gdp1000)+as.factor(ISOnum)+ OECD + popdens + urban + 
                                    agedep + male_edu + temp + rainfall1000 + earthquake + drought))
neomormi <- pool(fit.mult4)

##from last week complete case###
preds <- as.formula(" ~ as.factor(conflict_flag) + log(gdp1000)+as.factor(year)+as.factor(ISOnum)+ OECD + popdens + urban + 
                  agedep + male_edu + temp + rainfall1000 + earthquake + drought")

matmormod <- lm(update.formula(preds, MatMor ~ .), data = midata)
un5mormod <- lm(update.formula(preds, Under5Mor ~ .),data = midata)
infmormod <- lm(update.formula(preds, InfMor ~ .),data = midata)
neomormod <- lm(update.formula(preds, NeoMor ~ .),data = midata)

label <- list("MarMor"= "Maternal Mortality",
              "conflict_flag" = "Armed Conflict", 
              "log(gdp1000)" = "Log GDP (1000)", 
              "OECD" = "OECD", 
              "popdens" = "Population Density", 
              "urban" = "Urbanization", 
              "agedep" = "Age Dependency", 
              "male_edu" = "Male Education", 
              "temp" = "Temperature", 
              "rainfall1000" = "Rainfall", 
              "earthquake" = "Earthquake", 
              "drought" = "Drought")
model2_names <- c("Maternal Mortality","Maternal MI", "Under-5 Mortality","Under5 MI", "Infant Mortality","InfMor MI", "Neonatal Mortality", "NeoMor MI")

output <- screenreg(list(matmormod,matmormi, un5mormod, undermormi, infmormod,infmormi, neomormod, neomormi), 
                    ci.force = TRUE,
                    custom.coef.map = label,
                    custom.model.names = model2_names)

output
