library(dplyr)
install.packages("plm")
library(plm)
library(here)
install.packages("texreg")
library(texreg)
final1 <- read.csv(here("data","finaldata.csv"), header=TRUE)
lmmod <- lm(MatMor ~ -1 + conflict_flag + gdp1000 + OECD + popdens + urban +
            agedep + male_edu + temp + rainfall1000 + earthquake + drought +
              ISO + as.factor(year),###as.factor makes the second year a fixed effecs
            data = finaldata)

plmmod <- plm(MatMor ~ conflict_flag + gdp1000 + OECD + popdens + urban +
                agedep + male_edu + temp + rainfall1000 + earthquake + drought,
              index = c("ISO", "year"),###putting second fixed effect variable always sets a comparison group, notice how code doesnt show 2000
              effect = "twoways",##tells r there are two fixed effects
              model = "within",
              data = finaldata)

screenreg(list(lmmod, plmmod))
##panel linear model omits the coefficient for each country 
##we also want to add fixed effects for years not only for country
##what does htmlreg do?? look into it, she used it instead of screenreg
