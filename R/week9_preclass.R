## set simulation parameters
n <- 100 # sample size
pz <- 0.2 # probability of Z = 1
alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
alpha1 <- 1 # log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
beta0 <- -3 # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
beta1 <- 0
beta2 <- 2

## set seed
set.seed(2024)
## generate confounder Z from a binomial distribution
z <- rbinom(n, size = 1, prob = pz)
## compute probability of observing X = 1 from the inverse logit function
px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
## randomly generate binary variable X from the above probability
x <- rbinom(n, size = 1, prob = px)
## randomly generate binary variable Y from the inverse logistic function
py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
y <- rbinom(n, size = 1, prob = py)
## combine three random variables into a data frame
dat <- data.frame(lung = y, coffee = x, smoke = z)
## fit unadjusted logistic regression model
unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
summary(unadj.mod)
unadj.coef <- summary(unadj.mod)$coef
 ##since p value is in second row of the fourth colunmn you can do it by
unadj.coef 
unadj.p<- summary(unadj.mod)$coef[2,4]
##to tell r we will not reject, we create an indicator variable
unadj.rej <- (unadj.p<0.05)*1
## fit adjusted logistic regression model
adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
summary(adj.mod)
adj.coef <- summary(adj.mod)$coef
adj.p <- summary(adj.mod)$coef[2,4]
adj.coef
sum(dat$smoke)
adj.rej <- (adj.p<0.05)*1
c(unadj.rej,adj.rej)
# very simple example
library(foreach)
# set parameters
n <- 100
b0 <- 1; b1 <- 2
# repeat simulation 5 times
simout <- foreach(i=1:5, .combine=rbind) %do% {
  set.seed(i + 2024)  # set seed for reproducibility
  x <- rnorm(n)
  y <- b0 + b1*x + rnorm(n)
  bias <- coef(lm(y ~ x))[2] - b1
}
simout

library(parallel)
## set parameters
n <- 100; b0 <- 1; b1 <- 2

## Define the bias function
bias_f <- function(i){
  set.seed(i + 2024)  # set seed for reproducibility
  x <- rnorm(n)
  y <- b0 + b1 * x + rnorm(n)
  bias <- coef(lm(y ~ x))[2] - b1
  return(bias)
}

tictoc::tic() # Start timing

sim_out <- mclapply(
  1:5,         # Input for bias_f
  bias_f,      # Function to repeat, compute the bias
  mc.cores = 5 # Number of cores to use
) %>% 
  unlist() # Simplify result structure, because unlisting it becomes a vector

tictoc::toc()  # End timing
sim_out
mean(sim_out)

