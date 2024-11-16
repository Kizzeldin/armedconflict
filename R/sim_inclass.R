adj.mod <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
summary(adj.mod)
adj.coef <- summary(adj.mod)$coef
adj.p <- summary(adj.mod)$coef[2,4]
adj.coef
adj.rej <- (adj.p<0.05)*1


# very simple example
library(foreach)
# set parameters


simout <- foreach(i=1:1000, .combine=rbind) %do% {
  n <- 500 # sample size
  pz <- 0.2 # probability of Z = 1
  alpha0 <- 0 # logit probability of x = 1 in non-smokers (z = 0)
  alpha1 <- 1# log odds ratio of x = 1 in smokers (z = 1) vs non-smokers
  beta0 <- -3 # logit prob of y = 1 in non-coffee drinkers (x = 0) and non-smokers (z = 0)
  beta1 <- 0
  beta2 <- 2
  set.seed(i + 2024)  # set seed for reproducibility
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  unadj.mod <- glm(lung ~ coffee, data = dat, family = "binomial")
  unadj.p <- summary(unadj.mod)$coef[2, 4]
  unadj.rej <- (unadj.p < 0.05) * 1
  adj.mod <- glm(lung ~ coffee+smoke, data = dat, family = "binomial")
  adj.p <- summary(adj.mod)$coef[2, 4]
  adj.rej <- (adj.p < 0.05) * 1
  c(unadj.rej,adj.rej)
}
unadjrate <- mean(simout[,1])
adjrate <- mean(simout[,2])
c(unadjrate, adjrate)
  column_means

