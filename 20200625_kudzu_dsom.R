library(rjags)
library(runjags)
library(R2jags)

## data
data <- read.table("clipboard", header=TRUE)
loca <- read.table("clipboard", header=TRUE)
days <- read.table("clipboard", header=TRUE)

n.t <- 3
n.i <- nrow(data)/n.t

O <- matrix(data$o, nrow = n.i, ncol = n.t) # O[i,t]
rownames(O) <- 1:n.i
colnames(O) <- 1:n.t

z.zero <- loca$z.zero # z[i]
x <- loca$x # x[i]
y <- loca$y # y[i]

days <- days$days

data.list <- list(I = n.i,
                  J = n.i,
                  T = n.t,
                  O = O,
                  z.zero = z.zero,
                  x = x,
                  y = y,
                  days = days)

## initial values
inits1 <- list(p.tp = rep(0.8, n.t),
               p.fp = rep(0.2, n.t),
               e = rep(0.2, n.t),
               d = rep(0.8, n.t),
               k = rep(4, n.t),
               l = rep(-2, n.t))

inits2 <- list(p.tp = rep(0.8, n.t),
               p.fp = rep(0.2, n.t),
               e = rep(0.5, n.t),
               d = rep(0.5, n.t),
               k = rep(5, n.t),
               l = rep(0, n.t))

inits3 <- list(p.tp = rep(0.8, n.t),
               p.fp = rep(0.2, n.t),
               e = rep(0.8, n.t),
               d = rep(0.2, n.t),
               k = rep(6, n.t),
               l = rep(2, n.t))

inits.list <- list(inits1, inits2, inits3)
inits.list[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits.list[[1]]$.RNG.seed <- 123
inits.list[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits.list[[2]]$.RNG.seed <- 123
inits.list[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits.list[[3]]$.RNG.seed <- 123

## model_runjags
result_kudzu_dsom <- run.jags(method = "parallel",
                              model = "20200625_kudzu_dsom.txt",
                              monitor = c("deviance", "p.tp", "p.fp", "e", "d", "k", "l", "Dhalf"),
                              data = data.list,
                              inits = inits.list,
                              n.chains = 3,
                              adapt = 1000,
                              burnin = 10000,
                              sample = 6000,
                              thin = 50)
write.table(summary(result_kudzu_dsom), "20200625_result_kudzu_dsom.csv", sep = ",")
codaSamples_kudzu_dsom = as.mcmc.list(result_kudzu_dsom) 
source("DBDA2E-utilities.R")
diagMCMC(codaObject=codaSamples_kudzu_dsom, parName="k[3]")
plotPost(codaSamples_kudzu_dsom[,"k[3]"], main="k[3]", xlab=bquote(k[3]))
