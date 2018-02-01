# Table plot

Result.Matrix <- matrix(data = NA, nrow = 5, ncol = 9)
t.Set <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
Result.Matrix[2, ] <- as.matrix(pnorm(t.Set))
N.Set <- c(100, 1000, 10000)
Result.Matrix[1, ] <- t.Set
i = 3
for(N in N.Set){
  j = 1
  for(t in t.Set){
    x <- rnorm(N, mean = 0, sd = 1)
    Indicator.Variable <- as.numeric(x <= t)
    Fi.t <- mean(Indicator.Variable)
    Result.Matrix[i, j] <- Fi.t
    j = j+1
  }
  i = i+1
}



# Figures plot

N.Set <- c(100, 1000, 10000)
t.Set <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
for (t in t.Set){
  Bias <- matrix(data = NA, nrow = 3, ncol = 100)
  True.value <- pnorm(t)
  i = 1
  for(N in N.Set){
    for(j in 1:100){
    x <- rnorm(N, mean = 0, sd = 1)
    Indicator.Variable <- as.numeric(x <= t)
    Fi.t <- mean(Indicator.Variable)
    Bias[i, j] <- Fi.t - True.value
    }
  i=i+1
  }

Bias.t <- t(Bias)
Bias.N <- cbind(N100 = Bias.t[, 1],N1000 = Bias.t[, 2],N10000 = Bias.t[, 3])
boxplot(as.data.frame(Bias.N), ylim=c(-0.15, 0.15))
}

