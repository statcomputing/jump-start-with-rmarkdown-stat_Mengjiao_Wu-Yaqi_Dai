# Table plot

Result_Matrix <- matrix(data = NA, nrow = 5, ncol = 9)
t_set <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
Result_Matrix[2,] <- as.matrix(pnorm(t_set))
N_set <- c(100, 1000, 10000)
Result_Matrix[1,] <- t_set
i = 3
for(N in N_set){
  j = 1
  for(t in t_set){
    x <- rnorm(N, mean = 0, sd = 1)
    Indicator_variable <- as.numeric(x <= t)
    Fi_t <- mean(Indicator_variable)
    Result_Matrix[i,j] <- Fi_t
    j = j+1
  }
  i = i+1
}



# Figures plot

N_set <- c(100, 1000, 10000)
t_set <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)

for (t in t_set){
  Bias <- matrix(data = NA, nrow = 3, ncol = 100)
  true_value <- pnorm(t)
  i = 1
  for(N in N_set){
    for(j in 1:100){
    x <- rnorm(N, mean = 0, sd = 1)
    Indicator_variable <- as.numeric(x <= t)
    Fi_t <- mean(Indicator_variable)
    Bias[i,j] <- Fi_t
    }
  i=i+1
  }

Bias_t <- t(Bias)
Bias_N <- cbind(N100 = Bias_t[,1],N1000 = Bias_t[,2],N10000 = Bias_t[,3])
boxplot(as.data.frame(Bias_N))
}
