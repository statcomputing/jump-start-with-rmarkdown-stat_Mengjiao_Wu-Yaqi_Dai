N_set <- c(100,1000,10000)
t_set <- c(0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)


Bias  <- matrix(data = NA, nrow = 3, ncol = 100 )
true_value <- pnorm(0)
i=1
for(N in N_set){
  for(j in 1:100){
    x <- rnorm(N, mean=0, sd=1)
    Indicator_variable <- as.numeric(x <= 0)
    Fi_t = mean(Indicator_variable)
    Bias[i,j] <- Fi_t-true_value
  }
 i=i+1
  
}
