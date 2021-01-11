#Montecarlo
MC_unif <- function(iter,N,tau,g,b,s,k){
  # Set up
  gamma <- seq(-g,g, length.out = k)   #Parameters
  beta <- seq(-b,b,length.out = k)
  b_MC <- NULL
  
  #Start loop
  for (r in 1:iter){
    # Generate Data
    X <- matrix(runif(N*k,-s,s), N, k) 
    X[,1] <- rep(1,N)
    eps <- rnorm(N,0,1)
    eta <- rnorm(N,0,1)
    D <- (X %*% gamma - eta > 0)
    Y <- tau*D + X %*% beta + eps
    
    #DML
    tau_DML <- b_DML(Y,X,D)
    waldt = wald_test(Y,X,D)
    p_value = waldt$`Pr(>F)`[2]
    LMtest = lm_test(Y,X,D)
    
    #Save results
    b_MC <- rbind(b_MC, data.frame(tau_DML,p_value,LMtest = LMtest))
  }
  return(b_MC)
}


#Double Debiased Machine Learning
b_DML <- function(Y,X,D){
  DML1 <- cv.glmnet(X, Y, alpha = 1)
  yhat <- predict(DML1, X)
  res1 <- Y - yhat
  DML2 <- cv.glmnet(X, D, alpha = 1)
  Dhat <- predict(DML2, X)
  res2 <- D - Dhat
  DML <- lm(res1 ~ 0 + res2)
  b_DML <- unname(coef(DML))
  return(b_DML)
  
}

#Waldt Test
wald_test <- function(Y,X,D){
  DML1 <- cv.glmnet(X, Y, alpha = 1)
  yhat <- predict(DML1, X)
  res1 <- Y - yhat
  DML2 <- cv.glmnet(X, D, alpha = 1)
  Dhat <- predict(DML2, X)
  res2 <- D - Dhat
  DML <- lm(res1 ~ 0 + res2) 
  waldt <- waldtest(DML)
  return(waldt)
  
}

#LM Test
lm_test <- function(Y,X,D){
  n = length(X)
  DML1 <- cv.glmnet(X, Y, alpha = 1)
  yhat <- predict(DML1, X)
  res1 <- Y - yhat
  DML2 <- cv.glmnet(X, D, alpha = 1)
  Dhat <- predict(DML2, X)
  res2 <- D - Dhat
  DML <- lm(res1 ~ 0 + res2)
  res3 = resid(DML)
  lmtest = ((t(res1) %*% res2) / sqrt(n)) * (n / (t(res3^2) %*% res2^2))  * ((t(res1) %*% res2) / sqrt(n))
  return(lmtest)
  
}

#MC results
MC_result <- function(b_MC,tau){
  tau_dml_c <- b_MC$tau_DML
  hist(tau_dml_c, freq = FALSE)
  curve(dnorm(x, mean = 0, sd(b_MC$tau_DML)), add = TRUE)
  powers = pchisq((b_MC$X1), 1)
  hist(powers, breaks =  c(0.98,0.985,0.99,0.995,1,1.05), freq = FALSE)
  
}