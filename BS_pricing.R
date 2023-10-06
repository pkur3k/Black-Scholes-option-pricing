BSPrice -> function(K, T, s0, r, sigma)
  
  d1 <- (1/(sigma*sqrt(T)))*(log(s0/K)+(r+sigma^2/2)T)
  d2 <- d1-sigma*sqrt(T)
  C <- pnorm(d1)*s0-porm(d2)*K*exp(-r*T)
  
  return C
  
