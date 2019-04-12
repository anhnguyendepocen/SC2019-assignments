library(timeDate)
library(timeSeries)
library(fBasics)

Ske <- function(TBoot, xsd, i){
  y <- c(rnorm(50,mean = 0,sd = 1))
  x <- exp(y)
  Tboot <- skewness(x)
  TBoot[i] <- Tboot
  xsd[i] <- sd(TBoot)
  list(Tb = TBoot, xs = xsd)
}

writeboot <- function(NORM.INTERVAL,PIVOTAL.INTERVAL,QUATILE.INTERVAL, file){
  interval <- cbind(NORM.INTERVAL,PIVOTAL.INTERVAL,QUATILE.INTERVAL)
  names(interval) <- c('L_NORM','U_NORM','L_PIVOTAL','U_PIVOTAL','L_QUATILE', 'U_QUATILE')
  write.csv(interval, paste(file, '/INTERVAL.csv', sep = ''))
}

Bootsrap <- function(n = 100, m = 1000){
  
  L <- rep(0,n)
  U <- rep(0,n)
  NORM.INTERVAL <- data.frame(L,U)
  PIVOTAL.INTERVAL <- data.frame(L,U)
  QUATILE.INTERVAL <- data.frame(L,U)
  
  
  
  for (k in 1:n){
    TBoot <- rep(0,m)
    xsd <- rep(0,m)
    for(i in 1:m){
      output <- Ske(TBoot, xsd, i)
      TBoot <- output$Tb
      xsd <- output$xs
    }
    
    XSD <- sd(TBoot)
    
    # plot(1:5000, xsd)
    #  hist(TBoot)
    
    
    Alpha <- 0.05
    
    Lcl <- mean(TBoot) + qnorm(Alpha/2, 0, 1)*XSD
    Ucl <- mean(TBoot) - qnorm(Alpha/2, 0, 1)*XSD
    NORM.interval = c(Lcl, Ucl)
    NORM.INTERVAL[k,] <- NORM.interval
    
    Lclp <- 2*XSD - quantile(TBoot, 1-Alpha/2)
    Uclp <- 2*XSD - quantile(TBoot, Alpha/2)
    PIVOTAL.interval = c(Lclp, Uclp)
    PIVOTAL.INTERVAL[k,] <- PIVOTAL.interval
    
    Lclb <- quantile(TBoot, Alpha/2)
    Uclb <- quantile(TBoot, 1-Alpha/2)
    QUATILE.interval = c(Lclb, Uclb)
    QUATILE.INTERVAL[k,] <- QUATILE.interval
    
  }
  writeboot(NORM.INTERVAL,PIVOTAL.INTERVAL,QUATILE.INTERVAL, 'C:/Users/lenovo/Desktop')
}


#debug
system.time(Bootsrap(n = 200, m = 1000))
registerDoMC(cores = detectCores())
Rprof()
system.time(Bootsrap(n = 200, m = 1000))
Rprof(NULL)

Rprof()
system.time(Bootsrap(n = 100, m = 1000))
Rprof(NULL)
summaryRprof()