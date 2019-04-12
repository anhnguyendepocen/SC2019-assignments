#######
# L 6 #
#######


NEWNRMethod <- function(y,xlist,min = 10^-10,maxtime = 1000)
  {
  lenx <- length(x)
  allx <- c()
  ys <- c()
  for (i in 1:lenx)
  {
    assign(paste("x",i,sep = ""),x[i])
    allx <- c(allx,paste('x',i,sep = ''))
  }
  for (i in 1:1000) 
  {
    n <- 0
    fx0 <- 1
    fx1 <- 0
    while ((abs(fx0 - fx1) > min) && n <= maxtime)
    {
      n <- n + 1
      fx0 <- eval(f)
      dx <- eval(deriv(f,xs,hessian = TRUE))
      x <- x - solve(matrix(attributes(dx)$hessian,nrow = 2),t(attributes(dx)$gradient))
      
      for (i in 1:xnum)
      {
        assign(paste("x",i,sep = ""),x[i])
      }
      
      fx <- eval(f)
    }
    if (n > maxtime) 
    {
      x<-NULL
    } 
    if (is.null(x)==FALSE)
    {result <-c(result,round(x,5))}
  }
  index <- duplicated(result)
  result[!index]
}

