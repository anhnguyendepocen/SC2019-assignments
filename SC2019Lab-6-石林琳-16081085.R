newton.raphson<-function(fun,x0,ep=1e-10,times=100)
{
  for (i in 1:times)
  {
    x=x0-f(x0)/(ff(x0))
    if (abs(x-x0)<ep)
    {
      print(i)
      break
    }
    else x0=x
  }
  print(x)
}

#(1)
f<-function(x)
{
  x^2-5
}

ff<-function(x)
{
  2*x
}
newton.raphson(f,2)

#(2)
f<-function(x)
{
  sqrt(abs(x))
}

ff<-function(x)
{
  if (x>=0)
    1/2*x^(-1/2)
  else
    1/2*(-x)^(-1/2)
}
newton.raphson(f,0.25)

#(3)
f<-function(x)
{
  x*exp(-x^2)-0.4*(exp(x)+1)^(-1)+0.2
}

ff<-function(x)
{
  (1-2*x^2)*exp(-x^2)+0.4*exp(x)/(exp(x)+1)^2
}
newton.raphson(f,0.5)
newton.raphson(f,0.6)


