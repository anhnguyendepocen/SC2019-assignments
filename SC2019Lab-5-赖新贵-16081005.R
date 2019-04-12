
#newton-method
##############

#Find the root of f(x)=x^2-5
#iterative formula:Xn-1=Xn-(x^2-5)/(2*x)

#x:initial guess of the root
#f:function
#e:the tolerance level
newton.rephson<-function(x,f,e){
  n=1
  while (n>0) {
    x<-x-eval(f)/eval(D(f,"x"))
  if (abs(eval(f))<e)
    break
  else 
    n<-n+1
  }
  return(list(x=x,n=n))
}
f=expression(x^2-5)
newton.rephson(2,f,1e-10)

#############
#find the root of f(x)=sqrt(abs(x)),initial value :0.25

g=expression(sqrt(x))
#here we have a problem that abs(x) can't be directly derived
#so I remove the absolute value symbol in the function
#and take a absolute value of Xn+1 in the iteration formula
newton.rephson1<-function(x,f,e){
  n=1
  while (n>0) {
    x<-abs(x-eval(f)/eval(D(f,"x")))
    if (abs(eval(f))<e)
      break
    else 
      n<-n+1
  }
  return(list(x=x,n=n))
}
debug(newton.rephson1)
newton.rephson1(0.25,g,1e-10)
#it show that the initial value 0.25 will lead to a series that diverges.
#now let's try other initial values.
newton.rephson1(0.5,g,1e-10)
newton.rephson1(1,g,1e-10)#also lead to a series that diverges!!

#Then I found that the iteration formula of this question is "Xn+1=-Xn",
#which cause the fact that Xn just change the sign during the iteration,
#but not change its absolute value!!
#So we can never calculate its answer with this method

############
#find the root of x*exp(-x^2)-0.4/(exp(x)+1)-0.2=0

h=expression(x*exp(-x^2)-0.4/(exp(x)+1)-0.2)
newton.rephson(0.5,h,1e-10)
newton.rephson(0.6,h,1e-10)
newton.rephson(0.7,h,1e-10)
#we can see that if the initial value is farther from the true value,
#the number of iterations will increase and they will converge to different roots

#note that function D() can only get a expression of derivative,not the specific value