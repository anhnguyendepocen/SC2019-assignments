Newton_method <- function(f, x0, y0, i=0.00001, N = 100, gradient = NULL, hessian = NULL){
  x <- x0
  y <- y0
  if(is.null(gradient)&is.null(hessian)){
    
    d <- deriv(f, c("x", "y"), hessian = T)
    gradient <- expression(attributes(eval(d))$gradient)   #as function: eval(grandient), without given x
    hessian <- expression(matrix(attributes(eval(d))$hessian, byrow = F, 2,2))
  }
  else if(!is.null(gradient)&!is.null(hessian))(next)  
  else(return("Input the complete derivative expression or none"))    
  
  k <- c(10, 20)        #k[1]:previous solution  k[2]:present solution
  num <- 1         #count times
  vector_x <- matrix(c(x,y))  #initial guess vector, save solution into a vector
  while(abs(k[2] - k[1]) > i){
    if(num > N){print("Can't solve");return(vector_x)}
    x <- vector_x[1,1]
    y <- vector_x[2,1]
    vector_x <- vector_x - solve(eval(hessian), t(eval(gradient)))
    k[1] <- k[2]
    k[2] <- eval(f)
    num = num + 1
  }
  return(vector_x)
}

f = expression(x^2 -x*y + y^2 + exp(y))
Newton_method(f, 1,2)


#plot  from demo in ?persp
x <- seq(-5, 5, length= 100)
y <- seq(-6, 4, length= 100)
f <- function(x, y) {x^2 -x*y + y^2 + exp(y)}
z <- outer(x, y, f)

nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") ) 
# Generate the desired number of colors from this palette
nbcol <- 50
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

persp(x, y, z, col = facetcol[facetcol], theta = 80, phi = 20, expand = 0.7, ticktype = "detailed")

