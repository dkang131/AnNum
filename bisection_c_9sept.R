f=function(x) x-(2^(-x))

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,0.65,0.05))

bisectionroot = function(f, xmin, xmax, tol=1e-4){
  a= xmin; b= xmax
  #check inputs
  if (a >= b){
    cat("error: xmin > xmax \n")
    return(NULL)
  }
  if (f(a) == 0){
    return(a)
  }
  else if (f(b) == 0){
    return(b)
  }
  else if (f(a)*f(b) > 0){
    cat("error: f(xmin) and f(xmax) of same sign \n")
    return(NULL)
  }
  #if inputs OK, converge to root
  iter = 0
  while ((b-a) > tol){
    c= (a+b)/2
    if (f(c)==0){
      return (c)
    }
    else if (f(a)*f(c) <0){
      b =c 
    }
    else {
      a = c
    }
    iter = iter +1
  }
  return (c((a+b)/2, iter, (b-a))) #root, iterations, precision
}
bisectionroot(f,0.6,0.65)