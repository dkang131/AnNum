f=function(x) 5*(sin(x))^2- 8*(cos(x))^5

graphic <- function(f,x){
  my.frame = data.frame(x,f(x))
  print(my.frame)
  plot(x,f(x), type="l")
}
graphic(f,seq(0,1,0.05))

regalroot = function(f, xmin, xmax, tol=1e-4){
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
    c= b - ((f(b)*(a-b))/(f(a)-f(b)))
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
  return (c(b - ((f(b)*(a-b))/(f(a)-f(b))), iter, (b-a))) #root, iterations, precision
}
regalroot(f,0.7,0.75)
