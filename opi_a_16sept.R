f <- function(x) (2-(exp(x))+x^2)/3
n = 30
m = matrix(0,2,n)
x = matrix(0,2,n)
ea = matrix(0,2,n)
i = 1
es = 0.05

opi <- function(f,xa){
  x[i] = f(xa)
  ea[i] = abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for(i in 1:n){
    i=i+1
    x[i]=f(x[i-1])
    ea[i] = abs(((x[i]-x[i-1])/x[i])*100)
    if(ea[i] < es){
      break
    }
    print(c(x[i],ea[i]))
  }
}

opi(f,1.3)