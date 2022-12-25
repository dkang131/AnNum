f1 <- function(x) exp(x)+(2^(-x))+(2*(cos(x)))-6
f2 <- function(x) exp(x)-(2^(-x)*log(2))-(2*sin(x))
n=20
m = matrix(0,2,n)
x = matrix(0,2,n)
ea = matrix(0,2,n)
i = 1
es = 0.05

NT <- function(f1,f2,xa){
  x[i]=xa - (f1(xa)/f2(xa))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for (i in 1:n){
    i=i+1
    x[i]=x[i-1]-(f1(x[i-1])/f2(x[i-1]))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i] < es){
      break
    }
  }
}

NT(f1,f2,1.8)

#########################################################

f11 <- function(x) 2*x*cos(2*x)-(x-2)^2
f12 <- function(x) -2*x + 2*(-2*x*sin(2*x)+cos(2*x))+4
n=10
m = matrix(0,2,n)
x = matrix(0,2,n)
ea = matrix(0,2,n)
i = 1
es = 0.05

NT <- function(f11,f12,xa){
  x[i]=xa - (f11(xa)/f12(xa))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for (i in 1:n){
    i=i+1
    x[i]=x[i-1]-(f11(x[i-1])/f12(x[i-1]))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i] < es){
      break
    }
  }
}

NT(f11,f12,0.05)


