f1 <- function(x) exp(-x)-x
n=10
m = matrix(0,2,n)
x = matrix(0,2,n)
ea = matrix(0,2,n)
i = 1
es = 0.05
#xa < xb in this example 0 < 0.55
secant <- function(f1,xa,xb){
  x[1]=xb
  x[2]=xb - ((f1(xb)*(xb-xa))/(f1(xb)-f1(xa)))
  ea[i]=abs(((x[i]-xa)/x[i])*100)
  print(c(x[i],ea[i]))
  for (i in 1:n){
    i=i+1
    x[i+1]=x[i]-((f1(x[i])*(x[i-1]-x[i]))/(f1(x[i-1])-f1(x[i])))
    ea[i]=abs(((x[i]-x[i-1])/x[i])*100)
    print(c(x[i],ea[i]))
    if(ea[i] < es){
      break
    }
  }
}

secant(f1,0,0.55)

