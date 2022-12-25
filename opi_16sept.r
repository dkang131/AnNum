y = exp(x) + 2^(-x) + 2*cos(x) - 6 ; y
x = seq(1,10,0.05); x

a=data.frame(x,y) ; a

f = function(x) exp(-x)-x
m = matrix(0,2,5)
ea = matrix(0,2,5)


bisection <- function(a,b){
  i=0
  for(i in 1:10){
    m[i]=(a+b)/2
    if(f(m[i]) == 0){
      return (m[i])
    }
    else if(f(a)*f(m[i]) < 0){
      b = m[i]
    }
    else{
      a = m[i]
    }
    ea = abs(((m[i] - m[i-1])/m[i])*100)
    print(c(a,b,f(a),f(b),m[i],ea))
    i = i+1
  }
}

bisection(0.55,0.6)
