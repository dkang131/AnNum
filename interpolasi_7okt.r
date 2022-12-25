yint = function(x,y,xx){
n = length(x)
b = matrix(0,n,n)
b[,1]=y

for(j in 2:n){
  for(i in 1:(n-j+1)){
    b[i,j]=(b[i+1,j-1]-b[i,j-1])/(x[i+j-1]-x[i])
  }
}
  xt = 1
  yint = b[1,1]
  for(j in 1:(n-1)){
    xt = xt*(xx-x[j])
    yint = yint+b[1,j+1]*xt
  }
    
  print(yint)
}

yint(c(0,8,16,24,32,40),c(14.621,11.843,9.87,8.418,7.305,6.413),27)



