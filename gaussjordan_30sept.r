A = matrix(c(1,0,1,0,2,6,8,5,0),byrow = T, nrow = 3, ncol = 3 )
b = matrix(c(2,8,13),nrow=3,ncol=1)

A1 = matrix(c(3,-0.1,-0.2,0.1,7,-0.3,0.3,-0.2,10),byrow=T, nrow = 3, ncol=3)
b1 = matrix(c(7.85,-19.3,71.4))
gauss_jordan <- function(A, b){
  n <- nrow(A)
  c <- matrix(cbind(A, b), nrow = n)
  x <- matrix(rep(0,n),  ncol = 1)
  
  for(i in 1:(n-1)){
    for(j in (i+1) : n){
      pivot <- c[j,i]/c[i,i]
      for(k in 1 : (n+1)){
        c[j,k] <- c[j,k] - pivot*c[i,k]
      }
    }
  }
  
  for(i in n:2){
    for(k in (i-1):1){
      pivot <- c[k,i]/c[i,i]
      c[k,i] <- c[k,i] - pivot*c[i,i]
      c[k,(n+1)] <- c[k,(n+1)] - pivot*c[i,(n+1)]
    }
  }
  
  for(i in 1:n){
    x[i] <- c[i,(n+1)]/c[i,i]
    print(x[i])
  }
}
gauss_jordan(A, b)

##########################################################################
install.packages("matlib")
library(matlib)

showEqn(A, b)
echelon(A, b)
echelon(A, b, verbose = T)
echelon(A, b, verbose = T, fraction = T)

showEqn(c,d)
echelon(c,d)
