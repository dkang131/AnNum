factorial <- function(x){
  if(x==0)
    return (1)
  else
    return (x*factorial(x-1))
}

combination <- function(x,y){
  factorial(x)/(factorial(y)*factorial((x-y)))
}

combination(5,3)

###########################################################
#combination without factorial recursion

binomial_coef <- function(n,r){
  if(n == r | r == 0){
    return(1)
  }
  else{
    result <- binomial_coef(n-1,r-1)+binomial_coef(n-1,r)
    return (result)
  }
}

binomial_coef(5,2)

########################################################################
#new mean and variance

new_mean <- function(new_x){
  n=length(y)
  y=matrix(0,1,n)
  old_mean=mean(y)
  new_mean=((new_x+(n*old_mean))/(n+1))
}

y=c(1,2,3,4,5)
new_mean(6)


