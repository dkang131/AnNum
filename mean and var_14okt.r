data <- c(4,6,12,9)

miu <- mean(data);miu
var <- ((1/length(data))*(sum((data-miu)^2)))
var

n_miu <- function(miu,x,i){
  n=length(data)
  n=n+i
  new=(1/n)*((n-1)*miu + x)
  print(new)
}

miu1=n_miu(miu,8,1)


n_var <- function(n_miu,var,x,i){
  n=length(data)
  n=n+i
  new=((((n-1)/n)*var)+((1/(n-1))*(((x-n_miu)^2))))
  print(new)
}

var1 = n_var(miu1,var,8,1)

