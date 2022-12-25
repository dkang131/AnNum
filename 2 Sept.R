x = seq(0,0.65,0.05); x

y = exp(-x)-x ; y

plot(x,y,type = "l")

a = data.frame(x,y) ; a

graph <- function(x){
  y = exp(-x)-x
  data.frame(x,y)
  print(data.frame(x,y))
  plot(x,y,type = "l")
}

graph(seq(0,0.65,0.05))

x2 = seq(0,5,0.05); x2
y2 = sin(10*x2)+cos(3*x2); y2

b = data.frame(x2,y2); b

plot(x2,y2,type = "l")

graph2 <- function(x2){
  y2 = sin(10*x2)+cos(3*x2)
  data.frame(x2,y2)
  print(data.frame(x2,y2))
  plot(x2,y2,type = "l")
}

graph2(seq(0,5,0.05))
graph2(seq(0.35,0.4,0.002))
graph2(seq(0.362,0.364,0.0001))
graph2(seq(0.3624,0.36245,0.00002))
