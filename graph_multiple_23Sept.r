graph <- function(x1){
  y1 = 9 - (3*x1/2)
  y2 = 1 + (x1/2)
  data.frame(x1,y1,y2)
  print(data.frame(x1,y1,y2))
  plot(x1,y1,type = "l")
  lines(x1,y2,type = "l")
}

graph(seq(0,10,1))
