x = seq(0,0.65,0.05); x

y = exp(-2*x)- sin(x) ; y

a = data.frame(x,y) ; a

plot(x,y,type = "l")


x2 = seq(0,5,0.05); x2

y2 = 2*(x2^3) - (2.5*x2) - 5 ;y2

b = data.frame(x2,y2) ; b

plot(x2,y2,type = "l")


