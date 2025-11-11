## ----echo=FALSE----------------------------------------------------------------------------------------------
set.seed(666)


## ------------------------------------------------------------------------------------------------------------
emaitza.zuzenak <- c(13, 2, 7, 9, 18, 16, 5, 4, 2, 11, 8, 11, 10, 6, 16, 10, 23, 1, 1, 7)


## ------------------------------------------------------------------------------------------------------------
dnbinom(x=10, size=5, prob=0.43)


## ------------------------------------------------------------------------------------------------------------
k <- 3
p <- 0.1  #Asumitzen ari gara hau dela benetako balioa, laginak sortzeko
lagina.5  <- rnbinom(5, size=k, prob=p)
lagina.50 <- rnbinom(50, size=k, prob=p)

lagina.5 #Lagin txikia bistaratuko dugu


## ------------------------------------------------------------------------------------------------------------
#A priori parametroak
a.prior <- 1  #alpha (a priori)
b.prior <- 1  #beta  (a priori)

#Lagin txikirako (n=5)  a posteriori parametroak
a.lagina.5 <- a.prior + k*length(lagina.5) #alpha (a posteriori)
b.lagina.5 <- b.prior + sum(lagina.5)      #beta  (a posteriori)

#Lagin handirako (n=50) a posteriori parametroak
a.lagina.50 <- a.prior + k*length(lagina.50) #alpha (a posteriori)
b.lagina.50 <- b.prior + sum(lagina.50)      #beta  (a posteriori)


## ----fig.asp=0.33--------------------------------------------------------------------------------------------
p.seq <- seq(0, 1, 0.001)
dens.prior   <- dbeta(x=p.seq, shape1=a.prior,     shape2=b.prior)     #a priori banaketa
dens.post.5  <- dbeta(x=p.seq, shape1=a.lagina.5,  shape2=b.lagina.5)  #a posteriori (n=5)
dens.post.50 <- dbeta(x=p.seq, shape1=a.lagina.50, shape2=b.lagina.50) #a posteriori (n=50)

#Dentsitate-funtzioen bistaraketa:
layout(matrix(1:3, nrow=1))
par(cex=0.5)
plot(p.seq, dens.prior, type="l",
     main="A priori", xlab="p parametroa", ylab="Dentsitatea")

plot(p.seq, dens.post.5, type="l",
     main="A posteriori: 5 behaketa", xlab="p parametroa", ylab="Dentsitatea")

plot(p.seq, dens.post.50, type="l",
     main="A posteriori: 50 behaketa", xlab="p parametroa", ylab="Dentsitatea")


## ----fig.asp=0.33--------------------------------------------------------------------------------------------
#Lagin txikia erabilita (n=5)
estimazioa.p.5 <- a.lagina.5 / (a.lagina.5 + b.lagina.5)
estimazioa.p.5
#Lagin handia erabilita (n=50)
estimazioa.p.50 <- a.lagina.50 / (a.lagina.50 + b.lagina.50)
estimazioa.p.50


## ------------------------------------------------------------------------------------------------------------
#Lagin txikia erabilita (n=5)
p.inf.5 <- qbeta(0.025, a.lagina.5, b.lagina.5)
p.sup.5 <- qbeta(0.975, a.lagina.5, b.lagina.5)
message("%95ko tarte Bayesiarra: [", round(p.inf.5, 3), ",", round(p.sup.5,3), "]")

#Lagin handia erabilita (n=50)
p.inf.50 <- qbeta(0.025, a.lagina.50, b.lagina.50)
p.sup.50 <- qbeta(0.975, a.lagina.50, b.lagina.50)
message("%95ko tarte Bayesiarra: [",  round(p.inf.50, 3), ",", round(p.sup.50,3), "]")


## ------------------------------------------------------------------------------------------------------------
a.prior <- 1
b.prior <- 1

k <- 3
a <- a.prior + k*length(emaitza.zuzenak)
b <- b.prior + sum(emaitza.zuzenak)

library(extraDistr)
prob.irabazi <- 1 - pbnbinom(q=22, size=k, alpha=a, beta=b)
prob.irabazi

