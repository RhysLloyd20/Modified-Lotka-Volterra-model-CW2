#Installing the 'deSolve' library
library(deSolve)
require(deSolve)

#First population dynamics plot (figure 3)

predator.prey.lotka.volterra<- function(times, N0, params) {
  N1<- N0[1]
  N2<- N0[2]
  with(as.list(params), {
    dN1.dt<- ((r1*N1)*(1-(N1/k)))-((alpha12*N1*N2)/(M+N1))
    dN2.dt<- ((alpha21*N1*N2)/(M+N1))-(d2*N2)
    return(list(c(dN1.dt, dN2.dt)))
  })
}
#setting the parameter values
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Setting the initial abundance conditions
N0<- c(100,100)
#Setting the number of time-steps
t.values<- seq(0, 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
par(mfrow=c(1,1))
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, col = c('black', 'red'))

######################################################################################################################

#Varying prey growth rate (figure 4)

#Increasing prey growth rate (a)

#setting the parameter values
params<- c(r1= 2.0, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
par(mfrow=c(2,2))
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Decreasing prey growth rate (b)

#setting the parameter values
params<- c(r1= 0.5, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Further decreasing prey growth rate (c)

#setting the parameter values
params<- c(r1= 0.05, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Phase-space plot (d)

#Including line for increased prey growth rate
params<- c(r1= 2.0, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
plot(predator.prey.out[,c(3)], predator.prey.out[,c(2)], type='l', lty= 1, lwd = 2, ylab = 'Prey Abundance', xlab = 'Predator Abundance', xlim= c(0, 350))
#Including line for decreased prey growth rate
params<- c(r1= 0.5, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "red")
#Including line for further decreased prey growth rate
params<- c(r1= 0.05, d2= 0.1, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "blue")
#Adding a legend
legend('topright', c('r1 = 2.0', 'r1 = 0.5', 'r1 = 0.05'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red', 'blue'))

###########################################################################################################################

#Varying predator growth rate (figure 5)

#Increasing predator growth rate (a)

#setting the parameter values
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.8, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
par(mfrow=c(2,2))
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Further increasing predator growth rate (b)

#setting the parameter values
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 2.0, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time', xlim= c(0, 45), ylim = c(0,400))
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Decreasing predator growth rate below the predator death rate (c)

#setting the parameter values
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.09, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Phase-space plot (d)

#Including line for increased predator growth rate
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.8, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
plot(predator.prey.out[,c(3)], predator.prey.out[,c(2)], type='l', lty= 1, lwd = 2, ylab = 'Prey Abundance', xlab = 'Predator Abundance', xlim = c(0,400))
#Including line for further increased predator growth rate
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 2.0, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "red")
#Including line for decreased predator growth rate below the predator death rate
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.9, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "blue")
#Adding a legend
legend('topright', c('a21 = 0.8', 'a21 = 2.0', 'a21 = 0.9'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red', 'blue'))

#######################################################################################################################################

#Predator growth rate set at 1.6 times greater than predator mortality rate (figure 6)

#Changing the number of time-steps
t.values<- seq(0, 1500)

#Decreasing predator growth rate (a)

#setting the parameter values
params<- c(r1= 1.1, d2= 0.1, alpha12= 1.1, alpha21= 0.16, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
par(mfrow=c(2,2))
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Phase plane (b)
plot(predator.prey.out[,c(3)], predator.prey.out[,c(2)], type='l', lty= 1, lwd = 2, ylab = 'Prey Abundance', xlab = 'Predator Abundance')

#Increasing predator mortality rate (c)

#setting the parameter values
params<- c(r1= 1.1, d2= 0.25, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Phase plane (d)
plot(predator.prey.out[,c(3)], predator.prey.out[,c(2)], type='l', lty= 1, lwd = 2, ylab = 'Prey Abundance', xlab = 'Predator Abundance')

###########################################################################################################################################

#Varying predator mortality rate (NOT INCLUDED IN THE TEXT)

#Setting number of time-steps
t.values<- seq(0, 300)

#Increasing predator mortality rate above the predator growth rate

#setting the parameter values
params<- c(r1= 1.1, d2= 0.5, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
par(mfrow=c(2,2))
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Decreasing predator mortality rate

#setting the parameter values
params<- c(r1= 1.1, d2= 0.06, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time')
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Further decreasing predator mortality rate

#setting the parameter values
params<- c(r1= 1.1, d2= 0.01, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
#Using ode to simulate the continuous time dynamic
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)

#Plotting the results
matplot(predator.prey.out[,c(2,3)], type='l', lty= 1, lwd = 2, ylab = 'Abundance', xlab = 'Time', xlim= c(0, 210), ylim = c(0,250))
legend('topright', c('Prey', 'Predator'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red'))

#Phase-space plot

#Including line for increased predator mortality rate above the predator growth rate
params<- c(r1= 1.1, d2= 0.5, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
plot(predator.prey.out[,c(3)], predator.prey.out[,c(2)], type='l', lty= 1, lwd = 2, ylab = 'Prey Abundance', xlab = 'Predator Abundance', xlim = c(0, 300), ylim = c(0, 300))
#Including line for decreased predator mortality rate
params<- c(r1= 1.1, d2= 0.06, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "red")
#Including line for further decreased predator mortality rate
params<- c(r1= 1.1, d2= 0.01, alpha12= 1.1, alpha21= 0.4, M= 70, k= 300)
predator.prey.out<- ode(N0, t.values, predator.prey.lotka.volterra, params)
lines(predator.prey.out[,c(3)], predator.prey.out[,c(2)], lwd = 2, col = "blue")
#Adding a legend
legend('topright', c('d2 = 0.5', 'd2 = 0.06', 'd2 = 0.01'), lty=1, lwd=2, cex = 0.65, col = c('black', 'red', 'blue'))

