#simple trail laying choice model (Deneuboug 2003)

#parameters:
time = 60 #[min] simulation time
step = 60 #[s] lenght of one time step
flux = 0.2 #[probability/s] that one ant leaves the nest
q_1 = 1 #quantity of pheromone deposited by an ant returning from patch 1 
q_2 = 1 #...
k = 5 #parameter in choice-function
n = 2 #
f = 0.0005 #[1/s] 1/mean lifetime of pheromone

#initialize:
trail_1 = 0 #pheromone concentration = C, trail 1
trail_2 = 0 #...
results_1 = numeric(time) #output vector for patch 1
results_2 = numeric(time) #...
x = c(1:time) #x-values for plot

#simulate:
for (i in 1:time)
{
  p_1 = ((k+trail_1)^n) / ((k+trail_1)^n + (k+trail_2)^n) #probabilities
  p_2 = ((k+trail_2)^n) / ((k+trail_1)^n + (k+trail_2)^n)
  ants_1 = rpois(1,lambda=p_1*flux*step)
  #ant numbers
  ants_2 = rpois(1,lambda=p_2*flux*step)
  trail_1 = trail_1*(exp(-step*f))+ants_1*q_1
  #new concentrations
  trail_2 = trail_2*(exp(-step*f))+ants_2*q_2
  results_1[i] = ants_1
  results_2[i] = ants_2
}

#plot:
plot(x,results_1,xlim=c(0,time),ylim=c(0,max(results_1,results_2)),xlab="Zeit[min]",ylab="Ameisen",col="red",type="l")
lines(x,results_2,col="blue")