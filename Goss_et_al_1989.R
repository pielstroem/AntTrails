#Model by Goss etal 1989: short and long trail experiment

#parameters:

time = 30  #[min] simulation time
step = 60  #[s] lenght of one time step
flux = 0.2 #[probability/s] that one ant leaves the nest
q    = 1   #quantity of pheromone deposited
T    = 20  #time needed to walk the short trail
r    = 2   #ratio long/short
k    = 20  #constant decision parameter
n    = 2   #constant decision parameter

#initialize:
q_s_1 = 1
q_s_2 = 1
q_l_1 = 1
q_l_2 = 1
p_1   = numeric(time) #probabilitie to schoose the short trail at first point
p_2   = numeric(time) #...
x     = c(1:time) #x-values for plot

#simulate:
for (t in 1:time)
{
  p_1[t] = (k+q_s_1)^n/((k+q_s_1)^n+(k+q_l_1)^n)
  p_2[t] = (k+q_s_2)^n/((k+q_s_2)^n+(k+q_l_2)^n)
  q_s_1 = q_s_1 + rpois(1,lambda=p_1[t]*flux*step)
  q_s_2 = q_s_2 + rpois(1,lambda=p_2[t]*flux*step)
  q_l_1 = q_l_1 + rpois(1,lambda=(1-p_1[t])*flux*step)
  q_l_2 = q_l_2 + rpois(1,lambda=(1-p_2[t])*flux*step)
  if (t > T)
  {
    q_s_1 = q_s_1 + rpois(1,lambda=p_1[t-T]*flux*step)
    q_s_2 = q_s_2 + rpois(1,lambda=p_2[t-T]*flux*step)
  }
  if (t > T*r)
  {
    q_l_1 = q_l_1 + rpois(1,lambda=(1-p_1[t-T*r])*flux*step)
    q_l_2 = q_l_2 + rpois(1,lambda=(1-p_2[t-T*r])*flux*step)
  }
}

#plot:
plot(x,p_1,xlim=c(0,time),ylim=c(0,1),xlab="time[min]",ylab="ants",col="red",type="l")
lines(x,1-p_1,col="blue")