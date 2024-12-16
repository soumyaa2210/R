#Right censoring
n <- 100
lambda <- 0.5
life_time <- rexp(n, rate = lambda)
#Type1 censoring

c1=50
censored_type1 <- life_time > c1
life_time[censored_type1]=c1
life_time

#Type-II Censoring (Stop after a certain number of events)
life_time <- rexp(n, rate = lambda)
c2 <- 70  # Number of events we want to observe before stopping
life_time=sort(life_time)
censored_data<-ifelse(life_time>life_time[c2],life_time[c2],life_time)
censored_data

#Left censoring
n <- 100
lambda <- 0.5
life_time<-rexp(n, rate = lambda)
c3=1
left_censored=life_time<c3
left_censored
life_time[left_censored]=c3
life_time

#Interval_censoring
n <- 100
lambda <- 0.5
life_time=rexp(n, rate = lambda)
list=c(2,4,6,8,10,12)
for(i in list){
  interval_censored=(life_time>=i)&(life_time<=i+1)
  print(length(life_time[interval_censored]))
}