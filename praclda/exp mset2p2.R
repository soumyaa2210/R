n<-200
lamda<-0.3
m<-c(40,80,120,150,180,200)
simulation<-1000
evaluate_performace<-function(n,rate,m,simulation){
  estimates<-numeric(simulation)
  for(i in 1:simulation){
  life<-rexp(n,rate)
  censored_life<-sort(life)[1:m]
  estimates[i]<-m/(sum(censored_life)+(n-m)*sort(life)[m])
  }
  bias<-mean(estimates)-rate
  variance<-var(estimates)
  mse<-mean((estimates-rate)^2)
  return(list(bias=bias,variance=variance,mse=mse,estimate=mean(estimates)))
}
results<-data.frame(
  m=integer(),
  lamda_hat=numeric(),
  bias=numeric(),
  mse=numeric()
)
#starting loop for different m
for (i in m){
  performance<-evaluate_performace(n,lamda,i,simulation)
  #creating table
  results<-rbind(results,data.frame(
    m=i,
    lamda_hat=performance$estimate,
    bias=performance$bias,
    variance=performance$variance,
    mse=performance$mse
     ))
}
print(results)