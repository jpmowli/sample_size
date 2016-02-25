
simulation_loop<-function(nstart=100, target, r1, r2=2, nsim=100000, ref.effect, test.effect, veh.effect){
  report<-c(0,0,0,0,0,0)
  n1<-nstart
  while(report[6]<target){  


  # n1=PP sample size active arm
  

  #r1= pp/mITT
  #r2= n.active/n.vehicle
  r3=0.75   #mITT/randomized
  n2<-n1/r1   #active mITT
  n3<-n2/r2    #vehicle pp
  n4<-n3/r1   #vehicle mITT
  n2rand<-n2/r3
  n4rand<-n4/r3

  nsim<-10000   #number of simulations

  pr<-NULL
  pt<-NULL
  pr2<-NULL
  pt2<-NULL
  pv<-NULL
  pv2<-NULL

    for (n in 1:nsim){
    r<-rbinom(n2, 1, ref.effect)
    t<-rbinom(n2, 1, test.effect)
    v<-rbinom(n4, 1, veh.effect)

    pr2[n]=mean(r)
    pt2[n]=mean(t)
    pv2[n]=mean(v)

    pr[n]=mean(sample(r, n1))
    pt[n]=mean(sample(t, n1))
    pv[n]=mean(sample(v, n3))
    }

  seEq=(((pt*(1-pt)/n1))+((pr*(1-pr)/n1)))^0.5
  LEq=(pt-pr)-1.645*seEq-((1/n1)+(1/n1))/2
  UEq=(pt-pr)+1.645*seEq+((1/n1)+(1/n1))/2

  seSupR=(((pr2*(1-pr2)/n2))+((pv*(1-pv)/n4)))^0.5
  LSupR=(pr2-pv2)-1.960*seSupR-((1/n2)+(1/n4))/2
  USupR=(pr2-pv2)+1.960*seSupR+((1/n2)+(1/n4))/2

  seSupT=(((pt2*(1-pt2)/n2))+((pv*(1-pv)/n4)))^0.5
  LSupT=(pt2-pv2)-1.960*seSupT-((1/n2)+(1/n4))/2
  USupT=(pt2-pv2)+1.960*seSupT+((1/n2)+(1/n4))/2

  outcomeEq<-NULL
  outcomeSupR<-NULL
  outcomeSupT<-NULL


  for (n in 1:nsim){
    if (LEq[n]<=-0.20|UEq[n]>=0.20) outcomeEq[n]=0 else outcomeEq[n]=1
    if (LSupR[n]<=0&USupR[n]>=0) outcomeSupR[n]=0 else outcomeSupR[n]=1
    if (LSupT[n]<=0&USupT[n]>=0) outcomeSupT[n]=0 else outcomeSupT[n]=1
  }

  outcomeSup<-NULL
  outcome<-NULL
  outcomeSup=outcomeSupR*outcomeSupT
  outcome=outcomeEq*outcomeSup

  report<-c(n2rand, n2, n1, mean(outcomeEq), mean(outcomeSup), mean(outcome))
  print(report)
  
  n1<-n1+1

  }
}

############################################################


simulation_loop(nstart=124, target= 0.85, r1=0.85, r2=2, ref.effect=0.5, test.effect=0.475, veh.effect=0.2)

simulation_loop(nstart=124, target= 0.85, r1=0.85, r2=1, ref.effect=0.5, test.effect=0.475, veh.effect=0.2)

simulation_loop(nstart=180, target= 0.85, r1=0.85, r2=2, ref.effect=0.5, test.effect=0.475, veh.effect=0.3)

simulation_loop(nstart=153, target= 0.85, r1=0.85, r2=1, ref.effect=0.5, test.effect=0.475, veh.effect=0.3)