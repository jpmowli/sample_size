
# n1=PP sample size active arm
n1<-134 # active pp
# n2= mITT sample size placebo arm
#r1= 2*pp/mITT
r1<-0.85
n2<-n1/r1 #active mITT
n3<-n2/2 # vehicle pp
n4<-n3/r1


#nsim= number of simulations
nsim<-10000

pr<-NULL
pt<-NULL
pr2<-NULL
pt2<-NULL
pv<-NULL
pv2<-NULL

for (n in 1:nsim){
r<-rbinom(n2, 1, 0.5)
t<-rbinom(n2, 1, 0.5)
v<-rbinom(n4, 1, 0.2)

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



mean(outcomeEq)
mean(outcomeSupR)
mean(outcomeSupT)
mean(outcomeSup)
mean(outcome)
############################################################

