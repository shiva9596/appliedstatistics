##mid2

library("alr4")
data(fuel2001)
#predictor = Tax, Dlic=1000xDrivers/Pop, and log(Miles).
#Response = Fuel=1000xFuelc/Pop

x=((fuel2001$Tax , fuel2001$Dlic=1000*Drivers/pop,fuel2001$log(miles))
y=1000*fuelc/pop


plot((fuel2001$Tax , fuel2001$Dlic=1000*Drivers/pop,fuel2001$log(miles)),fuel20011=1000*Fuelc/pop)
## multiple linear regression
p <- E(Fuel)=β0+β1Tax+β2Dlic+β3log(Miles)+β4Tax*log(Miles)


#2nd question b party

library("alr4")
data("MinnLand")
head(MinnLand)
MinnLand$fregion = as.factor(MinnLand$region)
p <- lm(log(acrePrice) ~ year + region, data=MinnLand)
p
summary(p)

q <- lm(log(acrePrice) ~ year + region + year : region, data=MinnLand)
q
summary(q)

Anova(P)
ANOVA(P)
ANNOVA(Q)
annova(p)
annova(q)

##===
data("MinnLand")
head(MinnLand)
MinnLand$fregion = as.factor(MinnLand$region)
a = lm(log(acrePrice) ~ year + fregion, data = MinnLand)
b = lm(log(acrePrice) ~ year + fregion + year:fregion, data = MinnLand)
summary(a)
summary(b)

Anova(p)
anova(q)


##1.1
library(alr4)
data("fuel2001")
b<- dim(fuel2001)[1]
fuel2001$Dlic<- fuel2001$Drivers/fuel2001$Pop*1000

fuel2001$miles1<- log(fuel2001$Miles)
fuel2001$miles2<- fuel2001$miles1*fuel2001$Tax
Fuelb<-fuel2001$FuelC/fuel2001$Pop*1000
c=matrix(c(rep(1,n),fuel2001$Tax,fuel2001$Dlic,fuel2001$miles1,fuel2001$miles2),nrow=n,ncol=5,byrow=F)

beta.hat=solve(t(X)%*%X)%*%t(X)%*%Fueln
sigmasecond.hat=t(Fueln-X%*%beta.hat)%*%(Fueln-X%*%beta.hat)/(n-5)
sigma.hat=sqrt(sigmasecond.hat)
sigma.hat

d1=beta.hat[1]/sqrt(sigmasecond.hat*solve(t(X)%*%X)[1,1])
e.value1=2*(1-pt(abs(t1),n-5))
d1
e.value1

d2=beta.hat[2]/sqrt(sigmasecond.hat*solve(t(X)%*%X)[2,2])
e.value2=2*(1-pt(abs(t2),n-5))
d2
e.value2

d3=beta.hat[3]/sqrt(sigmasecond.hat*solve(t(X)%*%X)[3,3])
e.value3=2*(1-pt(abs(t3),n-5))
d3
e.value3

d4=beta.hat[4]/sqrt(sigmasecond.hat*solve(t(X)%*%X)[4,4])
e.value4=2*(1-pt(abs(t4),n-5))
d4
e.value4

d5=beta.hat[5]/sqrt(sigmasecond.hat*solve(t(X)%*%X)[5,5])
e.value5=2*(1-pt(abs(t4),n-5))
d5
e.value5

new_model <- lm(formula = Fueln ~ Tax + Dlic + miles1 + miles2 , data = fuel2001)
summary(new_model)

E(Fuel) = β0 + β1Tax ·log(Miles).

new_model2 <- lm(fuel2001$Fueln ~ fuel2001$Tax*log(fuel2001$miles1),data=fuel2001

model <- lm()
