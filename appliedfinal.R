#x1= avg rent paid
#x2=density of dairy cows
#x3=proportion of farmland used as pasture

library(alr4)
head(landrent)
data(landrent)
?landrent

result <- lm(Y ~ X1 + X2+ X3 + X4, data = landrent)
summary(result)



##1
library(alr4)
data("landrent")
n=dim(landrent)[1]
p<-4
x1<- landrent$X1
x2<- landrent$X2
x3<- landrent$X3
x4<- x2*x3
y<- landrent$Y
X=matrix(c(rep(1,n),x1,x2,x3,x4),nrow=n,ncol=5,byrow=F)

beta.hat=solve(t(X)%*%X)%*%t(X)%*%y 
sigma2.hat=t(y-X%*%beta.hat)%*%(y-X%*%beta.hat)/(n-5) 
sigma.hat=sqrt(sigma2.hat) 
sigma.hat 
y.hat=X%*%beta.hat
t1=beta.hat[1]/sqrt(sigma2.hat*solve(t(X)%*%X)[1,1]) 
p.value1=2*(1-pt(abs(t1),n-5)) 
t1 
p.value1 
t2=beta.hat[2]/sqrt(sigma2.hat*solve(t(X)%*%X)[2,2]) 
p.value2=2*(1-pt(abs(t2),n-5)) 
t2 
p.value2 
t3=beta.hat[3]/sqrt(sigma2.hat*solve(t(X)%*%X)[3,3]) 
p.value3=2*(1-pt(abs(t3),n-5)) 
t3 
p.value3 
t4=beta.hat[4]/sqrt(sigma2.hat*solve(t(X)%*%X)[4,4]) 
p.value4=2*(1-pt(abs(t4),n-5)) 
t4 
p.value4 
t5=beta.hat[5]/sqrt(sigma2.hat*solve(t(X)%*%X)[5,5]) 
p.value5=2*(1-pt(abs(t4),n-5)) 
t5 
p.value5 

p.red=1
df.red=n-p.red-1
df.ful=n-p-1
X.red=cbind(rep(1,n),x1,x2,x3)
beta.hat.red=solve(t(X.red)%*%X.red)%*%t(X.red)%*%y
y.hat.red=X.red%*%beta.hat.red
Rss.red=as.numeric(t(y-y.hat.red)%*%(y-y.hat.red))
Rss.ful=as.numeric(t(y-y.hat)%*%(y-y.hat))
F.stat=((Rss.red-Rss.ful)/(df.red-df.ful))/(Rss.ful/df.ful)
pvalue.F=1-pf(F.stat,df.red-df.ful,df.ful)
pvalue.F
F.stat



##2
X=matrix(c(rep(1,n),landrent$X1,landrent$X2,landrent$X3,landrent$X4),nrow=n,ncol=5,byrow=F)
beta.hat=solve(t(X)%*%X)%*%t(X)%*%landrent$Y
sigma2.hat=t(landrent$Y-X%*%beta.hat)%*%(landrent$Y-X%*%beta.hat)/(n-5)
sigma2.hat
beta.hat

sigma.hat=sqrt(sigma2.hat)
sigma.hat

##3

#For slope beta1: 
lower.b=beta.hat[2,1]-qt(1-0.01/2,length(landrent$Y)-2)*0.069
lower.b
upper.b=beta.hat[2,1]+qt(1-0.01/2,length(landrent$Y)-2)*0.069 
upper.b 

#For slope beta2: 
lower.b=beta.hat[3,1]-qt(1-0.01/2,length(landrent$Y)-2)*0.108
lower.b
upper.b=beta.hat[3,1]+qt(1-0.01/2,length(landrent$Y)-2)*0.108 
upper.b 

#For slope beta3: 
lower.b=beta.hat[4,1]-qt(1-0.01/2,length(landrent$Y)-2)*11.89 
lower.b
upper.b=beta.hat[4,1]+qt(1-0.01/2,length(landrent$Y)-2)*11.89 
upper.b 

#For slope beta4: 
lower.b=beta.hat[5,1]-qt(1-0.01/2,length(landrent$Y)-2)*2.84 
lower.b
upper.b=beta.hat[5,1]+qt(1-0.01/2,length(landrent$Y)-2)*2.84 
upper.b 

#5)
n <- dim(landrent)[1]

X <- matrix(c(rep(1,n), landrent$X1, landrent$X2, landrent$X3, (landrent$X2 *landrent$X3)), nrow = n, ncol = 5, byrow=F)
Y <- matrix(c(rep(1,n), landrent$X1, landrent$X2, landrent$X3, (landrent$X2 *landrent$X3)), nrow = n, ncol = 5, byrow=F)

Hat.matrix <- X%*%solve(t(X)%*%X)%*%t(X)
beta.hat <- solve(t(X)%*%X)%*%t(X)%*%Y
Y.hat <- X %*%beta.hat

residuals <- Y - Y.hat
plot(Y.hat, residuals, xlab = "fitted response", ylab= "Residuals")


##5 2nd

n <- dim(landrent)
X <- matrix(c(rep(1,n), landrent$X1, landrent$X2, landrent$X3, (landrent$X2 *landrent$X3)), nrow = n, ncol = 5, byrow=F)
Hat.matrix <- X%*%solve(t(X)%*%X)%*%t(X)
beta.hat <- solve(t(X)%*%X)%*%t(X)%*%Y
Y.hat <- X %*%beta.hat
residuals <- Y - Y.hat
plot(Y.hat, residuals, xlab = "fitted response")


##2nd part,2nd ans

Sk= as.factor(Challeng$fail)
Sh=length(Challeng)
glm(Sk~temp+pres+temp:pres, family-binomial, data-Challeng)
Challeng$fail
my.glm- gIm(Sk~temp+pres+temp:pres, family-binomial, data-Challeng) predictors<-data. frame(temp=31, pres=100)
predict (my.glm, newdata=predictors)



##2nd part 1st question 
library(alr4)
data(Challeng)
Challeng$fail <- Challeng$n - Challeng$fail
Challeng$fail <- as.factor(Challeng$fail)

model <- glm(fail ~ temp + pres + temp:pres, family = binomial(link = "logit"), data = Challeng)
summary(model)

##2nd part 2nd question

predict(model, data.frame(temp = 31, pres = 100), type = "response")

#2nd question part3

model2 <- glm(fail ~ temp + pres, family = binomial(link = "logit"), data = Challeng)
summary(model2)


Y=T[,1]
n=length
p=dim(T)[2]-1
Design.mat=cbind(rep(1,n),T[,2:p])
Hat.mat=Design.mat%%solve(t(Design.mat)%%Design.mat)%*%t(Design.mat)
Beta.hat=solve(t(Design.mat)%%Design.mat)%%t(Design.mat)%*%Y
Y.hat=Design.mat%*%Beta.hat
Residuals=Y-Y.hat
Sigma2.hat=as.numeric(t(Residuals)%*%Residuals/(n-p-1))
Residuals.std=(sqrt(Sigma2.hat))^(-1)*Residuals/sqrt(1-diag(Hat.mat))
Cooks.dist=(p+1)^(-1)Residuals.std^2(diag(Hat.mat))/(1-diag(Hat.mat))
Cooks.dist
