##9.3.1
install.packages("alr4")
library("alr4")
data <- pipeline
str(data)
head(pipeline)
x = pipeline$Field
y = pipeline$Lab
plot(x,y, xlab = "Field", ylab = "Lab", main =" Scatterplot of Lab versus Field")

model <- lm(y~x)
summary(model)


##9.3.2 
install.packages("alr4")
library("alr4")
p1<-lm(pipeline$Lab~pipeline$Field,pipeline) 
summary(p1)
residualPlot(p1)
ncvTest(p1)

abline(p1,col="blue") 
abline(0,1,lty=2)
summary()
residualPlot(p1)
ncv.test(p1)

##
t1<-lm(pipeline$Lab~pipeline$Field,pipeline) 
abline(t1,col="blue") 
abline(0,1,lty=2)




##9.11 code part

install.packages("alr4")
library("alr4")
data("fuel2001")

Fuel=fuel2001$FuelC/fuel2001$Pop*1000
Dlic=fuel2001$Drivers/fuel2001$Pop*1000
log.Miles=log(fuel2001$Miles)
Income=fuel2001$Income/1000
datas=cbind(Fuel,fuel2001$Tax,Dlic,Income,log.Miles)
Y=datas[,1]
n=length(Y)
p=dim(datas)[2]-1
design.mat=cbind(rep(1,n),datas[,2:p])
Hat.mat=design.mat%*%solve(t(design.mat)%*%design.mat)%*%t(design.mat)
beta.hat=solve(t(design.mat)%*%design.mat)%*%t(design.mat)%*%Y
Y.hat=design.mat%*%beta.hat
residuals=Y-Y.hat
sigma2.hat=as.numeric(t(residuals)%*%residuals/(n-p-1))
residuals.std=(sqrt(sigma2.hat))^(-1)*residuals/sqrt(1-diag(Hat.mat))
Cooks.dist=(p+1)^(-1)*residuals.std^2*(diag(Hat.mat))/(1-diag(Hat.mat))
Cooks.dist

infuential=which(Cooks.dist>(4/n))
infuential

##9.19
data("drugcost")
head(drugcost)
colnames(drugcost)
pairs(drugcost)
summary(pairs)


##r code for cookes 
##9.11
e=c(-163.145,-137.599,-102.409,183.499,-49.452)
h=c(0.256,0.162,0.206,0.084,0.415)
y=c(514.279,374.164,426.349,842.792,317.492)
s=e%*%e
D=as.array(NA)
for(i in 1: length(y)){
D[i]=e[i]^2/(s*4)*h[i]*(1-h[i]^2)
}
D



##Studentized residual formula is given by :
  
t=as.array(NA)
sigma_hat=64.891
t[i]=e[i]/(sigma_hat* sqrt(1-h[i]))
for(i in 1: length(y)){
t[i]=e[i]/(sigma_hat* sqrt(1-h[i]))

}
for(i in 1: length(y)){
t[i]=e[i]/(sigma_hat* sqrt(1-h[i]))
}
t
