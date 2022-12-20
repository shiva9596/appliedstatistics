#2.6.1
library("alr4")
data(ftcollinstemp)
attach(ftcollinstemp)
plot(fall,winter,xlab="average temperature in fall",ylab="averagetemperatureinwinter")

#2.6.2
library("alr4")
data(ftcollinstemp)
lm(winter~fall)
plot(fall,winter)
plot(fall,winter,xlab="average temperature in fall",ylab="averagetemperatureinwinter")
abline(lm(winter~fall))

summary(lm(winter~fall))

#2.6.3
library("alr4")
data(ftcollinstemp)
attach(ftcollinstemp)
anova(lm(winter~fall))

#2.6.4
library("alr4")
data(ftcollinstemp)
attach(ftcollinstemp)
data1=ftcollinstemp[year<=1989,]
attach(data1)
summary(lm(winter~fall))
data2=ftcollinstemp[year>=1990,]
attach(data2)
summary(lm(winter~fall))

