#6.1
###installing all required packages
install.packages("alr4") 

##checking whether the package is installed or not if installed we will be getting promoted with some things
library('alr4')

data(UN11)

NH <- lm(UN11$lifeExpF ~ 1,UN11)
summary(NH)

AH <- lm(UN11$lifeExpF ~ group,UN11)
summary(AH)

##anova
anova(NH,AH)



#b part
##installing libraries
install.packages("alr4") 

##library to import UN11
library('alr4') 

#data fille
data(UN11)

##running linear regression models
s1 <- lm(lifeExpF ~log(ppgdp) + group:log(ppgdp),UN11)
summary(s1)

# running linear regresion model2
r1 <- lm(lifeExpF ~group + log(ppgdp)+ group:log(ppgdp) ,UN11)
summary(r1)

pq = anova(s1,r1)
anova(s1,r1)

summary(pq)
