getwd()
A2<-read.csv("Numerical-data.csv")
View(A2)
 # Number of missing per row#
rowSums(is.na(A2))
 # Number of missing per column/variable#
colSums(is.na(A2))
##### list wise deletion of missing values###
A3<-na.omit(A2)
View(A3)
#####replace 25.0 with 24.0 in density#
A2[A2$density==25.0 ,"density"] <- 24.0
#####replace Na with mean in income###

A2$Mean.income.Rs[is.na(A2$Mean.income.Rs)] <- mean(A2$Mean.income.Rs, na.rm = TRUE)  
View(A2)
#####replace ,na with 0###
A2$literacy[is.na(A2$literacy)] <- 0   
View(A2)
colSums(is.na(A2))
###replace na with median###
A2$gdp.Rs[is.na(A2$gdp.Rs)] <-median(A2$gdp.Rs, na.rm = TRUE)
  View(A2)
colSums(is.na(A2))
###central tendency###
mean(A2$literacy)
median(A2$density)
require(modeest)
mfv(A2$Mean.income.Rs)


mean <- tapply(A2$gdp.Rs,A2$Mean.income.Rs, mean) 
sd <- tapply(A2$gdp.Rs,A2$Mean.income.Rs, sd)
median <- tapply(A2$gdp.Rs,A2$Mean.income.Rs, median)
max <- tapply(A2$gdp.Rs,A2$Mean.income.Rs, max)
m<-cbind(mean, median, sd, max)
View(m)

summary(A2)

install.packages("pastecs")
library(pastecs)
stat.desc(A2)
stat.desc(A2[,c("literacy","gdp.Rs","Mean.income.Rs")])
stat.desc(A2[,c("literacy","gdp.Rs","Mean.income.Rs")], basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)

####Example-1#######t-test-one sided#####
Ex<-A2[1:15,]
View(Ex)

###### https://www.omnicalculator.com/math/e-power-x ##


###The average literacy level of all the coumtries in the world is greater than 45%.#######
#####mu=45####
###H0:mu<=45##
####Ha:mu>45###

t1<-t.test(Ex$literacy , mu = 45, alternative = "greater")
t1

#####e^-6=0.00248###

#######Your-turn####
H<-read.csv("hblevels.csv")
H
######mu=13####
###H0:mu>=13##
####Ha:mu<13###
mean(H$Hb.levels)
sd(H$Hb.levels)
t2<-t.test(H$Hb.levels, mu = 13, alternative = "less",paired=FALSE)
t2

######Example-2###
###H0:There is no association between gdp and literacy####
####Ha:There is association between gdp and literacy###
X1<-read.csv("Categorical-data.csv")
View(X1)

Ch<-chisq.test(x=X1$Reliability ,y=X1$Workaholism ,p=rep(1/2012,2012))
print(Ch)
View(Ch$observed)
View(Ch$expected)

######As####

##########Your-turn######
###H0:There is no association between smoking and drinking in pregnant women####
###Ha:There is  association between smoking and drinking in pregnant women####
C<-read.csv("Day 3.csv")
View(C)
Ch1<-chisq.test(x=C$Smoker,y=C$Alcohal.consumption ,p=rep(1/1105,1105))
print(Ch1)
Ch1$observed
Ch1$expected

#############p<0.05 we reject null hypothesis###
####Example-3#######Z-test-one sided#####
Ex3<-A2[45:100,]
View(Ex3)
qqnorm(Ex3$gdp.Rs)
qqline
###The average population density of all the coumtries in the world is lesser than 15.#######
#####mu=15###
###H0:mu>=15##
####Ha:mu<15###

require(BSDA)
ZT<-rnorm(200,mean=5551,sd=23)
View(ZT)
qqnorm(ZT)
qqline(ZT)

Z1<-z.test(ZT , y = NULL, alternative = "less", mu = 5555,sigma.x = 5, conf.level = 0.95)
Z1
#####p>0.05 so we reject the null hypothesis####

####Ho:mu==170####
#####Ha:mu!=170###
ZT1<-rnorm(49,mean=160,sd=56)
View(ZT1)
mean(ZT1)
sd(ZT1)
qqnorm(ZT1)
qqline(ZT1)
Z2<-z.test(ZT1, y = NULL, alternative = "two.sided", mu = 170, sigma.x =17 ,
       sigma.y = NULL, conf.level = 0.95)
Z2

#######H0<0.05 so we reject null hypothesis###

######H0:mu1==m2####
#####Ha:mu1!=m2###
T3<-read.csv("PairedTtest.csv")
T3
T3test<-t.test(x=T3$Garage.1, y=T3$Garage.2, paired = TRUE, alternative = "two.sided")
T3test
###p<0.05 so we reject null hypothesis###


######H0:mu1==m2####
#####Ha:mu1!=m2###
T4test<-t.test(x=T3$Health.education , y=T3$No.Health.education ,  var.equal = TRUE, )
T4test
sd(T3$Health.education)
sd(T3$No.Health.education)
####p<0.05 so we reject Null hypothesis##