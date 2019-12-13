library(alr4)
head(UN11)
lifeexp = UN11$lifeExpF
head(lifeexp)
lifeexp.r = round(lifeexp, 0)
length(which(lifeexp.r==80))
lifeexp.r



all=length(lifeexp.r)
all
p1=length(which(lifeexp.r<=80))
p2=length(which(lifeexp.r==75))
p3.1=length(which(lifeexp.r<=69))
p3.1
p3.2=length(which(lifeexp.r<=65))
p3.2
p3=p3.1-p3.2
p3

P1=p1/all
P2=p2/all
P3=p3/all
P1
P2
P3
head(UN11)
lifeexp
plot.ecdf(lifeexp)
pdf(lifeexp)
plot(pdf(lifeexp))
plot(density(lifeexp))
plot(lifeexp)

head(UN11)
Fertility=UN11$fertility
fertility.r=round(Fertility)
fertility.r
lifeexp
head(lifeexp)
mean(lifeexp)
var(lifeexp)

range(fertility.r)
p=fertility.r/7
p
weighted.mean(lifeexp,p)
head(p)
weighted.mean(p,lifeexp)
var(fertility.r,lifeexp)
var(lifeexp,fertility.r)
head(UN11)
ppgdp1=UN11$ppgdp
head(ppgdp1)
plot(x=ppgdp1,y=Fertility)

abline(lm(Fertility~ppgdp1))
summary(lm(Fertility~ppgdp1))

plot(x=log(ppgdp1),y=log(Fertility))
abline(lm(log(Fertility)~log(ppgdp1)))
summary(lm(log(Fertility)~log(ppgdp1)))

plot(x=log(ppgdp1),y=log(Fertility),base=2)
plot(x=log(ppgdp1,base=100),y=log(Fertility,base=100))
abline(lm(log(Fertility,base=100)~log(ppgdp1,base=100)))

wblake
head(wblake)

data(UN11)
data(UN11,Fertility=1)
print(data)
print(UN11)
UN11
match(Fertility,UN11,1)
UN11
a=data.frame(UN11,Fertility)
a
head(a)
data.frame(UN11$fertility,lifeexp.r)
AA=data.frame(UN11$lifeExpF,fertility.r)
head(AA)
E1=AA[which.names("1",AA$fertility.r),]
mean(E1$UN11.lifeExpF)
var(E1$UN11.lifeExpF)
E2=AA[which.names("2",AA$fertility.r),]
E3=AA[which.names("3",AA$fertility.r),]
E4=AA[which.names("4",AA$fertility.r),]
E5=AA[which.names("5",AA$fertility.r),]
E6=AA[which.names("6",AA$fertility.r),]
E7=AA[which.names("7",AA$fertility.r),]
E1
E2
E3
mean(E1,E2)
mean(E1$UN11.lifeExpF)
mean(E2$UN11.lifeExpF)
mean(E3$UN11.lifeExpF)
mean(E4$UN11.lifeExpF)
mean(E5$UN11.lifeExpF)
mean(E6$UN11.lifeExpF)
mean(E7$UN11.lifeExpF)

var(E1$UN11.lifeExpF)
var(E2$UN11.lifeExpF)
var(E3$UN11.lifeExpF)
var(E4$UN11.lifeExpF)
var(E5$UN11.lifeExpF)
var(E6$UN11.lifeExpF)
var(E7$UN11.lifeExpF)
E7


sum(E1,E2,E3,E4,E5,E6,E7)
head(wblake)


tapply(wblake$Length,wblake$Age,mean)
tapply(wblake$Length,wblake$Age,var)


Ave.Length=tapply(wblake$Length,wblake$Age,mean)
Ave.Length

plot(Ave.Length,xlab="Age")
AAge=c(1:8)
AAge
lines(1:8,Ave.Length, lty=2)

abline(lm(Ave.Length~AAge))

Var1=tapply(wblake$Length,wblake$Age,var)
Var1


plot(Var1,xlab = "Age", ylab = "Standar Deviations")
lines(1:8,Var1,lty=2)
abline(lm(Var1~AAge))


rm(list=ls())
