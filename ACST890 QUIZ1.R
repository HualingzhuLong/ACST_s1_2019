#GitHub username:HualingzhuLong
#repositories name:ACST_s1_2019
#file name:ACST890 QUIZ1.R & ACST890 QUIZ1.PDF

#question 1
#c=coupon,n=number of coupon payment,t= time until coupon paid(half year as a period),F= face value,P=price
#The yield of interest is effective semi-annually interest rate respect with tj
#assume the all the value of variables are known except p
p<-function(c,n,F,Yields){
  t=seq(1,n,by=1)
  y<-c(Yields)
  price<-0
  rec<- list(t,y)
  rec
  price=sum(c*exp(-y*t))+F*exp(-rec[[2]][n]*rec[[1]][n])
return(price)
}
#using assuming value of data
answer=p(100,10,1000,c(0.3,0.1,0.2,0.2,0.1,0.05,0.3,0.1,0.15,0.25))
answer

#question 3
#(a)
dataset<- read.csv(file.choose("singapore.economy.csv"), header=T)
dataset
#(b)
dataset1=na.omit(dataset)
dataset1
#(c)
attach(dataset1)
plot(time,gdp,xlab="Time",ylab="GDP(%)",main="Singapore GDP growth")
#(d)
GDP1=dataset1[period=="1","gdp"]
M1<- mean(GDP1)
M1
SD1<- sd(GDP1)
SD1
GDP2=dataset1[period=="2","gdp"]
M2<- mean(GDP2)
M2
SD2<- sd(GDP2)
SD2
GDP3=dataset1[period=="3","gdp"]
M3<-mean(GDP3)
M3
SD3<- sd(GDP3)
SD3
mean<- c(M1,M2,M3)
sd<- c(SD1,SD2,SD3)
period<- c(1,2,3)
stat.table<- data.frame(period,mean,sd)
stat.table
#(e)
pair<- pairs(dataset1[,3:10])
pair
#(f)
SLR.GDP=lm(gdp~exp)
SLR.GDP
summary(SLR.GDP)
#When exp=0, the gdp=1.19032%. An increase of 0.19076% of gdp is associated with the increase of 1 unit exp.
#As p value is small enough, we have strong evidence that we could reject null hypothesis which is beta1=0.
#(g)
MLR.GDP= lm(gdp~exp+epg+hpr+oil+gdpus+crd)
MLR.GDP
summary(MLR.GDP)
#The predictor of exp,epg abd hpr are in siginificant level 1%. The predictor oil,gdpus and crd shows insiginificant at level 1% as p-value is not samll enough.
#multiple R-squared provided that there are 0.372 proportion variability in gdp is explained by linear regression on prosictors.
#the F test show F>1, so that H1 is ture which is at least 1 betaj is non zero.
# the p-value shows that the linear regression function is siginificant as p is small enough.
#(h)
Quan<- quantile(gdp,0.05)
Quan
state<- ifelse(gdp<Quan,"crisis","normal")
state
econ.table<- data.frame(dataset1,state)
econ.table
trainingdata<- subset(econ.table, econ.table$period==1|econ.table$period==2)
trainingdata
testdata <- subset(econ.table, econ.table$period==3)
testdata
logisticstate<- glm(state~bci, data=trainingdata,family=binomial)
logisticstate
prediction<- predict(logisticstate,testdata,type="response")
glmpred <- rep("crisis", 38)
glmpred[prediction>0.5]="normal"
table(glmpred, testdata$state)