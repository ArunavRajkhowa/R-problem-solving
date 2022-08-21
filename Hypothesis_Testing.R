
setwd("D:\\IITK Data Analytics\\R\\R-problem-solving")
wq=read.csv("D:\\IITK Data Analytics\\R\\R-problem-solving\\census_income.csv")


#Problem1-------------------------------------
# Are capital.gain and capital.loss really different ?
  # select an appropriate hypothesis test, explain your conclusion.

#gain or loss are dependent on each other and hence we can use the paired t test

t.test(wq$capital.gain,wq$capital.loss,paired = TRUE)
library(ggplot2)
ggplot(wq,aes(x=capital.gain,y=capital.loss))+geom_point()


#Problem2-----------------------------------------
# Find out if hours.per.week are significantly different across genders , income levels.
# select appropriate hypothesis tests, explain your conclusions.

sex_m=wq$hours.per.week[wq$sex==' Male']
sex_f=wq$hours.per.week[wq$sex==' Female']
income_less=wq$hours.per.week[wq$Y==' <=50K']
income_more=wq$hours.per.week[wq$Y==' >50K']

var.test(sex_m,sex_f)
t.test(sex_m,sex_f,paired = FALSE,var.equal = F)
ggplot(wq,aes(x=sex,y=hours.per.week))+geom_boxplot()

var.test(income_less,income_more)
t.test(income_less,income_more,paired = FALSE,var.equal = F)
ggplot(wq,aes(x=Y,y=hours.per.week))+geom_boxplot()


#Problem3-----------------------------------------------
# Intuitively it seems that education levels should be different across workclasses 
# , examine whether this is true using your data. Also report which classes have education levels 
# siginificantly higher/lower in comparison to others ; using an additional test. 
# [variables involved : education.num and workclass].
# select appropriate hypothesis tests, explain your conclusions.


fit = aov(education.num ~ workclass ,data=wq)
summary(fit) #check p value
pairwise.t.test(wq$education.num, wq$workclass) #to get more idea acorss classes

ggplot(wq,aes(workclass,education.num)) + geom_boxplot(aes(fill=workclass)) +theme(axis.text.x=element_blank())



#Problem4--------------------------------------------------
# Find if income levels are affected by race.
# select appropriate hypothesis tests, explain your conclusions
chisq.test(table(wq$Y,wq$race))
round(prop.table(table(wq$race,wq$Y),1),2)
ggplot(wq,aes(x=race,fill=Y))+
  geom_bar(position = "fill")+
  ylab("Percentage")+
  theme(axis.text.x = element_text(face="bold", color="#993333",size=9, angle=45))


