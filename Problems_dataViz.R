
setwd("D:/IITK Data Analytics/R/R-problem-solving/")
df=read.csv("census_income.csv",stringsAsFactors = F)
View(df)
class(df)
library(ggplot2)
#-----------------------------------

p=ggplot(df,aes(x=df$hours.per.week,color='red'))
p + geom_density() +   stat_function(fun=dnorm,
                                     args=list(mean=mean(df$hours.per.week),
                                               sd=sd(df$hours.per.week)),
                                     color="green") + theme(legend.position="none")
#------------------------------------

p1= ggplot(df,aes(x=race,fill=Y)) + geom_bar(position = 'fill')
p1 + ylab('Percentage') + theme(axis.text.x = element_text(face="bold", color="#993333",
                                                          size=9, angle=45))

p2= ggplot(df,aes(x=relationship,fill=Y)) + geom_bar(position = 'fill')
p2 + ylab('Percentage') + theme(axis.text.x = element_text(face="bold", color="#993333",
                                                          size=9, angle=45))

#------------------------------------- 
sex=as.factor(df$sex)
p=ggplot(df,aes(x=age,y=hours.per.week)) + geom_point(aes(color=sex))
p + geom_smooth(aes(color=factor_sex))
#---------------------------------------

p1=ggplot(df,aes(x=Y,y=capital.gain,color=Y)) + geom_point()
p1 +geom_jitter()

p2=ggplot(df,aes(x=Y,y=capital.loss,color=Y)) + geom_point()
p2 +geom_jitter()
#---------------------------------------
p1=ggplot(df,aes(x=Y,y=fnlwgt,color=Y,fill=sex)) + geom_violin()
p1 
#-----------------------------------
