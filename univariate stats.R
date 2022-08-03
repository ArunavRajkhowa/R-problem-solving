setwd("D:/IITK Data Analytics/R/R-problem-solving/")
df=read.csv("census_income.csv",stringsAsFactors = F)
View(df)
class(df)

#problem1
library(psych)
describe(df[,c(1,3,5,11:13)])


#problem2
for (i in 1:ncol(df)){
  
  if (class(df[,i])=='character'){  #looking for categorical value
    print (paste('Summary of :',names(df)[i]))  
    print(table(df[,i])) #to get the frequency
  }
}


#problem 3 cross table variable education & Y
tab1 = xtabs(~education+Y,data=df)
round(prop.table(tab1,1),2)

#problem 4  Plot histogram for variables fnlwgt and education.num.
hist(df$fnlwgt,breaks = "Sturges")
hist(df$education.num,breaks = "Sturges")



#problem5
#Find q1,q2 and IQR values for these variables and use 
# following limits to
#report number of outliers according to 
#each variable : [q1 - 1.5IQR, q3 + 1.5IQR ].
#HINT : Use function “quantile” to find q1 and q3 
#which are nothing but 25 and 75 percentiles of the data.

outliers=function(x){
  
  nu = mean(x)
  std = sd(x)
  #l2 = nu - 3 * std #doesnt give accurate results bcz not symmetric
  #u2 = nu + 3 * std
  #limits=c(l2,u2)
  
  q1 = quantile(x)[2]
  q3 = quantile(x)[4]
  iqr = IQR(x)
  l1 = q1 - 1.5 * iqr
  u1 = q3 + 1.5 * iqr
  limits=c(l1,u1)
  names(limits)=NULL
  return (limits)
}

print("Outlier Limits For fnlwgt are :")
outliers(df$fnlwgt)
n1 = outliers(df$fnlwgt)
print ('Number of outliers according to these limits for fnlwgt:')
sum(df$fnlwgt<n1[1] | df$fnlwgt>n1[2])


print("Outlier Limits For education.num are :")
outliers(df$education.num)
n2 = outliers(df$education.num)
print ('Number of outliers according to these limits for education.num:')
sum(df$education.num<n2[1] | df$education.num>n2[2])