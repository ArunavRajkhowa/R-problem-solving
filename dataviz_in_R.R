#this script contains exploring datat Vizualization in R

library(ggplot2)
p=ggplot(mtcars,aes(x=wt,y=mpg)) 
p + geom_point()


#adding a 3rd parameter 'color'. aes: x,y,size,shape,color
#have to change to 'factor' type since discrete values
p=ggplot(mtcars,aes(x=wt,y=mpg,color=factor(vs))) 
p + geom_point(show.legend = T) 


#adding more features in aesthetics
# mtcars$am=as.factor(mtcars$am) -> can also be used to changed to a factor
p=ggplot(mtcars,aes(x=wt,y=mpg,color=factor(vs),size=cyl)) 
p + geom_point(show.legend = T) 


#adding more features in aesthetics
p=ggplot(mtcars,aes(x=wt,y=mpg,size=cyl,shape=factor(vs))) 
p + geom_point(aes(color=factor(am)))
#change the variable positions to get diff aesthetics of plot

#--------------------------
p=ggplot(mtcars,aes(x=wt,y=mpg,color=factor(am)))

p+geom_point()+geom_smooth()+geom_line()
#we see here that there is an overlap of aesthetics . 
#to solve this we map color to geom_point
p=ggplot(mtcars,aes(x=wt,y=mpg))

p+geom_point(aes(color=factor(am),shape=factor(vs)))+geom_smooth()
#----------------------------------------------------
#----------------------------------------------------
library(vcd)
library(hflights)
library(dplyr)
#------------------
## single numeric variable
# histogram
f_sub=filter(hflights,DepDelay<100) #modified dataframe
#----------------------------------
ggplot(f_sub,aes(x=DepDelay))+
  geom_histogram(bins = 200) #change bin size for fine variations
#density plot
ggplot(f_sub,aes(x=DepDelay))+
  geom_density()
#boxplots
ggplot(f_sub,aes(y=DepDelay))+
    geom_boxplot()
IQR(f_sub$DepDelay) 
quantile(f_sub$DepDelay)

# comparing density with normal

ggplot(f_sub,aes(x=DepDelay))+geom_density(color="red")+
  stat_function(fun=dnorm,
                args=list(mean=mean(f_sub$DepDelay),
                          sd=sd(f_sub$DepDelay)),
                color="green")


###### single categorical variable

# f_sub=hflights %>% filter(UniqueCarrier %in% c("UA","AA","XE","WN"))
unique(hflights$UniqueCarrier)
length(unique(hflights$UniqueCarrier))

f_sub=hflights[hflights$UniqueCarrier %in% c("UA","AA","XE","WN"), ]


ggplot(f_sub,aes(x=UniqueCarrier))+
  geom_bar(fill='white',color='blue',width = 0.5)+
  coord_flip()+
  xlab("Carrier Name")+
  ylab(" Frequency")+
  ggtitle("Carrier Frequencies")

ggplot(f_sub,aes(x=UniqueCarrier))+
  geom_bar(fill='white',color='blue',width=0.5)+
  coord_polar()+
  xlab("Carrier Name")+
  ylab(" Frequency")+
  ggtitle("Carrier Frequencies")

ggplot(f_sub,aes(y=UniqueCarrier))+
  geom_bar(aes(fill = factor(DayOfWeek)))+
 coord_polar()+
  xlab("Carrier Name")+
  ylab(" Frequency")+
  ggtitle("Carrier Frequencies") +
  scale_fill_brewer(palette = 'Accent') #modify plot graphics

ggplot(f_sub,aes(x=UniqueCarrier,fill = factor(DayOfWeek)))+
  geom_bar()+
  coord_polar(theta='x')+ #unique carriers along x
  scale_fill_brewer(palette = 'Accent') #modify plot graphics
#these are a hit and trial method till you get a visualization that makes sense + appealing
  



### num-cat visualizations

# how to plot density curves of two categories in the same visualisation

## bar charts without frequency on y axis

df = data.frame(Months=c("Jan","Feb","Mar","Apr"),
                Sales=c(4.2, 10, 29.5,15))

ggplot(df, aes(x=Months, y=Sales)) +
  geom_bar(stat='identity',aes(fill=Months)) + 
scale_x_discrete(limits = month.abb[1:4]) #how to sort by month


ggplot(df, aes(x=Months, y=Sales)) +
  geom_bar(stat='identity',aes(fill=Months))+
  geom_text(aes(label=Sales),vjust=-0.5) + #hjust,angle
  scale_x_discrete(limits = month.abb[1:4])
### how to give a predefined order to values on the x axis ?

ggplot(data=df, aes(x=Months, y=Sales)) +
  geom_bar(stat="identity",aes(color=Sales))+
  geom_text(aes(label=Sales,vjust=Sales/2),size=3.5)

##### bar plots with labels

# Outside bars
ggplot(df,aes(x=Months,y=Sales))+
  geom_bar(stat='identity')

ggplot(df,aes(x=Months,y=Sales))+
  geom_bar(stat='identity',color='blue',fill='white')

ggplot(df,aes(x=Months,y=Sales))+
  geom_bar(stat='identity',color='blue',aes(fill=Months))

# how to make the text labels appear in the middle of the bar

# Change barplot line colors by groups
p=ggplot(df, aes(x=Months, y=Sales, color=Months))+
  geom_bar(stat="identity", fill=c("#999999", "#E69F00", "#56B4E9","#800000"))

p

# Change legend position
p + theme(legend.position="top")
p + theme(legend.position="bottom")


# Remove legend
p + theme(legend.position="none")

# The allowed values for the arguments legend.position are : “left”,“top”, “right”, “bottom”.
# Explore theme function for changing gridlines , background colors

#add trend lines
p=ggplot(df, aes(x=Months, y=Sales, fill=Months)) +
  geom_bar(stat="identity")

p+geom_line(aes(y=Sales),group=1,size=2,color='steelblue')+
  geom_text(aes(label=Sales),vjust=-.5,size=3.5)

### cat-cat
ggplot(Arthritis,aes(x=Treatment,fill=Improved))+
  geom_bar()


