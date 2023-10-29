LifeEx_Metro<- read.csv("Life Expectancy at Birth-IL_Metro.csv",header=TRUE)
LifeEx_NonMetro <- read.csv("Life Expectancy at Birth-IL_NonMetro.csv",header=TRUE)
LifeEx_Long<-read.csv("Life Expectancy at Birth-IL_LongYears.csv")
LifeEx_Combined<-read.csv("Life Expectancy at Birth -IL-Combined.csv",header=TRUE)
#Checked differences, they are normal

x= LifeEx_Metro$value
y= LifeEx_NonMetro$value

library(ggplot2)
LifeEx_Long$MorN<-as.factor(LifeEx_Long$MorN)
is.factor(LifeEx_Long$MorN)
ggplot(data=LifeEx_Long,aes(x=Value,y=MorN))+geom_boxplot()
#can tell mean is higher for metro, higher variance in metro
boxplot(x~Metro,LifeEx_Metro$value)

m = length(x)
n = length(y)

sp = sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2) / (m+n-2))
t.stat = (mean(x) - mean(y))/(sp*sqrt(1/m + 1/n))



t.stat
tstatistic = function(x, y){
  m=length(x)
  n=length(y)
  sp=sqrt(((m-1)*sd(x)^2 + (n-1)*sd(y)^2)/(m+n-2))
  t.stat = (mean(x) - mean(y))/(sp*sqrt(1/m +1/n))
  return(t.stat)
}

tstatistic(x, y)
alpha=0.05
abs(t.stat) > qt(1-alpha/2, n+m-2)

#Since that is true, t statistic is more extreme than t critical value,
#reject null (lifeEx_Metro=LifeExNonMetropolitan area). They are different

#Mean and Standard Deviation


# Calculate the mean

LifeEx_Metro_Mean <- mean(x)

LifeEx_NonMetro_Mean <- mean(y)

# Calculate the Standard Deviation

LifeEx_Metro_Sd <- sd(x)

LifeEx_NonMetro_Sd <- sd(y)

# Print Results

print(LifeEx_NonMetro_Mean)

print(LifeEx_Metro_Mean)

print(LifeEx_NonMetro_Sd)

print(LifeEx_Metro_Sd)

# Prepare data
value <- c("Mean", "Standard Deviation")
Life_Metro <- c("78.76963", "1.452757")
Life_NonMetro <- c("78.00187","1.186667")

# Create a data frame
table_data <- data.frame(Value = value, Metro = Life_Metro, NonMetro = Life_NonMetro)

# Print the table
print(table_data)

#Run a regression on x and y

model<-lm(Value~MorN,data=LifeEx_Long)
summary(model)
#P value shows that the average value is signifigantly different
#for every unit increase in the nonmetro (1) the life expectancy
#is expected to decrease by 0.7678 in comparison to those in metro
#We expect the average of nonmetro to be lower than metro

#F-statistic >1 then it is a good model because it explains more
#of the systematic variance than the nonsystematic variance
#(variance that we cant explain)
#since p-val<.05, we can use our model
#model explains about 7.8% of the variablitity of outcome in data
#That percent could be higher if we had higher sample size

#can adjust by year,

model<-lm(Value~MorN + X,data=LifeEx_Long)

#see if we see that the NorM variable is signifigant
#Factor in Obesity if we can
