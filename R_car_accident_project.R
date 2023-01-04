#1: Introduction to Data Set 

#The number and types of motor vehicle crash deaths differ widely among the 50 states 
#and the District of Columbia in the U.S. Fatality rates per capita and per vehicle miles travelled 
#provide a way of examining motor vehicle deaths relative to the population and amount of driving. 
#However, many factors can affect these rates, including types of vehicles driven, 
#single or multiple vehicle collision, whether the passengers were wearing a seatbelt or not 
#(restrained fatally injured occupants versus unrestrained fatally injured occupants), 
#whether the accident occurred in an urban or rural area


#read in the data
car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)
#check data
car_crash_data

head(car_crash_data)#check first six rows
tail(car_crash_data)#check last six rows


sum(is.na(car_crash_data))#check for missing values
which(is.na(car_crash_data))#integer zero - there are some 0 values in the dataset - this is not an error
#check the amount of rows has been imported correctly and later for validating correct no of rows and cols after modification of model
nrow(car_crash_data)
ncol(car_crash_data)


#first I will check model and correlations for all observations and variables 
#and look to adjust model after based on relation between variables and possible redundancy of other variables

#2: Data Analysis (a) Descriptive data analysis
str(car_crash_data)#gives structure of the data set- what cols are called and what values are


#str function to check the structure of the data - currently there 20 int variables and 1 character
summary(car_crash_data)#summary of data for all variables
boxplot(car_crash_data$Deaths.per.100.000.population)#response variable is normally distributed
hist(car_crash_data$Deaths.per.100.000.population)
boxplot(car_crash_data$Population)#Population variable is not normally distributed

#(b)i correlations 
#cor func
round(cor(car_crash_data[,2:21]),2)#cor function to look for correlations between variables combined with 
##round to 2 decimal places. Fatal crashes and deaths are perfectly correlated when rounded to 2 decimal places. 
##There is also a very high degree of correlation between these two variables and car occupants variable
##because of this I have decided to drop them for my final model.
##highest correlation of the dependent variable - deaths per 100,000 population is with Deaths.per.100.million.vehicle.miles.traveled - model 3 I looked at removing these entries as 7 is small sample size

pairs(car_crash_data[,2:21])#See appendix - visual representation of correlations between the variables Pick up and SUV are 


#ii. Graphical summaries:

# (a)boxplot
boxplot(car_crash_data$Population)#look to drop as outliers and dependent var. is deaths per capita
boxplot(car_crash_data$Vehicle.miles.traveled..millions.)#.16 correlation with Deaths.per.100.million.vehicle.miles.traveled and .11 correlation with Deaths.per.100.000.population
boxplot(car_crash_data$Fatal.crashes)
boxplot(car_crash_data$Deaths)
boxplot(car_crash_data$Deaths.per.100.000.population)
boxplot(car_crash_data$Deaths.per.100.million.vehicle.miles.traveled)
boxplot(car_crash_data$Car.occupants)
boxplot(car_crash_data$Pickup.and.SUV.occupants)
boxplot(car_crash_data$Large.truck.occupants)
boxplot(car_crash_data$Motorcyclists)
boxplot(car_crash_data$Pedestrians)
boxplot(car_crash_data$Bicyclists)
boxplot(car_crash_data$Unknown.mode.of.transport)
boxplot(car_crash_data$Single.vehicle)
boxplot(car_crash_data$Multiple.vehicle)
boxplot(car_crash_data$Unrestrained.fatally.injured.occupants)
boxplot(car_crash_data$Restrained.fatally.injured.occupants)
boxplot(car_crash_data$Unknown.restraint.status.of.fatally.injured.occupants)
boxplot(car_crash_data$Urban)
boxplot(car_crash_data$Rural)

#none of the dependent variables are normally distributed
hist(car_crash_data$Population)#look to drop as outliers and dependent var. is deaths per capita
hist(car_crash_data$Vehicle.miles.traveled..millions.)#.16 correlation with Deaths.per.100.million.vehicle.miles.traveled and .11 correlation with Deaths.per.100.000.population
hist(car_crash_data$Fatal.crashes)
hist(car_crash_data$Deaths)
hist(car_crash_data$Deaths.per.100.000.population)
hist(car_crash_data$Deaths.per.100.million.vehicle.miles.traveled)
hist(car_crash_data$Car.occupants)
hist(car_crash_data$Pickup.and.SUV.occupants)
hist(car_crash_data$Large.truck.occupants)
hist(car_crash_data$Motorcyclists)
hist(car_crash_data$Pedestrians)
hist(car_crash_data$Bicyclists)
hist(car_crash_data$Unknown.mode.of.transport)
hist(car_crash_data$Single.vehicle)
hist(car_crash_data$Multiple.vehicle)
hist(car_crash_data$Unrestrained.fatally.injured.occupants)
hist(car_crash_data$Restrained.fatally.injured.occupants)
hist(car_crash_data$Unknown.restraint.status.of.fatally.injured.occupants)
hist(car_crash_data$Urban)
hist(car_crash_data$Rural)









#(b)

hist(car_crash_data$Deaths.per.100.000.population)#response variable is normally distributed

#(c)Scatterplots
plot(Carcrashmodel)#see appendix
#outliers rows 9,35,44 and 51
car_crash_data[51,]#state with largest Cook's distance- Texas - possibly as Texas is a highly populous state with large urban and rural areas.
#Washington D.C., North Dakota and Wyoming are also outliers. DC is a small State in terms of area - it registers zero in terms of pick up and suv and large truck deaths.
#(b) Determine the Best Predictive
#Model Describe results from: - first I ran the model with all variables and observations included
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:21])#linear model including all rows and variables
Carcrashmodel
summary(Carcrashmodel)#summary of the model - most significant t value is population ie. as population increases deaths per 100,000 decrease -3.265
anova(Carcrashmodel)#F- values significantly different from zero are - population, Vehicle.miles.traveled..millions. , fatal crashes, Deaths.per.100.million.vehicle.miles.traveled and Single.vehicle
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC of 269.3467

#(c)Perform analysis of the best regression
#model test 1
car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)
car_crash_data <- car_crash_data[,-c(2,3,4,5)]#drop cols population, Vehicle.miles.traveled..millions., fatal crashes and deaths
car_crash_data <- car_crash_data[-c(9,35,44,51),]#also drop outlier states Texas, Washington DC, North Dakota and Wyoming. Outlier State's taken from (c)Scatterplots - plot(Carcrashmodel)#see appendix
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:17])
summary(Carcrashmodel)#adjusted r squared increased to .6987 meaning nearly 70% is explained by the new model ie dropping highly correlated variables and the outlier states improved the fit of the model
anova(Carcrashmodel)
summary.aov(Carcrashmodel)#significant F value for Pickup.and.SUV.occupants - usually used more in rural areas
AIC(Carcrashmodel)#AIC dropped to 228.5519
#dropping the columns that I have dropped based on Cook's distance brings the r squared to nearly 70% and the aic to 228. This model is a better overall fit


#model test 2 - dropping population outlier rows
car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)
hist(car_crash_data$Population)#not normally distributed but when I dropped the columns, the rows for these states remained potentially distorting the data
boxplot((car_crash_data$Population))#big population outliers. I decided to sort data by population - highest to lowest

boxplot(car_crash_data$Deaths.per.100.000.population)
hist(car_crash_data$Deaths.per.100.000.population)

car_crash_data <- car_crash_data[order(car_crash_data$Population,decreasing = TRUE),]#sort population highest to lowest
#The top 4 states are big outliers. I decided to drop the first four rows and last four rows which will hopefully give me more trimmed means. 

car_crash_data <- car_crash_data[-c(1,2,3,4,48,49,50,51),]#drop the top 4 and bottom 4 population centres
boxplot((car_crash_data$Population))
hist((car_crash_data$Population))#still not normally distributed - population variable is dropped below
boxplot(car_crash_data$Deaths.per.100.000.population)
hist(car_crash_data$Deaths.per.100.000.population)
nrow(car_crash_data)
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:21])
summary(Carcrashmodel)#adjusted r squared val of .7194
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC dropped to 208.9896- thus removing the population outliers has meant a better model overall fit
car_crash_data <- car_crash_data[,-c(2,3,4,5)]#drop previous multicollinearity columns
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:17])
summary(Carcrashmodel)#r-squared value of .6496
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC 217.431
#model 1 I only had to remove 4 rows rather than 8 here. I also feel that the rows/States removed were targeted better
plot(Carcrashmodel)#Cook's distance reveals 2 states - Hawaii and South Carolina
car_crash_data[c(19,36),1]
nrow(car_crash_data)



#model test 3 - drop val 2 - deaths per millions of kms travelled
car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)
car_crash_data <- car_crash_data[order(car_crash_data$Deaths.per.100.million.vehicle.miles.traveled,decreasing =  TRUE),]
head(car_crash_data)#check data has been sorted correctly
car_crash_data <- car_crash_data[-c(1:7),]#drop the first 7 rows as these are the only states with Deaths.per.100.million.vehicle.miles.traveled
car_crash_data <- car_crash_data[order(car_crash_data$Population,decreasing = TRUE),]#sort the population from highest to lowest
head(car_crash_data)
nrow(car_crash_data)
#car_crash_data <- car_crash_data[-c(1,2,3,4,41,42,43,44),]#drop the top and bottom 4 states by population

car_crash_data <- car_crash_data[,-c(2,3,4,5)]#drop cols population, Vehicle.miles.traveled..millions., fatal crashes and deaths
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:17])

summary(Carcrashmodel)#adjusted r-squared brought to .4301
anova(Carcrashmodel)
summary.aov(Carcrashmodel)#high significance value of 25.811 for Pickup.and.SUV.occupants
AIC(Carcrashmodel)#AIC 180.0354, lowest AIC value that I was able to obtain with my models
plot(Carcrashmodel)#Cook's distance shows South Dakota is an outlier for this model
car_crash_data[36,]
nrow(car_crash_data) 
car_crash_data <- car_crash_data[-36,]#Remove South Dakota as it was distorting the model per Cook's distanc measure




#model test 4 - create new column - as factor of Deaths per 100 million vehicle miles traveled

car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)#import the data
car_crash_data$veh.miles.tr.asfactor <- as.factor(car_crash_data[,7])#create new variable - factor of the column Deaths per 100 million vehicle miles traveled
levels(car_crash_data$veh.miles.tr.asfactor)
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:22])
summary(Carcrashmodel)
car_crash_data <- car_crash_data[,-c(2,3,4,5)]
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:18])
summary(Carcrashmodel)
anova(Carcrashmodel)#high F-value for Pick up and SUV's
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#High AIC value of 281.5396
#####Test for hypothesis code##################

###Hypotheses test using variable veh.miles.tr.asfactor 
###to determine Is there a difference in Deaths.per.100.000.population  with Vehicle.miles.traveled..millions.
###using the variable veh.miles.tr.asfactor.


###variances are the same so proceed to pooled standard deviation test



### for the purposes of naming the variables 1 as factor is ccm.urban and 2 as factor is ccm.rural
ccm.urban <- car_crash_data[car_crash_data$veh.miles.tr.asfactor == 1,]$Deaths.per.100.000.population

ccm.rural <- car_crash_data[car_crash_data$veh.miles.tr.asfactor == 2,]$Deaths.per.100.000.population
car_crash_data$Vehicle.miles.traveled..millions.
var.test(ccm.urban, ccm.rural)
### p value > than significance value so cannot reject null hypotheses

var(ccm.rural)
var(ccm.urban)

var(ccm.rural)/var(ccm.urban)
var(ccm.urban)/var(ccm.rural)

t.test(ccm.rural, ccm.urban, var.equal = TRUE)
t.test(Deaths.per.100.000.population ~ veh.miles.tr.asfactor, data = car_crash_data, var.equal = TRUE)
### p value is less than signifcance level 3.368e-08 so I conclude that the amount of miles travelled in millions - level 2 - affects the deaths per 100,000

#####Test for hypothesis code########################






#model 5 - import new column based on State area
car_crash_data <- read.csv(file.choose(), sep = ",", header =  TRUE)
#Brought in new col to include area of State in square miles
car_crash_data$Area.in.square.miles_deaths.peronehundthoupop <- (car_crash_data[,6]/car_crash_data[,22])#create new variable
hist(car_crash_data$Area.in.square.miles_deaths.peronehundthoupop)
boxplot(car_crash_data$Area.in.square.miles_deaths.peronehundthoupop)#boxplot for this variable shows massive outliers
car_crash_data <- car_crash_data[order(car_crash_data$Area.in.square.miles_deaths.peronehundthoupop,decreasing = TRUE),]#sort new column from largest to smallest

head(car_crash_data)#seems to show States with smaller area DC, Delaware have a higher deathsperonehundredthousand to area ratio
tail(car_crash_data)#Texas, Alaska and California - States with the biggest area have the lowest deathsperonehundredthousand to area ratio
car_crash_data[1:2,1]#top 2 states are D.C and Delaware
ncol(car_crash_data)
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:22])#linear model including all rows and variables
Carcrashmodel
summary(Carcrashmodel)#summary of the model - most significant t value is population ie. as population increase deaths per 100,000 decrease -3.265
anova(Carcrashmodel)#F- values significantly different from zero are - population, Vehicle.miles.traveled..millions. , fatal crashes, Deaths.per.100.million.vehicle.miles.traveled and Single.vehicle
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC of 264.4311
car_crash_data <- car_crash_data[,-c(2,3,4,5)]#drop cols due to previously discussed multicollinearity
Carcrashmodel = lm(Deaths.per.100.000.population ~ ., car_crash_data[,2:18])
summary(Carcrashmodel)
anova(Carcrashmodel)#Again high F-value for pick up and SUV's
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC of 277.7482 - has not improved the overall fit of the model however in my opinion has enabled the finding of significant outliers in Washington DC and Delaware
plot(Carcrashmodel)#States 2, 10 and 44 are outliers in this model
car_crash_data[44,1]#Oregon
car_crash_data[10,1]#South Carolina
car_crash_data[2,1]#Delaware
car_crash_data <- car_crash_data[-c(2,10,44),]
summary(Carcrashmodel)#adjusted r squared brought to .5189 
anova(Carcrashmodel)
summary.aov(Carcrashmodel)
AIC(Carcrashmodel)#AIC of 281.5396


