# ******************************************************************
# LINEAR MODEL
# ******************************************************************

---
  title: "R Notebook"
output: html_notebook
---
  load("C:/Users/HARPREET/Downloads/R/data_cleaned.RData")
Earhquake<-read.csv('Earthquake_Data_cleaned_01.csv')




#library(rpart)
library(dplyr)
library(ggplot2)
library(dplyr)
library(caTools)
library(rpart.plot)
library(grid)
library(gridExtra)
library(scatterplot3d)
library(ggmap)
library(cowplot)

library(rvest)
library(caret)
library(randomForest)

View(Earthquake)
Earhquake_1<-read.csv('Earthquake_Data_cleaned_01.csv')
Earth_1<-read.csv('Earthquake_Data_cleaned_01.csv')
View(Earth_1)
Co2<-read.csv('Co2_Emission_Cleaned_Dataset.csv')
View(Co2)
country1<-c('India','China','Pakistan','Indonesia','Iran')
View(Co2)



Co2 %>% filter(Co2$entity %in% country1)->Co2_Count1

View(Co2_Count1)

ggplot(data=Co2_Count1, aes(x=entity,y=annual_co2_emissions_zero_filled))+geom_point(color='blue') +ggtitle('Co2 vs Country') +xlab('Country')+ylab('CO2_Emissions')
View(Earth_1)

Earth_1 %>% filter(Earth_1$country=='India')->India_Data
View(India_Data)
Earth_1 %>% filter(Earth_1$country=='Pakistan')->Pakistan_Data
Earth_1 %>% filter(Earth_1$country=='China')->China_Data
Earth_1 %>% filter(Earth_1$country=='Africa')->Africa_Data
Earth_1 %>% filter(Earth_1$country=='Iran')->Iran_Data
#library(caTool)



#India RMSE
sample.split(India_Data$mag , SplitRatio = 0.70)->Split_Tag
subset(India_Data , Split_Tag==T)->Training_India
subset(India_Data , Split_Tag==F)->Testing_India
nrow(Training_India)

nrow(Testing_India)

lm(mag~depth+latitude+longitude , data=Training_India) ->lm_india
lm_india


lm(formula = mag ~ depth + latitude + longitude, data = Training_India)

predict(lm_india ,data=Testing_India)->Predict_india
View(Predict_india)
cbind(Actual=Testing_India$mag , Predicted=Predict_india)->Actual_Predicted_India

cbind(Actual = Testing_India$mag, Predicted = Predict_india)
View(Actual_Predicted_India)
class(Actual_Predicted_India)

as.data.frame(Actual_Predicted_India)->Actual_Predicted_India
class(Actual_Predicted_India)

(Actual_Predicted_India$Predicted - Actual_Predicted_India$Actual)->India_Error
cbind(Actual_Predicted_India,India_Error)->Final_India
View(Final_India)
sqrt(mean(Final_India$India_Error^2))->rmse_india
rmse_india
#China RMSE
sample.split(China_Data$mag , SplitRatio = 0.70)->Split_Tag
subset(China_Data , Split_Tag==T)->Training_China
subset(China_Data , Split_Tag==F)->Testing_China

nrow(Training_China)

#subset(China_Data , Split_Tag==F)-Testing_China
nrow(Testing_China)

lm(mag~depth+latitude+longitude , data=Training_China) ->lm_China
lm_China

predict(lm_China, data=Testing_China)->Predict_China
cbind(Actual=Testing_China$mag , Predicted=Predict_China)->Actual_Predicted_China

View(Actual_Predicted_China)
as.data.frame(Actual_Predicted_China)->Actual_Predicted_China
(Actual_Predicted_China$Predicted - Actual_Predicted_China$Actual)->China_Error
cbind(Actual_Predicted_China,China_Error)->Final_China
View(Final_China)
sqrt(mean((Final_China$China_Error)^2))->rmse_china
rmse_china
#Pakistan RMSE
sample.split(Pakistan_Data$mag , SplitRatio = 0.70)->Split_Tag
subset(Pakistan_Data , Split_Tag==T)->Training_Pakistan
subset(Pakistan_Data , Split_Tag==F)->Testing_Pakistan
nrow(Training_Pakistan)

nrow(Testing_Pakistan)

lm(mag~depth+latitude+longitude , data=Training_Pakistan) ->lm_Pakistan
lm_Pakistan

predict(lm_Pakistan, data=Testing_Pakistan)->Predict_Pakistan
cbind(Actual=Testing_Pakistan$mag , Predicted=Predict_Pakistan)->Actual_Predicted_Pakistan

View(Actual_Predicted_Pakistan)
as.data.frame(Actual_Predicted_Pakistan)->Actual_Predicted_Pakistan
(Actual_Predicted_Pakistan$Predicted - Actual_Predicted_Pakistan$Actual)->Pakistan_Error
cbind(Actual_Predicted_Pakistan,Pakistan_Error)->Final_Pakistan
View(Final_Pakistan)
sqrt(mean(Final_Pakistan$Pakistan_Error^2))-rmse_Pakistan
rmse_Pakistan
#Africa RMSE
#sample.split(Africa_Data$mag , SplitRatio = 0.70)-Split_Tag
#sample.split(Africa_Data$mag , SplitRatio = 0.70)-Split_Tag1

#sample.split(Africa_Data$mag , SplitRatio = 0.70)-Split_Tag

#sample.split(Africa_Data ,SplitRatio = 0.60)-Split_Africa
#subset(Africa_Data , Split_Tag==T)->Training_Africa
# subset(Africa_Data , Split_Tag==F)->Testing_Africa
#nrow(Training_Africa)

#nrow(Testing_Africa)

#lm(mag~depth+latitude+longitude , data=Training_Africa) -lm_Africa

#View(Training_Africa)
#View(Africa_Data)

View(Iran_Data)
sample.split(Iran_Data ,SplitRatio = 0.70)->Split_Iran
subset(Iran_Data , Split_Iran==T)->Training_Iran
subset(Iran_Data , Split_Iran==F)->Testing_Iran
lm(mag~depth+latitude+longitude , data=Training_Iran) ->lm_Iran
lm_Iran
nrow(Testing_Iran)
View(Testing_Iran)
View(Training_Iran)

predict(lm_Iran, data=Testing_Iran)->Predict_Iran
cbind(Actual=Testing_Iran$mag , Predicted=Predict_Iran)->Actual_Predicted_Iran
View(Actual_Predicted_Iran)
as.data.frame(Actual_Predicted_Iran)->Actual_Predicted_Iran
(Actual_Predicted_Iran$Predicted - Actual_Predicted_Iran$Actual)->Iran_Error
cbind(Actual_Predicted_Iran,Iran_Error)->Final_Iran
sqrt(mean(Final_Iran$Iran_Error^2))->rmse_Iran
rmse_Iran

#Afganistan Data

# country1<-c('India','China','Pakistan','Afghanistan','South Africa')
#Co2 %>% Filter(Co2$entity %in% country1)-Country_Co2

#Co2 %>% filter(Co2$entity %in% country1)->Country_Co2
#ggplot(data=Country_Co2 ,aes(x=entity, y=annual_co2_emissions_zero_filled
#))+geom_point(color="Red")
#Earth_1 %>% filter(Earth_1$country=='Afghanistan')->Afghan_Data
# View(Afghan_Data)
# sample.split(Afghan_Data$mag , SplitRatio = 0.70)->Split_Tag
# subset(Afghan_Data , Split_Tag==T)->Training_Afghan
# subset(Afghan_Data , Split_Tag==F)->Testing_Afghan
# lm(mag~depth+latitude+longitude , data=Training_Afghan) ->lm_Afghan
# lm_Afghan


#predict(lm_Afghan, data=Testing_Afghan)->Predict_Afghan
#cbind(Actual=Testing_Afghan$mag , Predicted=Predict_Afghan)->Actual_Predicted_Afghan


#View(Actual_Predicted_Afghan)
#as.data.frame(Actual_Predicted_Afghan)->Actual_Predicted_Afghan
#(Actual_Predicted_Afghan$Predicted - Actual_Predicted_Afghan$Actual)->Afghan_Error
# cbind(Actual_Predicted_Afghan,Afghan_Error)->Final_Afghan
#View(Final_Afghan)
#sqrt(mean(Final_Afghan$Afghan_Error^2))->rmse_Afghan
#rmse_Afghan

ggplot(data=Country_Co2 ,aes(x=entity, y=annual_co2_emissions_zero_filled
))+geom_point(color="Red")


#country2<-c('India','China','Pakistan','Afghanistan','South Africa')
#Co2 %% Filter(Co2$entity %in% country2)-Country_Co2_1

#Co2 %>% filter(Co2$entity %in% country2)-Country_Co2_1
# ggplot(data=Country_Co2_1 ,aes(x=entity, y=annual_co2_emissions_zero_filled ))+geom_point(color="Red")
#ggplot(data=Country_Co2_1 ,aes(x=annual_co2_emissions_zero_filled, y=entity ))+geom_point(color="Red")
#country2<-c('India','China','Pakistan','Vietnam','South Africa')
#Co2 %>% Filter(Co2$entity %in% country2)-Country_Co2_1

# Co2 %>% filter(Co2$entity %in% country2)->Country_Co2_1
# ggplot(data=Country_Co2_1 ,aes(x=entity, y=annual_co2_emissions_zero_filled ))+geom_point(color="Red")
# Earth_1 %% filter(Earth_1$country=='Vietnam')->Vietnam_Data
#View(Vietnam_Data)

#Indonesia Data
Earth_1 %>% filter(Earth_1$country=='Indonesia')->Indonesia_Data
# View(Indonesia_Data)
#sample.split(Indonesia_Data$mag , SplitRatio = 0.70)->Split_Tag
# subset(Vietnam_Data , Split_Tag==T)->Training_Indonesia
subset(Vietnam_Data , Split_Tag==F)->Testing_Indonesia
## lm(mag~depth+latitude+longitude , data=Training_Indonesia) ->lm_Indonesia
# lm_Indonesia

# nrow(Training_Indonesia)

# nrow(Testing_Indonesia)

# predict(lm_Indonesia, data=Testing_Indonesia)->Predict_Indonesia
## cbind(Actual=Testing_Indonesia$mag , Predicted=Predict_Indonesia)->Actual_Predicted_Indonesia
# View(Actual_Predicted_Indonesia)
# View(Testing_Indonesia)
# sample.split(Indonesia_Data$mag , SplitRatio = 0.65)->Split_Tag1
# subset(Vietnam_Data , Split_Tag==T)-Training_Indonesia
View(Training_Indonesia)
# sample.split(Indonesia_Data$mag , SplitRatio = 0.55)->Split_Tag2
#Split_Tag2
#subset(Vietnam_Data , Split_Tag2==T)-Training_Indonesia
View(Training_Indonesia)

#sample.split(data=Indonesia_Data,SplitRatio = 2/3)-split_indo


sample.split(data=Indonesia_Data,SplitRatio = 0.66)->split_indo

sample.split(Indonesia_Data$mag,SplitRatio = 0.66)->split_indo
subset(Indonesia_Data , split_indo==T)->Training_Indo
View(Training_Indo)
subset(Indonesia_Data , split_indo==F)->Test_Indo
lm(mag~depth+latitude+longitude , data=Training_Indo) ->lm_Indo
lm_Indo

predict(lm_Indo, data=Testing_Indo)->Predict_Indo
View(Predict_Indo)
# cbind(Actual=Testing_Indo$mag , Predicted=Predict_Indo)->Actual_Predicted_Indo

cbind(Actual=Test_Indo$mag , Predicted=Predict_Indo)->Actual_Predicted_Indo

View(Actual_Predicted_Indo)
as.data.frame(Actual_Predicted_Indo)->Actual_Predicted_Indo
(Actual_Predicted_Indo$Predicted - Actual_Predicted_Indo$Actual)->Indo_Error
cbind(Actual_Predicted_Indo,Indo_Error)->Final_Indo
View(Final_Indo)
sqrt(mean((Final_Indo$Indo_Error)^2))->rmse_Indonesia
rmse_Indonesia

sqrt(mean((Final_Indo$Indo_Error)^2))-rmse_Indo
rmse_Indo

country5<-c('India','China','Pakistan','Indonesia','South Africa')
Co2 %>% filter(Co2$entity %in% country5)->ountry_Co2_11
#ggplot(data=Country_Co2_11 ,aes(x=entity, y=annual_co2_emissions_zero_filled ))+geom_point(color="Red")

ggplot(data=Country_Co2_11 ,aes(x=annual_co2_emissions_zero_filled, y=entity ))+geom_point(color="Red")

#sqrt(mean((Final_Pakistan$Pakistan_Error)^2))->rmse_Pakistan
#rmse_Pakistan

sqrt(mean((Final_Pakistan$Pakistan_Error)^2))->rmse_Pak
rmse_Pak

sqrt(mean((Final_Indo$Indo_Error)^2))->rmse_Indo1
rmse_Indo1

sqrt(mean((Final_India$India_Error)^2))->rmse_India1
rmse_India1


sqrt(mean((Final_China$China_Error)^2))->c_rmse
c_rmse





data<-read.csv('Earthquake_Data_cleaned_01.csv')
# view('Earthquake_Data_cleaned_01.csv')

View(data)
tree<-rpart(mag~depth+latitude+longitude , data)
a<-data.frame(depth=c(10),latitude=c(29.9548),longitude=c(-113.8311))
result<-predict(tree,a)
result

print(result)

rpart.plot(tree)
tree1<-rpart(mag~depth+latitude+longitude , data)
a1<-data.frame(depth=c(480.00),latitude=c(-19.8113),longitude=c(-177.6750))
result1<-predict(tree1,a)
result1


View(Co2)

View(Co2)

count1<-read.csv('Earthquake_Data_cleaned_01.csv')

country1<-c('India','Pakistan','China','Indonesia','South Africa')
country1
count1 %>% filter(count1$country %in% country1)->plot_count1

ggplot(data=plot_count1 , aes(x=country, y=mag))+geom_point()

View(Co2_Country)
Co2 %>% filter(Co2$entity %in% country1)->Plot2_count2

ggplot(data=plot_count1 , aes(x=country, y=mag))+geom_point()
ggplot(data=plot2_count2 , aes(x=entity, y=))+geom_line()

View(Plot2_count2)

#plotting a graph between the countries we have taken the example and there respective year

earth_country<-c('India','China','Pakistan','Indonesia','Iran')
earth_country
View(Earth_1)
Earth_1 %>% filter(Earth_1$country %in% earth_country)->Train_Country
ggplot(data = Train_Country ,aes(x=mag,y=time))+geom_point()
ggplot(data = Train_Country ,aes(x=time))+geom_bar()
ggplot(data = Train_Country ,aes(x=time))+geom_histogram()


#co2 plotting a graphn between years 2017-2021

View(Co2)

Co2 %>% filter(Co2$entity %in% earth_country)->Co2_Country
View(Co2_Country)
year1=c(2017,2018,2019,2020,2021)
Co2_Country %>% filter(Co2_Country$year %in% year1)->Co2_Year

ggplot(data=Co2_Country ,aes(x=annual_co2_emissions_zero_filled,y=year))+geom_point()

ggplot(data=Co2_Year ,aes(x=annual_co2_emissions_zero_filled,y=year))+geom_point()


#visualization 
qplot(mag, data = Earth_1, bins = 92)

#qplot(mag, data = earthquake_all, bins = 92)

#scatterplotMatrix(~mag + depth, data = Earth_1, spread = FALSE, 
#diagonal = "histogram", lty = 1, main = "Scatterplot Matrix of Mag and Depth")


#plot11<-ggplot(Train_Country ,aes(x=country,y=mag))+geom_point()+ggtitle('plot1')
#plot2<-ggplot(Co2_Country , aes(x=entity, y=annual_co2_emissions_zero_filled))+geom_point()
#plot(data=Train_Country ,Train_Country$mag , Train_Country$country)
#plot(Co2_Country, col = "Black", ylab="annual_co2_emissions_zero_filled", xlab="entity")
##plot11
#ggplot() +
#geom_point(data = Train_Country, aes(x = mag, y = country), color = "blue") + # must include argument label "data"
#geom_line(data = Co2_Country, aes(x =annual_co2_emissions_zero_filled, y =entity),color='red')

p1<-ggplot(data = Train_Country,aes(x=country , y=mag))+geom_point(size=1,color='red')+ggtitle('Earthquake Vs Country')+xlab(
  'Country') +ylab('Earthquake(Mag)')
p1

p2<-ggplot(data = Co2_Country,aes(x=entity , y=annual_co2_emissions_zero_filled))+geom_point(size=1,color='green')+ggtitle('CO2 Vs Country')+xlab('Country')+ylab('CO2_Emission')
p2

grid.newpage()
#layout view
push.viewport(viewport(layout = grid.layout(1,2)))

#put the chart into that area

print(p1 , vp=viewport(layout.pos.row = 1 , layout.pos.col = 1))
print(p2 , vp=viewport(layout.pos.row = 1 , layout.pos.col = 2))


citation("gridExtra")

grid.arrange(p1,p2,nrow=1,ncol=2)


ab<-ggplot(data=Train_Country , aes(x=depth+latitude+longitude, y=mag)) +geom_point()
mag<-Train_Country$depth
latitude<-Train_Country$latitude
longitude<-Train_Country$longitude
ab


scatterplot3d(mag,latitude,longitude ,pch=16,highlight.3d = TRUE)

summary(lm_india)
ggplot(data=Train_Country , aes(x=depth+latitude+longitude, y=mag)) +geom_point()
abline(h =6.1) 

View(Train_Country$time)





mymap<-map_data("world")
pl<-ggplot() +geom_polygon(data=mymap , aes(x=long,y=lat, group=group))

pl<-pl+geom_point(data=Train_Country ,aes(x=longitude,y=latitude),color="red")
pl<pl + theme_minimal()
pl


ggplot(data=Train_Country ,aes(x=mag,y=country))+geom_point()
ggplot(data=Train_Country ,aes(x=mag,y=country))+geom_bar()

View(Train_Country)

p1<-ggplot(data= Train_Country ,aes(x=depth+longitude+latitude,y=mag))+geom_point(color='green')+ggtitle('Magnitude VS Location')+xlab('Depth_Lat_long')+ylab('Magnitude')

#I<-lm(mag~depth+latitude+longitude, data=India_Data)
##coef=coefficients(I)
#coef
#eq = paste0("y = ", round(coef[2],1), "*x ", round(coef[1],1))

#plot(India_Data)
#abline(I, col="blue")

View(India_Data)
coefficients(lm_india)
summary(lm_india)
coefficients(lm_China)
summary(lm_China)
summary(lm_Indo)
summary(lm_Pakistan)
summary(lm_SAfrica)
summary(lm_Iran)

###########
rmse_china
rmse_india
rmse_Indo
rmse_Pakistan
rmse_Iran

year1<-c(1750,1780,1800,1850,1900,1950,1970,2000,2002,2005,2010,2015,2017,2019,2020)
Co2_Country %>% filter(Co2_Country$entity=='China')->China_Co2
China_Co2 %>% filter(China_Co2$year %in% year1)->China_Co2_Year

Co2_Country %>% filter(Co2_Country$entity=='India')->India_Co2
India_Co2 %>% filter(India_Co2$year %in% year1)->India_Co2_Year

Co2_Country %>% filter(Co2_Country$entity=='Pakistan')->Pakistan_Co2
Pakistan_Co2 %>% filter(Pakistan_Co2$year %in% year1)->Pakistan_Co2_Year

#

View(Co2_Country)
Co2_Country %>% filter(Co2_Country$entity=='Indonesia')->Indo_Co2_Year

View(Indo_Co2_Year)
Indo_Co2_Year %>% filter(Indo_Co2_Year$year %in% year1)->Indo_Co2_Year1

View(Indo_Co2_Year)
##

Co2_Country %>% filter(Co2_Country$entity=='Iran')->Iran_Co2
Iran_Co2 %>% filter(Iran_Co2$year %in% year1)->Iran_Co2_Year

View(China_Co2_Year)

a1<-ggplot(data=China_Co2_Year ,aes(x=year,y=annual_co2_emissions_zero_filled,fill=year))+geom_bar(colour="black", stat="identity")+ggtitle('China_CO2 Vs Year') +xlab('Year')+ylab('CO2_Emission')
b1<-ggplot(data=India_Co2_Year ,aes(x=year,y=annual_co2_emissions_zero_filled,fill=year))+geom_bar(colour="green", stat="identity")+ggtitle('India_CO2 Vs Year') +xlab('Year')+ylab('CO2_Emission')
c1<-ggplot(data=Pakistan_Co2_Year ,aes(x=year,y=annual_co2_emissions_zero_filled,fill=year))+geom_bar(colour="blue", stat="identity")+ggtitle('Pakistan_CO2 Vs Year') +xlab('Year')+ylab('CO2_Emission')
d1<-ggplot(data=Indo_Co2_Year1 ,aes(x=year,y=annual_co2_emissions_zero_filled,fill=year))+geom_bar(colour="red", stat="identity")+ggtitle('Indonesia_CO2 Vs Year') +xlab('Year')+ylab('CO2_Emission')
e1<-ggplot(data=Iran_Co2_Year ,aes(x=year,y=annual_co2_emissions_zero_filled,fill=year))+geom_bar(colour="red", stat="identity")+ggtitle('Iran_CO2 Vs Year') +xlab('Year')+ylab('CO2_Emission')

citation("gridExtra")

grid.arrange(a1,b1,c1,d1,e1,nrow=3,ncol=2)




lm_india
lm_China
lm_Indo
lm_Pakistan
lm_Iran


fit<-lm(mag~latitude, data=Train_Country)
plot(Train_Country$latitude , Train_Country$mag)

abline(fit)





# ******************************************************************
# LOGISTIC MODEL
# ******************************************************************


# Reading the csv file "Earthquake_Data_cleaned_01 (1).csv"

Earthquake <- read.csv("Earthquake_Data_cleaned_01 (1).csv")

print(head(Earthquake))

# str(), gives the structure of the file
str(Earthquake)

# for logistic regression the dependent variable should return in "YES" or "NO" format
# So considering earthquake below 5 as as minor and above 5 as major earthquake
# Changing Magnitude,Latitude,Longitude
Earthquake$mag <- ifelse(test=Earthquake$mag<5, yes='Minor', no='Major')
Earthquake$mag <- as.factor(Earthquake$mag)



# library(dplyr) for selecting pipe filter

library(dplyr)
IC<-c('India','China','Pakistan','Indonesia')
Earthquake %>% filter(Earthquake$country %in% IC)->IC_Earth
View(Earthquake)
View(IC_Earth)
str(IC_Earth)



xtabs(~ mag +country, data=IC_Earth)

# mag     China India Indonesia Pakistan
# Major    49    35       390        8
# Minor   185   148      1224       33


logistic <- glm(mag~ country, data=IC_Earth, family ="binomial")
summary(logistic)


ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null

1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))



predicted.data <- data.frame(
  probability.of.mag=logistic$fitted.values,
  country=IC_Earth$country)

predicted.data<- predicted.data[
  order(predicted.data$probability.of.mag, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

library(ggplot2)
install.packages("cowplot")
library(cowplot)

ggplot(data=predicted.data, aes(x=probability.of.mag, y=country)) +
  geom_point(aes(color=country), size=5) +
  xlab("mag") +
  ylab("country")









# ******************************************************************
# RANDOM FOREST
# ******************************************************************

# Reading the CSV file
data <- read.csv("Earthquake_Data_cleaned_01 (1).csv")


#Library for Random Forest



#Library for Prediction and Confusion Matrix



# ******************************************************************
# Data processing
# Updating the magnitude parameter, || Setting Minor for magnitude less than 5 and Major the the reverse
data$mag <- ifelse(test = data$mag < 5, yes = "Minor", no = "Major")
# Changing magnitude to factor
data$mag <- as.factor(data$mag)


#Filtering the unwanted data
data$mag_type <- NULL
data$time <- NULL
data$net <- NULL
data$nst <- NULL
data$id <- NULL
data$updated <- NULL
data$place <- NULL
data$type <- NULL
data$status <- NULL
data$mag_source <- NULL
data$country <- NULL


# Checking any NA present in the data
data[!complete.cases(data),]


#Eliminating all the rows with NA values
data <- na.omit(data)


# Showing the Magnitude column in a table format
table(data$mag)


# Data Partition
set.seed(123)
partition <- sample(2, nrow(data), replace = TRUE, prob = c(0.7,0.3))

train <- data[partition == 1,]
test <- data[partition == 2,]


# Random Forest
set.seed(222)
rf <- randomForest(mag ~ depth+latitude+longitude, data = train)

print(rf)

#Prints all the attributes that the Random Forest consists of.
attributes(rf)


# Prediction & Confusion Matrix - train data
p_1 <- predict(rf, train)

confusionMatrix(p_1, train$mag)


# Prediction & Confusion Matrix - test data
p_2 <- predict(rf, test)

confusionMatrix(p_2, test$mag)


#Plotting Random Forest
# Error rate of Random Forest
plot(rf, main = "Error Rate")

####
##
model = randomForest(mag ~ depth+latitude+longitude, data=train, ntree=500, proximity=T)
print(model)
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(model, log="y", main = 'RF Error')
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)
##
####


# No.of nodes for the trees
hist(treesize(rf), 
     main = "No.of nodes for the trees",
     col = "5")


# Variable Importance, to plot the variable importance graph
varImpPlot(rf, main = "Variable Importance")
# The graph measures how pure the nodes are at the end of the tree without each variable



# ******************************************************************
# Decision Tree
# ******************************************************************

# Decision Tree Implementation



library(rpart)


library(rpart.plot)
data<-read.csv('Earthquake_Data_cleaned_01.csv')
# view('Earthquake_Data_cleaned_01.csv')

View(data)
tree<-rpart(mag~depth+latitude+longitude , Train_Country)
a<-data.frame(depth=c(10),latitude=c(29.9548),longitude=c(-113.8311))
result<-predict(tree,a)
result

print(result)

rpart.plot(tree )
tree1<-rpart(mag~depth+latitude+longitude , Train_Country)
a1<-data.frame(depth=c(480.00),latitude=c(-19.8113),longitude=c(-177.6750))
result1<-predict(tree1,a)
result1

predict_unseen2 <-predict(tree1, Train_Country)
View(predict_unseen2)

table_mat1 <- table(Train_Country$mag, predict_unseen1)
View(table_mat1)

accuracy_Test1 <- sum(diag(table_mat1)) / sum(table_mat1)

print(paste('Accuracy for test', accuracy_Test1))

#Co2 Accuracy in decision Tree


View(data)
tree<-rpart(entity~annual_co2_emissions_zero_filled , Co2_Country)
a<-data.frame(entity('India'))
result<-predict(tree,a)
result

print(result)

rpart.plot(tree )

predict_unseen <-predict(tree, Co2_Country)
View(predict_unseen)

table_mat <- table(Co2_Country, predict_unseen)
View(table_mat)

accuracy_Test <- sum(diag(table_mat)) /sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))



# ******************************************************************
# UNSUPERVISED
# ******************************************************************

#install.packages("ggplot2")
install.packages("anytime")
install.packages("leaflet")
install.packages("stats")
install.packages("corrplot")
library(anytime)
library(leaflet)
library(stats)
library(corrplot)

#wd<-"~/R/workplace"

#data <- read.csv(
#  file=paste0(wd,"Earthquake_Data_cleaned_01.csv"),
#  stringsAsFactors = FALSE, header=TRUE, row.names=1
#)
data <- read.csv("Earthquake_Data_cleaned_01 (1).csv")
View(data)


#Unsupervised learning

#Geographical clusstering of earthqaukes on leaflet map

```{r data}
data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude,~latitude,clusterOptions =  markerClusterOptions(maxClusterRadius=40))
```
#Plotting the exact locations of earthquakes
```{r presure, echo=FALSE}
data %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(~longitude,~latitude,radius = 7,color = "red")
```


#Clustering on geographic data
geo_data <- data[,c("latitude","longitude")]

set.seed(1234)
data_kmeans <- kmeans(geo_data, centers=50)
centers <- data.frame(data_kmeans$centers)

#Plotting the centers of k-means clusterring
```{r presure, echo=FALSE}
centers %>%
  leaflet() %>%
  addTiles() %>%
  addCircles(~longitude,~latitude,radius = 8,color = "blue")
```



#K-means identifying distance values of K

rang<-2:50 #K from 2 to 20
runs <-100 #Run the K Means algorithm 100 times
avrgs <-integer(length(rang)) #Set up an empty vector to hold all of points
for(v in rang){ # For each value of the range variable
  withinss <-integer(runs) #Set up an empty vector to hold the 100 tries
  for(i in 1:runs){
    k.temp <-kmeans(geo_data,centers=v) #Run kmeans
    withinss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avrgs[v-1] <-mean(withinss) #Average the 100 total withinss
}

plot(rang,avrgs,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")


# ***********************************************************************************************
