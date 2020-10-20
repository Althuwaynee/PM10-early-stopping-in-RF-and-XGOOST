##################################################.
## Project: PM 10 prediction using machine learning supervised classification techniques.
## Script purpose:Random forest machine (RF) in CARET package
## Date: 28 Feb. 2019
## Author: Omar AlThuwaynee
##################################################.

# Disclaimer:
#            As with most of my R posts, I've arranged the functions using ideas from other people that are much more clever than me. 
#            I've simply converted these ideas into a useful form in R.
# References

# https://www.guru99.com/r-random-forest-tutorial.html


# 1  Load R packages----
.libPaths("G:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")

library(caret)        # machine laerning
#install.packages("randomForest")
library(randomForest) # Random Forest
library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # ploting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # ploting
library(sp)           # spatial data
library(doParallel)   # Parallel processing



# 2 Data prepration ---------------------------------------------------------
# Go to URL of local folder and select and Copy. (H:\IKC\International projects\Kirkuk air pollution\Data\pm10)


# Go to URL of local folder and select and Copy.(H:\Projects\Kirkuk\WorkingDir)
path=readClipboard()
setwd(path)
getwd() # for checking
.libPaths("H:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")
#.libPaths("C:/Users/user/Desktop/WorkingDIR/libraryfake")
#.libPaths()
sessionInfo()
#installed.packages()
#"aish"="Hawaaaa"

# Import training and testing data ----
list.files( pattern = "csv$", full.names = TRUE)
original <-  read.csv("./stat1_June.csv", header = T,stringsAsFactors = FALSE)
original1 <-  read.csv("./stat2_June.csv", header = T,stringsAsFactors = FALSE)
original2 <-  read.csv("./stat3_June.csv", header = T,stringsAsFactors = FALSE)
original= cbind(original,original1, original2)
names(original)
rm(original1,original2)

####################
original_noPM=original[,-1]
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

original %>%
  gather(-LevelAve, key = "var", value = "value") %>%
ggplot(aes(x=value, y=LevelAve))+geom_point()+facet_wrap(~ var, scales="free")+ theme_bw()




# Load the Raster data
list.files( "./LST_ave",pattern = "tif$", full.names = TRUE)
LST = raster("./LST_ave/mean_17_19_LST_june.tif",epsg=32638 )

list.files( "./SAVI_ave",pattern = "tif$", full.names = TRUE)
SAVI <-  raster("./SAVI_ave/mean_june_SAVI.tif" ) 

list.files( "./BU_ave",pattern = "tif$", full.names = TRUE)
BU= raster("./BU_ave/mean_june_BU.tif" ) 

list.files( "./NDMI_ave",pattern = "tif$", full.names = TRUE)
NDMI = raster("./NDMI_ave/mean_june_NDMI.tif" )

list.files( "./NDVI",pattern = "tif$", full.names = TRUE)
NDVI = raster("./NDVI/mean_june_NDVI.tif" )

list.files( "./Dis to Landuses",pattern = "tif$", full.names = TRUE)
DisToRoads = raster("./Dis to Landuses/Dis_To_Roads.tif" )

list.files( "./Dis to Landuses",pattern = "tif$", full.names = TRUE)
DisToPetrol = raster("./Dis to Landuses/Dis_To_PetrolWells.tif" )

list.files( "./Dis to Landuses",pattern = "tif$", full.names = TRUE)
DisToindust = raster("./Dis to Landuses/Dis_To_otherIndust.tif" )

list.files( "./Dis to Landuses",pattern = "tif$", full.names = TRUE)
DisToflare = raster("./Dis to Landuses/Dis_To_flare.tif" )

list.files( "./Dis to Landuses",pattern = "tif$", full.names = TRUE)
DisToPump = raster("./Dis to Landuses/Dis_To_PPumpStations.tif" )

list.files( "./B2_ave",pattern = "tif$", full.names = TRUE)
band2= raster("./B2_ave/mean_june_band_2.tif")
#names(band2..MEAN)="band2"

list.files( "./B3_ave",pattern = "tif$", full.names = TRUE)
band3= raster("./B3_ave/mean_june_band_3.tif") 

list.files( "./B4_ave",pattern = "tif$", full.names = TRUE)
band4= raster("./B4_ave/mean_june_band_4.tif" ) 
names(band4)= "band4"
list.files( "./B5_ave",pattern = "tif$", full.names = TRUE)
band5= raster("./B5_ave/mean_june_band_5.tif" ) 

list.files( "./B6_ave",pattern = "tif$", full.names = TRUE)
band6= raster("./B6_ave/mean_june_band_6.tif" ) 

list.files( "./B7_ave",pattern = "tif$", full.names = TRUE)
band7 = raster("./B7_ave/mean_june_band_7.tif" )

# stackoverflow.com/questions/32252294/changing-the-extent-and-resolution-of-raster-layers-in-r-for-successful-stacking

DisToRoads <- projectRaster(DisToRoads, band5) # match the extent to 
#choose a raster that has 
#the projection and extent that you 
# want and use that as a template for the others
DisToflare <- projectRaster(DisToflare, band5)
DisToindust <- projectRaster(DisToindust, band5)
DisToPetrol <- projectRaster(DisToPetrol, band5)
DisToPump <- projectRaster(DisToPump, band5)


Rasters= stack(NDMI, SAVI, BU,LST,NDVI,DisToRoads,DisToflare,DisToindust,DisToPetrol,DisToPump,band2,band3,band4,band5,band6,band7)
names(Rasters)

names(Rasters)<-c("NDMI","SAVI" , "BU", "LST","NDVI","DisToRoads","DisToflare", "DisToindust", "DisToPetrol", "DisToPump",  
                  "band2" , "band3","band4","band5","band6","band7" )
rm(NDMI,SAVI , BU, LST,NDVI,DisToRoads,DisToflare, DisToindust,DisToPetrol,DisToPump,  
   band2 , band3,band4,band5,band6,band7)


# If you want to re-scale the shapfile
Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")
#Study_area2 = gBuffer(Study_area, width = -8000) # scale polygon to cover new extent
#class(Study_area2)=class(Study_area)
#Study_area2@data=Study_area@data

#extent(NDMI)
#NDMI <- mask(crop(NDMI, Study_area), Study_area)

## Training and testing data prepration
# June max value for 2017,2018,2019
original= original[,c(4,10:14,24:28,38:43)]
#names(original)
# July max value for 2017,2018,2019
#original= original[,c(13,17:21,38:42,59:64)]
names(original)
# Aug max value for 2017,2018,2019
#original= original[,c(15,17:21,38:42,59:64)]
#names(original)
original <-(na.omit(original))
original <-data.frame(original)  # to remove the unwelcomed attributes

# rearrange follow: "LevelAve" ,"NDMI","SAVI","BU","LST","NDVI","DisToRoads","DisToflare", "DisToindust","DisToPetrol", "DisToPump",
#"band2","band3", "band4","band5","band6","band7"
original= original[c(1,4,6,3,2,5,9,10,11,8,7,12:17)] # re-arrange
head(original)

#Step 2: Data Cleaning
names(original) 
# exclude ,,
names(original) <- c("LevelAve" ,"NDMI","SAVI","BU","LST","NDVI","DisToRoads","DisToflare", "DisToindust","DisToPetrol", "DisToPump", "band2","band3", "band4","band5","band6","band7") # remove unnecessary variables
names(original)
str(original)
original$LevelAve=as.factor(original$LevelAve)
summary(original)

#Step 3: Data Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

names(original)
original.n <- as.data.frame(lapply(original[,2:17], normalize)) # Keep the "LevelAve" variables since it the response variable that needs to be predicted.                     


##### to predict which variable would be the best one for splitting the Decision Tree, plot a graph that represents the split for each of the 9 variables, ####

#Creating seperate dataframe for '"LevelAve" features which is our target.
number.perfect.splits <- apply(X=original.n, MARGIN = 2, FUN = function(col){
  t <- table(original$LevelAve,col)
  sum(t == 0)})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
tiff("Number of perfect splits vs feature.tiff",res = 100) #save as tiff
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,main="Number of perfect splits vs feature",xlab="",ylab="Feature",las=3,col="lightblue",cex.names=0.9 ) # SAVI and LST are the best classifiers
#dev.off()



######Step 4: Data Splicing----
set.seed(123)
data.d <- sample(1:nrow(original),size=nrow(original)*0.65,replace = FALSE) #random selection of 65% data.
train.data <- original.n[data.d,] # 65% training data
test.data <- original.n[-data.d,] # remaining 35% test data

#Creating seperate dataframe for '"LevelAve" features which is our target.
train.data_labels <- original[data.d,1]
test.data_labels <-original[-data.d,1]
train.data$PM=train.data_labels
original.n$PM=original$LevelAve

##################################
# Step 1) Default settings

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

#Let's try the build the model with the default values.


print("Processing starts here #############################")
set.seed(1234)
# Run the model
fit.RF_train <- train(PM~., 
                     data=train.data,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)
# Print the results
print(fit.RF_train)     
plot(fit.RF_train, main="fit.RF_train")


fit.RF_train$finalModel         # Results mtry=5 Number of trees: 500
fit.RF_train$results 

#  The algorithm uses 500 trees and tested three different values of mtry: 2, 5, 9.
# The final value used for the model was mtry = 5 with an accuracy of 0.8212617  Kappa =0.7318892. Let's try to get a higher score.

# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest

#  ntree
#       Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.

#   mtry
#       Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)

# maxnodes
#        Maximum number of terminal nodes trees in the forest can have. 
#        If not given, trees are grown to the maximum possible (subject to limits by nodesize) [ YOU CAN SKIP THE FOLLOWING STEP]

# Modeling delay test ----------------------------------------
# Delay test : A comparison of run times for fastest method:
# Use the Elapsed (sec.)
Delay= fit.RF_train$times

fit.RF_train$resample
fit.RF_train$modelInfo
X.xgb = varImp(fit.RF_train)

#Confusion Matrix - train data
p1<-predict(fit.RF_train, test.data, type = "raw")
confusionMatrix(p1, as.factor(test.data_labels))  # using more deep tree, the accuracy linearly increases! 
z=as.data.frame.list(confusionMatrix(p1, as.factor(test.data_labels))$overall)  # using more deep tree, the accuracy linearly increases! 

##### Now you can skip to the Loop
(names(train.data))
(Delay)
(confusionMatrix(p1, as.factor(test.data_labels)))  # using more deep tree, the accuracy linearly increases! 
(plot(X.xgb, main= "Var. importance RF Training data"))

# which model reach more than 80% accuracy with lower number of variables
# discuss the shape of ROC with diffrent pattern (rigid or smooth)
# discuss the overfitting of first variables, like DistoFlares.

library(pROC)
# the model is used to predict the test data. However, you should ask for type="prob" here
predictions <- as.data.frame(predict(fit.RF_train, test.data, type = "prob"))
##  Since you have probabilities, use them to get the most-likely class.
# predict class and then attach test class
predictions$predict <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
predictions$observed <- test.data_labels
head(predictions)

#    Now, let's see how to plot the ROC curves. For each class, convert the multi-class problem into a binary problem. Also, 
#    call the roc() function specifying 2 arguments: i) observed classes and ii) class probability (instead of predicted class).
# 1 ROC curve,  VUnheal, Good, UHealSesn vs non  VUnheal non Good non UHealSesn

#tiff("XGBoost June ROC.tiff", height = 180, width = 180, units = 'mm', res = 90) #save as tiff
par(mfrow=c(2,2))
roc.Good <- roc(ifelse(predictions$observed=="Good", "Good", "non-Good"), as.numeric(predictions$Good))
roc.Moderate <- roc(ifelse(predictions$observed== "Moderate", "Moderate", "non-Moderate"), as.numeric(predictions$Moderate))
roc.Haz <- roc(ifelse(predictions$observed=="Haz", "Haz", "non-Haz"), as.numeric(predictions$Haz))

plot(roc.Moderate, col = "#FF9900", main="All levels prediction of June PM10 data", cex.main = 0.8, cex.lab=0.8)
lines(roc.Haz, col = "red")
lines(roc.Good, col = "green")
legend("bottomright",c("Haz","Moderate","Good"),fill=c("red","#FF9900","green"), cex = 0.7)

###########################################################
#To get confidence intervals for the Area Under the Curve:

#https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
library(pROC)

roc.Good <- roc(ifelse(predictions$observed=="Good", "Good", "non-Good"), as.numeric(predictions$Good),smoothed = TRUE,ci=TRUE, 
                ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE,main="Good level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
sens.ci <- ci.se(roc.Good)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

roc.Haz <- roc(ifelse(predictions$observed=="Haz", "Haz", "non-Haz"), as.numeric(predictions$Haz),smoothed = TRUE,ci=TRUE, 
                     ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                     print.auc=TRUE, show.thres=TRUE,main="Haz level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
sens.ci <- ci.se(roc.Haz)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

roc.Moderate <- roc(ifelse(predictions$observed=="Moderate", "Moderate", "non-Moderate"), as.numeric(predictions$Moderate),smoothed = TRUE,ci=TRUE, 
                    ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                    print.auc=TRUE, show.thres=TRUE,main="Moderate level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
sens.ci <- ci.se(roc.Moderate)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

pdf("plot_June_RF.pdf")
print("############################# Output starts here and follow######################")
for (i in 1:16) {
  if (!z$Accuracy > 0.80) {
    next
  }
  
  set.seed(849)
  
  # Using all data
  fit.RF_train <- train(PM~., 
                        data=train.data,
                        method = "rf",
                        metric = "Accuracy",
                        trControl = trControl)
  

  # Modeling delay test ----------------------------------------
  
  # Delay test : A comparison of run times for fastest method:
  # Use the Elapsed (sec.)
  Delay= fit.RF_train$times
  
  fit.RF_train$resample
  fit.RF_train$modelInfo
  X.xgb = varImp(fit.RF_train)
  
  #tiff("Number of perfect splits vs feature.tiff",res = 100) #save as tiff
  

  
  #Confusion Matrix - train data
  p1<-predict(fit.RF_train, test.data, type = "raw")
  confusionMatrix(p1, as.factor(test.data_labels))  # using more deep tree, the accuracy linearly increases! 
  
  z=as.data.frame.list(confusionMatrix(p1, as.factor(test.data_labels))$overall)  # using more deep tree, the accuracy linearly increases! 
  
  ######################
  # To exclude the highest important variable
  # 1.Get the highest value from importance function
  new= as.data.frame.table(X.xgb$importance)  # convert the importance list into table
  new= new[ which (new$Overall>=99),] # extact the highest value based on overall
  new= subset(new,  select=c(Var1))  # extract the variable related to highest weight
  
  #2. Exclude the column from original dataset
  b=as.data.frame(new$Var1)
  colnames(b)=b[1,]
  train.data= train.data[,!(names(train.data)%in%colnames(b))]
  names(train.data)
  
  print(names(train.data))
  print (Delay)
  print(confusionMatrix(p1, as.factor(test.data_labels)))  # using more deep tree, the accuracy linearly increases! 
  print(plot(X.xgb))
  
  
  
  ######## Hyperparameter----

  
  # Plot ROC curves
  # https://stackoverflow.com/questions/46124424/how-can-i-draw-a-roc-curve-for-a-randomforest-model-with-three-classes-in-r
  #install.packages("pROC")
  library(pROC)
  
  # the model is used to predict the test data. However, you should ask for type="prob" here
  predictions <- as.data.frame(predict(fit.RF_train, test.data, type = "prob"))
  ##  Since you have probabilities, use them to get the most-likely class.
  # predict class and then attach test class
  predictions$predict <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
  predictions$observed <- test.data_labels
  head(predictions)
  
  #    Now, let's see how to plot the ROC curves. For each class, convert the multi-class problem into a binary problem. Also, 
  #    call the roc() function specifying 2 arguments: i) observed classes and ii) class probability (instead of predicted class).
  # 1 ROC curve,  Moderate, Good, Haz vs non  Moderate non Good non Haz
  
  #tiff("XGBoost June ROC.tiff", height = 180, width = 180, units = 'mm', res = 90) #save as tiff
  par(mfrow=c(2,2))
  roc.Good <- roc(ifelse(predictions$observed=="Good", "Good", "non-Good"), as.numeric(predictions$Good))
  roc.Moderate <- roc(ifelse(predictions$observed== "Moderate", "Moderate", "non-Moderate"), as.numeric(predictions$Moderate))
  roc.Haz <- roc(ifelse(predictions$observed=="Haz", "Haz", "non-Haz"), as.numeric(predictions$Haz))
  
  plot(roc.Moderate, col = "#FF9900", main="All levels prediction of June PM10 data", cex.main = 0.8, cex.lab=0.8)
  lines(roc.Haz, col = "red")
  lines(roc.Good, col = "green")
  
  # calculating the values of AUC for ROC curve
  #results= c("Haz AUC" = roc.Haz$auc,"Moderate AUC" = roc.Moderate$auc,"Good AUC" = roc.Good$auc)
  #print(results)
  legend("bottomright",c("Haz","Moderate","Good"),fill=c("red","#FF9900","green"), cex = 0.7)
  #legend("bottomright",c("Haz","Moderate","Good"),fill=c("red","#FF9900","green"),inset = (0.46), cex = 0.7)
  
  
  ###########################################################
  #To get confidence intervals for the Area Under the Curve:
  
  #https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
  library(pROC)
  
  roc.Good <- roc(ifelse(predictions$observed=="Good", "Good", "non-Good"), as.numeric(predictions$Good),smoothed = TRUE,ci=TRUE, 
                  ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                  print.auc=TRUE, show.thres=TRUE,main="Good level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
  sens.ci <- ci.se(roc.Good)
  plot(sens.ci, type="shape", col="lightblue")
  plot(sens.ci, type="bars")
  
  roc.Haz <- roc(ifelse(predictions$observed=="Haz", "Haz", "non-Haz"), as.numeric(predictions$Haz),smoothed = TRUE,ci=TRUE, 
                       ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                       print.auc=TRUE, show.thres=TRUE,main="Haz level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
  sens.ci <- ci.se(roc.Haz)
  plot(sens.ci, type="shape", col="lightblue")
  plot(sens.ci, type="bars")
  
  roc.Moderate <- roc(ifelse(predictions$observed=="Moderate", "Moderate", "non-Moderate"), as.numeric(predictions$Moderate),smoothed = TRUE,ci=TRUE, 
                      ci.alpha=0.9, stratified=FALSE, plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE,main="Moderate level prediction of June PM10 data",cex.main = 0.8, cex.lab=0.8)
  sens.ci <- ci.se(roc.Moderate)
  plot(sens.ci, type="shape", col="lightblue")
  plot(sens.ci, type="bars")
  
  #########################################
  # Based on elapsed time we choose the variables that:
  # 1- has a multible variables contribution
  # 2- low elapsed
  # 3- confusion matrix =100
  #"NDMI"       "SAVI"       "BU"         "LST"        "NDVI"      
  #"DisToRoads" "band2"      "band3"      "band4"      "band5"     
  # "band6"      "band7"        
  #######################################################################
  
  
  #Train xgbTree model USING aLL dependent data
  #We will use the train() function from the of caret package with the "method" parameter "xgbTree" wrapped from the XGBoost package.
  
  set.seed(849)
  fit.rfAll<- train(PM~., 
                    data=original.n,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl,
                    importance = TRUE)
  

  X.xgbAll = varImp(fit.rfAll)
  
  original.n= original.n[,!(names(original.n)%in%colnames(b))]
  
  print(names(original.n))
  
  #tiff("varImportance XB All tunned.tiff",res = 100) #save as tiff
  print(" Final prediction analysis started################")
  print(plot(X.xgbAll, main="varImportance RF All tunned"))
 
  
  # 6  Produce prediction map using Raster data --------------
  
  #Produce LSM map using Training model results and Raster layers data
  ##### Now Restart R studio or clear it and save memory
  
  library(caret)        # machine laerning
  library(randomForest) # Random Forest
  library(rgdal)        # spatial data processing
  library(raster)       # raster processing
  library(plyr)         # data manipulation 
  library(dplyr)        # data manipulation 
  library(RStoolbox)    # ploting spatial data 
  library(RColorBrewer) # color
  library(ggplot2)      # ploting
  library(sp)           # spatial data
  library(doParallel)   # Parallel processing
  
  
  # 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------
  
  #Convert raster to dataframe with Long-Lat
  Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
  head(Rasters.df,1)
  names(Rasters.df)
  #Rasters.df=Rasters.df[,c(-6)] #
  
  # Now:Prediction using imported Rasters
  # remove x, y
  coor=Rasters.df%>% select(x,y)
  Rasters.df_N= Rasters.df[,!(names(Rasters.df)%in%colnames(coor))]
  names(Rasters.df_N)
  # Data Normalization
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  # Keep the "LevelAve" variables since ita????s the response variable that needs to be predicted.
  
  Rasters.df_N_Nor <- as.data.frame(lapply(Rasters.df_N, normalize))    
  str(Rasters.df_N_Nor)
  
  
  #Convert Dataframe back to raster with Long-Lat
  #https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r
  
  
  # PRODUCE PROBABILITY MAP
  p3<-as.data.frame(predict(fit.rfAll, Rasters.df_N_Nor, type = "prob"))
  summary(p3)
  Rasters.df$Levels_Haz<-p3$Haz
  Rasters.df$Levels_good<-p3$Good
  Rasters.df$Levels_Moderate<-p3$Moderate
  
  x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
  r_ave_good <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_good")])
  proj4string(r_ave_good)=CRS(projection(Study_area))
  
  r_ave_Haz <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_Haz")])
  proj4string(r_ave_Haz)=CRS(projection(Study_area))
  
  r_ave_Moderate <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_Moderate")])
  proj4string(r_ave_Moderate)=CRS(projection(Study_area))
  
  # Plot Maps
  print(spplot(r_ave_Haz, main="Haz June RF"))
  writeRaster(r_ave_Haz,filename="Probabiliy_June_RF_Haz.tif", format="GTiff", overwrite=TRUE) 
  
  print(spplot(r_ave_Moderate, main="Moderate June RF"))
  writeRaster(r_ave_Moderate,filename="Probabiliy_June_RF_Moderate.tif", format="GTiff", overwrite=TRUE) 
  
  print(spplot(r_ave_good, main="GOOD June RF"))
  writeRaster(r_ave_good,filename="Probabiliy_June_RF_GOOD.tif", format="GTiff", overwrite=TRUE) 
  
  
  # PRODUCE CLASSIFICATION MAP
  #Prediction at grid location
  p3<-as.data.frame(predict(fit.rfAll, Rasters.df_N_Nor, type = "raw"))
  summary(p3)
  # Extract predicted levels class
  head(Rasters.df, n=2)
  Rasters.df$Levels_ave<-p3$`predict(fit.rfAll, Rasters.df_N_Nor, type = "raw")`
  head(Rasters.df, n=2)
  
  # Import levels ID file 
  ID<-read.csv("./Levels_key.csv", header = T)
  
  # Join landuse ID
  grid.new<-join(Rasters.df, ID, by="Levels_ave", type="inner") 
  # Omit missing values
  grid.new.na<-na.omit(grid.new)    
  head(grid.new.na, n=2)
  
  #Convert to raster
  x<-SpatialPointsDataFrame(as.data.frame(grid.new.na)[, c("x", "y")], data = grid.new.na)
  r_ave <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Level_ID")])
  
  # coord. ref. : NA 
  # Add coord. ref. system by using the original data info (Copy n Paste).
  # borrow the projection from Raster data
  proj4string(r_ave)=CRS(projection(Study_area)) # set it to lat-long
  
  # Export final prediction map as raster TIF ---------------------------
  # write to a new geotiff file
  #writeRaster(r_ave,filename="Classification_Map XGBoost Tunned_Ave.tif", format="GTiff", overwrite=TRUE) 
  
  
  #Plot Landuse Map:
  # Color Palette follow Air index color style
  #https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html
  
  myPalette <- colorRampPalette(c("light green","#FFFF00","#FFC600", "#FFAA00","pink","red" ))
  
  # Plot Map
  LU_ave<-spplot(r_ave,"Level_ID", main="PM10 ave. concentration prediction: RF June" , 
                 colorkey = list(space="right",tick.number=1,height=1, width=1.5,
                                 labels = list(at = seq(1.2,5.9,length=6),cex=1.0,
                                               lab = c("Good" ,"Moderate", "UHealSesn", "UHeal", "VUnheal", "Haz"))),
                 col.regions=myPalette,cut=5)
  print(LU_ave)
  #jpeg("Prediction_Map XGBoost_AveAllRec.jpg", width = 1000, height = 700)
}
print("############################# Output Ends here and follow######################")
dev.off()








####################################################################
#####################################################################
# Hyperparameter process:


#  The algorithm uses 500 trees and tested three different values of mtry: 2, 5, 9.
# The final value used for the model was mtry = 5 with an accuracy of 0.8212617  Kappa =0.7318892. Let's try to get a higher score.

# https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest

#  ntree
#       Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.

#   mtry
#       Number of variables randomly sampled as candidates at each split. Note that the default values are different for classification (sqrt(p) where p is number of variables in x) and regression (p/3)

# maxnodes
#        Maximum number of terminal nodes trees in the forest can have. 
#        If not given, trees are grown to the maximum possible (subject to limits by nodesize) [ YOU CAN SKIP THE FOLLOWING STEP]


# Step 2) Search best mtry

set.seed(1234)
# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

tuneGrid <- expand.grid(.mtry = c(3: 20))
rf_mtry <- train(PM~., 
                 data=train.data,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE)
#,
#nodesize = (c(2:14)),
#ntree = 300)
print(rf_mtry)
rf_mtry$bestTune$mtry
#You can store it and use it when you need to tune the other parameters.
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry



# Step 3) Search the best maxnodes SKIP

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 30)) {
  set.seed(1234)
  rf_maxnode <- train(PM~., 
                      data=train.data,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)
(results_mtry)


# Step 3) Search the best best num of tree

store_bestree <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (bestree in c(200,300,400,500,600,700)) {
  set.seed(1234)
  rf_bestree <- train(PM~., 
                      data=train.data,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = 2,
                      ntree = bestree)
  current_iterationT <- toString(bestree)
  store_bestree[[current_iterationT]] <- rf_bestree
}
results_mtryT <- resamples(store_bestree)
summary(results_mtryT)
(results_mtryT)



#You have your final model. You can train the random forest with the following parameters:

#  ntree =default (700) trees will be trained
# mtry=2  features is chosen for each iteration
# maxnodes = 5

set.seed(1234)
rf_Hyper <- train(PM~., 
                    data=train.data,
                    method = "rf",
                    metric = "Accuracy",
                    tuneGrid = tuneGrid,
                    trControl = trControl,
                    importance = TRUE,
                    nodesize = 14,
                    maxnodes = 2,
                    ntree = 700)

# Print the results
print(rf_Hyper)     
plot(rf_Hyper, main="rf_Hyper")


rf_Hyper$finalModel         # Results mtry=5 Number of trees: 500
rf_Hyper$results 


# Modeling delay test ----------------------------------------
# Delay test : A comparison of run times for fastest method:
# Use the Elapsed (sec.)
Delay= rf_Hyper$times

rf_Hyper$resample
rf_Hyper$modelInfo
X.xgb = varImp(rf_Hyper)

#Confusion Matrix - train data
p1H<-predict(rf_Hyper, test.data, type = "raw")
confusionMatrix(p1H, as.factor(test.data_labels))  # using more deep tree, the accuracy linearly increases! 




###############################################
###############################################.

# Produce final probability maps

#1 # GOOD Level PM10 RF
# Import related rasters
list.files( "./",pattern = "tif$", full.names = TRUE)
Probabiliy_June_RF_GOOD= raster("Probabiliy_June_RF_GOOD.tif" )
Probabiliy_July_RF_GOOD= raster("Probabiliy_July_RF_GOOD.tif" )
Probabiliy_August_RF_GOOD= raster("Probabiliy_August_RF_GOOD.tif" )

Prob_All_Good_RF=(Probabiliy_June_RF_GOOD+Probabiliy_July_RF_GOOD+Probabiliy_August_RF_GOOD)/3
spplot(Prob_All_Good_RF, main="Probability Good level Ju,Jul,Aug RF")
writeRaster(Prob_All_Good_RF,filename="Prob_Final_RF_Ju_Jul_Aug_GOOD.tif", format="GTiff", overwrite=TRUE) 

#2 # Moderate Level PM10 RF
list.files( "./",pattern = "tif$", full.names = TRUE)
Probabiliy_June_RF_Moderate= raster("Probabiliy_June_RF_Moderate.tif" )
Probabiliy_August_RF_Moderate= raster("Probabiliy_August_RF_Moderate.tif" )

Prob_All_Moderate_RF=(Probabiliy_June_RF_Moderate+Probabiliy_August_RF_Moderate)/2
spplot(Prob_All_Moderate_RF, main="Probability Moderate level Ju,Aug RF")
writeRaster(Prob_All_Moderate_RF,filename="Prob_Final_RF_Ju_Aug_Moderate.tif", format="GTiff", overwrite=TRUE) 


#3 # Unhealthy Sensative Level PM10 RF
list.files( "./",pattern = "tif$", full.names = TRUE)
Probabiliy_August_RF_UHealSesn= raster("./Probabiliy_August_RF_UHealSesn.tif" )
spplot(Probabiliy_August_RF_UHealSesn, main="Probability UHealSesn level August RF")
writeRaster(Probabiliy_August_RF_UHealSesn,filename="Prob_Final_RF_Aug_UHealSesn.tif", format="GTiff", overwrite=TRUE) 


#4 # Very Unhealthy Level PM10 RF
list.files( "./",pattern = "tif$", full.names = TRUE)
Probabiliy_July_RF_VUnheal= raster("./Probabiliy_July_RF_VUnheal.tif" )
spplot(Probabiliy_July_RF_VUnheal, main="Probability VUhealthy level July RF")
writeRaster(Probabiliy_July_RF_VUnheal,filename="Prob_Final_RF_July_VUnheal.tif", format="GTiff", overwrite=TRUE) 


#5 # Hazardous Level PM10 RF
list.files( "./",pattern = "tif$", full.names = TRUE)
Probabiliy_June_RF_Haz= raster("./Probabiliy_June_RF_Haz.tif") 
Probabiliy_July_RF_Haz= raster("./Probabiliy_July_RF_Haz.tif")

Prob_All_Haz_RF= (Probabiliy_June_RF_Haz+Probabiliy_July_RF_Haz)/2
spplot(Prob_All_Haz_RF, main="Probability Haz level Ju_July RF")
writeRaster(Prob_All_Haz_RF,filename="Prob_Final_RF_Ju_Jul_Haz.tif", format="GTiff", overwrite=TRUE) 
