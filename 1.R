
# 1 Introduction ------------------------------------------------------------


##################################################.
## Project: PM 10 prediction using machine learning supervised classification techniques.
## Script purpose:Extreme Gradient Boosting (XGBoost) in CARET package
## Date: 07 Jan. 2020
## Author: Omar AlThuwaynee
##################################################.

# Disclaimer:
#            As with most of my R posts, I've arranged the functions using ideas from other people that are much more clever than me. 
#            I've simply converted these ideas into a useful form in R.
# References

#1#   https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
#2#   https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/


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

# Install packages
install.packages("xgboost")
install.packages("rlang")
install.packages("doSNOW")
install.packages("RStoolbox") 
install.packages("doParallel")
install.packages("Matrix")
install.packages("e1071")

library(xgboost)
library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # plotting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # plotting
library(sp)           # spatial data
library(caret)        # machine laerning
library(doParallel)   # Parallel processing
library(doSNOW)
library(e1071)

# SKIP BELOW IF YOU HAVE YOUR DATA READY
###############################################################################
# Distance to points

#### Distance to objects:

##1 ## Convert shp to Raster (Rasterization) with value of 1
.libPaths("G:/University Pc HD/Projects/Dormants research/Practical/WD Dromants/library")

library(sp)
#install.packages("maptools")
library(maptools)
library(rgdal)
library(raster)
#install.packages("rgeos")
library(rgeos)
#install.packages("plotKML")
#library(plotKML)

#In QGIS 
Flares <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/Flares.shp")
PetrolWells <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/PetrolWells.shp")
#industries_others <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/industries_others1.shp")
Roads <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Data/Main Roads kirkuk.shp")
Focused_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")
PPumpStations <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/PPumpStations.shp")
otherIndust <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/otherIndust.shp")


raster::plot(Focused_area)
raster::plot(Flares,add=T)
raster::plot(PetrolWells,col="red", add=T)
raster::plot(industries_others,col= "green", add=T)
raster::plot(Roads, add=T)
# add column with value of 1

names(Flares@data)
Flares=Flares[,c(-1:-142)] # if you have multiple columns and need to keep specific ones
Flares$num= 1

names(PetrolWells@data)
PetrolWells=PetrolWells[,c(-1:-142)]
PetrolWells$num= 1

#names(industries_others@data)
#industries_others=industries_others[,c(-1:-141)]
#industries_others$num= 1

names(Roads@data)
Roads=Roads[,c(-1:-44)]
Roads$num= 1

names(otherIndust@data)
otherIndust=otherIndust[,c(-1:-141)]
otherIndust$num= 1

names(PPumpStations@data)
PPumpStations=PPumpStations[,c(-1:-141)]
PPumpStations$num= 1

# Create a raster for all objects
library(sp)
library(maptools)
#library(spatstat)

rast <- raster()
extent(rast) <- extent(Focused_area) # this might be unnecessary
res(rast)= 30
crs(rast)= CRS("+init=epsg:32638")
values(rast)= 1:ncell(rast)
spplot(rast)

Flares_ras <- rasterize(Flares,fun='last', rast, Flares$num) #cell value equal 1
PetrolWells_ras <- rasterize(PetrolWells,fun='last', rast, PetrolWells$num) #cell value equal 1
#industries_others_ras <- rasterize(industries_others,fun='last', rast, industries_others$num) #cell value equal 1
Roads_ras <- rasterize(Roads,fun='last', rast, Roads$num) #cell value equal 1
otherIndust_ras <- rasterize(otherIndust,fun='last', rast, otherIndust$num) #cell value equal 1
PPumpStations_ras <- rasterize(PPumpStations,fun='last', rast, PPumpStations$num) #cell value equal 1

writeRaster(Flares_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/Flare_rast.tif", format="GTiff",overwrite=TRUE)
writeRaster(PetrolWells_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/PetrolWells_rast.tif", format="GTiff",overwrite=TRUE)
#writeRaster(industries_others_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/industries_others_rast.tif", format="GTiff",overwrite=TRUE)
writeRaster(Roads_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/Roads_ras.tif", format="GTiff",overwrite=TRUE)
writeRaster(otherIndust_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/otherIndust_ras.tif", format="GTiff",overwrite=TRUE)
writeRaster(PPumpStations_ras,"H:/IKC/International projects/Kirkuk air pollution/Practical/PPumpStations_ras.tif", format="GTiff",overwrite=TRUE)

### For a single RasterLayer (y is missing) this method 
#computes the distance, for all cells that 
#are NA, to the nearest cell that is not NA.
# Import raster
#Distance_drains = raster("./GIS data/Distance_drains1.tif")

Dis_To_flare= distance(Flares_ras)
Dis_To_flare_mask=mask(Dis_To_flare,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_flare.tif",overwrite=TRUE)
#plot(freq(Dis_To_flare_mask))
#writeRaster(Dis_To_drains_mask,"./GIS data/Dis_To_drains.tif", format="GTiff",overwrite=TRUE)

Dis_To_PetrolWells= distance(PetrolWells_ras)
Dis_To_PetrolWells_mask=mask(Dis_To_PetrolWells,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_PetrolWells.tif",overwrite=TRUE)

#Dis_To_industries_others= distance(industries_others_ras)
#Dis_To_industries_others_mask=mask(Dis_To_industries_others,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_industries_others.tif",overwrite=TRUE)

Dis_To_Roads= distance(Roads_ras)
Dis_To_Roads_mask=mask(Dis_To_Roads,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_Roads.tif",overwrite=TRUE)

Dis_To_PPumpStations= distance(PPumpStations_ras)
Dis_To_PPumpStations_mask=mask(Dis_To_PPumpStations,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_PPumpStations.tif",overwrite=TRUE)

Dis_To_otherIndust= distance(otherIndust_ras)
Dis_To_otherIndust_mask=mask(Dis_To_otherIndust,Focused_area, filename = "H:/IKC/International projects/Kirkuk air pollution/Practical/Dis_To_otherIndust.tif",overwrite=TRUE)

############################################################################



# Import training and testing data ----
list.files( pattern = "csv$", full.names = TRUE)
original <-  read.csv("./stat1_June.csv", header = T,stringsAsFactors = FALSE)
original1 <-  read.csv("./stat2_June.csv", header = T,stringsAsFactors = FALSE)
original2 <-  read.csv("./stat3_June.csv", header = T,stringsAsFactors = FALSE)
original= cbind(original,original1, original2)
names(original)



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

---------------#EXTRA started----
# if you have diffrent extent, then try to Resample them using the smallest area
NDVI_r <- resample(NDVI,LST, resample='bilinear') 
SLOPE_r <- resample(SLOPE,LANDCOVER, resample='bilinear') 
TWI_r <- resample(TWI,LANDCOVER, resample='bilinear') 
CURVATURE_r <- resample(CURVATURE,LANDCOVER, resample='bilinear') 
SPI_r <- resample(SPI,LANDCOVER, resample='bilinear') 
ASPECT_r <- resample(ASPECT,LANDCOVER, resample='bilinear') 

extent(ASPECT_r) # check the new extent
extent(LANDCOVER)

# write to a new geotiff file
writeRaster(ASPECT_r,filename="resampled/ASPECT.tif", format="GTiff", overwrite=TRUE) 
writeRaster(SPI_r,filename="resampled/SPI.tif", format="GTiff", overwrite=TRUE)
writeRaster(CURVATURE_r,filename="resampled/CURVATURE.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWI_r,filename="resampled/TWI.tif", format="GTiff", overwrite=TRUE)
writeRaster(ELEVATION_r,filename="resampled/ELEVATION.tif", format="GTiff", overwrite=TRUE)
writeRaster(SLOPE_r,filename="resampled/SLOPE.tif", format="GTiff", overwrite=TRUE)
writeRaster(LANDCOVER,filename="resampled/LANDCOVER.tif", format="GTiff", overwrite=TRUE)

#Stack_List= stack(ASPECT_r,LS_r)#,pattern = "tif$", full.names = TRUE)
#names(Stack_List)
#Stack_List.df = as.data.frame(Stack_List, xy = TRUE, na.rm = TRUE)
#head(Stack_List.df,1)

#Read rasters stack for clear Environment---- 

#--------------#-----#EXTRA ended----

# check attributes and projection and extent

# stack multiple raster files
#Rasters<- stack(list.files( "G:/bda/Landsat 20180629 Auto Processed by ESPA/Training/June 2018",pattern = "tif$", full.names = TRUE))
###########################

# If you want to re-scale the shapfile
Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")
#Study_area2 = gBuffer(Study_area, width = -8000) # scale polygon to cover new extent
#class(Study_area2)=class(Study_area)
#Study_area2@data=Study_area@data

#extent(NDMI)
#NDMI <- mask(crop(NDMI, Study_area), Study_area)
###########################################################

## Training and testing data prepration
# June max value for 2017,2018,2019
original= original[,c(4,10:14,24:28,38:43)]
names(original)
# July max value for 2017,2018,2019
#original= original[,c(8,10:14,24:28,38:43)]
#names(original)
# Aug max value for 2017,2018,2019
#original= original[,c(8,10:14,24:28,38:43)]
#names(original)
original <-(na.omit(original))
original <-data.frame(original)  # to remove the unwelcomed attributes
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


#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=3,
                          returnResamp='all', 
                          allowParallel=TRUE)

#Parameter for Tree Booster
#In the grid, each algorithm parameter can be specified as a vector of possible values . These vectors combine to define all the possible combinations to try.
# We will follow the defaults proposed by https://xgboost.readthedocs.io/en/latest//parameter.html

tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance

# Step 5 modeling
summary(train.data)

pdf("plot_June_XGBoost.pdf")
print("Processing starts here #############################")
set.seed(849)
# Using all data to define the Z value (highly important variable)
fit.xgb_train<- train(PM~ ., 
                      data=train.data,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)

# Modeling delay test ----------------------------------------
# Delay test : A comparison of run times for fastest method:
# Use the Elapsed (sec.)
Delay= fit.xgb_train$times

fit.xgb_train$resample
fit.xgb_train$modelInfo
X.xgb = varImp(fit.xgb_train)

#Confusion Matrix - train data
p1<-predict(fit.xgb_train, test.data, type = "raw")
confusionMatrix(p1, as.factor(test.data_labels))  # using more deep tree, the accuracy linearly increases! 
z=as.data.frame.list(confusionMatrix(p1, as.factor(test.data_labels))$overall)  # using more deep tree, the accuracy linearly increases! 

##### Now you can skip to the Loop
(names(train.data))
(Delay)
(confusionMatrix(p1, as.factor(test.data_labels)))  # using more deep tree, the accuracy linearly increases! 
(plot(X.xgb, main= "Var. importance XGB Training data"))

# which model reach more than 80% accuracy with lower number of variables
# discuss the shape of ROC with diffrent pattern (rigid or smooth)
# discuss the overfitting of first variables, like DistoFlares.

library(pROC)
# the model is used to predict the test data. However, you should ask for type="prob" here
predictions <- as.data.frame(predict(fit.xgb_train, test.data, type = "prob"))
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


print("############################# Output starts here and follow######################")
for (i in 1:16) {
  if (!z$Accuracy > 0.80) {
    next
  }
  
  set.seed(849)
 
# Using all data
fit.xgb_train<- train(PM~ ., 
                      data=train.data,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)
# Modeling delay test ----------------------------------------

# Delay test : A comparison of run times for fastest method:
# Use the Elapsed (sec.)
Delay= fit.xgb_train$times

fit.xgb_train$resample
fit.xgb_train$modelInfo
X.xgb = varImp(fit.xgb_train)

#tiff("Number of perfect splits vs feature.tiff",res = 100) #save as tiff
plot(X.xgb)

#dev.off()

#Confusion Matrix - train data
p1<-predict(fit.xgb_train, test.data, type = "raw")
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

#tune_grid2 <- expand.grid(nrounds = c(200,210),           # the max number of iterations INCREASE THE PROCESSING TIME COST
#                          max_depth = c(6,18,22),            # depth of a tree EFFECTIVE OPTIMIZATION
#                          eta = c(0.05,0.3,1),               # control the learning rate
#                          gamma = c(0,0.01,0.1),             # minimum loss reduction required
#                          colsample_bytree = c(0.75,1),  # subsample ratio of columns when constructing each tree
#                          min_child_weight = c(0,1,2),     # minimum sum of instance weight (hessian) needed in a child 
#                          subsample = c(0.5,1))            # subsample ratio of the training instance

#set.seed(849)
#fit.xgb_train2<- train(PM~DisToPetrol+DisToflare+ DisToRoads +DisToindust+NDMI +SAVI+BU+LST+band2+band3+band4+band5+band6+band7, 
#                       data=train.data,
#                       method = "xgbTree",
#                       metric= "Accuracy",
#                       preProc = c("center", "scale"), 
#                       trControl = myControl,
#                       tuneGrid = tune_grid2,
#                       tuneLength = 10)

#summaryRes=fit.xgb_train2$results # nrounds was fixed = 210
#head(summaryRes)
#summary(summaryRes)
#head(summaryRes[order(summaryRes$Accuracy, decreasing = TRUE),],n=6)  # sort max to min for first 5 values based on Accuracy

# Plot
#tiff("Pairs of hyperparameter XGBoost.tiff",res = 100) #save as tiff
#pairs(summaryRes[,c(-7,-9:-11)])
#dev.off()

# Save it
#write.csv(fit.xgb_train2$results,file = "fit.xgb_train_hyper.csv")#, sep = "",row.names = T)

#### Read from saved file
#list.files( pattern = "csv$", full.names = TRUE)
#summaryRes <-  read.csv("./fit.xgb_train_hyper.csv", header = T,stringsAsFactors = FALSE)
#summaryRes=summaryRes[,c(-1,-8,-11:-12)] # nrounds was fixed = 210
#head(summaryRes)
#summary(summaryRes)
#head(summaryRes[order(summaryRes$Accuracy, decreasing = TRUE),],n=6)  # sort max to min for first 5 values based on Accuracy
# Plot
#pairs(summaryRes[-8])

# Re-run using recomended settings of expand.grid

#X.xgb = varImp(fit.xgb_train3)
#plot(X.xgb)

#Confusion Matrix - train data
#p2_xgb_train3<-predict(fit.xgb_train3, test.data, type = "raw")
#confusionMatrix(p2_xgb_train3, as.factor(test.data_labels))  # using more deep tree, the accuracy linearly increases! 
#while increase the iterations to 220 double the processing time with slight accuracy improvment!

# Plot ROC curves
# https://stackoverflow.com/questions/46124424/how-can-i-draw-a-roc-curve-for-a-randomforest-model-with-three-classes-in-r
#install.packages("pROC")
library(pROC)

# the model is used to predict the test data. However, you should ask for type="prob" here
predictions <- as.data.frame(predict(fit.xgb_train, test.data, type = "prob"))
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

# calculating the values of AUC for ROC curve
#results= c("Haz AUC" = roc.Haz$auc,"Moderate AUC" = roc.Moderate$auc,"Good AUC" = roc.Good$auc)
#print(results)
legend("bottomright",c("Haz","Moderate","Good"),fill=c("red","#FF9900","green"), cex = 0.7)
#legend("bottomright",c("Haz","Moderate","Good"),fill=c("red","#FF9900","green"),inset = (0.46), cex = 0.7)


############################# EXTRA STARTED
# Continue  https://stackoverflow.com/questions/47820166/add-auc-by-group-on-roc-plots-in-r

# for more professional plots: 
# https://stackoverflow.com/questions/14860078/plot-multiple-lines-data-series-each-with-unique-color-in-r
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
#brewer.pal(n = 3, name = "RdBu")
# Plot colors
#library("RColorBrewer")
#display.brewer.all()
############################# EXTRA FINISHED

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
fit.xgbAll<- train(PM~ .,
                   data=original.n,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneGrid = tune_grid,
                   tuneLength = 10,
                   importance = TRUE)

X.xgbAll = varImp(fit.xgbAll)

original.n= original.n[,!(names(original.n)%in%colnames(b))]

print(names(original.n))

#tiff("varImportance XB All tunned.tiff",res = 100) #save as tiff
print(" Final prediction analysis started################")
print(plot(X.xgbAll, main="varImportance XB All tunned"))
#dev.off()
# Plot graph
# 1. Open jpeg file
#jpeg("varImportance XB All tunned.jpg", width = 800, height = 500)
# 2. Create the plot
#plot(X.xgbAll,main="varImportance All XB" )
# 3. Close the file
#dev.off()

####################################################################################################################################################################################################################################
###### http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/


# 6  Produce prediction map using Raster data --------------
#Produce prediction map using Trained model results and Raster layers data


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
p3<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_Nor, type = "prob"))
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
print(spplot(r_ave_Haz, main="Haz june XGB"))
#writeRaster(r_ave_VUnheal,filename="Prediction_XGBoostTunned_Ave_VUnheal.tif", format="GTiff", overwrite=TRUE) 

print(spplot(r_ave_Moderate, main="Moderate june XGB"))
#writeRaster(r_ave_UHealSesn,filename="Prediction_XGBoostTunned_Ave_UHealSesn.tif", format="GTiff", overwrite=TRUE) 

print(spplot(r_ave_good, main="GOOD june XGB"))
#writeRaster(r_ave_good,filename="Prediction_XGBoost Tunned_Ave_GOOD.tif", format="GTiff", overwrite=TRUE) 


# PRODUCE CLASSIFICATION MAP
#Prediction at grid location
p3<-as.data.frame(predict(fit.xgbAll, Rasters.df_N_Nor, type = "raw"))
summary(p3)
# Extract predicted levels class
head(Rasters.df, n=2)
Rasters.df$Levels_ave<-p3$`predict(fit.xgbAll, Rasters.df_N_Nor, type = "raw")`
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
LU_ave<-spplot(r_ave,"Level_ID", main="PM10 ave. concentration prediction: XGBoost June" , 
               colorkey = list(space="right",tick.number=1,height=1, width=1.5,
                               labels = list(at = seq(1.2,5.9,length=6),cex=1.0,
                                             lab = c("Good" ,"Moderate", "UHealSesn", "UHeal", "VUnheal", "Haz"))),
               col.regions=myPalette,cut=5)
print(LU_ave)
#jpeg("Prediction_Map XGBoost_AveAllRec.jpg", width = 1000, height = 700)
}
print("############################# Output Ends here and follow######################")
dev.off()





#   nrounds   max_depth  eta gamma colsample_bytree min_child_weight subsample      Accuracy    Kappa  AccuracySD     KappaSD
# 1     200        18     0.07     0.01             0.75                0       0.5 0.8131468 0.719717 0.003342402 0.005013939