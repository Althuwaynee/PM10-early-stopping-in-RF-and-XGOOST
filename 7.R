# Bism Allah Alrahaman Alraheem#

#### Import raw band 10 to convert into surface temprature ####

#### Environment settings ####
# H:\Projects\Kirkuk\WorkingDir
path=readClipboard()
setwd(path)
getwd() # for checking

.libPaths("H:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")
#.libPaths("C:/Users/user/Desktop/WorkingDIR/libraryfake")
#.libPaths()


# Source:  https://rpubs.com/geka/UrbanWarm


#For calculating of surface temperature we use formula:

#             LST(?C)=Bt/[1+(w???Bt/p)???ln(e)]???273.15
# Where:

#     Bt is At satellite temperature

#     w is wavelength of emitted radiance

#     p is h???c/s, where h is Planc's constant, c is velocity of light, s is Boltzmann constant. p is equal to 14388

#     e is electromagnetic emissivity. For urban areas I use formula from Stathopolou (2007):

#             e=0.017???PV+0.963
#     In formula of electromagnetic emissivity PV is "Proportion of Vegetation". For PV calculation is used the NDVI.

#             PV=[(NDVI???NDVImin)/(NDVImax???NDVImin)]2
####################


library(raster)
library(RStoolbox)
#set a temporary data directory for large graphical objects
#rasterOptions(tmpdir = "/home/geka/TEMP_R/")
#! You must have a large free space on your hard disk because we will get several large raster objects

# study area
Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")
#plot(Study_area, lwd = 2, col = "red", add = TRUE)
plot(Study_area)
#########################################################################################################################################################


####      2017 ####
#H:\Projects\Kirkuk\WorkingDir
path=readClipboard()
setwd(path)
getwd() # for checking

.libPaths("H:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")
library(raster)
library(RStoolbox)
Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")


####      JUNE 2017 ####
# import band 10 info
list.dirs("./Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244",pattern = "MTL.txt$", full.names = TRUE)
metaData_june2017 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_MTL.txt")
lsat8_june2017 <- stackMeta(metaData_june2017)
hazeDN_june2017    <- estimateHaze(lsat8_june2017, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_june2017 <- radCor(lsat8_june2017, metaData = metaData_june2017,hazeValues = hazeDN_june2017, hazeBands = 1:4,
                              method = "sdos")
## Find LST
lsat8.ndvi_june2017 <- spectralIndices(lsat8_sdos_june2017, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_june2017 <- ((lsat8.ndvi_june2017 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_june2017 <- 0.017 * lsat8.PV_june2017 + 0.963
lsat8.LST_june2017 <- lsat8_sdos_june2017$B10_bt / (1 + 10.8 * (lsat8_sdos_june2017$B10_bt/14388) * log(lsat8.emissiv_june2017)) - 273.15
# Clip study area
lsat8.LST_june2017 <- mask(crop(lsat8.LST_june2017, Study_area), Study_area)

spplot(lsat8.LST_june2017)
####      JULY 2017 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301",pattern = "MTL.txt$", full.names = TRUE)
metaData_july2017 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_MTL.txt")
lsat8_july2017 <- stackMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_MTL.txt")
hazeDN_july2017    <- estimateHaze(lsat8_july2017, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_july2017 <- radCor(lsat8_july2017, metaData = metaData_july2017,hazeValues = hazeDN_july2017,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_july2017 <- spectralIndices(lsat8_sdos_july2017, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_july2017 <- ((lsat8.ndvi_july2017 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_july2017 <- 0.017 * lsat8.PV_july2017 + 0.963
lsat8.LST_july2017 <- lsat8_sdos_july2017$B10_bt / (1 + 10.8 * (lsat8_sdos_july2017$B10_bt/14388) * log(lsat8.emissiv_july2017)) - 273.15
lsat8.LST_july2017 <- mask(crop(lsat8.LST_july2017, Study_area), Study_area)

####    August 2017 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224",pattern = "MTL.txt$", full.names = TRUE)
metaData_August2017 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_MTL.txt")
lsat8_August2017 <- stackMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_MTL.txt")
hazeDN_August2017    <- estimateHaze(lsat8_August2017, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_August2017 <- radCor(lsat8_August2017, metaData = metaData_August2017,hazeValues = hazeDN_August2017,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_August2017 <- spectralIndices(lsat8_sdos_August2017, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_August2017 <- ((lsat8.ndvi_August2017 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_August2017 <- 0.017 * lsat8.PV_August2017 + 0.963
lsat8.LST_August2017 <- lsat8_sdos_August2017$B10_bt / (1 + 10.8 * (lsat8_sdos_August2017$B10_bt/14388) * log(lsat8.emissiv_August2017)) - 273.15
lsat8.LST_August2017 <- mask(crop(lsat8.LST_August2017, Study_area), Study_area)
mean_2017_LST <- calc(stack(lsat8.LST_june2017,lsat8.LST_july2017,lsat8.LST_August2017), fun = mean, na.rm = T)
plot(mean_2017_LST )
writeRaster(mean_2017_LST,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2017_LST.tiff", format="GTiff", overwrite=TRUE)


####      2018 ####
#H:\Projects\Kirkuk\WorkingDir
#path=readClipboard()
#setwd(path)
#getwd() # for checking
#.libPaths("H:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")
library(raster)
library(RStoolbox)
#Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")

####      JUNE 2018 ####
# import band 10 info
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315",pattern = "MTL.txt$", full.names = TRUE)

metaData_june2018 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_MTL.txt")
lsat8_june2018 <- stackMeta(metaData_june2018)
hazeDN_june2018    <- estimateHaze(lsat8_june2018, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_june2018 <- radCor(lsat8_june2018, metaData = metaData_june2018,hazeValues = hazeDN_june2018, hazeBands = 1:4,
                              method = "sdos")
## Find LST
lsat8.ndvi_june2018 <- spectralIndices(lsat8_sdos_june2018, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_june2018 <- ((lsat8.ndvi_june2018 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_june2018 <- 0.017 * lsat8.PV_june2018 + 0.963
lsat8.LST_june2018 <- lsat8_sdos_june2018$B10_bt / (1 + 10.8 * (lsat8_sdos_june2018$B10_bt/14388) * log(lsat8.emissiv_june2018)) - 273.15
# Clip study area
lsat8.LST_june2018 <- mask(crop(lsat8.LST_june2018, Study_area), Study_area)

####      JULY 2018 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314",pattern = "MTL.txt$", full.names = TRUE)
metaData_july2018 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_MTL.txt")
lsat8_july2018 <- stackMeta(metaData_july2018)
hazeDN_july2018    <- estimateHaze(lsat8_july2018, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_july2018 <- radCor(lsat8_july2018, metaData = metaData_july2018,hazeValues = hazeDN_july2018,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_july2018 <- spectralIndices(lsat8_sdos_july2018, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_july2018 <- ((lsat8.ndvi_july2018 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_july2018 <- 0.017 * lsat8.PV_july2018 + 0.963
lsat8.LST_july2018 <- lsat8_sdos_july2018$B10_bt / (1 + 10.8 * (lsat8_sdos_july2018$B10_bt/14388) * log(lsat8.emissiv_july2018)) - 273.15
lsat8.LST_july2018 <- mask(crop(lsat8.LST_july2018, Study_area), Study_area)

####    August 2018 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239",pattern = "MTL.txt$", full.names = TRUE)
metaData_August2018 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_MTL.txt")
lsat8_August2018 <- stackMeta(metaData_August2018)
hazeDN_August2018    <- estimateHaze(lsat8_August2018, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_August2018 <- radCor(lsat8_August2018, metaData = metaData_August2018,hazeValues = hazeDN_August2018,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_August2018 <- spectralIndices(lsat8_sdos_August2018, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_August2018 <- ((lsat8.ndvi_August2018 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_August2018 <- 0.017 * lsat8.PV_August2018 + 0.963
lsat8.LST_August2018 <- lsat8_sdos_August2018$B10_bt / (1 + 10.8 * (lsat8_sdos_August2018$B10_bt/14388) * log(lsat8.emissiv_August2018)) - 273.15
lsat8.LST_August2018 <- mask(crop(lsat8.LST_August2018, Study_area), Study_area)

# Check the extent
extent(lsat8.LST_june2018)
extent(lsat8.LST_july2018)
extent(lsat8.LST_August2018)

stack_2018_LST=stack(lsat8.LST_june2018,lsat8.LST_july2018,lsat8.LST_August2018)
mean_2018_LST <- calc(stack_2018_LST, fun = mean, na.rm = T)
writeRaster(mean_2018_LST,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2018_LST.tiff", format="GTiff", overwrite=TRUE)




####      2019 ####
#H:\Projects\Kirkuk\WorkingDir
#path=readClipboard()
#setwd(path)
#getwd() # for checking
#.libPaths("H:/IKC/International projects/Kirkuk air pollution/Data/pm10/pm10 library")
library(raster)
library(RStoolbox)
#Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")

####      JUNE 2019 ####
# import band 10 info
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819",pattern = "MTL.txt$", full.names = TRUE)

#open MTL file and extract the information
metaData_june2019 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_MTL.txt")
lsat8_june2019 <- stackMeta(metaData_june2019)
hazeDN_june2019    <- estimateHaze(lsat8_june2019, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_june2019 <- radCor(lsat8_june2019, metaData = metaData_june2019,hazeValues = hazeDN_june2019, hazeBands = 1:4,
                              method = "sdos")
#lsat8_sdos_june2019

## Find LST
lsat8.ndvi_june2019 <- spectralIndices(lsat8_sdos_june2019, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
#plot(lsat8.ndvi_june2019)
#writeRaster(lsat8.ndvi,filename="output/ndvi.tif", format="GTiff", overwrite=TRUE) 

lsat8.PV_june2019 <- ((lsat8.ndvi_june2019 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_june2019 <- 0.017 * lsat8.PV_june2019 + 0.963
#lsat8.emissiv_june2019
lsat8.LST_june2019 <- lsat8_sdos_june2019$B10_bt / (1 + 10.8 * (lsat8_sdos_june2019$B10_bt/14388) * log(lsat8.emissiv_june2019)) - 273.15
plot(lsat8.LST_june2019)

# Clip study area
lsat8.LST_june2019 <- mask(crop(lsat8.LST_june2019, Study_area), Study_area)

####      JULY 2019 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307",pattern = "txt$", full.names = TRUE)
metaData_july2019 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_MTL.txt")
lsat8_july2019 <- stackMeta(metaData_july2019)
hazeDN_july2019    <- estimateHaze(lsat8_july2019, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_july2019 <- radCor(lsat8_july2019, metaData = metaData_july2019,hazeValues = hazeDN_july2019,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_july2019 <- spectralIndices(lsat8_sdos_july2019, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_july2019 <- ((lsat8.ndvi_july2019 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_july2019 <- 0.017 * lsat8.PV_july2019 + 0.963
lsat8.LST_july2019 <- lsat8_sdos_july2019$B10_bt / (1 + 10.8 * (lsat8_sdos_july2019$B10_bt/14388) * log(lsat8.emissiv_july2019)) - 273.15
lsat8.LST_july2019 <- crop(lsat8.LST_july2019, Study_area)
lsat8.LST_july2019 <- mask(lsat8.LST_july2019, Study_area)

####    August 2019 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248",pattern = "txt$", full.names = TRUE)
metaData_August2019 <- readMeta("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_MTL.txt")
lsat8_August2019 <- stackMeta(metaData_August2019)
hazeDN_August2019    <- estimateHaze(lsat8_August2019, hazeBands = 1:4, darkProp = 0.01)
lsat8_sdos_August2019 <- radCor(lsat8_August2019, metaData = metaData_August2019,hazeValues = hazeDN_August2019,hazeBands = 1:4,method = "sdos")
lsat8.ndvi_August2019 <- spectralIndices(lsat8_sdos_August2019, red = "B4_sre", nir = "B5_sre", indices = "NDVI")
lsat8.PV_August2019 <- ((lsat8.ndvi_August2019 - (-0.3736169)) / (0.9435266 + (-0.3736169)))^2
lsat8.emissiv_August2019 <- 0.017 * lsat8.PV_August2019 + 0.963
lsat8.LST_August2019 <- lsat8_sdos_August2019$B10_bt / (1 + 10.8 * (lsat8_sdos_August2019$B10_bt/14388) * log(lsat8.emissiv_August2019)) - 273.15
lsat8.LST_August2019 <- crop(lsat8.LST_August2019, Study_area)
lsat8.LST_August2019 <- mask(lsat8.LST_August2019, Study_area)

stack_2019_LST=stack(lsat8.LST_june2019,lsat8.LST_july2019,lsat8.LST_August2019)
mean_2019_LST <- calc(stack_2019_LST, fun = mean, na.rm = T)

#plot(mean_2019_LST)
writeRaster(mean_2019_LST,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2019_LST.tiff", format="GTiff", overwrite=TRUE)


######### Average 2017, 2018, 2019 LST
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave",pattern = "tif$", full.names = TRUE)

mean_2017_LST <- raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2017_LST.tif" )
mean_2018_LST <- raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2018_LST.tif" )
mean_2019_LST <- raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_2019_LST.tif" )

# Check the extent
extent(mean_2017_LST)
extent(mean_2018_LST)
extent(mean_2019_LST)


# By Month
stack_17_19_LST_june=stack(lsat8.LST_june2017,lsat8.LST_june2018,lsat8.LST_june2019)
stack_17_19_LST_july=stack(lsat8.LST_july2017,lsat8.LST_july2018,lsat8.LST_july2019)
stack_17_19_LST_August=stack(lsat8.LST_August2017,lsat8.LST_August2018,lsat8.LST_August2018)

mean_17_19_LST_june <- calc(stack_17_19_LST_june, fun = mean, na.rm = T)
writeRaster(mean_17_19_LST_june,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_17_19_LST_june.tiff", format="GTiff", overwrite=TRUE)

mean_17_19_LST_july <- calc(stack_17_19_LST_july, fun = mean, na.rm = T)
writeRaster(mean_17_19_LST_july,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_17_19_LST_july.tiff", format="GTiff", overwrite=TRUE)

mean_17_19_LST_August <- calc(stack_17_19_LST_August, fun = mean, na.rm = T)
writeRaster(mean_17_19_LST_August,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_17_19_LST_August.tiff", format="GTiff", overwrite=TRUE)


# By year
stack_17_19_LST=stack(mean_2017_LST,mean_2018_LST,mean_2019_LST)
mean_17_19_LST <- calc(stack_17_19_LST, fun = mean, na.rm = T)
writeRaster(mean_17_19_LST,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/LST_ave/mean_17_18_19_LST.tiff", format="GTiff", overwrite=TRUE)



#######################################################################




#### Landsat 8 indicies ####
library(raster)
library(RStoolbox)
#set a temporary data directory for large graphical objects
#rasterOptions(tmpdir = "/home/geka/TEMP_R/")
#! You must have a large free space on your hard disk because we will get several large raster objects

# study area
Study_area <- shapefile("H:/IKC/International projects/Kirkuk air pollution/Practical/study area new_Extent1.shp")
plot(Study_area)#, lwd = 2, col = "red", add = TRUE)


### Built up index
# NDVI = (Band 5 - Band 4) / (Band 5 + Band 4) 
# NDBI = (Band 6 - Band 5) / (Band 6 + Band 5) = (SWIR - NIR) / (SWIR + NIR)
# BU= NDBI- NDVI
# NDMI = (Band 5 - Band 6) / (Band 5 + Band 6) = (NIR - SWIR) / (NIR + SWIR)

###################################################################################################################################################################################

### 2017 ####

### june 2017 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244",pattern = "band6.tif$", full.names = TRUE)

band_7_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band4.tif" ), Study_area), Study_area)
band_3_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_june_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017061001T1-SC20191101102244/LC08_L1TP_169035_20170610_20170627_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_june_2017 <- mask(crop(((band_5_june_2017 - band_4_june_2017) / (band_5_june_2017 + band_4_june_2017 + 0.5)) * (1.5), Study_area), Study_area)
BU_june_2017 <- mask(crop(((band_6_june_2017 - band_5_june_2017) / (band_6_june_2017 + band_5_june_2017))-((band_5_june_2017 - band_4_june_2017) / (band_5_june_2017 + band_4_june_2017)), Study_area), Study_area)
NDMI_june_2017 <- mask(crop(((band_5_june_2017 - band_6_june_2017) / (band_6_june_2017 + band_5_june_2017)), Study_area), Study_area)
NDVI_june_2017<- mask(crop(((band_5_june_2017 - band_4_june_2017) / (band_5_june_2017 + band_4_june_2017)), Study_area), Study_area)

### july 2017 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301",pattern = "band6.tif$", full.names = TRUE)

band_7_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band4.tif" ), Study_area), Study_area)
band_3_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_july_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017071201T1-SC20191101102301/LC08_L1TP_169035_20170712_20170726_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_july_2017 <- mask(crop(((band_5_july_2017 - band_4_july_2017) / (band_5_july_2017 + band_4_july_2017 + 0.5)) * (1.5), Study_area), Study_area)
BU_july_2017 <- mask(crop(((band_6_july_2017 - band_5_july_2017) / (band_6_july_2017 + band_5_july_2017))-((band_5_july_2017 - band_4_july_2017) / (band_5_july_2017 + band_4_july_2017)), Study_area), Study_area)
NDMI_july_2017 <- mask(crop(((band_5_july_2017 - band_6_july_2017) / (band_6_july_2017 + band_5_july_2017)), Study_area), Study_area)
NDVI_july_2017 <- mask(crop(((band_5_july_2017 - band_4_july_2017) / (band_5_july_2017 + band_4_july_2017)), Study_area), Study_area)

### Aug 2017 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224",pattern = "band6.tif$", full.names = TRUE)
band_7_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band4.tif"), Study_area), Study_area)
band_3_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band3.tif"), Study_area), Study_area)
band_2_aug_2017 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352017081301T1-SC20191101102224/LC08_L1TP_169035_20170813_20170825_01_T1_sr_band2.tif"), Study_area), Study_area)
SAVI_aug_2017 <- mask(crop(((band_5_aug_2017 - band_4_aug_2017) / (band_5_aug_2017 + band_4_aug_2017 + 0.5)) * (1.5), Study_area), Study_area)
BU_aug_2017 <- mask(crop(((band_6_aug_2017 - band_5_aug_2017) / (band_6_aug_2017 + band_5_aug_2017))-((band_5_aug_2017 - band_4_aug_2017) / (band_5_aug_2017 + band_4_aug_2017)), Study_area), Study_area)
NDMI_aug_2017 <- mask(crop(((band_5_aug_2017 - band_6_aug_2017) / (band_6_aug_2017 + band_5_aug_2017)), Study_area), Study_area)
NDVI_aug_2017 <- mask(crop(((band_5_aug_2017 - band_4_aug_2017) / (band_5_aug_2017 + band_4_aug_2017)), Study_area), Study_area)



# By year
mean_2017_band_7 <- calc(stack(band_7_june_2017,band_7_july_2017,band_7_aug_2017), fun = mean, na.rm = T)
mean_2017_band_6 <- calc(stack(band_6_june_2017,band_6_july_2017,band_6_aug_2017), fun = mean, na.rm = T)
mean_2017_band_5 <- calc(stack(band_5_june_2017,band_5_july_2017,band_5_aug_2017), fun = mean, na.rm = T)
mean_2017_band_4 <- calc(stack(band_4_june_2017,band_4_july_2017,band_4_aug_2017), fun = mean, na.rm = T)
mean_2017_band_3 <- calc(stack(band_3_june_2017,band_3_july_2017,band_3_aug_2017), fun = mean, na.rm = T)
mean_2017_band_2 <- calc(stack(band_2_june_2017,band_2_july_2017,band_2_aug_2017), fun = mean, na.rm = T)
mean_2017_SAVI <- calc(stack(SAVI_june_2017,SAVI_july_2017,SAVI_aug_2017), fun = mean, na.rm = T)
mean_2017_BU <- calc(stack(BU_june_2017,BU_july_2017,BU_aug_2017), fun = mean, na.rm = T)
mean_2017_NDMI <- calc(stack(NDMI_june_2017,NDMI_july_2017,NDMI_aug_2017), fun = mean, na.rm = T)

writeRaster(mean_2017_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_2017_band7", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_2017_band6", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_2017_band5", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_2017_band4", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_2017_band3", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_2017_band2", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_2017_SAVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_2017_BU", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_2017_NDMI", format="GTiff", overwrite=TRUE)


### 2018 ####

### june 2018 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315",pattern = "band6.tif$", full.names = TRUE)

band_7_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band4.tif" ), Study_area), Study_area)
band_3_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_june_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018061301T1-SC20191101102315/LC08_L1TP_169035_20180613_20180615_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_june_2018 <- mask(crop(((band_5_june_2018 - band_4_june_2018) / (band_5_june_2018 + band_4_june_2018 + 0.5)) * (1.5), Study_area), Study_area)
BU_june_2018 <- mask(crop(((band_6_june_2018 - band_5_june_2018) / (band_6_june_2018 + band_5_june_2018))-((band_5_june_2018 - band_4_june_2018) / (band_5_june_2018 + band_4_june_2018)), Study_area), Study_area)
NDMI_june_2018 <- mask(crop(((band_5_june_2018 - band_6_june_2018) / (band_6_june_2018 + band_5_june_2018)), Study_area), Study_area)
NDVI_june_2018 <- mask(crop(((band_5_june_2018 - band_4_june_2018) / (band_5_june_2018 + band_4_june_2018)), Study_area), Study_area)

### july 2018 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314" ,pattern = "band6.tif$", full.names = TRUE)

band_7_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band4.tif" ), Study_area), Study_area)
band_3_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_july_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018073101T1-SC20191101102314/LC08_L1TP_169035_20180731_20180814_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_july_2018 <- mask(crop(((band_5_july_2018 - band_4_july_2018) / (band_5_july_2018 + band_4_july_2018 + 0.5)) * (1.5), Study_area), Study_area)
BU_july_2018 <- mask(crop(((band_6_july_2018 - band_5_july_2018) / (band_6_july_2018 + band_5_july_2018))-((band_5_july_2018 - band_4_july_2018) / (band_5_july_2018 + band_4_july_2018)), Study_area), Study_area)
NDMI_july_2018 <- mask(crop(((band_5_july_2018 - band_6_july_2018) / (band_6_july_2018 + band_5_july_2018)), Study_area), Study_area)
NDVI_july_2018 <- mask(crop(((band_5_july_2018 - band_4_july_2018) / (band_5_july_2018 + band_4_july_2018)), Study_area), Study_area)

### Aug 2018 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239",pattern = "band6.tif$", full.names = TRUE)
band_7_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band4.tif"), Study_area), Study_area)
band_3_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band3.tif"), Study_area), Study_area)
band_2_aug_2018 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352018081601T1-SC20191101102239/LC08_L1TP_169035_20180816_20180829_01_T1_sr_band2.tif"), Study_area), Study_area)
SAVI_aug_2018 <- mask(crop(((band_5_aug_2018 - band_4_aug_2018) / (band_5_aug_2018 + band_4_aug_2018 + 0.5)) * (1.5), Study_area), Study_area)
BU_aug_2018 <- mask(crop(((band_6_aug_2018 - band_5_aug_2018) / (band_6_aug_2018 + band_5_aug_2018))-((band_5_aug_2018 - band_4_aug_2018) / (band_5_aug_2018 + band_4_aug_2018)), Study_area), Study_area)
NDMI_aug_2018 <- mask(crop(((band_5_aug_2018 - band_6_aug_2018) / (band_6_aug_2018 + band_5_aug_2018)), Study_area), Study_area)
NDVI_aug_2018 <- mask(crop(((band_5_aug_2018 - band_4_aug_2018) / (band_5_aug_2018 + band_4_aug_2018)), Study_area), Study_area)

mean_2018_band_7 <- calc(stack(band_7_june_2018,band_7_july_2018,band_7_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_2018_band7", format="GTiff", overwrite=TRUE)
mean_2018_band_6 <- calc(stack(band_6_june_2018,band_6_july_2018,band_6_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_2018_band6", format="GTiff", overwrite=TRUE)
mean_2018_band_5 <- calc(stack(band_5_june_2018,band_5_july_2018,band_5_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_2018_band5", format="GTiff", overwrite=TRUE)
mean_2018_band_4 <- calc(stack(band_4_june_2018,band_4_july_2018,band_4_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_2018_band4", format="GTiff", overwrite=TRUE)
mean_2018_band_3 <- calc(stack(band_3_june_2018,band_3_july_2018,band_3_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_2018_band3", format="GTiff", overwrite=TRUE)
mean_2018_band_2 <- calc(stack(band_2_june_2018,band_2_july_2018,band_2_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_2018_band2", format="GTiff", overwrite=TRUE)
mean_2018_SAVI <- calc(stack(SAVI_june_2018,SAVI_july_2018,SAVI_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_2018_SAVI", format="GTiff", overwrite=TRUE)
mean_2018_BU <- calc(stack(BU_june_2018,BU_july_2018,BU_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_2018_BU", format="GTiff", overwrite=TRUE)
mean_2018_NDMI <- calc(stack(NDMI_june_2018,NDMI_july_2018,NDMI_aug_2018), fun = mean, na.rm = T)
writeRaster(mean_2018_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_2018_NDMI", format="GTiff", overwrite=TRUE)




### 2019 ####

### june 2019 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819",pattern = "band6.tif$", full.names = TRUE)

band_7_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band4.tif"), Study_area), Study_area)
band_3_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_june_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019061601T1-SC20191117020819/LC08_L1TP_169035_20190616_20190620_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_june_2019 <- mask(crop(((band_5_june_2019 - band_4_june_2019) / (band_5_june_2019 + band_4_june_2019 + 0.5)) * (1.5), Study_area), Study_area)
BU_june_2019 <- mask(crop(((band_6_june_2019 - band_5_june_2019) / (band_6_june_2019 + band_5_june_2019))-((band_5_june_2019 - band_4_june_2019) / (band_5_june_2019 + band_4_june_2019)), Study_area), Study_area)
NDMI_june_2019 <- mask(crop(((band_5_june_2019 - band_6_june_2019) / (band_6_june_2019 + band_5_june_2019)), Study_area), Study_area)
NDVI_june_2019 <- mask(crop(((band_5_june_2019 - band_4_june_2019) / (band_5_june_2019 + band_4_june_2019)), Study_area), Study_area)

### july 2019 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307" ,pattern = "band6.tif$", full.names = TRUE)

band_7_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band4.tif" ), Study_area), Study_area)
band_3_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band3.tif" ), Study_area), Study_area)
band_2_july_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019071801T1-SC20191101102307/LC08_L1TP_169035_20190718_20190731_01_T1_sr_band2.tif" ), Study_area), Study_area)
SAVI_july_2019 <- mask(crop(((band_5_july_2019 - band_4_july_2019) / (band_5_july_2019 + band_4_july_2019 + 0.5)) * (1.5), Study_area), Study_area)
BU_july_2019 <- mask(crop(((band_6_july_2019 - band_5_july_2019) / (band_6_july_2019 + band_5_july_2019))-((band_5_july_2019 - band_4_july_2019) / (band_5_july_2019 + band_4_july_2019)), Study_area), Study_area)
NDMI_july_2019 <- mask(crop(((band_5_july_2019 - band_6_july_2019) / (band_6_july_2019 + band_5_july_2019)), Study_area), Study_area)
NDVI_july_2019 <- mask(crop(((band_5_july_2019 - band_4_july_2019) / (band_5_july_2019 + band_4_july_2019)), Study_area), Study_area)

### Aug 2019 ####
list.dirs("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data")
list.files( "G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248",pattern = "band6.tif$", full.names = TRUE)
band_7_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band7.tif") , Study_area), Study_area)
band_6_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band6.tif") , Study_area), Study_area)
band_5_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band5.tif" ), Study_area), Study_area)
band_4_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band4.tif"), Study_area), Study_area)
band_3_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band3.tif"), Study_area), Study_area)
band_2_aug_2019 <- mask(crop(raster("G:/University Pc HD/Projects/Kirkuk/WorkingDir/Data/LC081690352019081901T1-SC20191101102248/LC08_L1TP_169035_20190819_20190902_01_T1_sr_band2.tif"), Study_area), Study_area)
SAVI_aug_2019 <- mask(crop(((band_5_aug_2019 - band_4_aug_2019) / (band_5_aug_2019 + band_4_aug_2019 + 0.5)) * (1.5), Study_area), Study_area)
BU_aug_2019 <- mask(crop(((band_6_aug_2019 - band_5_aug_2019) / (band_6_aug_2019 + band_5_aug_2019))-((band_5_aug_2019 - band_4_aug_2019) / (band_5_aug_2019 + band_4_aug_2019)), Study_area), Study_area)
NDMI_aug_2019 <- mask(crop(((band_5_aug_2019 - band_6_aug_2019) / (band_6_aug_2019 + band_5_aug_2019)), Study_area), Study_area)
NDVI_aug_2019 <- mask(crop(((band_5_aug_2019 - band_4_aug_2019) / (band_5_aug_2019 + band_4_aug_2019)), Study_area), Study_area)

mean_2019_band_7 <- calc(stack(band_7_june_2019,band_7_july_2019,band_7_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_2019_band7", format="GTiff", overwrite=TRUE)
mean_2019_band_6 <- calc(stack(band_6_june_2019,band_6_july_2019,band_6_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_2019_band6", format="GTiff", overwrite=TRUE)
mean_2019_band_5 <- calc(stack(band_5_june_2019,band_5_july_2019,band_5_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_2019_band5", format="GTiff", overwrite=TRUE)
mean_2019_band_4 <- calc(stack(band_4_june_2019,band_4_july_2019,band_4_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_2019_band4", format="GTiff", overwrite=TRUE)
mean_2019_band_3 <- calc(stack(band_3_june_2019,band_3_july_2019,band_3_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_2019_band3", format="GTiff", overwrite=TRUE)
mean_2019_band_2 <- calc(stack(band_2_june_2019,band_2_july_2019,band_2_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_2019_band2", format="GTiff", overwrite=TRUE)
mean_2019_SAVI <- calc(stack(SAVI_june_2019,SAVI_july_2019,SAVI_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_2019_SAVI", format="GTiff", overwrite=TRUE)
mean_2019_BU <- calc(stack(BU_june_2019,BU_july_2019,BU_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_2019_BU", format="GTiff", overwrite=TRUE)
mean_2019_NDMI <- calc(stack(NDMI_june_2019,NDMI_july_2019,NDMI_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_2019_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_2019_NDMI", format="GTiff", overwrite=TRUE)


# Save all variables


######### Average June 2017, 2018, 2019
# By month
mean_june_band_7 <- calc(stack(band_7_june_2017,band_7_june_2018,band_7_june_2019), fun = mean, na.rm = T)
mean_july_band_7 <- calc(stack(band_7_july_2017,band_7_july_2018,band_7_july_2019), fun = mean, na.rm = T)
mean_aug_band_7 <- calc(stack(band_7_aug_2017,band_7_aug_2018,band_7_aug_2019), fun = mean, na.rm = T)

writeRaster(mean_june_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_june_band_7", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_july_band_7", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_aug_band_7", format="GTiff", overwrite=TRUE)


mean_june_band_6 <- calc(stack(band_6_june_2017,band_6_july_2017,band_6_aug_2017), fun = mean, na.rm = T)
mean_july_band_6 <- calc(stack(band_6_july_2017,band_6_july_2018,band_6_july_2019), fun = mean, na.rm = T)
mean_aug_band_6 <- calc(stack(band_6_aug_2017,band_6_aug_2018,band_6_aug_2019), fun = mean, na.rm = T)

writeRaster(mean_june_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_june_band_6", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_july_band_6", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_aug_band_6", format="GTiff", overwrite=TRUE)

mean_june_band_5 <- calc(stack(band_5_june_2017,band_5_june_2017,band_5_june_2017), fun = mean, na.rm = T)
mean_july_band_5 <- calc(stack(band_5_july_2017,band_5_july_2018,band_5_july_2019), fun = mean, na.rm = T)
mean_aug_band_5 <- calc(stack(band_5_aug_2017,band_5_aug_2018,band_5_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_june_band_5", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_july_band_5", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_aug_band_5", format="GTiff", overwrite=TRUE)

mean_june_band_4 <- calc(stack(band_4_june_2017,band_4_june_2017,band_4_june_2017), fun = mean, na.rm = T)
mean_july_band_4 <- calc(stack(band_4_july_2017,band_4_july_2018,band_4_july_2019), fun = mean, na.rm = T)
mean_aug_band_4 <- calc(stack(band_4_aug_2017,band_4_aug_2018,band_4_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_june_band_4", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_july_band_4", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_aug_band_4", format="GTiff", overwrite=TRUE)


mean_june_band_3 <- calc(stack(band_3_june_2017,band_3_june_2017,band_3_june_2017), fun = mean, na.rm = T)
mean_july_band_3 <- calc(stack(band_3_july_2017,band_3_july_2018,band_3_july_2019), fun = mean, na.rm = T)
mean_aug_band_3 <- calc(stack(band_3_aug_2017,band_3_aug_2018,band_3_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_june_band_3", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_july_band_3", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_aug_band_3", format="GTiff", overwrite=TRUE)


mean_june_band_2 <- calc(stack(band_2_june_2017,band_2_july_2018,band_2_aug_2019), fun = mean, na.rm = T)
mean_july_band_2 <- calc(stack(band_2_july_2017,band_2_july_2018,band_2_july_2019), fun = mean, na.rm = T)
mean_aug_band_2 <- calc(stack(band_2_aug_2017,band_2_aug_2018,band_2_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_june_band_2", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_july_band_2", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_aug_band_2", format="GTiff", overwrite=TRUE)


mean_june_SAVI <- calc(stack(SAVI_june_2017,SAVI_june_2018,SAVI_june_2019), fun = mean, na.rm = T)
mean_july_SAVI <- calc(stack(SAVI_july_2017,SAVI_july_2018,SAVI_july_2019), fun = mean, na.rm = T)
mean_aug_SAVI <- calc(stack(SAVI_aug_2017,SAVI_aug_2018,SAVI_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_june_SAVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_july_SAVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_aug_SAVI", format="GTiff", overwrite=TRUE)


mean_june_BU <- calc(stack(BU_june_2017,BU_june_2018,BU_june_2019), fun = mean, na.rm = T)
mean_july_BU <- calc(stack(BU_july_2017,BU_july_2018,BU_july_2019), fun = mean, na.rm = T)
mean_aug_BU <- calc(stack(BU_aug_2017,BU_aug_2018,BU_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_june_BU", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_july_BU", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_aug_BU", format="GTiff", overwrite=TRUE)


mean_june_NDMI <- calc(stack(NDMI_june_2017,NDMI_june_2018,NDMI_june_2019), fun = mean, na.rm = T)
mean_july_NDMI <- calc(stack(NDMI_july_2017,NDMI_july_2018,NDMI_july_2019), fun = mean, na.rm = T)
mean_aug_NDMI <- calc(stack(NDMI_aug_2017,NDMI_aug_2018,NDMI_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_june_NDMI", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_july_NDMI", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_aug_NDMI", format="GTiff", overwrite=TRUE)


mean_june_NDVI <- calc(stack(NDVI_june_2017,NDVI_june_2018,NDVI_june_2019), fun = mean, na.rm = T)
mean_july_NDVI <- calc(stack(NDVI_july_2017,NDVI_july_2018,NDVI_july_2019), fun = mean, na.rm = T)
mean_aug_NDVI <- calc(stack(NDVI_aug_2017,NDVI_aug_2018,NDVI_aug_2019), fun = mean, na.rm = T)
writeRaster(mean_june_NDVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDVI/mean_june_NDVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_july_NDVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDVI/mean_july_NDVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_aug_NDVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDVI/mean_aug_NDVI", format="GTiff", overwrite=TRUE)







writeRaster(mean_2017_band_7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_2017_band7", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_2017_band6", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_2017_band5", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_2017_band4", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_2017_band3", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_band_2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_2017_band2", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_2017_SAVI", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_2017_BU", format="GTiff", overwrite=TRUE)
writeRaster(mean_2017_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_2017_NDMI", format="GTiff", overwrite=TRUE)




######### Average 2017, 2018, 2019

#### Total 2017,2018,2019 Indicies ####
# Check the extent

mean_17_18_19_band7 <- calc(stack(mean_2019_band_7,mean_2017_band_7,mean_2018_band_7), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band7,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B7_ave/mean_17_18_19_band7", format="GTiff", overwrite=TRUE)
mean_17_18_19_band6 <- calc(stack(mean_2019_band_6,mean_2017_band_6,mean_2018_band_6), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band6,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B6_ave/mean_17_18_19_band6", format="GTiff", overwrite=TRUE)
mean_17_18_19_band5 <- calc(stack(mean_2019_band_5,mean_2017_band_5,mean_2018_band_5), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band5,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B5_ave/mean_17_18_19_band5", format="GTiff", overwrite=TRUE)
mean_17_18_19_band4 <- calc(stack(mean_2019_band_4,mean_2017_band_4,mean_2018_band_4), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band4,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B4_ave/mean_17_18_19_band4", format="GTiff", overwrite=TRUE)
mean_17_18_19_band3 <- calc(stack(mean_2019_band_3,mean_2017_band_3,mean_2018_band_3), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band3,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B3_ave/mean_17_18_19_band3", format="GTiff", overwrite=TRUE)
mean_17_18_19_band2 <- calc(stack(mean_2019_band_2,mean_2017_band_2,mean_2018_band_2), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_band2,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/B2_ave/mean_17_18_19_band2", format="GTiff", overwrite=TRUE)
mean_17_18_19_SAVI <- calc(stack(mean_2019_SAVI,mean_2017_SAVI,mean_2018_SAVI), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_SAVI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/SAVI_ave/mean_17_18_19_SAVI", format="GTiff", overwrite=TRUE)
mean_17_18_19_BU <- calc(stack(mean_2019_BU,mean_2017_BU,mean_2018_BU), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_BU,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/BU_ave/mean_17_18_19_BU", format="GTiff", overwrite=TRUE)
mean_17_18_19_NDMI <- calc(stack(mean_2019_NDMI,mean_2017_NDMI,mean_2018_NDMI), fun = mean, na.rm = T)
writeRaster(mean_17_18_19_NDMI,filename="G:/University Pc HD/Projects/Kirkuk/WorkingDir/NDMI_ave/mean_17_18_19_NDMI", format="GTiff", overwrite=TRUE)

