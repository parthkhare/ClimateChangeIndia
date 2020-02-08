library(cowplot); library(googleway); library(ggrepel);library(data.table)
library(ggspatial); library(sf); library(rnaturalearth); library(ggmap)
library(rnaturalearthdata); library(lubridate); library(Hmisc); library(readxl);
options("scipen"=100, "digits"=4)


# Weather Station Geophysics
# ========================================================================================
load("/Users/parthkhare/Desktop/MOF Team CEA All Projects 2016-18/Rainfall Index/Data/Processed/District from source_Nov/Station Inside India/Rfall_30Nov.RData")


# Station-year Stats
# -------------------------------
# year
length(unique(rfm1$yr))         # 68
length(unique(rfm1$yr_est))     # 1024

# Station
length(unique(rfm1$MRF_ID))     # 302995
length(unique(rfm1$station))    # 9021 


# Lat-longs: Locations
length(unique(paste0(rfm1$lat,'-',rfm1$lon)))  # 8483 
length(unique(paste0(rfm1$lt,'-',rfm1$lg)))    # 336  => districts 1961 

# Lat-longs: Locations
length(unique(paste0(rfm1$lat,'-',rfm1$lon,"-",rfm1$yr)))  # 8483 
length(unique(paste0(rfm1$station,"-",rfm1$yr)))  # 8483 

# Station Present Across Years
# -------------------------------
rfm1$Stnll <- paste0(substr(as.character(rfm1$lat),1,6),"-",
                      substr(as.character(rfm1$lon),1,6))
ggplot(rfm1, aes(x=Stnll))

rfm1 <- data.table(rfm1)
stn_frq <- rfm1[,.(stn_py = length(unique(Stnll))), by=yr]
ggplot(data=stn_frq, aes(x=yr, y=stn_py)) + 
  geom_bar(stat = 'identity', fill='darkred')
  

# Station ID common across years
stn_yr <- rfm1[,.(yr = unique(yr)), by=Stnll]

# Groupby: Ll present across years
stn_yr_frq <- stn_yr[,.(num_yrs = length(unique(yr))), by=Stnll]
plot(density(stn_yr_frq$num_yrs))
ggplot(data=stn_yr_frq, aes(x=Stnll, y=num_yrs)) + 
  geom_bar(stat = 'identity', fill='darkred')

# Frequency
table(stn_yr_frq$num_yrs)
ggplot(data=stn_yr_frq, aes(x=num_yrs, y=Stnll)) + 
  geom_bar(stat = 'identity', fill='darkred')

# Proxy Data for checks
# -------------------------------
stn_yr_frq$yr_frq <- cut(stn_yr_frq$num_yrs, breaks = seq(0,70, by= 10), 
    labels = c("<10 years","10-20 years","20-30 years","30-40 years",
               "40-50 years", "50-60 years", "60 + years"))
stn_yr_frq[,counts := .N, by=yr_frq]

# Proxy Data for checks
# -------------------------------
ggplot(stn_yr_frq, aes(x=yr_frq)) + geom_bar(fill = "#0073C2FF") +
  coord_flip() + xlab("Precip Stations") + ylab("Number of years")
  stat_count(aes(label = ..count..), geom = "text", hjust = 1.1) +
  ggtitle("Stations Consistent of years")
  
# Station by Year Panel Matrix
# -------------------------------
s <- as.data.frame(table(rfm1$station, rfm1$yr))
s1 <- reshape(s, direction = 'wide', idvar = "Var1", timevar = 'Var2')

# Proxy Data for checks
# -------------------------------
stn_yr_frq[,uniqueN(Stnll[num_yrs>60])]
stn_yr_frq[,uniqueN(Stnll[num_yrs>50])]
stn_yr_frq[,uniqueN(Stnll[num_yrs>30])]
# ========================================================================================



# Spatial Coverage
# ========================================================================================
# GGMAP
# -------------------------------
library(ggmap)
#Set your API Key
ggmap::register_google(key = "AIzaSyB09J8woJyZjs9mu2tIv-AnFqj3YWzXSPo")

# India Extent: Method 1
# --------------------
bha <- c(68.18625, 6, 97.41529, 37)                 # Optimal
# Black n White Satellite
ww3 <- get_map(location=bha, color = "bw", zoom=4, maptype = "satellite")
Ind3 <- ggmap(ww3, extent = "device",legend = "bottomright", size = c(500,600)) 
# Centroids
Ind3 + geom_point(aes(x = lg, y = lt), data = rfm1, alpha = .5, 
                         color="darkred", size = 0.4) + 
  ggtitle("District Centroids")

# District Centroids: Stamen Method 2
# -------------------------------
myLocation <- c(lon = 82, lat = 21.5)
myMap <- get_map(location=myLocation,source="stamen", maptype="toner", 
                 crop=T, zoom=4)
ggmap(myMap) + geom_point(aes(x = lg, y = lt), data = rfm1,
                          alpha = .5, color="darkred", size = 0.4) +
  ggtitle("District Centroids")
# ========================================================================================


# Distribution
# ========================================================================================
# Decadal Variable
# -------------------------------
rfm1$decade <-ifelse(rfm1$yr %in% c(1950:1959), "1950's",
               ifelse(rfm1$yr %in% c(1960:1969), "1960's",
                ifelse(rfm1$yr %in% c(1970:1979), "1970's",
                 ifelse(rfm1$yr %in% c(1980:1989), "1980's",
                  ifelse(rfm1$yr %in% c(1990:1999), "1990's",
                   ifelse(rfm1$yr %in% c(2000:2009), "2000's", "2010's"))))))


# Distribution Plot
# -------------------------------
rfm1 <- data.table(rfm1)
ddec <- rfm1[,.(rfav = sum(total.1digit)/12), by=.(lat,lon,decade)]
ggplot(ddec, aes(rfav, colour = decade)) + #geom_density()
  stat_density(geom = "line", position = "identity") + xlim(0, 4000) +
  xlab("Rainfall mm") + ylab("Frequency")

ggplot(ddec, aes(rfav, colour=decade, fill=decade)) + 
  geom_histogram(binwidth=35) + xlim(0, 4000) +
  xlab("Rainfall mm") + ylab("Frequency")


# Distribution
# ========================================================================================
load('/Users/parthkhare/Desktop/MOF Team CEA All Projects 2016-18/Rainfall Index/Data/Processed/District from source_Nov/Station Inside India/Stemp_Daily_16Dec.RData')



load("/Users/parthkhare/Desktop/MOF Team CEA All Projects 2016-18/Rainfall Index/Data/Processed/District from source_Nov/Station Inside India/Stemp_Daily_16Dec.RData")
dim(clm1)
clm1 <- data.table(clm1)
clm1$Stnll <- paste0(substr(as.character(clm1$lat),1,7),"-",
                     substr(as.character(clm1$lon),1,7))

# Number of Stations across years
stn_frq_tp <- clm1[,.(stn_py = length(unique(Stnll))), by=yr]
ggplot(data=stn_frq_tp, aes(x=yr, y=stn_py)) + 
  geom_bar(stat = 'identity', fill='darkred') +
  ggtitle("Number of Ground Temp Stations over years") +
  xlab("Years") + ylab("Number of Precip Stations")



t1970 <- subset(clm1, yr==1970)
t2005 <- subset(clm1, yr==2005)

bha <- c(68.18625, 6, 97.41529, 37)                 # Optimal
# Black n White Satellite
ww3 <- get_map(location=bha, color = "bw", zoom=5, maptype = "terrain")
Ind3 <- ggmap(ww3, extent = "device",legend = "bottomright", size = c(500,600))

# Plot Stations: 1960
Ind3 + geom_point(aes(x = lon, y = lat), data = t2005, alpha = .6,
                  color="darkred", size = 0.15) 


clm1$decade <- ifelse(clm1$yr %in% c(1960:1969), "1960's",
                      ifelse(clm1$yr %in% c(1970:1979), "1970's",
                             ifelse(clm1$yr %in% c(1980:1989), "1980's",
                                    ifelse(clm1$yr %in% c(1990:1999), "1990's",
                                           ifelse(clm1$yr %in% c(2000:2009), "2000's", "2010's")))))

# Deacdal July
ddec_temp_jl <- clm1[,.(tp_jlav = Jul.av), by=.(lat,lon,decade)]
ggplot(ddec_temp_jl, aes(tp_jlav, colour = decade)) + 
  stat_density(geom = "line", position = "identity") + 
  xlab("Temperature") + ylab("Frequency") + ggtitle("Summer Temperature  Distribution")


