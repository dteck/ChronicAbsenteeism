#--Install and Load Required Libraries--
if (!require(readxl)) install.packages('readxl')
library(readxl)#enables readign excel files
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse) #enables data transoformation
if (!require(httr)) install.packages('httr')
library(httr) #enables web crawling
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2) #enables web crawling
library(spdep) #poly2nb
library(mapview) #mapshot
library(tigris) #get school distrcits
#webshot::install_phantomjs()
library(leaflet)
library(leaflet.extras)
#------

#ask users to set working directory
setwd(choose.dir())
workdir<-getwd()
#------

#Pull District report for 13-14, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2014/1314_LRC_DISTRICT.xls', write_disk(tf <- tempfile(fileext = ".xls")))
RC1314 <- read_excel(tf)
RC1314<-RC1314[,c("District IRN","Enrollment","Attendance 2013-14","Chronic Absenteeism  Rate 2013-14")]
#------

#Pull District report for 14-15, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2015/1415_LRC_DISTRICT.xls', write_disk(tf <- tempfile(fileext = ".xls")))
RC1415 <- read_excel(tf)
RC1415<-RC1415[,c("District IRN","Enrollment","Attendance 2014-15","Chronic Absenteeism Rate 2014-15")]
#------

#Pull District report for 15-16, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2016/DISTRICT_HIGH_LEVEL.xls', write_disk(tf <- tempfile(fileext = ".xls")))
RC1516 <- read_excel(tf)
RC1516<-RC1516[,c("District IRN","Enrollment 2015-2016","Attendance Rate 2015-2016","Chronic Absenteeism Percent 2015-2016")]
#------

#Pull District report for 16-17, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2017/1617_DISTRICT_HIGH_LEVEL_FINAL_W_ENRL.xls', write_disk(tf <- tempfile(fileext = ".xls")))
RC1617 <- read_excel(tf)
RC1617<-RC1617[,c("District IRN","Enrollment 2016-2017","Attendance Rate 2016-2017","Chronic Absenteeism Percent 2016-2017")]
#------

#Pull District report for 17-18, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2018/DISTRICT_HIGH_LEVEL_1718.xlsx', write_disk(tf <- tempfile(fileext = ".xlsx")))
RC1718 <- read_excel(tf)
RC1718<-RC1718[,c("District IRN","Enrollment 2017 - 2018","Attendance Rate 2017-2018","Chronic Absenteeism Percent 2017-2018")]
#------

#Pull District report for 18-19, and filter down
GET('https://reportcardstorage.education.ohio.gov/data-download-2019/DISTRICT_HIGH_LEVEL_1819.xlsx', write_disk(tf <- tempfile(fileext = ".xlsx")))
RC1819 <- read_excel(tf, sheet = "DISTRICT_OVERVIEW")
RC1819<-RC1819[,c("District IRN","Enrollment 2018-2019","Attendance Rate 2018-2019","Chronic Absenteeism Percent 2018-2019")]
#------

#Combine all Chronic Absenteeism data
AllYears<-merge.data.frame(RC1314, RC1415, by="District IRN", all = TRUE)
AllYears<-merge.data.frame(AllYears, RC1516, by="District IRN", all = TRUE)
AllYears<-merge.data.frame(AllYears, RC1617, by="District IRN", all = TRUE)
AllYears<-merge.data.frame(AllYears, RC1718, by="District IRN", all = TRUE)
AllYears<-merge.data.frame(AllYears, RC1819, by="District IRN", all = TRUE)
#------

#Select absenteeism columns and drop other data, rename columns
AllYears_absen<-str_detect(names(AllYears), "Abse", negate = FALSE)
AllYears_absen[1]<-TRUE
All_Chronic<-AllYears[,AllYears_absen]
rm(AllYears_absen)
colnames(All_Chronic)<-c("IRN", "CA1314", "CA1415", "CA1516", "CA1617", "CA1718", "CA1819")
#------

#Convert all columns to numeric
for(i in colnames(All_Chronic)){
  print(i)
  All_Chronic[[i]]<-as.numeric(as.character(All_Chronic[[i]]))
}
#------

#Gather summaries for each of the years, skip the IRN column
Summaries<-data.frame(Min=integer(), FirstQ=integer(), Median=integer(), Mean=integer(),ThirdQ=integer(), Max=integer(), Na=integer())

for( i in names(All_Chronic)){
  print(i)
  if(i=="IRN"){
    print('skip')
  } else{
    a<-data.frame(Min=summary(All_Chronic[[i]])[[1]], 
                  FirstQ=summary(All_Chronic[[i]])[[2]],
                  Median=summary(All_Chronic[[i]])[[3]],
                  Mean=summary(All_Chronic[[i]])[[4]],
                  ThirdQ=summary(All_Chronic[[i]])[[5]], 
                  Max=summary(All_Chronic[[i]])[[6]], 
                  Na=summary(All_Chronic[[i]])[[7]])
    Summaries<-rbind.data.frame(Summaries,a)
  }
}
#------

#Add an index and run a linear model along the median, calcualte IQR
Summaries$Index<-seq_along(Summaries$Min)
Sum_mean<-lm(Summaries$Mean~Summaries$Index)
Summaries$IQR<-Summaries$ThirdQ-Summaries$FirstQ
#------

#Generate boxplots for all years and add a trend line
ggplot() +
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1314, x=names(All_Chronic)[2])) +
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1415, x=names(All_Chronic)[3])) + 
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1516, x=names(All_Chronic)[4])) + 
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1617, x=names(All_Chronic)[5])) + 
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1718, x=names(All_Chronic)[6])) + 
  geom_boxplot(data = All_Chronic, aes_(y=All_Chronic$CA1819, x=names(All_Chronic)[7])) +
  geom_point(aes(x="CA1920",y=(Sum_mean[["coefficients"]][[1]]+Sum_mean[["coefficients"]][[2]]*(7))), size=4, color="red") +
  labs(title=paste("Distribution of Chronic Absenteeism Percentages by School Year, with Mean Trendline"),
       y= "Percentage", x="School Year",
       tag = paste("Y=",round(Sum_mean[["coefficients"]][[1]],4),"+",
                   round(Sum_mean[["coefficients"]][[2]],4),"x",
                   "    R^2: ",round(summary(Sum_mean)$r.squared,2),sep="")) +
  geom_abline(intercept = Sum_mean[["coefficients"]][[1]], 
              slope = Sum_mean[["coefficients"]][[2]],
              color="red", linetype="dashed", size=1) +
  theme(plot.tag.position = c(0.10, 0.01))
#------

#Pull typology report for 2017, extract defintions and merge
GET('http://education.ohio.gov/getattachment/Topics/Data/Frequently-Requested-Data/Typology-of-Ohio-School-Districts/2013-School-District-Typology.xlsx.aspx', write_disk(tf <- tempfile(fileext = ".xlsx")))
Typology <- read_excel(tf)
Typology_def <- read_excel(tf, sheet = "Exemplar Districts by Typology")
Typology<-Typology[,c("IRN","District Name","County","2013 Typology")]
Typology$IRN<-as.numeric(Typology$IRN)
Typology_def[is.na(Typology_def)] <- 0
Typology_def<-subset.data.frame(Typology_def,  str_detect(Typology_def$`Exemplar Districts by 2013 Typology Code`, "Typ", negate = FALSE))
Typology_def<-Typology_def[,c(1)]
#Typology_def$Index<-seq_along(Typology_def$`Exemplar Districts by 2013 Typology Code`)
Typology_def<-data.frame(t(data.frame(str_split(Typology_def$`Exemplar Districts by 2013 Typology Code`, "-"))))
Typology_def2<-data.frame(t(data.frame(str_split(Typology_def$X1, " "))))
Typology_def<-cbind.data.frame(Typology_def, Typology_def2$X3)
Typology_def$X1<-NULL
rownames(Typology_def)<-NULL
colnames(Typology_def)<-c("Grouping", "Desc", "2013 Typology")
Typology<-merge(Typology, Typology_def, by="2013 Typology")
rm(Typology_def, Typology_def2)
#------

#Merge typology and All_Chronic Absenteeism
All_Chronic<-merge(All_Chronic,Typology, by="IRN", all.x=TRUE)
#------

#Create list to subset typologies
CA_Typologies<-list()
CA_typ<-unique(unlist(All_Chronic$`2013 Typology`))
CA_typ<-CA_typ[!is.na(CA_typ)] #drop NA
for(i in seq(1,length(CA_typ),1)){
  print(CA_typ[i])
  CA_Typologies[[i]]<-subset.data.frame(All_Chronic, All_Chronic$`2013 Typology`==CA_typ[i])
}

#Build linear models for each typology and store slope/intercept
for (i in seq(1,length(CA_Typologies),1)){
  a<-lm(c(mean(CA_Typologies[[i]]$CA1314),
          mean(CA_Typologies[[i]]$CA1415),
          mean(CA_Typologies[[i]]$CA1516),
          mean(CA_Typologies[[i]]$CA1617),
          mean(CA_Typologies[[i]]$CA1718),
          mean(CA_Typologies[[i]]$CA1819, na.rm = TRUE))~c(1:6))
  CA_Typologies[[i]]$b<-a$coefficients[[1]]
  CA_Typologies[[i]]$mx<-a$coefficients[[2]]
  CA_Typologies[[i]]$r2<-summary(a)$r.squared
}
#------

#Create a year to year boxplot for each typology
CA_Typologies_Boxplot<-list()
for (i in seq(1,length(CA_Typologies),1)){
  CA_Typologies_Boxplot[[i]]<-ggplot() +
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1314, x=names(CA_Typologies[[i]])[2])) +
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1415, x=names(CA_Typologies[[i]])[3])) + 
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1516, x=names(CA_Typologies[[i]])[4])) + 
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1617, x=names(CA_Typologies[[i]])[5])) + 
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1718, x=names(CA_Typologies[[i]])[6])) + 
    geom_boxplot(data = CA_Typologies[[i]], aes_(y=CA_Typologies[[i]]$CA1819, x=names(CA_Typologies[[i]])[7])) +
    geom_point(aes_(x="CA1920",y=(CA_Typologies[[i]]$b[1]+CA_Typologies[[i]]$mx[1]*(7))), size=4, color="red") +
    labs(title=paste("Distribution of Chronic Absenteeism Percentages by School Year  [ Typology: ", 
                     CA_Typologies[[i]]$`2013 Typology`[1], "-",CA_Typologies[[i]]$Grouping[1], "-", 
                     CA_Typologies[[i]]$Desc[1],"]"), y= "Percentage", x="School Year",
         tag = paste("Y=",round(CA_Typologies[[i]]$b[1],4),"+", 
                     round(CA_Typologies[[i]]$mx[1],4),"x",
                     "   R^2: ",round(CA_Typologies[[i]]$r2[1],2), sep="")) +
    geom_abline(intercept = CA_Typologies[[i]]$b[1], slope = CA_Typologies[[i]]$mx[1], color="red", linetype="dashed", size=1) +
    theme(plot.tag.position = c(0.15, 0.01))
}
#------

#Create list to subset counties
CA_Counties<-list()
CA_count<-unique(unlist(All_Chronic$County))
CA_count<-CA_count[!is.na(CA_count)] #drop NA
for(i in seq(1,length(CA_count),1)){
  print(CA_count[i])
  CA_Counties[[i]]<-subset.data.frame(All_Chronic, All_Chronic$County==CA_count[i])
}
#------

#Build linear models for each county and store slope/intercept
for (i in seq(1,length(CA_Counties),1)){
  a<-lm(c(mean(CA_Counties[[i]]$CA1314),
          mean(CA_Counties[[i]]$CA1415),
          mean(CA_Counties[[i]]$CA1516),
          mean(CA_Counties[[i]]$CA1617),
          mean(CA_Counties[[i]]$CA1718),
          mean(CA_Counties[[i]]$CA1819))~c(1:6))
  CA_Counties[[i]]$b<-a$coefficients[[1]]
  CA_Counties[[i]]$mx<-a$coefficients[[2]]
  CA_Counties[[i]]$r2<-summary(a)$r.squared
}
#------

#Create a year to year boxplot for each County
CA_Counties_Boxplot<-list()
for (i in seq(1,length(CA_Counties),1)){
  CA_Counties_Boxplot[[i]]<-ggplot() +
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1314, x=names(CA_Counties[[i]])[2])) +
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1415, x=names(CA_Counties[[i]])[3])) + 
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1516, x=names(CA_Counties[[i]])[4])) + 
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1617, x=names(CA_Counties[[i]])[5])) + 
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1718, x=names(CA_Counties[[i]])[6])) + 
    geom_boxplot(data = CA_Counties[[i]], aes_(y=CA_Counties[[i]]$CA1819, x=names(CA_Counties[[i]])[7])) +
    geom_point(aes_(x="CA1920",y=(CA_Counties[[i]]$b[1]+CA_Counties[[i]]$mx[1]*(7))), size=4, color="red") +
    labs(title=paste("Distribution of Chronic Absenteeism Percentages by School Year  [ County: ",
                     CA_Counties[[i]]$County[1],"]"), y= "Percentage", x="School Year",
         tag = paste("Y=",round(CA_Counties[[i]]$b[1],4),"+", round(CA_Counties[[i]]$mx[1],4),"x",
                     "   R^2: ",round(CA_Counties[[i]]$r2[1],2),sep="")) +
    geom_abline(intercept = CA_Counties[[i]]$b[1], slope = CA_Counties[[i]]$mx[1], color="red", linetype="dashed", size=1) +
    theme(plot.tag.position = c(0.15, 0.01))
}
#------

#Build linear models for each district and store slope/intercept
All_Chronic<-subset.data.frame(All_Chronic,All_Chronic$CA1819>=0) #remove NAs
for (i in seq(1,length(All_Chronic$IRN),1)){
  a<-lm(c(mean(All_Chronic$CA1314[i]),
          mean(All_Chronic$CA1415[i]),
          mean(All_Chronic$CA1516[i]),
          mean(All_Chronic$CA1617[i]),
          mean(All_Chronic$CA1718[i]),
          mean(All_Chronic$CA1819))~c(1:6))
  All_Chronic$b[i]<-a$coefficients[[1]]
  All_Chronic$mx[i]<-a$coefficients[[2]]
  All_Chronic$r2[i]<-summary(a)$r.squared
}
#------

#Create a year to year line plot for each district
CA_district_plot<-list()
for (i in seq(1,length(All_Chronic$IRN),1)){
  a<-data.frame(y=c(All_Chronic$CA1314[i], 
                    All_Chronic$CA1415[i], 
                    All_Chronic$CA1516[i], 
                    All_Chronic$CA1617[i], 
                    All_Chronic$CA1718[i], 
                    All_Chronic$CA1819[i]), x=c(1:6))
  a$b<-All_Chronic$b[i]
  a$mx<-All_Chronic$mx[i]
  a$r2<-All_Chronic$r2[i]
  
  CA_district_plot[[i]]<-ggplot() +
  geom_line(data = a, aes(x=x, y=y), size=1) +
  geom_point(data = a, aes(x=x, y=y), size=3) +
  geom_abline(intercept = a$b[1], slope = a$mx[1], color="red", linetype="dashed", size=1) +
  geom_point(aes_(x=7, y=(a$b[1]+a$mx[1]*(7))), size=4, color="red") +
  labs(title=paste("Chronic Absenteeism Percentages by School Year  [ IRN: ", 
                   str_pad(as.character(All_Chronic$IRN[i]),6,side='left',pad = "0"),
                   "- District: ",All_Chronic$`District Name`[i]," ]"),
       y= "Percentage", x="School Year",
       tag = paste("Y=",round(a$b[1],4),"+", round(a$mx[1],4),"x",
                   "   R^2: ",round(a$r2[1],2),sep="")) +
  scale_x_continuous(breaks = c(1:7), labels = c(names(All_Chronic[,c(2:7)]),"CA1920")) +
  theme(plot.tag.position = c(0.15, 0.01))
  rm(a)
}
#------

#Calcualte 1718 to 1819 absolute change for each district
All_Chronic$Delta<-All_Chronic$CA1819-All_Chronic$CA1718
#------

#pull shapefiles for Ohio unified school districts
OHSD<-school_districts(39, year = 2019)
#------

#Create LEADID column for crosswalk
OHSD$LEAID<-paste(as.character(OHSD$STATEFP),as.character(OHSD$UNSDLEA),sep="")
OHSD$LEAID<-as.numeric(OHSD$LEAID)
#------

#Read in IRN/LEADID crosswalk
Crosswalk<-read_excel('./NCES_Crosswalk_LEA_County.xlsx')
#------

#Merge crosswalk data with polygons
OHSD_cross<-geo_join(OHSD, Crosswalk, by_sp= 'LEAID', by_df = 'LEA_ID', how='left')
#------

#Merge CA data by IRN with polygons, drop extra data
OHSD_cross$IRN<-as.numeric(OHSD_cross$IRN)
OHSD_cross<-geo_join(OHSD_cross, All_Chronic, by_sp= 'IRN', by_df = 'IRN', how='left')
OHSD_cross<-OHSD_cross[,c("GEOID", "NAME", "LEAID", "IRN", "CA1314", "CA1415", "CA1516", "CA1617", "CA1718", "CA1819", "X2013.Typology", "County", "Grouping", "Desc", "b", "mx", "r2", "Delta")]
#------

#add colors for increae or decrease in CA, drop NAs
OHSD_cross$color<-"black"
OHSD_cross<-subset.data.frame(OHSD_cross, is.na(OHSD_cross$Delta) ==FALSE)
for(i in seq(1,length(OHSD_cross$Delta),1)){
  if(OHSD_cross$Delta[i] >=0){
    print("bad")
    OHSD_cross$color[i]<-"red"
  } else {
    print("good")
    OHSD_cross$color[i]<-"green"
  }
}
#------

#build list of geogrpahic neighbors
OHSD_nei<-poly2nb(OHSD_cross, queen = TRUE)
#------

#convert neighbor index vlaues to GEOIDs
for (n in seq(1,length(OHSD_nei),1)){
  OHSD_nei[[n]]<-OHSD_cross$GEOID[OHSD_nei[[n]]] 
}
#------

#create subsets of referent and neighbors, color the referent block grey
subsets<-list()
for (n in seq(1,length(OHSD_nei),1)){
  subsets[[n]]<-subset.data.frame(OHSD_cross, OHSD_cross$GEOID==OHSD_cross$GEOID[n] | OHSD_cross$GEOID %in% OHSD_nei[[n]])
  subsets[[n]]$color[which(subsets[[n]]$GEOID==OHSD_cross$GEOID[n])]<-"black"
}
#------

#Create leaflet maps of each district and its neighbors
OHSD_Maps<-list()
for(i in seq(1,length(subsets),1)){
  print(i)
  OHSD_Maps[[i]]<-map <- leaflet() %>% 
    enableTileCaching() %>% 
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(minZoom=1, maxZoom=100))
  
  OHSD_Maps[[i]]<- OHSD_Maps[[i]] %>% addPolygons(data=subsets[[i]],
                            weight = 1,
                            fill = TRUE,
                            fillOpacity = 0.4,
                            fillColor = subsets[[i]]$color,
                            stroke = TRUE,
                            color = "black",
                            label = subsets[[i]]$NAME,
                            highlight = highlightOptions(weight = 5,
                                                         color = "Red",
                                                         bringToFront = TRUE))
}


#check/create queue, then export image of map
if(file.exists('./Map_done.rds')==TRUE){
  Map_done<-readRDS('./Map_done.rds')
} else{
  Map_done<-data.frame(IRN=OHSD_cross$IRN,Done=as.numeric(0))
  saveRDS(Map_done, './Map_done.rds')
}

for(i in seq(1,length(OHSD_Maps),1)){
  print(i)
  if(Map_done$Done[i]==0){
    mapshot(OHSD_Maps[[i]], file = paste("./",
                                             str_pad(as.character(OHSD_cross$IRN[i]),6,side='left',pad = "0"),
                                             "_Neighbors.png", sep = ""))
    Map_done$Done[i]<-1
  } else{
    print("skip")
  }
  if(i%%10==0){
    saveRDS(Map_done, './Map_done.rds')
  }
  }
#------



 




