library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd(dirname(path.expand("~")))

DataPath <- file.path(paste(getwd(),"Dropbox/BJ Customs Evasion", sep="/"))
CodePath <- file.path(paste(getwd(),"Documents/Comtrade", sep = "/"))

load(paste(DataPath,"Raw Data/Comtrade/Yearly/y_hs12/y_2012_hs12.Rda", sep = "/"))
y_2012_hs12 <- as.data.table(y_2012_hs12)

y_2012_hs12 <- subset(y_2012_hs12,substr(y_2012_hs12$`Commodity Code`,1,1)=='0')
y_2012_hs12 <- y_2012_hs12[grep("^[A-M]", y_2012_hs12$Reporter)]
y_2012_hs12 <- y_2012_hs12[grep("^[A-M]", y_2012_hs12$Partner)]

load(paste(DataPath,"Raw Data/Comtrade/Yearly/y_hs12/y_2013_hs12.Rda", sep = "/"))
y_2013_hs12 <- as.data.table(y_2013_hs12)

y_2013_hs12 <- subset(y_2013_hs12,substr(y_2013_hs12$`Commodity Code`,1,1)=='0')
y_2013_hs12 <- y_2013_hs12[grep("^[A-M]", y_2013_hs12$Reporter)]
y_2013_hs12 <- y_2013_hs12[grep("^[A-M]", y_2013_hs12$Partner)]

load(paste(DataPath,"Raw Data/Comtrade/Yearly/y_hs12/y_2014_hs12.Rda", sep = "/"))
y_2014_hs12 <- as.data.table(y_2014_hs12)

y_2014_hs12 <- subset(y_2014_hs12,substr(y_2014_hs12$`Commodity Code`,1,1)=='0')
y_2014_hs12 <- y_2014_hs12[grep("^[A-M]", y_2014_hs12$Reporter)]
y_2014_hs12 <- y_2014_hs12[grep("^[A-M]", y_2014_hs12$Partner)]

load(paste(DataPath,"Raw Data/Comtrade/Yearly/y_hs12/y_2015_hs12.Rda", sep = "/"))
y_2015_hs12 <- as.data.table(y_2015_hs12)

y_2015_hs12 <- subset(y_2015_hs12,substr(y_2015_hs12$`Commodity Code`,1,1)=='0')
y_2015_hs12 <- y_2015_hs12[grep("^[A-M]", y_2015_hs12$Reporter)]
y_2015_hs12 <- y_2015_hs12[grep("^[A-M]", y_2015_hs12$Partner)]

load(paste(DataPath,"Raw Data/Comtrade/Yearly/y_hs12/y_2016_hs12.Rda", sep = "/"))
y_2016_hs12 <- as.data.table(y_2016_hs12)

y_2016_hs12 <- subset(y_2016_hs12,substr(y_2016_hs12$`Commodity Code`,1,1)=='0')
y_2016_hs12 <- y_2016_hs12[grep("^[A-M]", y_2016_hs12$Reporter)]
y_2016_hs12 <- y_2016_hs12[grep("^[A-M]", y_2016_hs12$Partner)]

hs12 <- do.call("rbind", list(y_2012_hs12, y_2013_hs12, y_2014_hs12, y_2015_hs12, y_2016_hs12))

rm(y_2012_hs12, y_2013_hs12, y_2014_hs12, y_2015_hs12, y_2016_hs12)

hs12im <- hs12[`Trade Flow Code`==1]

hs12im[ ,`:=`(Classification = NULL, Year = NULL, 
                  `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                  `Reporter ISO` = NULL, `Partner ISO` = NULL,
                  `Qty Unit Code` = NULL,  Flag = NULL
                  )]


hs12im <- rename(hs12im, "Import Value" = "Trade Value (US$)") 
hs12im <- rename(hs12im, "Import Qty Unit" = "Qty Unit")
hs12im <- rename(hs12im, "Import Qty" = "Qty")
hs12im <- rename(hs12im, "Import Netweight (kg)" = "Netweight (kg)")


hs12ex <- hs12[`Trade Flow Code`==2]

hs12ex[ ,`:=`(Classification = NULL, Year = NULL, 
                  `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                  `Reporter ISO` = NULL, `Partner ISO` = NULL,
                  `Qty Unit Code` = NULL,  Flag = NULL
                  )]

hs12ex <- rename(hs12ex, "Export Value" = "Trade Value (US$)")
hs12ex <- rename(hs12ex, "Export Qty Unit" = "Qty Unit")
hs12ex <- rename(hs12ex, "Export Qty" = "Qty")
hs12ex <- rename(hs12ex, "Export Netweight (kg)" = "Netweight (kg)")

hs12 <- merge(hs12im, hs12ex, 
             by.x=c("Period", "Aggregate Level", 
                    "Reporter Code", "Reporter", "Partner Code", "Partner", 
                    "Commodity Code", "Commodity"),
             by.y=c("Period", "Aggregate Level", 
                    "Partner Code", "Partner", "Reporter Code", "Reporter", 
                    "Commodity Code", "Commodity"),all=TRUE)

hs12 <- rename(hs12, "Importer" = "Reporter")
hs12 <- rename(hs12, "Exporter" = "Partner")

hs12$Raw_gap = hs12$`Export Value` - hs12$`Import Value`

hs12$Log_gap = log(hs12$`Export Value`) - log(hs12$`Import Value`)

hs12$Gap_ratio = hs12$`Raw_gap`/(hs12$`Import Value` + hs12$`Export Value`)

hs12$`Export Netweight (kg)` <- as.numeric(hs12$`Export Netweight (kg)`)
hs12$`Import Netweight (kg)` <- as.numeric(hs12$`Import Netweight (kg)`)

hs12$Qty_raw_gap = hs12$`Export Netweight (kg)` - hs12$`Import Netweight (kg)`

hs12$Qty_log_gap = log(hs12$`Export Netweight (kg)`) - log(hs12$`Import Netweight (kg)`)

hs12$Qty_gap_ratio = hs12$`Qty_raw_gap`/(hs12$`Export Netweight (kg)` + hs12$`Import Netweight (kg)`)

save(hs12,file = paste(DataPath,"Analysis Data","hs12.Rda", sep = "/"))
rm(hs12, hs12ex, hs12im)

#Histogram by code: 2, 4, 6?



