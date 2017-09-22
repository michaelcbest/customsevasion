library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd(dirname(path.expand("~")))

DataPath <- file.path(paste(getwd(),"Dropbox/Customs Evasion/Raw Data/Comtrade/yearly", sep="/"))

#Michael's computer datapath for saving subsets of data, to keep files from getting too big
DataPath2 <- file.path(paste(getwd(),"Documents/hs2012", sep="/"))

#####REMOVE REPEATED VARIABLES#####

load(paste(DataPath,"y_hs12/y_2012_hs12.Rda", sep = "/"))

y_2012_hs12[ ,`:=`(Classification = NULL, Year = NULL, 
                   `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                   `Reporter ISO` = NULL, `Partner ISO` = NULL,
                   `Qty Unit Code` = NULL,  Flag = NULL
)]

load(paste(DataPath,"y_hs12/y_2013_hs12.Rda", sep = "/"))
y_2013_hs12 <- as.data.table(y_2013_hs12)

y_2013_hs12[ ,`:=`(Classification = NULL, Year = NULL, 
                   `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                   `Reporter ISO` = NULL, `Partner ISO` = NULL,
                   `Qty Unit Code` = NULL,  Flag = NULL
)]

hs12_1213 <- do.call("rbind", list(y_2012_hs12, y_2013_hs12))

load(paste(DataPath,"y_hs12/y_2014_hs12.Rda", sep = "/"))
y_2014_hs12 <- as.data.table(y_2014_hs12)

y_2014_hs12[ ,`:=`(Classification = NULL, Year = NULL, 
                   `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                   `Reporter ISO` = NULL, `Partner ISO` = NULL,
                   `Qty Unit Code` = NULL,  Flag = NULL
)]

hs12_1213 <- do.call("rbind", list(hs12_1213, y_2014_hs12))

save(hs12_1213,file = paste(DataPath2, "hs12_12-14.Rda", sep = "/"))

load(paste(DataPath,"y_hs12/y_2015_hs12.Rda", sep = "/"))
y_2015_hs12 <- as.data.table(y_2015_hs12)

y_2015_hs12[ ,`:=`(Classification = NULL, Year = NULL, 
                   `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                   `Reporter ISO` = NULL, `Partner ISO` = NULL,
                   `Qty Unit Code` = NULL,  Flag = NULL
)]

load(paste(DataPath,"y_hs12/y_2016_hs12.Rda", sep = "/"))
y_2016_hs12 <- as.data.table(y_2016_hs12)


y_2016_hs12[ ,`:=`(Classification = NULL, Year = NULL, 
                   `Period Desc.` = NULL, `Is Leaf Code` = NULL, 
                   `Reporter ISO` = NULL, `Partner ISO` = NULL,
                   `Qty Unit Code` = NULL,  Flag = NULL
)]

hs12_15_16 <- do.call("rbind", list(y_2015_hs12, y_2016_hs12))

Imports_1516 <- hs12_15_16[`Trade Flow Code`==1]
Exports_1516 <- hs12_15_16[`Trade Flow Code`==2]

rm(hs12_15_16)

save(Imports_1516,file = paste(DataPath2,"Imports1516.Rda", sep = "/"))
rm(Imports_1516)

save(Exports_1516,file = paste(DataPath2,"Exports1516.Rda", sep = "/"))

rm(Exports_1516)


#####CREATE DATASET OF ALL IMPORTS/EXPORTS FOR HS12#####

load(paste(DataPath,"Analysis Data/hs12_12-14.Rda", sep = "/"))

Imports_1214 <- hs12_1213[`Trade Flow Code`==1]

save(Imports_1214,file = paste(DataPath2,"Imports1214.Rda", sep = "/"))
rm(Imports_1214)

Exports_1214 <- hs12_1213[`Trade Flow Code`==2]

save(Exports_1214,file = paste(DataPath2,"Exports1214.Rda", sep = "/"))
rm(Exports_1214)
rm(hs12_1213)

load(paste(DataPath2,"Exports1214.Rda", sep = "/"))
load(paste(DataPath2,"Exports1516.Rda", sep = "/"))

exports <- do.call("rbind", list(Exports_1214, Exports_1516))
save(exports,file = paste(DataPath,"Analysis Data","exports_full12.Rda", sep = "/"))

load(paste(DataPath,"Analysis Data/Imports1214.Rda", sep = "/"))
load(paste(DataPath,"Analysis Data/Imports1516.Rda", sep = "/"))

imports_full12 <- do.call("rbind", list(Imports_1214, Imports_1516))
save(imports_full12,file = paste(DataPath,"Analysis Data","imports_full12.Rda", sep = "/"))


#####SPLIT IMPORTS/EXPORTS FOR EACH YEAR OF HS12 DATA#####

load(paste(DataPath,"Analysis Data/exports_full12.Rda", sep = "/"))

exports <- rename(exports, "Export Value" = "Trade Value (US$)")
exports <- rename(exports, "Export Qty Unit" = "Qty Unit")
exports <- rename(exports, "Export Qty" = "Qty")
exports <- rename(exports, "Export Netweight (kg)" = "Netweight (kg)")

exports2012  <- exports[Period=="2012", ]
save(exports2012,file = paste(DataPath2,"exports2012.Rda", sep = "/"))
rm(exports2012)

exports2013  <- exports[Period=="2013", ]
save(exports2013,file = paste(DataPath2,"exports2013.Rda", sep = "/"))
rm(exports2013)

exports2014  <- exports[Period=="2014", ]
save(exports2014,file = paste(DataPath2,"exports2014.Rda", sep = "/"))
rm(exports2014)

exports2015  <- exports[Period=="2015", ]
save(exports2015,file = paste(DataPath2,"exports2015.Rda", sep = "/"))
rm(exports2015)

exports2016  <- exports[Period=="2016", ]
save(exports2016,file = paste(DataPath2, "exports2016.Rda"))
rm(exports2016)

rm(exports)

load(paste(DataPath,"imports_full12.Rda", sep = "/"))

imports2012  <- imports_full12[Period=="2012", ]
save(imports2012,file = paste(DataPath2, "imports2012.Rda", sep = "/"))
rm(imports2012)

imports2013  <- imports_full12[Period=="2013", ]
save(imports2013,file = paste(DataPath2, "imports2013.Rda", sep = "/"))
rm(imports2013)

imports2014  <- imports_full12[Period=="2014", ]
save(imports2014,file = paste(DataPath2, "imports2014.Rda", sep = "/"))
rm(imports2014)

imports2015  <- imports_full12[Period=="2015", ]
save(imports2015,file = paste(DataPath2, "imports2015.Rda", sep = "/"))
rm(imports2015)

imports2016  <- imports_full12[Period=="2016", ]
save(imports2016,file = paste(DataPath2, "imports2016.Rda"))
rm(imports2016)


#####MERGE IMPORTS/EXPORTS FOR EACH YEAR OF HS12 DATA#####

#2012
load(paste(DataPath2,"imports2012.Rda", sep = "/"))
load(paste(DataPath2,"exports2012.Rda", sep = "/"))

hs12_12 <- merge(imports2012, exports2012, 
                 by.x=c("Period", "Aggregate Level", 
                        "Reporter Code", "Reporter", "Partner Code", "Partner", 
                        "Commodity Code", "Commodity"),
                 by.y=c("Period", "Aggregate Level", 
                        "Partner Code", "Partner", "Reporter Code", "Reporter", 
                        "Commodity Code", "Commodity"),all=TRUE)

hs12_12 <- rename(hs12_12, "Importer" = "Reporter")
hs12_12 <- rename(hs12_12, "Exporter" = "Partner")

hs12_12$Raw_gap = hs12_12$`Export Value` - hs12_12$`Import Value`

hs12_12$Log_gap = log(hs12_12$`Export Value`) - log(hs12_12$`Import Value`)

hs12_12$Gap_ratio = hs12_12$`Raw_gap`/(hs12_12$`Import Value` + hs12_12$`Export Value`)

hs12_12$`Export Netweight (kg)` <- as.numeric(hs12_12$`Export Netweight (kg)`)
hs12_12$`Import Netweight (kg)` <- as.numeric(hs12_12$`Import Netweight (kg)`)

hs12_12$Qty_raw_gap = hs12_12$`Export Netweight (kg)` - hs12_12$`Import Netweight (kg)`

hs12_12$Qty_log_gap = log(hs12_12$`Export Netweight (kg)`) - log(hs12_12$`Import Netweight (kg)`)

hs12_12$Qty_gap_ratio = hs12_12$`Qty_raw_gap`/(hs12_12$`Export Netweight (kg)` + hs12_12$`Import Netweight (kg)`)

hs12_12 <- hs12_12[`Commodity Code`!="TOTAL", ]

save(hs12_12,file = paste(DataPath, "Analysis Data", "hs12_12.Rda", sep = "/"))
rm(hs12_12, exports2012, imports2012)

#2013
load(paste(DataPath2,"imports2013.Rda", sep = "/"))
load(paste(DataPath2,"exports2013.Rda", sep = "/"))

hs12_13 <- merge(imports2013, exports2013, 
                 by.x=c("Period", "Aggregate Level", 
                        "Reporter Code", "Reporter", "Partner Code", "Partner", 
                        "Commodity Code", "Commodity"),
                 by.y=c("Period", "Aggregate Level", 
                        "Partner Code", "Partner", "Reporter Code", "Reporter", 
                        "Commodity Code", "Commodity"),all=TRUE)

hs12_13 <- rename(hs12_13, "Importer" = "Reporter")
hs12_13 <- rename(hs12_13, "Exporter" = "Partner")

hs12_13$Raw_gap = hs12_13$`Export Value` - hs12_13$`Import Value`

hs12_13$Log_gap = log(hs12_13$`Export Value`) - log(hs12_13$`Import Value`)

hs12_13$Gap_ratio = hs12_13$`Raw_gap`/(hs12_13$`Import Value` + hs12_13$`Export Value`)

hs12_13$`Export Netweight (kg)` <- as.numeric(hs12_13$`Export Netweight (kg)`)
hs12_13$`Import Netweight (kg)` <- as.numeric(hs12_13$`Import Netweight (kg)`)

hs12_13$Qty_raw_gap = hs12_13$`Export Netweight (kg)` - hs12_13$`Import Netweight (kg)`

hs12_13$Qty_log_gap = log(hs12_13$`Export Netweight (kg)`) - log(hs12_13$`Import Netweight (kg)`)

hs12_13$Qty_gap_ratio = hs12_13$`Qty_raw_gap`/(hs12_13$`Export Netweight (kg)` + hs12_13$`Import Netweight (kg)`)

hs12_13 <- hs12_13[`Commodity Code`!="TOTAL", ]

save(hs12_13,file = paste(DataPath, "Analysis Data", "hs12_13.Rda", sep = "/"))
rm(hs12_13, exports2013, imports2013)

#2014
load(paste(DataPath2,"imports2014.Rda", sep = "/"))
load(paste(DataPath2,"exports2014.Rda", sep = "/"))

hs12_14 <- merge(imports2014, exports2014, 
                 by.x=c("Period", "Aggregate Level", 
                        "Reporter Code", "Reporter", "Partner Code", "Partner", 
                        "Commodity Code", "Commodity"),
                 by.y=c("Period", "Aggregate Level", 
                        "Partner Code", "Partner", "Reporter Code", "Reporter", 
                        "Commodity Code", "Commodity"),all=TRUE)

hs12_14 <- rename(hs12_14, "Importer" = "Reporter")
hs12_14 <- rename(hs12_14, "Exporter" = "Partner")

hs12_14$Raw_gap = hs12_14$`Export Value` - hs12_14$`Import Value`

hs12_14$Log_gap = log(hs12_14$`Export Value`) - log(hs12_14$`Import Value`)

hs12_14$Gap_ratio = hs12_14$`Raw_gap`/(hs12_14$`Import Value` + hs12_14$`Export Value`)

hs12_14$`Export Netweight (kg)` <- as.numeric(hs12_14$`Export Netweight (kg)`)
hs12_14$`Import Netweight (kg)` <- as.numeric(hs12_14$`Import Netweight (kg)`)

hs12_14$Qty_raw_gap = hs12_14$`Export Netweight (kg)` - hs12_14$`Import Netweight (kg)`

hs12_14$Qty_log_gap = log(hs12_14$`Export Netweight (kg)`) - log(hs12_14$`Import Netweight (kg)`)

hs12_14$Qty_gap_ratio = hs12_14$`Qty_raw_gap`/(hs12_14$`Export Netweight (kg)` + hs12_14$`Import Netweight (kg)`)

hs12_14 <- hs12_14[`Commodity Code`!="TOTAL", ]

save(hs12_14,file = paste(DataPath, "Analysis Data", "hs12_14.Rda", sep = "/"))
rm(hs12_14, exports2014, imports2014)

#2015

load(paste(DataPath2,"imports2015.Rda", sep = "/"))
load(paste(DataPath2,"exports2015.Rda", sep = "/"))

hs12_15 <- merge(imports2015, exports2015, 
                 by.x=c("Period", "Aggregate Level", 
                        "Reporter Code", "Reporter", "Partner Code", "Partner", 
                        "Commodity Code", "Commodity"),
                 by.y=c("Period", "Aggregate Level", 
                        "Partner Code", "Partner", "Reporter Code", "Reporter", 
                        "Commodity Code", "Commodity"),all=TRUE)

hs12_15 <- rename(hs12_15, "Importer" = "Reporter")
hs12_15 <- rename(hs12_15, "Exporter" = "Partner")

hs12_15$Raw_gap = hs12_15$`Export Value` - hs12_15$`Import Value`

hs12_15$Log_gap = log(hs12_15$`Export Value`) - log(hs12_15$`Import Value`)

hs12_15$Gap_ratio = hs12_15$`Raw_gap`/(hs12_15$`Import Value` + hs12_15$`Export Value`)

hs12_15$`Export Netweight (kg)` <- as.numeric(hs12_15$`Export Netweight (kg)`)
hs12_15$`Import Netweight (kg)` <- as.numeric(hs12_15$`Import Netweight (kg)`)

hs12_15$Qty_raw_gap = hs12_15$`Export Netweight (kg)` - hs12_15$`Import Netweight (kg)`

hs12_15$Qty_log_gap = log(hs12_15$`Export Netweight (kg)`) - log(hs12_15$`Import Netweight (kg)`)

hs12_15$Qty_gap_ratio = hs12_15$`Qty_raw_gap`/(hs12_15$`Export Netweight (kg)` + hs12_15$`Import Netweight (kg)`)

hs12_15 <- hs12_15[`Commodity Code`!="TOTAL", ]

save(hs12_15,file = paste(DataPath, "Analysis Data", "hs12_15.Rda", sep = "/"))
rm(hs12_15, exports2015, imports2015)

#2016
load(paste(DataPath2,"imports2016.Rda", sep = "/"))
load(paste(DataPath2,"exports2016.Rda", sep = "/"))

hs12_16 <- merge(imports2016, exports2016, 
                 by.x=c("Period", "Aggregate Level", 
                        "Reporter Code", "Reporter", "Partner Code", "Partner", 
                        "Commodity Code", "Commodity"),
                 by.y=c("Period", "Aggregate Level", 
                        "Partner Code", "Partner", "Reporter Code", "Reporter", 
                        "Commodity Code", "Commodity"),all=TRUE)

hs12_16 <- rename(hs12_16, "Importer" = "Reporter")
hs12_16 <- rename(hs12_16, "Exporter" = "Partner")

hs12_16$Raw_gap = hs12_16$`Export Value` - hs12_16$`Import Value`

hs12_16$Log_gap = log(hs12_16$`Export Value`) - log(hs12_16$`Import Value`)

hs12_16$Gap_ratio = hs12_16$`Raw_gap`/(hs12_16$`Import Value` + hs12_16$`Export Value`)

hs12_16$`Export Netweight (kg)` <- as.numeric(hs12_16$`Export Netweight (kg)`)
hs12_16$`Import Netweight (kg)` <- as.numeric(hs12_16$`Import Netweight (kg)`)

hs12_16$Qty_raw_gap = hs12_16$`Export Netweight (kg)` - hs12_16$`Import Netweight (kg)`

hs12_16$Qty_log_gap = log(hs12_16$`Export Netweight (kg)`) - log(hs12_16$`Import Netweight (kg)`)

hs12_16$Qty_gap_ratio = hs12_16$`Qty_raw_gap`/(hs12_16$`Export Netweight (kg)` + hs12_16$`Import Netweight (kg)`)

hs12_16 <- hs12_16[`Commodity Code`!="TOTAL", ]

save(hs12_16,file = paste(DataPath, "Analysis Data", "hs12_16.Rda", sep = "/"))
rm(hs12_16, exports2016, imports2016)

