library(data.table)
library(plyr)
library(dplyr)

setwd(dirname(path.expand("~")))

DataPath <- file.path(paste(getwd(),"Dropbox/Customs Evasion", sep="/"))


#####CLEAN MFN DATA FOR MERGE#####

load(paste(DataPath,"Raw Data/hs12_MFN.Rda", sep = "/"))

EUmembers <- read.csv(paste(DataPath,"raw data/EUmembers.csv", sep = "/"))

EUmembers$Reporter_ISO_N <- as.character(EUmembers$Reporter_ISO_N)

hs12_MFN$Reporter_ISO_N <- as.character(hs12_MFN$Reporter_ISO_N)

hs12_MFN <- merge(hs12_MFN, EUmembers, 
                  by = c("Reporter_ISO_N"), allow.cartesian = T, all.x = T)

hs12_MFN$Reporter_ISO_N[!is.na(hs12_MFN$CountryCode)] <- hs12_MFN$CountryCode[!is.na(hs12_MFN$CountryCode)]

hs12_MFN <- hs12_MFN[ !(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2012)| 
                        is.na(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2012)]

hs12_MFN <- hs12_MFN[ !(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2013)| 
                        is.na(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2013)]

hs12_MFN <- hs12_MFN[, c("Country", "DateJoined", "CountryCode") :=NULL]

hs12_MFN$ProductCode[hs12_MFN$ProductCode<100000] <- paste("0", hs12_MFN$ProductCode[hs12_MFN$ProductCode<100000], sep="")

hs12_MFN$Year <- as.character(hs12_MFN$Year)

hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==250] <- 251 #France
hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==380] <- 381 #Italy
hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==578] <- 579 #Norway
hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==756] <- 757 #Switzerland
hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==840] <- 842 #USA
hs12_MFN$Reporter_ISO_N[hs12_MFN$Reporter_ISO_N==356] <- 699 #India

hs12_MFN$MFN <- 1

#####MERGE VALUE DATA W MFN TARIFFS######

#REPLACE WITH QTY OR VALUE
load(paste(DataPath,"Analysis Data/hs12_qty.Rda", sep = "/"))

hs12_all_tariffs <- merge(hs12_MFN, hs12_qty, 
                          by.x = c("Year", "Reporter_ISO_N", "ProductCode"),
                          by.y = c("Period", "Reporter Code", "Commodity Code"))

save(hs12_all_tariffs,file = "Documents/hs2012/hs12_all_tariffs_qty.Rda")

rm(hs12_qty, hs12_MFN, hs12_all_tariffs)


#####MERGE EU COUNTRIES WITH PREFERENTIAL TARIFFS####

load(paste(DataPath,"raw data/hs12_pref.Rda", sep = "/"))

hs12_pref$Reporter_ISO_N <- as.character(hs12_pref$Reporter_ISO_N)
hs12_pref$Partner <- as.character(hs12_pref$Partner)

hs12_pref$Partner <- substr(hs12_pref$Partner,
                            regexpr("[^0]",hs12_pref$Partner),
                            nchar(hs12_pref$Partner))

hs12_pref <- merge(hs12_pref, EUmembers, 
                   by = c("Reporter_ISO_N"), allow.cartesian = T, all.x = T)

hs12_pref$Reporter_ISO_N[!is.na(hs12_pref$CountryCode)] <- hs12_pref$CountryCode[!is.na(hs12_pref$CountryCode)]

hs12_pref <- hs12_pref[ !(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2012)| 
                          is.na(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2012)]

hs12_pref <- hs12_pref[ !(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2013)| 
                          is.na(Reporter_ISO_N == 191 & Country == "Croatia" & Year ==2013)]

hs12_pref <- hs12_pref[, c("Country", "DateJoined", "CountryCode") :=NULL]

save(hs12_pref,file = "Documents/hs2012/hs12_pref.Rda")
rm(hs12_pref)

#####CLEAN BENEFICIARY CODES####

load(paste(DataPath,"raw data/TRAINS_preference_beneficiaries.Rda", sep = "/"))

TRAINS_preference_beneficiaries$Partner <- as.character(TRAINS_preference_beneficiaries$Partner)

TRAINS_preference_beneficiaries$Partner <- substr(TRAINS_preference_beneficiaries$Partner,
                                                  regexpr("[^0]",TRAINS_preference_beneficiaries$Partner),
                                                  nchar(TRAINS_preference_beneficiaries$Partner))

TRAINS_preference_beneficiaries <- merge(TRAINS_preference_beneficiaries, EUmembers, 
                                         by.x = c("Partner"), by.y = c("Reporter_ISO_N"), allow.cartesian = T, all.x = T)

TRAINS_preference_beneficiaries$Partner[!is.na(TRAINS_preference_beneficiaries$CountryCode)] <- 
  TRAINS_preference_beneficiaries$CountryCode[!is.na(TRAINS_preference_beneficiaries$CountryCode)]

TRAINS_preference_beneficiaries <- TRAINS_preference_beneficiaries[, c("Country", "DateJoined", "CountryCode") :=NULL]

TRAINS_preference_beneficiaries <- rename(TRAINS_preference_beneficiaries, "Partner Code" = "Partner") 


#####CLEAN and SPLIT PREFERENCE TARIFFS#####

#Function for cleaning each section of pref tariffs
pref_split <- function(hs12_pref) {
  
  hs12_pref <- merge(hs12_pref, TRAINS_preference_beneficiaries, 
                     by.x = c("Partner"), by.y = c("RegionCode"), allow.cartesian = T, all.x = T)
  
  hs12_pref <- hs12_pref[!(`Partner Code` == 191 & PartnerName == "European Union" & Year == 2012)| 
                           is.na(`Partner Code` == 191 & PartnerName == "European Union" & Year == 2012)]
  
  hs12_pref <- hs12_pref[!(`Partner Code` == 191 & PartnerName == "European Union" & Year == 2013)| 
                           is.na(`Partner Code` == 191 & PartnerName == "European Union" & Year == 2013)]
  
  hs12_pref$ProductCode[hs12_pref$ProductCode<100000] <- 
    paste("0", hs12_pref$ProductCode[hs12_pref$ProductCode<100000], sep="")
  
  hs12_pref$`Partner Code` <- as.character(hs12_pref$`Partner Code`)
  hs12_pref$Partner <- as.character(hs12_pref$Partner)
  
  hs12_pref$`Partner Code`[is.na(hs12_pref$`Partner Code`)] <- 
    hs12_pref$Partner[is.na(hs12_pref$`Partner Code`)]
  
  hs12_pref[,Partner:=NULL]
  
  hs12_pref$Reporter_ISO_N <- as.character(hs12_pref$Reporter_ISO_N)
  hs12_pref$Year <- as.character(hs12_pref$Year)
  hs12_pref$ProductCode <- as.character(hs12_pref$ProductCode)
  
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==250] <- 251 #France
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==380] <- 381 #Italy
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==578] <- 579 #Norway
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==756] <- 757 #Switzerland
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==840] <- 842 #USA
  hs12_pref$Reporter_ISO_N[hs12_pref$Reporter_ISO_N==356] <- 699 #India
  
  
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==250] <- 251 #France
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==380] <- 381 #Italy
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==578] <- 579 #Norway
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==756] <- 757 #Switzerland
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==840] <- 842 #USA
  hs12_pref$`Partner Code`[hs12_pref$`Partner Code`==356] <- 699 #India
  
  
  hs12_pref$pref <- 1
  return(hs12_pref)
}

#Pref 1
load("Documents/hs2012/hs12_pref.Rda")

hs12_pref_1 <- hs12_pref[Reporter_ISO_N <= 251, ]

rm(hs12_pref)

hs12_pref_1 <- pref_split(hs12_pref_1)

save(hs12_pref_1,file = "Documents/hs2012/hs12_pref_1.Rda")
rm(hs12_pref_1)

#Pref 2
load("Documents/hs2012/hs12_pref.Rda")

hs12_pref_2 <- hs12_pref[Reporter_ISO_N > 251 & Reporter_ISO_N <= 400 | Reporter_ISO_N == 699, ]

rm(hs12_pref)

hs12_pref_2 <- pref_split(hs12_pref_2)

save(hs12_pref_2,file = "Documents/hs2012/hs12_pref_2.Rda")
rm(hs12_pref_2)

#Pref 3
load("Documents/hs2012/hs12_pref.Rda")

hs12_pref_3 <- hs12_pref[Reporter_ISO_N > 400 & Reporter_ISO_N <= 500, ]

rm(hs12_pref)

hs12_pref_3 <- pref_split(hs12_pref_3)

save(hs12_pref_3,file = "Documents/hs2012/hs12_pref_3.Rda")
rm(hs12_pref_3)

#Pref 4
load("Documents/hs2012/hs12_pref.Rda")

hs12_pref_4 <- hs12_pref[Reporter_ISO_N > 500 & Reporter_ISO_N <= 700 & Reporter_ISO_N!=699, ]

rm(hs12_pref)

hs12_pref_4 <- pref_split(hs12_pref_4)

save(hs12_pref_4,file = "Documents/hs2012/hs12_pref_4.Rda")
rm(hs12_pref_4)

#Pref 5
load("Documents/hs2012/hs12_pref.Rda")

hs12_pref_5 <- hs12_pref[Reporter_ISO_N > 700, ]

rm(hs12_pref)

hs12_pref_5 <- pref_split(hs12_pref_5)

save(hs12_pref_5,file = "Documents/hs2012/hs12_pref_5.Rda")
rm(hs12_pref_5, TRAINS_preference_beneficiaries)


#MERGE TRADE/MFN WITH PREF#####

load("Documents/hs2012/hs12_all_tariffs_qty.Rda")

#Section 1
hs12_all_tariffs_1 <- hs12_all_tariffs[Reporter_ISO_N <= 251, ]

load("Documents/hs2012/hs12_pref_1.Rda")

hs12_all_tariffs_1 <- merge(hs12_all_tariffs_1, hs12_pref_1, 
                            by=c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code"), all.x = T)



rm(hs12_pref_1)

#Section 2
hs12_all_tariffs_2 <- hs12_all_tariffs[Reporter_ISO_N > 251 & Reporter_ISO_N <= 400 | Reporter_ISO_N == 699, ]

load("Documents/hs2012/hs12_pref_2.Rda")

hs12_all_tariffs_2 <- merge(hs12_all_tariffs_2, hs12_pref_2, 
                            by=c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code"), all.x = T)

rm(hs12_pref_2)

#Section 3
hs12_all_tariffs_3 <- hs12_all_tariffs[Reporter_ISO_N > 400 & Reporter_ISO_N <= 500, ]

load("Documents/hs2012/hs12_pref_3.Rda")

hs12_all_tariffs_3 <- merge(hs12_all_tariffs_3, hs12_pref_3, 
                            by=c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code"), all.x = T)

rm(hs12_pref_3)

#Section 4
hs12_all_tariffs_4 <- hs12_all_tariffs[Reporter_ISO_N > 500 & Reporter_ISO_N <= 700 & Reporter_ISO_N != 699, ]

load("Documents/hs2012/hs12_pref_4.Rda")

hs12_all_tariffs_4 <- merge(hs12_all_tariffs_4, hs12_pref_4, 
                            by=c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code"), all.x = T)

rm(hs12_pref_4)

#Section 5
hs12_all_tariffs_5 <- hs12_all_tariffs[Reporter_ISO_N > 700, ]

load("Documents/hs2012/hs12_pref_5.Rda")

hs12_all_tariffs_5 <- merge(hs12_all_tariffs_5, hs12_pref_5, 
                            by=c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code"), all.x = T)

rm(hs12_pref_5)


hs12_all_tariffs <- do.call("rbind", list(hs12_all_tariffs_1, hs12_all_tariffs_2, 
                                          hs12_all_tariffs_3, hs12_all_tariffs_4,
                                          hs12_all_tariffs_5))

rm(hs12_all_tariffs_1, hs12_all_tariffs_2, hs12_all_tariffs_3, hs12_all_tariffs_4, hs12_all_tariffs_5)


#####CLEAN FULL TARIFF DATA#####

#Remove duplicates combinations with same tariff rates
hs12_all_tariffs <- hs12_all_tariffs[!duplicated(hs12_all_tariffs[, 
                                                                  c("Reporter_ISO_N", "Year", "ProductCode", "Partner Code", "SimpleAverage.y", "Max_Rate.y")])]

#If more than one avg tariff rate, keep lower of two
hs12_all_tariffs <- hs12_all_tariffs %>% 
  group_by(Reporter_ISO_N, Year, ProductCode, `Partner Code`) %>% 
  filter(SimpleAverage.y==min(SimpleAverage.y)|is.na(SimpleAverage.y))

#Take lower max rate if still some non-unique rates
hs12_all_tariffs <- hs12_all_tariffs %>% 
  group_by(Reporter_ISO_N, Year, ProductCode, `Partner Code`) %>% 
  filter(Max_Rate.y==min(Max_Rate.y)|is.na(Max_Rate.y))

hs12_all_tariffs <- as.data.table(hs12_all_tariffs)

hs12_all_tariffs$Sum_Of_Rates.x[!is.na(hs12_all_tariffs$Sum_Of_Rates.y)] <- 
  hs12_all_tariffs$Sum_Of_Rates.y[!is.na(hs12_all_tariffs$Sum_Of_Rates.y)]

hs12_all_tariffs$Min_Rate.x[!is.na(hs12_all_tariffs$Min_Rate.y)] <- 
  hs12_all_tariffs$Min_Rate.y[!is.na(hs12_all_tariffs$Min_Rate.y)]

hs12_all_tariffs$Max_Rate.x[!is.na(hs12_all_tariffs$Max_Rate.y)] <- 
  hs12_all_tariffs$Max_Rate.y[!is.na(hs12_all_tariffs$Max_Rate.y)]

hs12_all_tariffs$SimpleAverage.x[!is.na(hs12_all_tariffs$SimpleAverage.y)] <- 
  hs12_all_tariffs$SimpleAverage.y[!is.na(hs12_all_tariffs$SimpleAverage.y)]

hs12_all_tariffs$TotalNoOfLines.x[!is.na(hs12_all_tariffs$TotalNoOfLines.y)] <- 
  hs12_all_tariffs$TotalNoOfLines.y[!is.na(hs12_all_tariffs$TotalNoOfLines.y)]

hs12_all_tariffs$Nbr_NA_Lines.x[!is.na(hs12_all_tariffs$Nbr_NA_Lines.y)] <- 
  hs12_all_tariffs$Nbr_NA_Lines.y[!is.na(hs12_all_tariffs$Nbr_NA_Lines.y)]


hs12_all_tariffs <- hs12_all_tariffs[,
                                     c("Sum_Of_Rates.y", "Min_Rate.y", "Max_Rate.y", "SimpleAverage.y", 
                                       "TotalNoOfLines.y", "Nbr_NA_Lines.y", "EstCode.y", "NomenCode.y") :=NULL]

colnames(hs12_all_tariffs)[grep(".x",colnames(hs12_all_tariffs))] <- 
  gsub(".x$","",colnames(hs12_all_tariffs)[grep(".x",colnames(hs12_all_tariffs))]) 

save(hs12_all_tariffs,file = paste(DataPath,"Analysis Data","hs12_all_tariffs_qty.Rda", sep = "/"))
