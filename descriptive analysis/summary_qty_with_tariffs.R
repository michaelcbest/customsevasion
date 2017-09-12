library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lfe)
library(gridExtra)
library(cowplot)
library(pander)

setwd(dirname(path.expand("~")))

DataPath <- file.path(paste(getwd(),"Dropbox/Customs Evasion", sep="/"))

#####TARIFF DATA RELATIVE TO VALUE SAMPLE#####

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))
hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, Importer, Exporter)]

load(paste(DataPath,"Analysis Data/hs12_qty.Rda", sep = "/"))
hs12_qty <- hs12_qty[, .(Period, `Commodity Code`, Importer, Exporter)]

#For each year, how many product x o-d pairs in tariff data / trade product x o-d pairs?

product_year <- hs12_all_tariffs[, uniqueN(ProductCode), by=Year]
product_year <- rename(product_year, Products_tariffs = V1)

pair_year <- unique(setDT(hs12_all_tariffs), by = c("Importer", "Exporter", "Year"))
pair_year <- pair_year[, .N, by=Year]
pair_year <- rename(pair_year, Pairs_tariffs = N)

year_coverage <- merge(product_year, pair_year)

product_year_trade <- hs12_qty[, uniqueN(`Commodity Code`), by=Period]
product_year_trade <- rename(product_year_trade, Products_trade = V1)

pair_year_trade <- unique(setDT(hs12_qty), by = c("Importer", "Exporter", "Period"))
pair_year_trade <- pair_year_trade[, .N, by=Period]
pair_year_trade <- rename(pair_year_trade, Pairs_trade = N)

year_coverage_trade <- merge(product_year_trade, pair_year_trade)

year_coverage <- merge(year_coverage, year_coverage_trade, by.x = c("Year"), by.y = c("Period"), all = T)

year_coverage$Coverage <- (year_coverage$Products_tariffs*year_coverage$Pairs_tariffs)/
  (year_coverage$Products_trade*year_coverage$Pairs_trade)

year_coverage[is.na(year_coverage)] <- 0

pander(year_coverage)
rm(pair_year, pair_year_trade, product_year, product_year_trade, year_coverage, year_coverage_trade)

#For each product, how many year x o-d pairs / all possible year x o-d pairs?

year_product <- hs12_all_tariffs[, uniqueN(`Year`), by=ProductCode]
year_product <- rename(year_product, Years_tariffs = V1)

pair_product <- unique(setDT(hs12_all_tariffs), by = c("Importer", "Exporter", "ProductCode"))
pair_product <- pair_product[, .N, by= .(ProductCode)]
pair_product <- rename(pair_product, Pairs_tariffs = N)

product_coverage <- merge(year_product, pair_product)

year_product_trade <- hs12_qty[, uniqueN(`Period`), by=`Commodity Code`]
year_product_trade <- rename(year_product_trade, Years_trade = V1)

pair_product_trade <- unique(setDT(hs12_qty), by = c("Importer", "Exporter", "Commodity Code"))
pair_product_trade <- pair_product_trade[, .N, by = .(`Commodity Code`)]
pair_product_trade <- rename(pair_product_trade, Pairs_trade = N)

product_coverage_trade <- merge(year_product_trade, pair_product_trade)

product_coverage <- merge(product_coverage, product_coverage_trade, 
                          by.x = c("ProductCode"), by.y = c("Commodity Code"), all = T)

product_coverage$Coverage <- (product_coverage$Years_tariffs*product_coverage$Pairs_tariffs)/
  (product_coverage$Years_trade*product_coverage$Pairs_trade)

product_coverage[is.na(product_coverage)] <- 0

pander(product_coverage[order(Coverage)][1:10])
pander(product_coverage[order(-Coverage)][1:10])

rm(pair_product, pair_product_trade, year_product, year_product_trade, product_coverage, product_coverage_trade)

#For each o-d pair, how many year x product / all possible year x product?

product_pair <- hs12_all_tariffs[, uniqueN(ProductCode), by = c("Importer", "Exporter")]
product_pair <- rename(product_pair, Products_tariffs = V1)

year_pair <- hs12_all_tariffs[, uniqueN(`Year`), by = c("Importer", "Exporter")]
year_pair <- rename(year_pair, Years_tariffs = V1)

pair_coverage <- merge(product_pair, year_pair, by = c("Importer", "Exporter"))

product_pair_trade <- hs12_qty[, uniqueN(`Commodity Code`), by = c("Importer", "Exporter")]
product_pair_trade <- rename(product_pair_trade, Products_trade = V1)

year_pair_trade <- hs12_qty[, uniqueN(`Period`), by = c("Importer", "Exporter")]
year_pair_trade <- rename(year_pair_trade, Years_trade = V1)

pair_coverage_trade <- merge(product_pair_trade, year_pair_trade)

pair_coverage <- merge(pair_coverage, pair_coverage_trade, all = T)

pair_coverage$Coverage <- (pair_coverage$Products_tariffs*pair_coverage$Years_tariffs)/
  (pair_coverage$Products_trade*pair_coverage$Years_trade)

pair_coverage[is.na(pair_coverage)] <- 0

pair_coverage$Exporter <- strtrim(pair_coverage$Exporter, 15)

pair_coverage[order(-Coverage)][1:10]

rm(product_pair, product_pair_trade, year_pair, year_pair_trade, pair_coverage, pair_coverage_trade)


#yearXproduct imports reported by country, relative to exports to country reported by world

tariffs <- hs12_all_tariffs[, .N, by = "Importer"]
tariffs <- rename(tariffs, "Tariffs" = "N")

trade <- hs12_qty[, .N, by = "Importer"]
trade <- rename(trade, "Trade" = "N")

matches <- merge(tariffs, trade, by = c("Importer"), all = T)

matches[is.na(matches)] <- 0
matches$Share_covered <- matches$Tariffs / matches$Trade

pander(matches[order(-Share_covered)][1:10])

trade <- hs12_qty[Period!=2016 & `Commodity Code`!="999999", ]
trade <- subset(trade, nchar(`Commodity Code`) > 4)

trade <- trade[, .N, by = "Importer"]
trade <- rename(trade, "Trade" = "N")

matches <- merge(tariffs, trade, by = c("Importer"), all = T)

matches[is.na(matches)] <- 0
matches$Share_covered <- matches$Tariffs / matches$Trade

matches <- matches[order(-Share_covered)]
print(matches, nrow=144)

rm(hs12_all_tariffs, hs12_qty, matches, tariffs, trade)


#How has the trade gap changed over time?

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, Importer, Exporter, Qty_log_gap)]

hs12_all_tariffs$Year <- as.Date(hs12_all_tariffs$Year, "%Y")
hs12_all_tariffs$Year <- floor_date(hs12_all_tariffs$Year,"year")

Years <- hs12_all_tariffs[, .(mean = as.double(mean(Qty_log_gap)),
                              median = as.double(median(Qty_log_gap)),
                              p25 = as.double(quantile(Qty_log_gap,.25)),
                              p75 = as.double(quantile(Qty_log_gap,.75))
),
by=Year]

Years <- melt(Years, id = 'Year')
Years$variable <- factor(Years$variable, levels = c("p25","p75","median","mean"))

ggplot(data=Years ) +
  geom_line(data=Years, aes(x = Year, y = value, colour = variable, size=variable)) +
  scale_colour_manual(values=c("grey","grey","black","royalblue4")) +
  background_grid(major = 'y', minor = "none") +
  scale_size_manual(values = c(1,1,1.1,1.25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1.5,1.5), minor_breaks = NULL) +
  xlab(label = "") +
  ylab(label = "Quantity gap") +
  labs(title="Quantity Gap Over Time")

#Across products?
products <- hs12_all_tariffs[, .(mean = as.double(mean(Qty_log_gap)),
                                 median = as.double(median(Qty_log_gap)),
                                 p25 = as.double(quantile(Qty_log_gap,.25)),
                                 p75 = as.double(quantile(Qty_log_gap,.75))
),
by= ProductCode]

ggplot(data=products, aes(mean)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000))  +
  labs(title="Mean Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

ggplot(data=products, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  labs(title="Median Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

ggplot(data=products, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2000),  minor_breaks = NULL) +
  labs(title="25th Percentile Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

ggplot(data=products, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0),  limits = c(0, 2000), minor_breaks = NULL) +
  labs(title="75th Percentile Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

#Across countries?
countries <- hs12_all_tariffs[, .(mean = as.double(mean(Qty_log_gap)),
                                  median = as.double(median(Qty_log_gap)),
                                  p25 = as.double(quantile(Qty_log_gap,.25)),
                                  p75 = as.double(quantile(Qty_log_gap,.75))
),
by= c("Importer", "Exporter")]

ggplot(data=countries, aes(mean)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000),  minor_breaks = NULL) +
  labs(title="Mean Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6000), minor_breaks = NULL) +
  labs(title="Median Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000),  minor_breaks = NULL) +
  labs(title="25th Percentile Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000),  minor_breaks = NULL) +
  labs(title="75th Percentile Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

rm(Years, products, countries, hs12_all_tariffs)

#Regress trade gap on dummies and plot coefficients 

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, `Reporter_ISO_N`, `Partner Code`, Qty_log_gap)]

hs12_all_tariffs$Year <- as.Date(hs12_all_tariffs$Year, "%Y")
hs12_all_tariffs$Year <- floor_date(hs12_all_tariffs$Year,"year")

hs12_all_tariffs$Year.f <- factor(hs12_all_tariffs$Year)
hs12_all_tariffs$Products.f <- factor(hs12_all_tariffs$ProductCode)

hs12_all_tariffs$Importer.f <- factor(hs12_all_tariffs$`Reporter_ISO_N`)
hs12_all_tariffs$Exporter.f <- factor(hs12_all_tariffs$`Partner Code`)
hs12_all_tariffs$Pairs.f <- with(hs12_all_tariffs, interaction(Importer.f, Exporter.f))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, Qty_log_gap, Year.f, Products.f, Pairs.f)]

reg <- felm(Qty_log_gap ~ 1 | Year.f + Products.f + Pairs.f,
            data = hs12_all_tariffs,
            exactDOF = FALSE,
            keepX = FALSE,
            keepCX = FALSE)

fes <- getfe(reg,
             se=TRUE,
             bN = 50
)

Yearfes <- subset(fes,fe == "Year.f")

Yearfes$ci_ub <- Yearfes$effect + (1.96 * Yearfes$se)
Yearfes$ci_lb <- Yearfes$effect - (1.96 * Yearfes$se)
Yearfes <- merge(Yearfes,unique(hs12_all_tariffs[,list(Year,Year.f)]),by.x = "idx",by.y="Year.f")
Yearfes <- rename(Yearfes, Year = Year)

ggplot(data = Yearfes, aes(Year,effect)) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), color = "grey35") +
  geom_line(color = "royalblue4", size = 1) +
  geom_point(color = "royalblue4") +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(-.05,.05), minor_breaks = NULL) +
  xlab(label = "") +
  ylab(label = "Quantity gap") +
  labs(title = "Quantity Gap Over Time, Controlling for Product/Country Pairs")


productfes <- subset(fes,fe == "Products.f")
productfes <- productfes[,c("effect","idx")]

ggplot(data=productfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2000), minor_breaks = NULL) +
  labs(title="Mean Quantity Gap Across Products, Controlling for Country Pairs/Years") +
  labs(x="Quantity gap", y="Number of products")

pairfes <- subset(fes,fe == "Pairs.f")
pairfes <- pairfes[,c("effect","idx")]

ggplot(data=pairfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000), minor_breaks = NULL) +
  labs(title="Mean Quantity Gap Across Country Pairs, Controlling for Products/Years") +
  labs(x="Quantity gap", y="Number of country pairs")

rm(fes, hs12_all_tariffs, pairfes, Yearfes, productfes, reg)


######TARIFFS#####

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, Importer, Exporter, pref)]

pref <- hs12_all_tariffs[pref==1, .N, by = c("Importer")]
pref <- rename(pref, "Pref" = "N")

mfn <- hs12_all_tariffs[is.na(pref), .N, by = c("Importer")]
mfn <- rename(mfn, "MFN" = "N")

tariffs <- merge(pref, mfn, by = c("Importer"), all = T)

tariffs[is.na(tariffs)] <- 0

tariffs$All <- tariffs$Pref + tariffs$MFN
tariffs$Share_pref <- tariffs$Pref / tariffs$All

ggplot(tariffs, aes(Share_pref)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 bins = 10,
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25), minor_breaks = NULL) +
  labs(title="Preferential Tariffs as a Share of Total") +
  labs(x="Share of all tariffs", y="Number of importers")

rm(hs12_all_tariffs, mfn, pref, tariffs)

#Simple Average

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, Importer, Exporter, SimpleAverage)]
hs12_all_tariffs <- hs12_all_tariffs[!is.na(SimpleAverage)]

hs12_all_tariffs$Year <- as.Date(hs12_all_tariffs$Year, "%Y")
hs12_all_tariffs$Year <- floor_date(hs12_all_tariffs$Year,"year")

Years <- hs12_all_tariffs[, .(mean = as.double(mean(SimpleAverage)),
                              median = as.double(median(SimpleAverage)),
                              p25 = as.double(quantile(SimpleAverage,.25)),
                              p75 = as.double(quantile(SimpleAverage,.75))
), by=Year]

Years <- melt(Years, id = 'Year')
Years$variable <- factor(Years$variable, levels = c("p25","p75","median","mean"))

ggplot(data=Years ) +
  geom_line(data=Years, aes(x = Year, y = value, colour = variable, size=variable)) +
  scale_colour_manual(values=c("grey","grey","black","royalblue4")) +
  background_grid(major = 'y', minor = "none") +
  scale_size_manual(values = c(1,1,1.1,1.25)) +
  scale_y_continuous(expand = c(0, 0), minor_breaks = NULL) +
  xlab(label = "") +
  ylab(label = "Tariff rate (%)") +
  labs(title="Average Tariff Rate Over Time")

#Across products?
products <- hs12_all_tariffs[, .(mean = as.double(mean(SimpleAverage)),
                                 median = as.double(median(SimpleAverage)),
                                 p25 = as.double(quantile(SimpleAverage,.25)),
                                 p75 = as.double(quantile(SimpleAverage,.75))
),
by= ProductCode]

ggplot(data=products, aes(mean)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,3000))  +
  labs(title="Mean Tariff Rate Across Products") +
  labs(x="Tariff rate (%)", y="Number of products")

ggplot(data=products, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  labs(title="Median Tariff Rate Across Products") +
  labs(x="Tariff rate (%)", y="Number of products")

ggplot(data=products, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 6000), minor_breaks = NULL) +
  labs(title="25th Percentile Tariff Rate Across Products") +
  labs(x="Tariff rate (%)", y="Number of products")

ggplot(data=products, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000),  minor_breaks = NULL) +
  labs(title="75th Percentile Tariff Rate Across Products") +
  labs(x="Tariff rate (%)", y="Number of products")

#Across countries?
countries <- hs12_all_tariffs[, .(mean = as.double(mean(SimpleAverage)),
                                  median = as.double(median(SimpleAverage)),
                                  p25 = as.double(quantile(SimpleAverage,.25)),
                                  p75 = as.double(quantile(SimpleAverage,.75))
),
by= c("Importer", "Exporter")]

ggplot(data=countries, aes(mean)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), minor_breaks = NULL) +
  labs(title="Mean Tariff Rate Across Country Pairs") +
  labs(x="Tariff Rate (%)", y="Number of pairs")

ggplot(data=countries, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), minor_breaks = NULL) +
  labs(title="Median Tariff Rate Across Country Pairs") +
  labs(x="Tariff Rate (%)", y="Number of pairs")

ggplot(data=countries, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), minor_breaks = NULL) +
  labs(title="25th Percentile Tariff Rate Across Country Pairs") +
  labs(x="Tariff Rate (%)", y="Number of pairs")

ggplot(data=countries, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), minor_breaks = NULL) +
  labs(title="75th Percentile Tariff Rate Across Country Pairs") +
  labs(x="Tariff Rate (%)", y="Number of pairs")

rm(Years, products, countries, hs12_all_tariffs)

#Regress trade gap on dummies and plot coefficients 

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))
hs12_all_tariffs <- hs12_all_tariffs[!is.na(SimpleAverage)]

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, `Reporter_ISO_N`, `Partner Code`, SimpleAverage)]

hs12_all_tariffs$Year <- as.Date(hs12_all_tariffs$Year, "%Y")
hs12_all_tariffs$Year <- floor_date(hs12_all_tariffs$Year,"year")

hs12_all_tariffs$Year.f <- factor(hs12_all_tariffs$Year)
hs12_all_tariffs$Products.f <- factor(hs12_all_tariffs$ProductCode)

hs12_all_tariffs$Importer.f <- factor(hs12_all_tariffs$`Reporter_ISO_N`)
hs12_all_tariffs$Exporter.f <- factor(hs12_all_tariffs$`Partner Code`)
hs12_all_tariffs$Pairs.f <- with(hs12_all_tariffs, interaction(Importer.f, Exporter.f))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, SimpleAverage, Year.f, Products.f, Pairs.f)]

reg <- felm(SimpleAverage ~ 1 | Year.f + Products.f + Pairs.f,
            data = hs12_all_tariffs,
            exactDOF = FALSE,
            keepX = FALSE,
            keepCX = FALSE)

fes <- getfe(reg,
             se=TRUE,
             bN = 50
)

Yearfes <- subset(fes,fe == "Year.f")

Yearfes$ci_ub <- Yearfes$effect + (1.96 * Yearfes$se)
Yearfes$ci_lb <- Yearfes$effect - (1.96 * Yearfes$se)
Yearfes <- merge(Yearfes,unique(hs12_all_tariffs[,list(Year,Year.f)]),by.x = "idx",by.y="Year.f")

ggplot(data = Yearfes, aes(Year,effect)) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), color = "grey35") +
  geom_line(color = "royalblue4", size = 1) +
  geom_point(color = "royalblue4") +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(-3,1), minor_breaks = NULL) +
  xlab(label = "") +
  ylab(label = "Tariff rate (%)") +
  labs(title = "Tariff Rate Over Time, Controlling for Product/Country Pairs")


productfes <- subset(fes,fe == "Products.f")
productfes <- productfes[,c("effect","idx")]

ggplot(data=productfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,3000), minor_breaks = NULL) +
  labs(title="Mean Tariff Rate Across Products, Controlling for Country Pairs/Years") +
  labs(x="Tariff rate (%)", y="Number of products")

pairfes <- subset(fes,fe == "Pairs.f")
pairfes <- pairfes[,c("effect","idx")]

ggplot(data=pairfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000), minor_breaks = NULL) +
  labs(title="Mean Tariff Rate Across Country Pairs, Controlling for Products/Years") +
  labs(x="Tariff rate (%)", y="Number of country pairs")

rm(fes, hs12_all_tariffs, pairfes, Yearfes, productfes, reg)

######TARIFFS V TRADE GAP#####

load(paste(DataPath,"Analysis Data/hs12_all_tariffs_qty.Rda", sep = "/"))

hs12_all_tariffs <- hs12_all_tariffs[, .(Year, ProductCode, Importer, Exporter, SimpleAverage, Qty_log_gap)]
hs12_all_tariffs <- hs12_all_tariffs[!is.na(SimpleAverage)]

tariffs <- hs12_all_tariffs[, .(mean = mean(Qty_log_gap)), by = SimpleAverage]

tariffs <- melt(tariffs, id = "SimpleAverage")
tariffs <- rename(tariffs, "Legend" = "variable") 

ggplot(tariffs, aes(SimpleAverage, value, colour=Legend, size=Legend)) +
  geom_point(aes(colour = Legend), size = 2, alpha = .2) +
  geom_smooth(method = "lm") +
  scale_colour_manual(values=c("royalblue4")) +
  labs(title="Quantity Gap at Different Tariff Rates, 2012-2015") +
  background_grid(major = 'y', minor = "none") +
  labs(x="Tariff rate (%)", y="Quantity gap, log scale") +
  labs(caption="Each point represents a unique tariff rate") +
  theme(legend.position="none")

tariffs <- hs12_all_tariffs[SimpleAverage <= 300, ]
tariffs$SimpleAverage <- round(tariffs$SimpleAverage, digits = 0)

tariffs <- tariffs[, .(mean = mean(Qty_log_gap)), by = SimpleAverage]

tariffs <- melt(tariffs, id = "SimpleAverage")
tariffs <- rename(tariffs, "Legend" = "variable") 

ggplot(tariffs, aes(SimpleAverage, value, colour=Legend, size=Legend)) +
  geom_point(aes(colour = Legend), size = 2, alpha = .2) +
  geom_smooth(method = "lm") +
  scale_colour_manual(values=c("royalblue4")) +
  labs(title="Quantity Gap at Different Tariff Rates, 2012-2015") +
  scale_y_continuous(expand = c(0, 0), limits = c(-5, 5)) +
  background_grid(major = 'y', minor = "none") +
  labs(x="Tariff rate (%)", y="Quantity gap, log scale") +
  labs(caption="Each point = average tariff rate, rounded to nearest whole number") + 
  theme(legend.position="none")

simpreg = lm(Qty_log_gap ~ SimpleAverage, data = hs12_all_tariffs)
summary(simpreg)