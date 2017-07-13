library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lfe)

setwd(dirname(path.expand("~")))

DataPath <- file.path(paste(getwd(),"Dropbox/BJ Customs Evasion", sep="/"))
CodePath <- file.path(paste(getwd(),"Documents/comtrade", sep = "/"))

load(paste(DataPath,"Analysis Data/hs12.Rda", sep = "/"))
hs12 <- as.data.table(hs12)

hs12 <- hs12[!is.na(Qty_log_gap)]
hs12 <- hs12[!is.infinite(Qty_log_gap)]

#For each year, how many product x o-d pairs / all possible product x o-d pairs?

product <- hs12[, uniqueN(`Commodity Code`)]
product_year <- hs12[, uniqueN(`Commodity Code`), by=Period]
product_year <- rename(product_year, Products = V1)

pair <- unique(setDT(hs12), by = c("Importer", "Exporter"))
pair <- pair[, .N]
pair_year <- unique(setDT(hs12), by = c("Importer", "Exporter", "Period"))
pair_year <- pair_year[, .N, by=Period]
pair_year <- rename(pair_year, Pairs = N)

year_coverage <- merge(product_year, pair_year)
year_coverage$Total_products <- product
year_coverage$Total_pairs <- pair

year_coverage$Coverage <- (year_coverage$Products*year_coverage$Pairs)/
  (year_coverage$Total_products*year_coverage$Total_pair)

year_coverage
rm(pair_year, product_year, year_coverage)

#For each product, how many year x o-d pairs / all possible year x o-d pairs?

year <- hs12[, uniqueN(`Period`)]
year_product <- hs12[, uniqueN(`Period`), by=`Commodity Code`]
year_product <- rename(year_product, Years = V1)

pair_product <- unique(setDT(hs12), by = c("Importer", "Exporter", "Commodity Code"))
pair_product <- pair_product[, .N, by= .(`Commodity Code`)]
pair_product <- rename(pair_product, Pairs = N)

product_coverage <- merge(year_product, pair_product)
product_coverage$Total_years <- year
product_coverage$Total_pairs <- pair

product_coverage$Coverage <- (product_coverage$Years*product_coverage$Pairs)/
  (product_coverage$Total_years*product_coverage$Total_pairs)

#Note: Quantity is not reported at the two-digit level
product_coverage[order(Coverage)][1:10]
product_coverage[order(-Coverage)][1:10]

rm(year_product, pair_product, product_coverage)

#For each o-d pair, how many year x product / all possible year x product?

product_pair <- hs12[, uniqueN(`Commodity Code`), by = c("Importer", "Exporter")]
product_pair <- rename(product_pair, Products = V1)

year_pair <- hs12[, uniqueN(`Period`), by = c("Importer", "Exporter")]
year_pair <- rename(year_pair, Years = V1)

pair_coverage <- merge(product_pair, year_pair, by = c("Importer", "Exporter"))
pair_coverage$T_products <- product
pair_coverage$T_years <- year

pair_coverage$Coverage <- (pair_coverage$Products*pair_coverage$Years)/
  (pair_coverage$T_products*pair_coverage$T_years)

pair_coverage$Exporter <- strtrim(pair_coverage$Exporter, 15)
pair_coverage[order(Coverage)][1:10]
pair_coverage[order(-Coverage)][1:10]

rm(product_pair, year_pair, pair_coverage, pair, product, year)

hs12$Period <- as.Date(hs12$Period, "%Y")
hs12$Period <- floor_date(hs12$Period,"year")


periods <- hs12[, .(mean = as.double(mean(Qty_log_gap)),
                    median = as.double(median(Qty_log_gap)),
                    p25 = as.double(quantile(Qty_log_gap,.25)),
                    p75 = as.double(quantile(Qty_log_gap,.75))
),
by=Period]

periods <- melt(periods, id = 'Period')
periods$variable <- factor(periods$variable, levels = c("p25","p75","median","mean"))

ggplot(data=periods ) +
  geom_line(data=periods, aes(x = Period, y = value, colour = variable, size=variable)) +
  background_grid(major = 'y', minor = "none") +
  scale_colour_manual(values=c("grey","grey","black","royalblue4")) +
  scale_size_manual(values = c(1,1,1.1,1.25)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1,1), minor_breaks = NULL) +
  xlab(label = "") +
  ylab(label = "Quantity gap") +
  labs(title="Quantity Gap Over Time")

products <- hs12[, .(mean = as.double(mean(Qty_log_gap)),
                     median = as.double(median(Qty_log_gap)),
                     p25 = as.double(quantile(Qty_log_gap,.25)),
                     p75 = as.double(quantile(Qty_log_gap,.75))
),
by= `Commodity Code`]

ggplot(data=products, aes(mean)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,300), minor_breaks = NULL) +
  labs(title="Mean Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

ggplot(data=products, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title="Median Quantity Gap Across Products") +
  labs(x="Quantity gap", y="Number of products")

ggplot(data=products, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,200), minor_breaks = NULL) +
  labs(title="25th Percentile Quantity Gap Across Products") +
  labs(x="Trade gap", y="Number of products")

ggplot(data=products, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,200), minor_breaks = NULL) +
  background_grid(major = 'y', minor = "none") +
  labs(title="75th Percentile Quantity Gap Across Products") +
  labs(x="Trade gap", y="Number of products")


countries <- hs12[, .(mean = as.double(mean(Qty_log_gap)),
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
  scale_y_continuous(expand = c(0, 0), limits = c(0,2000), minor_breaks = NULL) +
  labs(title="Mean Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(median)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2500), minor_breaks = NULL) +
  labs(title="Median Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(p25)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1500), minor_breaks = NULL) +
  labs(title="25th Percentile Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

ggplot(data=countries, aes(p75)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,2000), minor_breaks = NULL) +
  labs(title="75th Percentile Quantity Gap Across Country Pairs") +
  labs(x="Quantity gap", y="Number of pairs")

rm(periods, products, countries)

#Regress quantity gap on dummies and plot coefficients 

hs12$Period <- as.Date(hs12$Period, "%Y")
hs12$Period <- floor_date(hs12$Period,"year")

hs12$Period.f <- factor(hs12$Period)
hs12$Products.f <- factor(hs12$`Commodity Code`)

hs12$Importer.f <- factor(hs12$`Reporter Code`)
hs12$Exporter.f <- factor(hs12$`Partner Code`)
hs12$Pairs.f <- with(hs12, interaction(Importer.f, Exporter.f))

reg <- felm(Qty_log_gap ~ 1 | Period.f + Products.f + Pairs.f,
            data = hs12,
            exactDOF = FALSE,
            keepX = FALSE,
            keepCX = FALSE)

fes <- getfe(reg,
             se=TRUE,
             bN = 50
)

periodfes <- subset(fes,fe == "Period.f")

periodfes$ci_ub <- periodfes$effect + (1.96 * periodfes$se)
periodfes$ci_lb <- periodfes$effect - (1.96 * periodfes$se)
periodfes <- merge(periodfes,unique(hs12[,list(Period,Period.f)]),by.x = "idx",by.y="Period.f")
periodfes <- rename(periodfes, period = Period)

ggplot(data = periodfes, aes(period,effect)) +
  geom_errorbar(aes(ymin = ci_lb, ymax = ci_ub), color = "grey35") +
  geom_line(color = "royalblue4", size = 1) +
  geom_point(color = "royalblue4") +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(-.05, .10), breaks=seq(-.05, .10, .05)) +
  xlab(label = "") +
  ylab(label = "Quantity gap") +
  labs(title = "Quantity Gap Over Time, Controlling for Product/Country Pair")


productfes <- subset(fes,fe == "Products.f")
productfes <- productfes[,c("effect","idx")]

ggplot(data=productfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,300), minor_breaks = NULL) +
  labs(title="Quantity Gap Across Products, controlling for Country Pair/Year") +
  labs(x="Quantity gap", y="Number of Products")

pairfes <- subset(fes,fe == "Pairs.f")
pairfes <- pairfes[,c("effect","idx")]

ggplot(data=pairfes, aes(effect)) +
  geom_histogram(col="royalblue4",
                 fill="royalblue4",
                 alpha=.2) +
  background_grid(major = 'y', minor = "none") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1500), minor_breaks = NULL) +
  labs(title="Quantity Gap Across Country Pairs, Controlling for Product/Year") +
  labs(x="Quantity gap", y="Number of country pairs")

rm(fes, hs12, pairfes, periodfes, productfes, reg)
