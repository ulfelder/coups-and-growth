library(dplyr)
library(tidyr)
library(readxl)
library(countrycode)
library(WDI)

# DATA INGESTION

# Powell & Thyne coup data
CoupsPT <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt") %>% # get the latest list of events from the web
    select(country, year, coup) %>% # get rid of columns we don't need
    group_by(country, year) %>% # group by country and year for event counting to follow
    summarise(cpt.succ.n = sum(as.logical(coup==2)), cpt.fail.n = sum(as.logical(coup==1))) # create country-year counts of successful and failed coups
CoupsPT$countrycode <- countrycode(CoupsPT$country, "country.name", "iso3c") # add iso3c country codes for merging

# Marshall & Marshall coup data
temp <- paste0(tempfile(), ".xls")
download.file("http://www.systemicpeace.org/inscr/CSPCoupsAnnual2014.xls", destfile = temp, mode = "wb")
CoupsMM <- read_excel(path = temp) %>%
    select(-ccode, -scode)
CoupsMM$countrycode <- countrycode(CoupsMM$country, "country.name", "iso3c")

# Penn World Tables
temp2 <- paste0(tempfile(), ".xlsx")
download.file("http://www.rug.nl/research/ggdc/data/pwt/v81/pwt81.xlsx", destfile = temp2, mode = "wb")
PWT <- read_excel(path = temp2, sheet = 3) %>%
    select(countrycode, year, rgdpe, rgdpna, pop)

# Polity
temp3 <- paste0(tempfile(), ".xls")
download.file("http://www.systemicpeace.org/inscr/p4v2014.xls", destfile = temp3, mode = "wb")
Polity <- read_excel(path = temp3) %>%
    select(country, year, democ, autoc, polity, polity2, durable, exrec, exconst, polcomp) %>% # cut down to vars we want
    filter(year >= 1950) # cut down to 1950- b/c that's when PWT starts
Polity$countrycode <- countrycode(Polity$country, "country.name", "iso3c") # add iso3c country codes for merging

# WDI
WDI <- c("NY.GDP.PCAP.PP.KD",          # GDP per capita, PPP (constant 2011 international $)
         "NE.TRD.GNFS.ZS",             # Trade (% of GDP)
         "NE.GDI.FTOT.ZS",             # FCF
         "NE.CON.GOVT.ZS",             # GovFCE
         "FP.CPI.TOTL.ZG",             # Inflation, consumer prices (annual %)
         "SP.POP.TOTL",                # Population, total
         "SE.SEC.ENRR",                # School enrollment, secondary (% of pop of official 2ndary school age)
         "NY.ADJ.DNGY.GN.ZS") %>%      # Adjusted savings: energy depletion (% of GNI)                                            
    WDI(country="all", indicator = ., extra = FALSE, start = 1960, end = 2014)
WDI$countrycode <- countrycode(WDI$iso2c, "iso2c", "iso3c")  # add iso3c country codes
WDI <- WDI %>%
    filter(is.na(countrycode)==FALSE) %>%  # drop rows for regions and world
    select(-iso2c, -country)  # drop extra country ids
names(WDI) <- c("year", "gdppc", "trade", "fcf", "govfce", "inflation", "population", "sec.enrr", "energy.gni", "countrycode")  # give vars better names

# MEPV
temp4 <- paste0(tempfile(), ".xls")
download.file("http://www.systemicpeace.org/inscr/MEPV2014.xls", destfile = temp4, mode = "wb")
MEPV <- read_excel(path = temp4) 
names(MEPV) <- tolower(names(MEPV))  # convert var names to lower case
MEPV$countrycode <- countrycode(MEPV$country, "country.name", "iso3c") # add iso3c country codes for merging
MEPV <- select(MEPV, countrycode, year, inttot, civtot)  # keep only vars we need 

# Unified Democracy Scores
temp5 <- tempfile()
download.file("http://unified-democracy-scores.org/files/20140312/z/uds_summary.csv.gz", temp5)
UDS <- read.csv(gzfile(temp5), stringsAsFactors = FALSE)
UDS$countrycode <- countrycode(UDS$country, "country.name", "iso3c")
UDS <- select(UDS, countrycode, year, uds.mean = mean)

# Merge them on country codes, using Polity as the base because it is only complete one
Meld <- left_join(Polity, PWT) %>% # add PWT to Polity, dropping PWT's country so we don't get extra sets of country names
    left_join(., select(ungroup(CoupsPT), -country)) %>% # add P&T coups to that; have to use ungroup to get rid of country for some reason
    left_join(., select(CoupsMM, -country)) %>% # add M&M coups to that
    left_join(., WDI) %>%  # add WDI
    left_join(., MEPV) %>%  # add MEPV
    left_join(., UDS) %>%  # add UDS
    mutate(cpt.succ.n = ifelse(is.na(cpt.succ.n), 0, cpt.succ.n), cpt.fail.n = ifelse(is.na(cpt.fail.n), 0, cpt.fail.n)) %>% # replace coup NAs with 0s
    arrange(country, year)

# Remove rows with duplicated values of countrycode/year pairs, keeping only the first appearance
Meld <- distinct(Meld, countrycode, year)

# Write out the resulting table
write.csv(Meld, "~/coups.and.growth/data.out/data.raw.csv", row.names=FALSE)
