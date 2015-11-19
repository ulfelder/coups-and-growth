# Application of synthetic control method to estimate effect of Madagascar 2009 coup on GDP growth

library(dplyr)
library(Synth)
library(gplots)

# DATA INGESTION AND TRANSFORMATION

World <- read.csv("~/coups.and.growth/data.out/data.raw.csv", stringsAsFactors=FALSE)

World$idno = as.numeric(as.factor(World$country))  # create numeric country id required for synth()

World$gdppc.ln = log(World$gdppc)  # GDP per capita, PPP (constant 2011 int'l $), logged
World$population.ln = log(World$population/1000)  # population size in 1000s, logged
World$trade.ln = log(World$trade)  # trade as % of GDP, logged
World$inflation.s = rep(NA, nrow(World))  # inflation, s-curve based on square root
for (i in 1:nrow(World))
    if (is.na(World$inflation[i])==TRUE) {
        World$inflation.s[i] = NA
    } else if (World$inflation[i] < 0) {
        World$inflation.s[i] = -1 * sqrt(abs(World$inflation[i]))
    } else {
        World$inflation.s[i] = sqrt(World$inflation[i])
    }
World$fcf.s = sqrt(abs(World$fcf))
World$govfce.s = sqrt(abs(World$govfce))
World$oda.s = sqrt(abs(World$oda))
World$civtot.ln = log1p(World$civtot)  # civil conflict scale, +1 and logged
World$durable.ln = log1p(World$durable)  # political stability, +1 and logged
World$polscore = with(World, ifelse(polity >= -10, polity, NA)) # create version of Polity score that's missing for -66, -77, and -88

# Markers for, and cumulative sums of, years with a) successful coups or b) any coup attempts, per P&T
World <- group_by(World, country) %>%
    arrange(year) %>%
    mutate(cpt.succ.d = ifelse(cpt.succ.n > 0, 1, 0),
           cpt.any.d = ifelse(cpt.succ.n > 0 | cpt.fail.n > 0, 1, 0),
           cpt.succ.cumsum = cumsum(cpt.succ.d),
           cpt.any.cumsum = cumsum(cpt.any.d))

# Clocks counting years since last coup (attempt) or 1950, whichever is most recent, raw and logged
World <- arrange(World, country, year) %>%
    group_by(country, idx = cumsum(cpt.succ.d == 1L)) %>%
    mutate(cpt.succ.clock = row_number(),
           cpt.succ.clock.ln = log1p(cpt.succ.clock)) %>%
    select(-idx) %>%
    as.data.frame()
World <- arrange(World, country, year) %>%
    group_by(country, idx = cumsum(cpt.any.d == 1L)) %>%
    mutate(cpt.any.clock = row_number(),
           cpt.any.clock.ln = log1p(cpt.any.clock)) %>%
    select(-idx) %>%
    as.data.frame()

# Get vectors of relevant years based on treatment year
MAG.coup.year = 2009
MAG.years = seq(MAG.coup.year - 5, MAG.coup.year + 4) # Have to truncate post-treatment obs bc WDI only runs thru 2013 right now

# Set vector of covariates for matching
match.criteria = c("gdppc.ln", "fcf.s", "govfce.s", "trade.ln", "oda.s", "inflation.s", "uds.mean",
  "cpt.any.clock.ln", "civtot.ln")

# Apparently, dataprep() requires balanced panels. So:
# Subset country-year data to balanced panels covering the period of observation and only including a) the treatment case and b) candidate
# controls that have observations for all years in the window but no treatment during the window. If you want to condition control candidacy
# on other stuff like region, this would be the place to do it.
MAG.criteria <- filter(World, year >= min(MAG.years) & year <= max(MAG.years)) %>% # filter to desired years
    group_by(idno) %>%  # organize by country
    summarise(coup.before = ifelse(first(cpt.any.cumsum) > 0, 1, 0), # marker for countries that had 1+ coup attempt before window
              coup.during = sum(cpt.succ.d),  # counts by country of years with successful coups during that period
              complete.panel = ifelse(length(year)==length(MAG.years), 1, 0), # get indicator for countries with complete panels for period of observation
              missing.gdp = sum(is.na(gdppc)),  # get count of years with missing data on dependent var
              missing.fcf = sum(is.na(fcf)),  # ditto for matching criteria
              missing.govfce = sum(is.na(govfce)),
              missing.trade = sum(is.na(trade)),
              missing.inflation = sum(is.na(inflation)),
              missing.oda = sum(is.na(oda)),
              missing.uds = sum(is.na(uds.mean)))
MAG.controls = MAG.criteria %>%  # use summaries just created to identify countries that meet all criteria
    filter(coup.before==1 & coup.during==0 & complete.panel==1 & missing.gdp <= 2 & missing.fcf <= 2 & missing.govfce <= 2 &
        missing.trade <= 2 & missing.oda <= 2 & missing.inflation <= 2 & missing.uds <= 2) %>%
    select(idno) %>%
    unlist()
names(MAG.controls) = NULL

# Filter original data frame by those cases plus Madagascar, then by desired years
MAG.World <- World[which(World$idno %in% c(MAG.controls, unique(World$idno[World$country=="Madagascar"]))),]
MAG.World <- filter(MAG.World, year >= min(MAG.years) & year <= max(MAG.years))

# Run dataprep() on preprocessed data
MAG.synth.dat <- dataprep(
    foo = MAG.World,
    predictors = match.criteria,
    predictors.op = "median",  # tried mean, too, but median produced lower MSPE
    time.predictors.prior = seq(from = min(MAG.years), to = MAG.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = unique(MAG.World$idno[MAG.World$country=="Madagascar"]),
    controls.identifier = MAG.controls,
    time.optimize.ssr = seq(from = min(MAG.years), to = MAG.coup.year - 1), # allow variation in treatment year
    time.plot = MAG.years
)

# Apply synth() to those data. Tried optimxmethod="All"; no improvement
MAG.synth.out <- synth(MAG.synth.dat)
# inspect matching results via tables
MAG.synth.tables <- synth.tab(dataprep.res = MAG.synth.dat, synth.res = MAG.synth.out) # generate tables
MAG.synth.tables$tab.pred  # inspect balance
MAG.synth.tables$tab.w  # inspect weights

# Plot path
path.case <- MAG.synth.dat$Y1plot
path.synth <- MAG.synth.dat$Y0plot %*% MAG.synth.out$solution.w
png("~/coups.and.growth/figs/MAG.synth.plots.path.png", width=5, height=4, unit="in", res=150)
par(cex.axis=3/4, cex.lab=4/5, mai=c(1/2,1,1/4,1/4))
plot(x=MAG.years, y=path.synth, type="n", axes=FALSE, ylim=c(1000,2000),
  xlab="year", ylab="GDP per capita (PPP, constant 2011 intl $)", lwd=2) 
abline(v=MAG.coup.year, col="red", lty=2) # Vertical line at the point of treatment
text(x = MAG.coup.year, y = 2000, "coup year", pos=2, cex=0.8)  # Label that line
axis(1, tick=FALSE)
axis(2, tick=FALSE, at=seq(1000,2000,250), labels=paste0("$", seq(1000,2000,250)), las=2, pos=min(MAG.years))
lines(x=MAG.years, y=path.case, type="l", lwd=2) # Plot the main series
lines(x=MAG.years, y=path.synth, type="l", lty=2, lwd=2, col="gray50") # Plot the synth series as dashed line
legend(x="bottomright", legend=c("Madagascar", "synthetic control"), lty=c(1,2), lwd=c(2,2), col=c("black", "gray50"), cex=3/4, bty="n")
dev.off()

# Plot gap
gap <- MAG.synth.dat$Y1plot - (MAG.synth.dat$Y0plot %*% MAG.synth.out$solution.w)  # Get vector of gap
gap.as.pct <- 100 * (gap / MAG.synth.dat$Y0plot %*% MAG.synth.out$solution.w)  # Calculate gap as % of synthetic control
png("~/coups.and.growth/figs/MAG.synth.plots.gap.png", width=5, height=4, unit="in", res=150)
par(cex.axis=3/4, cex.lab=4/5, mai=c(1/2,1,1/4,1/4))
plot(x=MAG.years, y=gap.as.pct, type="n", axes=FALSE, ylim=c(-25,25),
  xlab="year", ylab="Percent difference in GDP per capita \nrelative to synthetic control", lwd=2) 
segments(x0=min(MAG.years), x1=max(MAG.years), y0=0, y1=0, lwd=2, lty=2, col="gray25") # Horizontal Line at 0
abline(v=MAG.coup.year, col="red", lty=2) # Vertical line at the point of treatment
text(x = MAG.coup.year, y = 25, "coup year", pos = 2, cex = 0.8)
axis(1, tick=FALSE)
axis(2, tick=FALSE, at=seq(-20,20,10), labels=paste0(seq(-20,20,10), "%"), las=2)
lines(x=MAG.years, y=gap.as.pct, type="l", lwd=2) # Plot the series
dev.off()

# Output information about balance
png("~/coups.and.growth/figs/MAG.balance.v5.png", width=5, height=2.5, unit="in", res=150)
textplot(MAG.synth.tables$tab.pred, mar=c(1/4,1/4,1/4,1/2))
dev.off()

# Output information about weights
png("~/coups.and.growth/figs/MAG.weights.v5.png", width=3, height=9, unit="in", res=150)
textplot(MAG.synth.tables$tab.w[,1:2], show.rownames=FALSE, mar=c(1/4,1/4,1/4,1/2))
dev.off()

#PERMUTATION TESTS

# Function to apply; same specs as above for treated case
f.placebo.data <- function(id) {

  placebo.dat <- dataprep(
    foo = filter(MAG.World, country!="Madagascar"),  # drop Madagascar from data set for this
    predictors = match.criteria,
    predictors.op = "median",
    time.predictors.prior = seq(from = min(MAG.years), to = MAG.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = id,
    controls.identifier = MAG.controls[which(MAG.controls!=id)],
    time.optimize.ssr = seq(from = min(MAG.years), to = MAG.coup.year - 1),
    time.plot = MAG.years)

  return(placebo.dat)

}

placebo_data_list <- lapply(MAG.controls, f.placebo.data)

placebo_results_1 <- synth(placebo_data_list[[1]])
placebo_results_2 <- synth(placebo_data_list[[2]])
placebo_results_3 <- synth(placebo_data_list[[3]]) # failed to converge
placebo_results_4 <- synth(placebo_data_list[[4]]) # MSPE too high
placebo_results_5 <- synth(placebo_data_list[[5]])
placebo_results_6 <- synth(placebo_data_list[[6]])
placebo_results_7 <- synth(placebo_data_list[[7]])
placebo_results_8 <- synth(placebo_data_list[[8]])
placebo_results_9 <- synth(placebo_data_list[[9]])
placebo_results_10 <- synth(placebo_data_list[[10]])
placebo_results_11 <- synth(placebo_data_list[[11]])
placebo_results_12 <- synth(placebo_data_list[[12]]) # MSPE too high
placebo_results_13 <- synth(placebo_data_list[[13]])
placebo_results_14 <- synth(placebo_data_list[[14]]) 
placebo_results_15 <- synth(placebo_data_list[[15]])
placebo_results_16 <- synth(placebo_data_list[[16]]) # MSPE too high
placebo_results_17 <- synth(placebo_data_list[[17]]) # MSPE too high
placebo_results_18 <- synth(placebo_data_list[[18]]) 
placebo_results_19 <- synth(placebo_data_list[[19]])
placebo_results_20 <- synth(placebo_data_list[[20]]) # MSPE too high
placebo_results_21 <- synth(placebo_data_list[[21]]) # MSPE too high
placebo_results_22 <- synth(placebo_data_list[[22]]) # MSPE too high
placebo_results_23 <- synth(placebo_data_list[[23]])
placebo_results_24 <- synth(placebo_data_list[[24]]) # MSPE too high
placebo_results_25 <- synth(placebo_data_list[[25]])
placebo_results_26 <- synth(placebo_data_list[[26]]) # MSPE too high
placebo_results_27 <- synth(placebo_data_list[[27]])
placebo_results_28 <- synth(placebo_data_list[[28]]) # MSPE too high
placebo_results_29 <- synth(placebo_data_list[[29]]) # MSPE too high
placebo_results_30 <- synth(placebo_data_list[[30]])
placebo_results_31 <- synth(placebo_data_list[[31]]) # MSPE too high
placebo_results_32 <- synth(placebo_data_list[[32]]) # MSPE too high
placebo_results_33 <- synth(placebo_data_list[[33]]) # MSPE too high
placebo_results_34 <- synth(placebo_data_list[[34]])
placebo_results_35 <- synth(placebo_data_list[[35]])
placebo_results_36 <- synth(placebo_data_list[[36]]) # MSPE too high
placebo_results_37 <- synth(placebo_data_list[[37]]) 
placebo_results_38 <- synth(placebo_data_list[[38]])
placebo_results_39 <- synth(placebo_data_list[[39]])
placebo_results_40 <- synth(placebo_data_list[[40]]) # MSPE too high
placebo_results_41 <- synth(placebo_data_list[[41]]) # MSPE too high
placebo_results_42 <- synth(placebo_data_list[[42]])
placebo_results_43 <- synth(placebo_data_list[[43]]) # did not converge
placebo_results_44 <- synth(placebo_data_list[[44]])
placebo_results_45 <- synth(placebo_data_list[[45]]) # MSPE too high
placebo_results_46 <- synth(placebo_data_list[[46]])
placebo_results_47 <- synth(placebo_data_list[[47]]) # MSPE too high
placebo_results_48 <- synth(placebo_data_list[[48]]) # MSPE too high
placebo_results_49 <- synth(placebo_data_list[[49]]) # MSPE too high
placebo_results_50 <- synth(placebo_data_list[[50]])

## function to plot lines from placebo results by number
placebo.line.plot <- function(i) {
  solution.i <- eval(parse(text=paste0("placebo_results_", i, "$solution.w")))
  gap <- placebo_data_list[[i]]$Y1plot - (placebo_data_list[[i]]$Y0plot %*% solution.i)
  gap.as.pct <- 100 * (gap / placebo_data_list[[i]]$Y0plot %*% solution.i)
  lines(x = MAG.years, y = gap.as.pct, col = "grey70")
}

## Make plot

  # Find gap
  gap <- MAG.synth.dat$Y1plot - (MAG.synth.dat$Y0plot %*% MAG.synth.out$solution.w)
  gap.as.pct <- 100 * (gap / MAG.synth.dat$Y0plot %*% MAG.synth.out$solution.w)

  ## Make vector of placebo tests to plot
  placebos <- c(1:2,5:11,13:15,18:19,23,25,27,30,34:35,37:39,42,44,46,50)
  
  ## Start plot device
  png("~/coups.and.growth/figs/MAG.placebo.v5.png", width=5, height=5, unit="in", res=150)
  par(cex.axis=0.75)

  ## Set up plot using the main time series
  plot(x=MAG.years, y=gap.as.pct, type="n", xaxs="i", yaxs="i", ylim=c(-50,50),
    xlab="year", ylab="% difference in GDP per capita rel. to synthetic control", lwd=2)

  ## Plot the placebo time series
  for (k in placebos) {
    placebo.line.plot(k)
  }
  
  ## Horizontal Line at 0
  abline(h=0, lwd=2, lty=2)
  
  ## Vertical line at the point of treatment
  abline(v=MAG.coup.year, col="red", lty=2)

  ## Replot the main time series
  lines(x=MAG.years, y=gap.as.pct, type="l", lwd=2) 

  ## Legend
  legend(x="bottomleft", bty="n", legend=c("Madagascar", "placebo tests"), col=c("black","grey70"), lwd=c(2,1), cex=0.8)

  dev.off()
