library(dplyr)
library(Synth)

# DATA INGESTION AND TRANSFORMATION

World <- read.csv("https://raw.githubusercontent.com/ulfelder/coups-and-growth/master/data.raw.csv", stringsAsFactors=FALSE)

World$idno = as.numeric(as.factor(World$country))  # create numeric country id required for synth()

World$population.ln = log(World$population/1000)  # population size in 1000s, logged
World$trade.ln = log(World$trade)  # trade as % of GDP, logged
World$civtot.ln = log1p(World$civtot)  # civil conflict scale, +1 and logged
World$durable.ln = log1p(World$durable)  # political stability, +1 and logged
World$polscore = with(World, ifelse(polity >= -10, polity, NA)) # create version of Polity score that's missing for -66, -77, and -88

World <- World %>%  # create clocks counting years since last coup (attempt) or 1950, whichever is most recent
    arrange(countrycode, year) %>%
    mutate(cpt.succ.d = ifelse(cpt.succ.n > 0, 1, 0),
           cpt.any.d = ifelse(cpt.succ.n > 0 | cpt.fail.n > 0, 1, 0)) %>%
    group_by(countrycode, idx = cumsum(cpt.succ.d == 1L)) %>%
    mutate(cpt.succ.clock = row_number()) %>%
    ungroup() %>%
    group_by(countrycode) %>%
    mutate(cpt.succ.cumsum = order_by(year, cumsum(cpt.succ.d))) %>%  # get cumsum of years w/successful coups by country to use as selection criterion before call to dataprep()
    ungroup() %>%
    group_by(countrycode, idx = cumsum(cpt.any.d == 1L)) %>%
    mutate(cpt.any.clock = row_number()) %>%
    ungroup() %>%
    select(-idx) %>%
    mutate(cpt.succ.clock.ln = log1p(cpt.succ.clock), # include +1 log versions
           cpt.any.clock.ln = log1p(cpt.any.clock)) %>%
    as.data.frame(.)

# GENERIC TO ALL ITERATIONS

match.criteria = c("gdppc", "fcf", "govfce", "trade", "inflation", "sec.enrr", "polscore", "durable.ln", "cpt.any.clock.ln", "civtot.ln")

# THAILAND 2006

# Get vectors of relevant years based on treatment year
THI.coup.year = 2006
THI.years = seq(THI.coup.year - 5, THI.coup.year + 5)

# Apparently, dataprep() requires balanced panels. So:
# Subset country-year data to balanced panels covering the period of observation and only including a) the treatment case and b) candidate
# controls that have observations for all years in the window but no treatment during the window. If you want to condition control candidacy
# on other stuff like region, this would be the place to do it.
THI.World <- World %>%
    filter(year >= min(THI.years) & year <= max(THI.years)) %>% # filter to desired years
    group_by(idno) %>%  # organize by country
    summarise(coup.ever = sum(cpt.any.d),  # get counts by country of years with coup attempts during that period
              complete.panel = ifelse(length(year)==length(THI.years), 1, 0), # get indicator for countries with complete panels for period of observation
              missing.gdp = sum(is.na(gdppc)),  # get count of years with missing data on dependent var
              missing.fcf = sum(is.na(fcf)),
              missing.govfce = sum(is.na(govfce)),
              missing.trade = sum(is.na(trade)),
              missing.inflation = sum(is.na(inflation)),
              missing.sec.enrr = sum(is.na(sec.enrr)),
              missing.polscore = sum(is.na(polscore))) %>%
    left_join(filter(World, year >= min(THI.years) & year <= max(THI.years)), .) %>%
    # keep only those panels with no coup events, no missing values on PWT GDP, & fewer than 4 missing values on other vars with missingness
    filter(coup.ever==0 & cpt.succ.cumsum > 0 & complete.panel==1 & missing.gdp==0 & missing.fcf <= 3 & missing.govfce <= 3 & 
           missing.trade <= 3 & missing.inflation <= 3 & missing.sec.enrr <= 3 & missing.polscore <= 3) %>% 
    select(1:49) %>%  # cut variables created in summarize step above, which we don't need any more, so next step works
    full_join(., filter(World, country=="Thailand" & year >= min(THI.years) & year <= max(THI.years)))  # put Thailand back in
# Get vector of country ids for candidate controls from that data frame
THI.controls = unique(THI.World$idno[THI.World$country!="Thailand"])

# Run dataprep() on preprocessed data
THI.synth.dat <- dataprep(
    foo = THI.World,
    predictors = match.criteria,
    predictors.op = "mean",
    time.predictors.prior = seq(from = min(THI.years), to = THI.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = unique(THI.World$idno[THI.World$country=="Thailand"]),
    controls.identifier = THI.controls,
    time.optimize.ssr = seq(from = THI.coup.year, to = max(THI.years)),
    time.plot = THI.years
)

# Apply synth() to those data and inspect matching results via tables
THI.synth.out <- synth(THI.synth.dat, optimxmethod="All", verbose=TRUE)
THI.synth.tables <- synth.tab(dataprep.res = THI.synth.dat, synth.res = THI.synth.out)
THI.synth.tables$tab.pred  # inspect balance
THI.synth.tables$tab.w  # inspect weights

# Make diptych plot of path and gap
png("~/coups.and.growth/figs/THI.synth.plots.png", width=5, height=8, unit="in", res=150)
par(mfrow=c(2,1))
path.plot(synth.res = THI.synth.out, dataprep.res = THI.synth.dat,
    Ylab = "GDP per capita, PPP (constant 2011 int'l $)", Xlab = "year",
    Legend = c("Thailand", "synthetic Thailand"), Legend.position = "bottomright")
abline(v = THI.coup.year, lty = "dotted", lwd = 2, col = "gray50")
text(x = THI.coup.year, y = 16000, "coup year", pos = 2, cex = 0.8)
gaps.plot(synth.res = THI.synth.out, dataprep.res = THI.synth.dat,
    Ylab = "gap in GDP per capita, PPP (constant 2011 int'l $)", Xlab = "year",
    Main = NA)
abline(v = THI.coup.year, lty = "dotted", lwd = 2, col = "gray50")
text(x = THI.coup.year, y = 750, "coup year", pos = 2, cex = 0.8)
dev.off()

# PLACEBO TESTS
# Big thanks to Anton Strezhnev for sharing his code to do this part

placebo_list_THI <- list()
placebo_list_THI_data <- list()

### Loop over each of the control states and conduct a placebo test
for (k in THI.controls){

  ## Set numbers for placebo controls/treatments
  placebo_num <- k
  placebo_control <- THI.controls[-which(THI.controls == k)]
  
  ## Prep the data for synthetic matching
  THI_placebo <- dataprep(
    foo=THI.World[THI.World$country!="Thailand",],
    predictors = match.criteria,
    predictors.op="mean",
    time.predictors.prior = seq(from = min(THI.years), to = THI.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = placebo_num,
    controls.identifier = placebo_control,
    time.optimize.ssr = seq(from = THI.coup.year, to = max(THI.years)),
    time.plot = THI.years)
    
   ## Synthetic Matching
   placebo.result <- synth(data.prep.obj = THI_placebo, method="BFGS")

   ## Save to list
   placebo_list_THI[[k]] <- placebo.result
   placebo_list_THI_data[[k]] <- THI_placebo
}

placebo.plot <- function(synth.res, dataprep.res, synth.placebo.res, dataprep.placebo.res, placebo_codes, Ylab="", Xlab="", Main=NA, Ylim=NA, Legend=c("Thailand", "control countries"), exclude=NA, Y0=-1.5, text="Florida passes\n'Stand Your Ground' law:\n10/2005",X0=2002.5, X1=2004, vertical=2005){
  # Exclude placebos in list
  if (is.na(exclude) == FALSE){
    placebo_codes <- placebo_codes[-which(placebo_codes == exclude)]
  }

  # Find gap
  gap <- dataprep.res$Y1plot - (dataprep.res$Y0plot %*% synth.res$solution.w)
  
  # Get Ylim right
  if(sum(is.na(Ylim))>0)
  {
    Ylim <- c(
      -(.3*max(abs(gap)) + max(abs(gap))),
      (.3*max(abs(gap))  + max(abs(gap)))
    )
  }
  
  ## Plot the main time series
  plot(x=dataprep.res$tag$time.plot, y=gap, type="l", ylim=Ylim, xaxs="i", yaxs="i", xlab=Xlab, ylab=Ylab, lwd=2)

  
  ## Plot the placebo time series
  for (k in placebo_codes){
    ## Don't plot if MSRE of Placebo > 5*MSRE of treatment
    if (synth.placebo.res[[k]]$loss.v < synth.res$loss.v*5){
      ## compute gap
      gap_placebo <-  dataprep.placebo.res[[k]]$Y1plot - (dataprep.placebo.res[[k]]$Y0plot %*% synth.placebo.res[[k]]$solution.w)
      
      ## Plot
      lines(x=dataprep.placebo.res[[k]]$tag$time.plot, y=gap_placebo, col="grey70")
      #gaps.plot(synth.res = placebo_list_THI[[k]], dataprep.res = placebo_list_THI_data[[k]])
    }
  
  }
  
  ## Replot the main time series
  lines(x=dataprep.res$tag$time.plot, y=gap, type="l", lwd=2)
  
  ## Horizontal Line
  abline(h=0, lwd=2, lty=2)
  
  ## Plot the point of treatment
  abline(v=THI.coup.year, col="red", lty=2)
  
  ## Legend
  legend(x="bottomright", legend=Legend, col=c("black","grey70"), lwd=c(2,1))
  
}

### Plot the permutation/placebo test
png("~/coups.and.growth/figs/THI.placebo.png", width = 5, height = 5, unit = "in", res=150)
placebo.plot(synth.res=THI.synth.out, dataprep.res = THI.synth.dat, synth.placebo.res = placebo_list_THI, dataprep.placebo.res = placebo_list_THI_data, placebo_codes = THI.controls, Xlab="year", Ylab="gap in GDP per capita relative to synthetic control")
dev.off()


###########################
# MADAGASCAR 2009
###########################

MAG.coup.year = 2009

MAG.years = seq(MAG.coup.year - 5, MAG.coup.year + 4) # have to cut this one shorter than THI b/c WDI GDP ends in 2013

MAG.World <- World %>%
    filter(year >= min(MAG.years) & year <= max(MAG.years)) %>% # filter to desired years
    group_by(idno) %>%  # organize by country
    summarise(coup.ever = sum(cpt.any.d),  # get counts by country of years with coup attempts during that period
              complete.panel = ifelse(length(year)==length(MAG.years), 1, 0), # get indicator for countries with complete panels for period of observation
              missing.gdp = sum(is.na(gdppc)),  # get count of years with missing data on dependent var
              missing.fcf = sum(is.na(fcf)),
              missing.govfce = sum(is.na(govfce)),
              missing.trade = sum(is.na(trade)),
              missing.inflation = sum(is.na(inflation)),
              missing.sec.enrr = sum(is.na(sec.enrr)),
              missing.polscore = sum(is.na(polscore))) %>%
    left_join(filter(World, year >= min(MAG.years) & year <= max(MAG.years)), .) %>%
    # keep only those panels with no coup events, no missing values on PWT GDP, & fewer than 4 missing values on other vars with missingness
    filter(coup.ever==0 & cpt.succ.cumsum > 0 & complete.panel==1 & missing.gdp==0 & missing.fcf <= 3 & missing.govfce <= 3 & 
           missing.trade <= 3 & missing.inflation <= 3 & missing.sec.enrr <= 3 & missing.polscore <= 3) %>% 
    select(1:49) %>%  # cut variables created in summarize step above, which we don't need any more, so next step works
    full_join(., filter(World, country=="Madagascar" & year >= min(MAG.years) & year <= max(MAG.years)))  # put Madagascar back in
# Get vector of country ids for candidate controls from that data frame
MAG.controls = unique(MAG.World$idno[MAG.World$country!="Madagascar"])

MAG.synth.dat <- dataprep(
    foo = MAG.World,
    predictors = match.criteria,
    predictors.op = "mean",
    time.predictors.prior = seq(from = min(MAG.years), to = MAG.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = unique(MAG.World$idno[MAG.World$country=="Madagascar"]),
    controls.identifier = MAG.controls,
    time.optimize.ssr = seq(from = MAG.coup.year, to = max(MAG.years)),
    time.plot = MAG.years
)

MAG.synth.out <- synth(MAG.synth.dat, optimxmethod="All", verbose=TRUE)
MAG.synth.tables <- synth.tab(dataprep.res = MAG.synth.dat, synth.res = MAG.synth.out)
MAG.synth.tables$tab.pred  # inspect balance
MAG.synth.tables$tab.w  # inspect weights
png("~/coups.and.growth/figs/MAG.synth.plots.png", width=5, height=8, unit="in", res=150)
par(mfrow=c(2,1))
path.plot(synth.res = MAG.synth.out, dataprep.res = MAG.synth.dat,
    Ylab = "GDP per capita, PPP (constant 2011 int'l $)", Xlab = "year",
    Legend = c("Madagascar", "synthetic Madagascar"), Legend.position = "bottomright")
abline(v = MAG.coup.year, lty = "dotted", lwd = 2, col = "gray50")
text(x = MAG.coup.year, y = 2000, "coup year", pos = 2, cex = 0.8)
gaps.plot(synth.res = MAG.synth.out, dataprep.res = MAG.synth.dat,
    Ylab = "gap in GDP per capita, PPP (constant 2011 int'l $)", Xlab = "year",
    Main = NA)
abline(v = MAG.coup.year, lty = "dotted", lwd = 2, col = "gray50")
text(x = MAG.coup.year, y = 300, "coup year", pos = 2, cex = 0.8)
dev.off()

# PLACEBO TESTS

placebo_list_MAG <- list()
placebo_list_MAG_data <- list()

### Loop over each of the control states and conduct a placebo test
for (k in MAG.controls){

  ## Set numbers for placebo controls/treatments
  placebo_num <- k
  placebo_control <- MAG.controls[-which(MAG.controls == k)]
  
  ## Prep the data for synthetic matching
  MAG_placebo <- dataprep(
    foo=MAG.World[MAG.World$country!="Madagascar",],
    predictors = match.criteria,
    predictors.op="mean",
    time.predictors.prior = seq(from = min(MAG.years), to = MAG.coup.year - 1),
    dependent = "gdppc",
    unit.variable = "idno",
    unit.names.variable = "country",
    time.variable = "year",
    treatment.identifier = placebo_num,
    controls.identifier = placebo_control,
    time.optimize.ssr = seq(from = MAG.coup.year, to = max(MAG.years)),
    time.plot = MAG.years)
    
   ## Synthetic Matching
   placebo.result <- synth(data.prep.obj = MAG_placebo, method="BFGS")

   ## Save to list
   placebo_list_MAG[[k]] <- placebo.result
   placebo_list_MAG_data[[k]] <- MAG_placebo
}

placebo.plot <- function(synth.res, dataprep.res, synth.placebo.res, dataprep.placebo.res, placebo_codes, Ylab="", Xlab="", Main=NA, Ylim=NA, Legend=c("Madagascar", "control countries"), exclude=NA, Y0=-1.5, text="Florida passes\n'Stand Your Ground' law:\n10/2005",X0=2002.5, X1=2004, vertical=2005){
  # Exclude placebos in list
  if (is.na(exclude) == FALSE){
    placebo_codes <- placebo_codes[-which(placebo_codes == exclude)]
  }

  # Find gap
  gap <- dataprep.res$Y1plot - (dataprep.res$Y0plot %*% synth.res$solution.w)
  
  # Get Ylim right
  if(sum(is.na(Ylim))>0)
  {
    Ylim <- c(
      -(.3*max(abs(gap)) + max(abs(gap))),
      (.3*max(abs(gap))  + max(abs(gap)))
    )
  }
  
  ## Plot the main time series
  plot(x=dataprep.res$tag$time.plot, y=gap, type="l", ylim=Ylim, xaxs="i", yaxs="i", xlab=Xlab, ylab=Ylab, lwd=2)

  
  ## Plot the placebo time series
  for (k in placebo_codes){
    ## Don't plot if MSRE of Placebo > 5*MSRE of treatment
    if (synth.placebo.res[[k]]$loss.v < synth.res$loss.v*5){
      ## compute gap
      gap_placebo <-  dataprep.placebo.res[[k]]$Y1plot - (dataprep.placebo.res[[k]]$Y0plot %*% synth.placebo.res[[k]]$solution.w)
      
      ## Plot
      lines(x=dataprep.placebo.res[[k]]$tag$time.plot, y=gap_placebo, col="grey70")
      #gaps.plot(synth.res = placebo_list_MAG[[k]], dataprep.res = placebo_list_MAG_data[[k]])
    }
  
  }
  
  ## Replot the main time series
  lines(x=dataprep.res$tag$time.plot, y=gap, type="l", lwd=2)
  
  ## Horizontal Line
  abline(h=0, lwd=2, lty=2)
  
  ## Plot the point of treatment
  abline(v=MAG.coup.year, col="red", lty=2)
  
  ## Legend
  legend(x="bottomright", legend=Legend, col=c("black","grey70"), lwd=c(2,1))
  
}

### Plot the permutation/placebo test
png("~/coups.and.growth/figs/MAG.placebo.png", width = 5, height = 5, unit = "in", res=150)
placebo.plot(synth.res=MAG.synth.out, dataprep.res = MAG.synth.dat, synth.placebo.res = placebo_list_MAG, dataprep.placebo.res = placebo_list_MAG_data, placebo_codes = MAG.controls, Xlab="year", Ylab="gap in GDP per capita relative to synthetic control")
dev.off()
