library(tidyverse)

# ethnic comparison across years
year_select <- "AcrossYears" # "All (2000-2019)" "2000-2004"       "2005-2009"       "2010-2014"       "2015-2019"
cancer_site <- "Prostate"  # Breast" "Prostate" "Liver" "Pancreas"
sex_select <- "Male and Female"  # "Male and Female" "Male"            "Female"
race_select <- "AcrossAll"                     
if(year_select=="AcrossYears")  { year_select_v <- d$YEARRANGE!="All (2000-2019)" }
if(year_select!="AcrossYears")  { year_select_v <- d$YEARRANGE==year_select }
if(race_select=="AcrossAll")  { race_select_v <- d$csp_race_mod!="All Races/Ethnicities" }
if(race_select!="AcrossAll")  { race_select_v <- d$csp_race_mod==race_select }
d.select <- filter(d, year_select_v & race_select_v & SEERCODE==cancer_site & sex==sex_select)
# group by race/ethnicity and then across years
b <- barplot(aair~csp_race_mod+YEARRANGE, data=d.select, beside=T,
             cex.lab=1.0, font.lab=1.0,
             cex.axis=1.0, font.axis=1.0,
             las=2,
             xlab="", ylab="Age-adjusted Incidence Rates",
             main="", font.main=2, cex.main=1.5,
             col=rainbow(length(unique(d.select$csp_race_mod))),
             legend=F)
# group by year within each race/ethnicity and then across ethnicity 
c <- barplot(aair~YEARRANGE+csp_race_mod, data=d.select, beside=T,
             cex.lab=1.0, font.lab=1.0,
             cex.axis=1.0, font.axis=1.0,
             las=2,
             xlab="", ylab="Age-adjusted Incidence Rates",
             main="", font.main=2, cex.main=1.5,
             col=rainbow(length(unique(d.select$YEARRANGE))),
             legend=F)
# need to add legend to figure...