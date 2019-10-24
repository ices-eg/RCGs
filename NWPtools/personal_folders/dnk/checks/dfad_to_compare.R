
#DFAD_lookups_for_fishline

library(dplyr)
library(haven)
library(lubridate)

years <- c(2016:2019)


dfad <- c()
for (i in years) {

dfad_0 <- readRDS(paste("Q:/dfad/data/Data/udvidet_data/dfad_udvidet", i, ".rds", sep = ""))

dfad <- bind_rows(dfad, dfad_0)

}

dfad <- mutate(dfad, year = year(ldato))

hag <- summarise(group_by(filter(dfad, art == "HAG"), year, art, dfadfvd_ret), tons = round(sum(hel, na.rm = T)/1000, digits = 0))

