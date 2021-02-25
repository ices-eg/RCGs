#----------------------
# Subset data from 2019 
# RDB data
#----------------------

rm(list=(ls()))

library(data.table)
load("../RDB_All_Regions_CS_2019_prepared_20200502.Rdata")

setDT(tr)
set.seed(1)
tr[, .SD[sample(.N, 1)] , by=.(SamplingType, LandingCountry)] 

hh <- hh[hh$CS_TripId %in% tr$CS_TripId]
hl<- hl[hl$CS_TripId %in% tr$CS_TripId]
sl <- sl[sl$CS_TripId %in% tr$CS_TripId]
sl_master <- sl_master[sl_master$CS_TripId %in% tr$CS_TripId]
slmaster  <- slmaster[slmaster$CS_TripId %in% tr$CS_TripId]
ca <- ca[ca$CS_TripId %in% tr$CS_TripId]

save(tr, hh, hl, sl, sl_master, slmaster, ca, file_info_cs, file = "../RDB_All_Regions_CS_2019_prepared_20200502_SUBSET.Rdata")
