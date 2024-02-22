###################################################################
# Load fleet register
###################################################################
#
# This script is used to load Fleet Register data in the prepared
# form.
#
#
###################################################################
# Authors:
# - Kasia Krakówka
#
# Dev. notes:
#
# - 20240220: Created
#
###################################################################

# Empty warnings from previous code

assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Load data
load(
  paste0(
    params$data_dir_fleet,
    "/fleetRegister_Prepared_",
    params$year,
    ".Rdata"
  )
)

# put some necessary data prep part below
######################
# FILTER the data out
######################

fleetreg <- fleetreg[License_Ind == "Y",
                     c(
                       "Country_Code",
                       "CFR",
                       "Event_Code",
                       "Event_Start_Date",
                       "Event_End_Date",
                       "License_Ind",
                       "Loa",
                       "Ton_Gt",
                       "Ton_Oth",
                       "Ton_Gts",
                       "Power_Main",
                       "Power_Aux"
                     )]

#Format
fleetreg$Loa <-as.numeric(fleetreg$Loa)
fleetreg$Power_Main<-as.numeric(fleetreg$Power_Main)
fleetreg$Ton_Gt<-as.numeric(fleetreg$Ton_Gt)

#Vessel Length Class
fleetreg[, vesLenCat := cut(Loa,
                         breaks = c(0, 8, 10, 12, 18, 24, 40, 200),
                         labels=c("[0-8[","[8-10[","[10-12[","[12-18[","[18-24[","[24-40[","[40+["), include.lowest=T,
                         right = F)]

#checks
if(sum(is.na(fleetreg$vesLenCat))!=0 |sum(is.na(fleetreg$Power_Main))!=0 |
   sum(is.na(fleetreg$Ton_Gt))!=0){
  warning('Check fleet data, some NAs in data')
}

#creates status date and combines data

Date_0 <- as.numeric(paste0(params$year, '0101'))
Date_previous <- as.numeric(paste0(params$year - 1, '0101'))

fleetreg$Status <- 0

Fleet_Register_0 <-
  fleetreg[Event_Start_Date <= Date_0 & Event_End_Date >= Date_0,
           Status := Date_0, ]
Fleet_Register_0 <- Fleet_Register_0[Status != 0, ]

fleetreg$Status <- 0
Fleet_Register_Previous <-
  fleetreg[Event_Start_Date <= Date_previous &
             Event_End_Date >= Date_previous,
           Status := Date_previous, ]
Fleet_Register_Previous <- Fleet_Register_Previous[Status != 0, ]

Fleet<-rbind(Fleet_Register_0,Fleet_Register_Previous)

rm(fleetreg,Fleet_Register_0,Fleet_Register_Previous)

# Print end message
if (is_empty(warnings())) {
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed"))
  cat("\n")
  cat("\n")
} 
