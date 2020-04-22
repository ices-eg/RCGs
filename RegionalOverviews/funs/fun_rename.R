rename_var1<-function(var1){

  if (var1=="FlagCountry") {var1="Country"}
  if (var1=="TripsNumber") {var1="Number of trips"}
  if (var1=="FlagCountry_Loa") {var1="Fleet"}
  if (var1=="FishingActivityLvl5") {var1="Metier"}
  if (var1=="FishingActivityLvl6") {var1="Metier"}
  if (var1=="FishingGround") {var1="fishing ground"}
  if (var1=="Catch_group") {var1="catch group"}
  if (var1=="VesselLengthCategory"){var1="Vessel Length Category"}	  
return(var1)
  }
rename_var2<-function(var2){

  if (var2=="FlagCountry") {var2="Country"}
  if (var2=="TripsNumber") {var2="Number of trips"}
  if (var2=="FlagCountry_Loa") {var2="Fleet"}
  if (var2=="FishingActivityLvl5") {var2="Metier"}
  if (var2=="FishingActivityLvl6") {var2="Metier"}
  if (var2=="FishingGround") {var2="fishing ground"}
  if (var2=="Catch_group") {var2="catch group"}
  if (var2=="VesselLengthCategory"){var2="Vessel Length Category"}	
  if (var2=="LandingCountry"){var2="Landing Country"}

  return(var2)
}
rename_var<-function(Var){

  if (Var == "LandingWeight_1000ton"){Var="Landings (1000 t)"}
  if (Var == "DaysAtSea"){Var="days at Sea"}
  if (Var == "KWDays_1000x"){Var="KW-Days"}
  if (Var=="TripsNumber") {Var="Number of trips"}
  if (Var == "GTDays_1000x"){Var="GT-Days"}
  return(Var)
}