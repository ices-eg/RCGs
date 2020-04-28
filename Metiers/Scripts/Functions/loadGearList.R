loadGearList <- function(url){
  print("Loading gear list ...")
  x <- data.table(read.xlsx(url, sheet = "formatted"))
  x[,Group:=tolower(Group)]
  setnames(x, old = c("Code","Group"), new = c("gear_code", "gear_group"))
  return(x[,.(gear_code,gear_group)])
}
