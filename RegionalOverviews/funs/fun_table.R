fun_table<- function (year,year_pyear){
  
  name_row<-c("[0-8[","[8-10[","[10-12[","[12-18[","[18-24[","[24-40[","[40+[","Total")
  rownames(year)<-name_row
  rownames(year_pyear)<-name_row
  minus_i<-NULL
  minus_j<-NULL
  plus_i<-NULL
  plus_j<-NULL
  for (i in 1:nrow(year_pyear)){
    for (j in 1:ncol(year_pyear)){
      if (year_pyear[i,j]<0) {
        year_pyear[i,j]<-paste(year[i,j],"\n(",year_pyear[i,j],")",sep="")
        minus_i<-cbind(minus_i,i)
        minus_j<-cbind(minus_j,j)
      } else {
        (
          if (year_pyear[i,j]==0) {year_pyear[i,j]<-paste(year[i,j],"\n(=)",sep="")
          } else {
            year_pyear[i,j]<-paste(year[i,j],"\n(+",year_pyear[i,j],")",sep="")
            plus_i<-cbind(plus_i,i)
            plus_j<-cbind(plus_j,j)}
        )
      }
    }
  }
  ft <- flextable(cbind(name_row,year_pyear))
  ft <- theme_box(ft)
  ft<-fontsize(ft, part = "all", size = 6)
  ft<-bold(ft,part="header")
  ft<-bg(ft,part="header",bg="grey")
  ft<-bg(ft,j="name_row",bg="grey")
  ft<-color(ft,j=1,part="header",color="grey")
  ft <- align(ft, align = "center", part = "all")
  for (k in 1:ncol(minus_i)){
    ft<-bg(ft,i=minus_i[k],j=minus_j[k]+1,bg="#fcbfbc")}
  for (l in 1:ncol(plus_i)){
    ft<-bg(ft,i=plus_i[l],j=plus_j[l]+1,bg="#89ff89")}
  ft<-bold(ft,i=8)
  
 ft<-width(ft, width = 0.42)
  ft
  return(ft)
}
# Katarzyna Krakówka
# NMFRI
# kkrakowka@mir.gdynia.pl