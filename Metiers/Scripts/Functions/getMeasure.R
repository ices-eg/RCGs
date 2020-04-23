# Function that returns a measure to be used on a sequence level to determine dominant species/group 
getMeasure<-function(p.measure){
  idx<-which(p.measure=="value")
  if(length(idx)>0) return("value")
  else return("weight")
}