loadMetierList <- function(url){
  print("Loading metier list ...")
  x <- data.table(read.csv(url, sep = ",", stringsAsFactors = F, na.strings = ""))
  setnames(x, old = c("Metier_level6","Metier_level5"), 
           new = c("metier_level_6","metier_level_5"))
  
  #Split metier by parts
  x[,c("gear","target","mesh","sd","sd_mesh") := data.table(str_split_fixed(metier_level_6,"_",5))]
  x[str_detect(mesh,"-") == T, c("m_size_from","m_size_to"):=data.table(str_split_fixed(mesh,"-",2))]
  x[,":="(m_size_from=as.integer(m_size_from),m_size_to=as.integer(m_size_to),sd=as.integer(sd),sd_mesh=as.integer(sd_mesh))]
  
  x[substr(mesh,1,2) == ">=", ":="(m_size_from=as.integer(gsub("[[:punct:]]", " ",mesh)),
                                   m_size_to=as.integer(999))]
  x[substr(mesh,1,1) == ">" & substr(mesh,2,2) != "=", ":="(m_size_from=as.integer(1)+as.integer(gsub("[[:punct:]]", " ",mesh)),
                                                            m_size_to=as.integer(999))]
  x[substr(mesh,1,1) == "<" & substr(mesh,2,2) != "=", ":="(m_size_to=as.integer(gsub("[[:punct:]]", " ",mesh))-as.integer(1),
                                                            m_size_from=as.integer(1))]
  x[mesh == "0", ":="(m_size_from=as.integer(0),
                      m_size_to=as.integer(999))]
  
  return(x[,.(RCG,metier_level_6,Start_year,End_year,metier_level_5,gear,target,
              mesh,sd,sd_mesh,m_size_from,m_size_to)])
}