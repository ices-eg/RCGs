loadMetierList <- function(url){
  print("Loading metier list ...")
  x <- data.table(read.csv(url, sep = ",", stringsAsFactors = F, na.strings = ""))
  setnames(x, old = "Metier_level6", new = "metier_level_6")
  
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
  
  x[metier_level_6 %in% c("OTB_DEF_>=105_1_120", "OTB_DEF_>=105_1_110") & RCG=="BALT",
    ":="(m_size_from=105, m_size_to=114)]
  x[metier_level_6 == "OTB_DEF_>=115_0_0" & RCG=="BALT",
    ":="(m_size_from=115, m_size_to=119)]
  x[metier_level_6 == "OTB_DEF_>=120_0_0" & RCG=="BALT",
    ":="(m_size_from=120, m_size_to=999)]
  
  return(x)
}