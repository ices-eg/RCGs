# Function that finds a metier code based on the given parameters
getMetier<-function(p.rcg, p.year, p.gear, p.reg_target, p.dom_group, p.mesh, 
                    p.selection_type, p.selection_mesh){
  #Registered target assemblage has priority over the calculated dominating assemlage
  p.target <- ifelse(is.na(p.reg_target),p.dom_group,p.reg_target)
  #First step - assign metier based on rcg, gear, target assemblage, mesh size, selection dev.
  #Ignore >0 metiers, they will be included at the end when no other metier was assigned
  metier<-as.character(metier.list[!mesh %in% c("0", ">0") & RCG==p.rcg & 
                                     gear==p.gear & 
                                     target==p.target & 
                                     data.table::between(p.mesh,m_size_from,m_size_to) &
                                     (p.selection_type == sd & p.selection_mesh == sd_mesh) &
                                     ((is.na(Start_year) & is.na(End_year)) | 
                                        (p.year<=End_year & is.na(Start_year)) | 
                                        (p.year>=Start_year & is.na(End_year)) |
                                        (p.year>=Start_year & p.year<=End_year)),
                                   metier_level_6][1])
  #Second step - if metier is not assigned then try without the info on selection dev.
  #Still ignore >0 metiers
  if(is.na(metier)){
    metier<-as.character(metier.list[!mesh %in% c("0", ">0") & RCG==p.rcg &
                                       gear==p.gear &
                                       target==p.target &
                                       data.table::between(p.mesh,m_size_from,m_size_to) &
                                       ((is.na(Start_year) & is.na(End_year)) |
                                          (p.year<=End_year & is.na(Start_year)) |
                                          (p.year>=Start_year & is.na(End_year)) |
                                          (p.year>=Start_year & p.year<=End_year)),
                                     metier_level_6][1])
  }
  #Third step - if metier is not assigned then try to assign a metier with >0 mesh size range
  if(is.na(metier)){
    metier<-as.character(metier.list[mesh %in% c("0", ">0") & RCG==p.rcg &
                                       gear==p.gear &
                                       target==p.target &
                                       ((is.na(Start_year) & is.na(End_year)) |
                                          (p.year<=End_year & is.na(Start_year)) |
                                          (p.year>=Start_year & is.na(End_year)) |
                                          (p.year>=Start_year & p.year<=End_year)),
                                     metier_level_6][1])
  }
  return(metier)
}
