# The code from this function was temporarly moved to the main script
# This function is an intermediate step befor applying getMetier function
assignMetiers <- function(x){
  print("Assigning metiers ...")
  x$metier_level_6<-NA
  x[,metier_level_6:=as.character(pmap(list(RCG,
                                            year,
                                            gear, 
                                            registered_target_assemblage,
                                            seq_dom_group, 
                                            mesh, 
                                            selection_type,
                                            selection_mesh),
                                       function(r,y,g,t,d,m,st,sm) getMetier(r,y,g,t,d,m,st,sm)))]
  return(x)
}