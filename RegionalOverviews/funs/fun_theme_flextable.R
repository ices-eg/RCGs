theme_flextable <-function(data){
  library(dplyr)
  library(flextable)
data %>%
  t() %>%    
  head() %>%
  as.data.frame() %>% 
  add_rownames() %>% 
  flextable() %>%
  theme_box() %>%
  fontsize(part = "all", size = 11) %>%
  bold(part="header") %>%
  bold(j=1) %>%
  bg(part="header",bg="#F3F9FF") %>%
  color(part="header",color="black") %>%
  color(j=1,part="header",color="#F3F9FF") %>%
  bg(j=1, bg="#F3F9FF")%>%
  align(align = "center", part = "all")->ft
  return(ft)
}
# Katarzyna Krakówka
# NMFRI
# kkrakowka@mir.gdynia.pl