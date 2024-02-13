#data
load("Q:/dfad/users/jostou/home/ISSG_Catch/data/RDBES_All_Regions_CE_2021_2021_prepared_20240129.Rdata")
load("Q:/dfad/users/jostou/home/ISSG_Catch/data/RDBES_All_Regions_Cl_2021_2021_prepared_20240129.Rdata")


barplot <- function(data = data,
                     x = "CLarea",
                     y = "CLscientificWeight_1000ton", 
                     group = "",
                     asPct = F,
                     titel = "",
                     ylab = "",
                     xlab = "",
                     save_plot_to_list=TRUE){
  
  
  #colour
  col <- read.csv2("Q:/dfad/users/jostou/home/ISSG_Catch/RCGs/RegionalOverviews/data/colours2.csv")
  country <- read.table("Q:/dfad/users/jostou/home/ISSG_Catch/RCGs/RegionalOverviews/data/aux_countries.txt", sep = ",", header = T)
  names(country)[1] <- "CountryName"
  
  col <- merge(col, country, by = "CountryName", all.x = T)
  col <- col[, c("ISO2Code", "colour5")]
  col <- setNames(object = col$colour5, nm = col$ISO2Code)
  
  # set parametors
  data <- data.frame(data)
  data$x <- data[, x]
  data$y <- data[, y]
  
  if (substr(group, 1,2) %in% c("CL", "CE")) {
    data$grp <- as.factor(data[, group])
  } else {
    data$grp <- as.factor(data$x)
  }
  
  setDT(data)
  data <- data[ ,. (y = sum(y, na.rm = T)),
                        by = .(x, grp)] 
  
  #
  if (asPct == T) {
    data <- data[ ,. (y = (y/sum(y, na.rm = T))*100,
                      grp = unique(grp)),
                  by = .(x)] 
    
    #plot struff
    p <- ggplot(data=data, aes(x = as.factor(x), y = y, fill = grp)) +
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste(titel)) +
      xlab(paste(xlab)) +
      ylab(paste(ylab))
    
  } else {
    #plot struff
    p <- ggplot(data=data, aes(x = reorder(x, -y), y = y, fill = grp)) +
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste(titel)) +
      xlab(paste(xlab)) +
      ylab(paste(ylab))
  }
  
  if (group %like% "Country")
    p <- p +
    scale_fill_manual(values = col)
  
  if (group = "")
    p + theme(legend.position="none")
  
  p
}

