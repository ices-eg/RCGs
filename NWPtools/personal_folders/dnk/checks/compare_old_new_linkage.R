

# Compare areaBis in linkage file - dnk vs. common. The dnk was made by hand and not scripted

library(dplyr)

input_path_common <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/gits/RCGs/NWPtools/common/input"

dnk <- read.csv("Q:/scientific-projects/eu-data-collection/Work_Plan/2019/scripts/EUMAP_table_1a/input/EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC_DNK_v3.csv", sep = ";")

common <- read.csv(file.path(input_path_common,'EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC.csv'), sep=';',header=TRUE, as.is=TRUE)

names(dnk)
names(common)

dnk_1 <- select(dnk, region, latinName_old, area, areaBis)
dnk_1 <- mutate(dnk_1, area = str_replace_all(area, " ", ""))
common_1 <- select(common, region, latinName, area, areaBis)
common_1 <- mutate(common_1, latinName = str_replace_all(latinName, "spp", "spp."), area = str_replace_all(area, " ", ""))

comb <- full_join(dnk_1, common_1, by = c("region" = "region", "latinName_old" = "latinName", "area" = "area"))
comb_1 <- filter(comb, areaBis.x != areaBis.y)
