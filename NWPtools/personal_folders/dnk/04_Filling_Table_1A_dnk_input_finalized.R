
#Based on 4_Filling Table 1A_DNK_v3_final from 2016

#Final work on table 1A

path <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/gits/RCGs/NWPtools/personal_folders/dnk/"
path_dtu <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/table_1a/"

t1a <- read.csv(paste(path, "DK_table1A_filled_dnk.csv", sep = ""), header = TRUE, sep = ';', as.is = TRUE)
t1a <- subset(t1a, region != "Mediterranean and Black Sea")
t1a$MS <- "DNK"
t1a$area_new <- t1a$area
t1a$areaBis_new <- t1a$areaBis
t1a$region_new <- t1a$region
t1a$commDNK <- ""

#Landing of Nephrop from FU 32 & 33 are so small, so these are not included anymore

# t1a$select[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 32 (TAC IIa, IV)"] <-"Y"
# t1a$commDNK[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 32 (TAC IIa, IV)"] <- "TAC covers all of VI. Selected for sampling due to high pct. of landings"
# t1a$refYears[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 32 (TAC IIa, IV)"] <-"2015"
# 
# t1a$select[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 33 (TAC IIa, VI)"] <-"Y"
# t1a$commDNK[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 33 (TAC IIa, VI)"] <- "TAC covers all of VI. Selected for sampling due to high pct. of landings"
# t1a$refYears[t1a$spp == "Nephrops norvegicus" & t1a$area=="functional units 33 (TAC IIa, VI)"] <-"2015"

t1a$area_new[t1a$spp == "Solea solea" & t1a$area == "IIIa"] <- "IIIa, 22"
t1a$areaBis_new[t1a$spp == "Solea solea" & t1a$area == "IIIa"] <- "27_3_A,27_3_C_22"
t1a$commDNK[t1a$spp == "Solea solea" & t1a$area == "IIIa"] <- "Combined with area 22 for sampling, since stock covers both IIIa and 22"
t1a$select[t1a$spp == "Solea solea" & t1a$area == "22"] <- "Y"
t1a$area_new[t1a$spp == "Solea solea" & t1a$area == "22"] <- "IIIa, 22"
t1a$region_new[t1a$spp == "Solea solea" & t1a$area == "22"] <- "North Sea and Eastern Arctic"
t1a$areaBis_new[t1a$spp == "Solea solea" & t1a$area == "22"] <- "27_3_A,27_3_C_22"
t1a$commDNK[t1a$spp == "Solea solea" & t1a$area == "22"] <- "Selected and combined with IIIa for sampling, since stock covers both IIIa and 22"

t1a$select[t1a$spp == "Gadus morhua" & t1a$area == "IIIaS"] <- "Y"
t1a$commDNK[t1a$spp == "Gadus morhua" & t1a$area == "IIIaS"] <- "Selected due to international obligation to sample the stock"

t1a$select[t1a$spp == "Salmo salar" & t1a$area == "22-31"] <- "Y"
t1a$commDNK[t1a$spp == "Salmo salar" & t1a$area == "22-31"] <- "Selected due to international obligation to sample landings of salmon"

t1a$area_new[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IIIa"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IIIa"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IIIa"] <- "Combined with area IV for sampling, since stock covers both areas"
t1a$area_new[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IV"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IV"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Glyptocephalus cynoglossus" & t1a$area == "IV"] <- "Combined with area IIIa for sampling, since stock covers both areas"

t1a$area_new[t1a$spp == "Limanda limanda" & t1a$area == "IIIa"] <- "IIIa, IV, VIId"
t1a$areaBis_new[t1a$spp == "Limanda limanda" & t1a$area == "IIIa"] <- "27_3_A,27_4,27_7_D"
t1a$commDNK[t1a$spp == "Limanda limanda" & t1a$area == "IIIa"] <- "Combined with area IV for sampling, since stock covers both areas"
t1a$area_new[t1a$spp == "Limanda limanda" & t1a$area == "IV, VIId"] <- "IIIa, IV, VIId"
t1a$areaBis_new[t1a$spp == "Limanda limanda" & t1a$area == "IV, VIId"] <- "27_3_A,27_4,27_7_D"
t1a$commDNK[t1a$spp == "Limanda limanda" & t1a$area == "IV, VIId"] <- "Combined with area IIIa for sampling, since stock covers both areas"
t1a$select[t1a$spp == "Limanda limanda" & t1a$area == "IV, VIId"] <- "Y"

t1a$select[t1a$spp == "Micromesistius poutassou" & t1a$area == "IV, VIId"] <- "N"
t1a$commDNK[t1a$spp == "Micromesistius poutassou" & t1a$area == "IV, VIId"] <- "Unselected, since areas are covered in the North Atlantic"

t1a$select[t1a$spp == "Scomber scombrus" & t1a$area == "II"] <- "N"
t1a$commDNK[t1a$spp == "Scomber scombrus" & t1a$area == "II"] <- "Unselected, since areas are covered in the North Atlantic"
t1a$select[t1a$spp == "Scomber scombrus" & t1a$area == "IV, VIId"] <- "N"
t1a$commDNK[t1a$spp == "Scomber scombrus" & t1a$area == "IV, VIId"] <- "Unselected, since areas are covered in the North Atlantic"
t1a$select[t1a$spp == "Scomber scombrus" & t1a$area == "IIIa"] <- "N"
t1a$commDNK[t1a$spp == "Scomber scombrus" & t1a$area == "IIIa"] <- "Unselected, since areas are covered in the North Atlantic"

t1a$select[t1a$spp == "Merluccius merluccius" & t1a$area == "IIIa"] <- "N"
t1a$commDNK[t1a$spp == "Merluccius merluccius" & t1a$area == "IIIa"] <- "Unselected, since areas are covered in the North Atlantic"
t1a$select[t1a$spp == "Merluccius merluccius" & t1a$area == "IV VII"] <- "N"
t1a$commDNK[t1a$spp == "Merluccius merluccius" & t1a$area == "IV VII"] <- "Unselected, since areas are covered in the North Atlantic"
t1a$select[t1a$spp == "Merluccius merluccius" & t1a$area == "IIIa, IV, VI, VII, VIIIab"] <- "Y"
t1a$commDNK[t1a$spp == "Merluccius merluccius" & t1a$area == "IIIa, IV, VI, VII, VIIIab"] <- "Selected, since areas cover the stock"

t1a$area_new[t1a$spp == "Microstomus kitt" & t1a$area == "IV, VIId"] <- "IIIa, IV, VIId"
t1a$areaBis_new[t1a$spp == "Microstomus kitt" & t1a$area == "IV, VIId"] <- "27_3_A,27_4,27_7_D"
t1a$commDNK[t1a$spp == "Microstomus kitt" & t1a$area == "IV, VIId"] <- "Area IIIa included for sampling, since stock covers this area as well"

t1a$area_new[t1a$spp == "Molva molva" & t1a$area == "IV"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Molva molva" & t1a$area == "IV"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Molva molva" & t1a$area == "IV"] <- "Area IIIa included for sampling, since stock covers this area as well"

t1a$area_new[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IIIa"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IIIa"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IIIa"] <- "Combined with area IV for sampling, since stock covers both areas"
t1a$select[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IV"] <- "Y"
t1a$area_new[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IV"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IV"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Melanogrammus aeglefinus" & t1a$area == "IV"] <- "Selected and combined with IIIa for sampling, since stock covers both areas"

t1a$select[t1a$spp == "Merlangius merlangus" & t1a$area == "IV, VIId"] <- "Y"
t1a$commDNK[t1a$spp == "Merlangius merlangus" & t1a$area == "IV, VIId"] <- "Selected due to high level of discard and Industial by-catch"

t1a$select[t1a$spp == "Pollachius virens" & t1a$area == "IIIa"] <- "Y"
t1a$area_new[t1a$spp == "Pollachius virens" & t1a$area == "IIIa"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Pollachius virens" & t1a$area == "IIIa"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Pollachius virens" & t1a$area == "IIIa"] <- "Selected due to high pct. of landings. Combined with area IV for sampling, since stock covers both areas"
t1a$select[t1a$spp == "Pollachius virens" & t1a$area == "IV"] <- "Y"
t1a$area_new[t1a$spp == "Pollachius virens" & t1a$area == "IV"] <- "IIIa, IV"
t1a$areaBis_new[t1a$spp == "Pollachius virens" & t1a$area == "IV"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Pollachius virens" & t1a$area == "IV"] <- "Selected due to high pct. of landings. Combined with IIIa for sampling, since stock covers both areas"


t1a$select[t1a$spp == "Platichthys flesus" & t1a$area == "22-32"] <- "Y"
t1a$commDNK[t1a$spp == "Platichthys flesus" & t1a$area == "22-32"] <- "Selected due to high level of discard"

#Landings are increasing again, so it is already selected
# t1a$select[t1a$spp == "Gadus morhua" & t1a$area=="IIIaS"] <-"Y"
# t1a$commDNK[t1a$spp == "Gadus morhua" & t1a$area=="IIIaS"] <-"Selected due to ...."

t1a$area_new[t1a$spp == "Pandalus borealis" & t1a$area == "IIIa"] <- "IIIa, IVa"
t1a$areaBis_new[t1a$spp == "Pandalus borealis" & t1a$area == "IIIa"] <- "27_3_A,27_4_A,27_4"
t1a$commDNK[t1a$spp == "Pandalus borealis" & t1a$area == "IIIa"] <- "Combined with area IVa & IV for sampling, since stock and sampling covers the fishery the Norway Depp"
t1a$select[t1a$spp == "Pandalus borealis" & t1a$area == "IV"] <- "Y"
t1a$area_new[t1a$spp == "Pandalus borealis" & t1a$area == "IV"] <- "IIIa, IVa"
t1a$areaBis_new[t1a$spp == "Pandalus borealis" & t1a$area == "IV"] <- "27_3_A,27_4_A,27_4"
t1a$commDNK[t1a$spp == "Pandalus borealis" & t1a$area == "IVa"] <- "Selected and combined with IIIa & IVa for sampling, since stock and sampling covers the fishery the Norway Depp"
t1a$select[t1a$spp == "Pandalus borealis" & t1a$area == "IVa"] <- "Y"
t1a$area_new[t1a$spp == "Pandalus borealis" & t1a$area == "IVa"] <- "IIIa, IVa"
t1a$areaBis_new[t1a$spp == "Pandalus borealis" & t1a$area == "IVa"] <- "27_3_A,27_4_A,27_4"
t1a$commDNK[t1a$spp == "Pandalus borealis" & t1a$area == "IVa"] <- "Selected and combined with IIIa & IV for sampling, since stock and sampling covers the fishery the Norway Depp"

#t1a$select[t1a$spp == "Pandalus borealis" & t1a$area == "I, II"] <- "N"
#t1a$commDNK[t1a$spp == "Pandalus borealis" & t1a$area == "I, II"] <- "Unselected, since nearly all the Danish alndings are taken in area I, where there is not any TAC. The TAC used for this stock covers area IV and IIa and these landings are covered elsewhere"

t1a$select[t1a$spp == "Macrourus berglax" & t1a$area == "IV"] <- "N"
t1a$commDNK[t1a$spp == "Macrourus berglax" & t1a$area == "IV"] <- "Unselected, no landings"

t1a$select[t1a$spp == "Trachurus trachurus" & t1a$area == "IIa"] <- "N"
t1a$commDNK[t1a$spp == "Trachurus trachurus" & t1a$area == "IIa"] <- "Unselected, no landings and area is covered in the North Atlantic"

t1a$commDNK[t1a$spp == "Trachurus trachurus" & t1a$area == "IVbc, VIId"] <- "IVa excluded from area defintion, since this area is covered in the North Atlantic"

t1a$select[t1a$spp == "Crangon crangon" & t1a$area == "IV, VIId"] <- "Y"
t1a$commDNK[t1a$spp == "Crangon crangon" & t1a$area == "IV, VIId"] <- "Selected due to high discard of flatfishes"

t1a$area_new[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IV"] <- "IV,IIIa"
t1a$areaBis_new[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IV"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IV"] <- "Combined with area IIIa for sampling, since stock covers both areas"
t1a$select[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IIIa"] <- "Y"
t1a$area_new[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IIIa"] <- "IV,IIIa"
t1a$areaBis_new[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IIIa"] <- "27_3_A,27_4"
t1a$commDNK[t1a$spp == "Trisopterus esmarkii" & t1a$area == "IIIa"] <- "Selected and combined with IV for sampling, since stock covers both areas"

t1a$commDNK[t1a$spp == "Selachii"] <- "It is not possible to calulate landings for these specis due to the resultion of the official statistic"
t1a$spp[t1a$spp == "Selachii"] <- "Selachii, Rajidae"

#t1a$commDNK <- ifelse(t1a$commDNK != "", paste("OBS!!", t1a$commDNK), "")

#Danish names 
nerv <- read.table('Q:/mynd/SAS Library/Arter/art.csv', header = TRUE, sep = ",", as.is = TRUE)
names(nerv)

t1b <- merge(t1a,nerv,by.x = c("spp"),by.y = c("faoLatin"),all.x = T)

t1b$art[is.na(t1b$art)] <- ""

t1dup <- t1b[duplicated(t1b[,c("MS","refYears","spp","region","RFMO","area","select","landings","TAC","shareLanding","Thresh","Comments","commDNK","areaBis","speciesIncluded",
                             "area_new","areaBis_new","region_new")]),]

t1b <- t1b[!duplicated(t1b[,c("MS","refYears","spp","region","RFMO","area","select","landings","TAC","shareLanding","Thresh","Comments","commDNK","areaBis","speciesIncluded",
                           "area_new","areaBis_new","region_new")]),]

#merge comments
t1b <- t1b[,c("MS","refYears","spp","art","region","RFMO","area","select","landings","TAC","shareLanding","Thresh","Comments","commDNK","areaBis","speciesIncluded",
            "area_new","areaBis_new","region_new")]

t1b$commentsFinal<-ifelse(t1b$Comments!="" & t1b$commDNK!="" & t1b$art!="", paste("(",t1b$art,")","|",t1b$Comments,"|",t1b$commDNK,sep=" "), 
                   ifelse(t1b$Comments!="" & t1b$commDNK!="" & t1b$art=="", paste(t1b$Comments,"|",t1b$commDNK,sep=" "),
                          ifelse(t1b$Comments!="" & t1b$commDNK=="" & t1b$art!="", paste("(",t1b$art,")","|",t1b$Comments,sep=" "),
                          ifelse(t1b$Comments!="" & t1b$commDNK=="" & t1b$art=="", t1b$Comments,
                                 ifelse(t1b$Comments=="" & t1b$commDNK!="" & t1b$art!="", paste("(",t1b$art,")","|",t1b$commDNK,sep=" "),
                                 ifelse(t1b$Comments=="" & t1b$commDNK!="" & t1b$art=="", t1b$commDNK,
                                        ifelse(t1b$Comments=="" & t1b$commDNK=="" & t1b$art!="", paste("(",t1b$art,")",sep=" "),"")))))))

t1b<-t1b[,c("MS","refYears","spp","region","RFMO","area","select","landings","TAC","shareLanding","Thresh","commentsFinal","areaBis","speciesIncluded",
            "area_new","areaBis_new","region_new")]

write.table(t1b, paste(path, "DK_table1A_filled_dnk_finalized.csv", sep =""), sep = ';',row.names=FALSE, quote=FALSE)
write.table(t1b, paste(path_dtu, "DK_table1A_filled_dnk_finalized.csv", sep =""), sep = ';',row.names=FALSE, quote=FALSE)
