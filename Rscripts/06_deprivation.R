####Adding deprivation data to the study population
mums <- read_parquet(paste0(datafiles, "mothers_studypop_eth.parquet"))
mums[, pracid := as.character(pracid)]
babies <-  read_parquet(paste0(datafiles, "children_studypop_eth.parquet"))
babies[, pracid := as.character(pracid)]
#loading the linked data, patient level first
dep_pat <- data.table(read.delim(paste0(linked_data, "patient_2019_imd_22_001706.txt")))
dep_prac <- data.table(read.delim(paste0(linked_data, "practice_imd_22_001706.txt")))

dep_pat[, patid := as.character(patid)]
dep_pat[, pracid := as.character(pracid)]
dep_prac[, pracid := as.character(pracid)]

#adding patient level deprivation
mums <- merge(mums, dep_pat, by  = c("patid", "pracid"))
babies <- merge(babies, dep_pat, by  = c("patid", "pracid"))

mums[,imd := e2019_imd_5]
babies[,imd := e2019_imd_5]
mums[,e2019_imd_5 := NULL]
babies[,e2019_imd_5:= NULL]

tabyl(mums$imd)
# mums$imd      n      percent valid_percent
# 1 107737 0.2010378725     0.2011445
# 2 100713 0.1879310474     0.1880307
# 3  99191 0.1850909864     0.1851891
# 4 109109 0.2035980325     0.2037060
# 5 118870 0.2218121156     0.2219297
# NA    284 0.0005299457            NA

tabyl(babies$imd)
# babies$imd      n      percent valid_percent
# 1 103978 0.2002831524     0.2003928
# 2  97509 0.1878225193     0.1879253
# 3  96330 0.1855515212     0.1856531
# 4 106133 0.2044341285     0.2045460
# 5 114921 0.2213616357     0.2214828
# NA    284 0.0005470428            NA

mums1 <- mums[!is.na(imd)]
mums2 <- mums[is.na(imd)]
babies1 <- babies[!is.na(imd)]
babies2 <- babies[is.na(imd)]

mums2 <- merge(mums2, dep_prac, by = "pracid")
babies2 <- merge(babies2, dep_prac, by = "pracid")

mums2[,imd := e2019_imd_5]
babies2[,imd := e2019_imd_5]
mums2[,e2019_imd_5 := NULL]
babies2[,e2019_imd_5:= NULL]
mums2[,ni2017_imd_5 := NULL]
babies2[,ni2017_imd_5:= NULL]
mums2[,s2020_imd_5 := NULL]
babies2[,s2020_imd_5:= NULL]
mums2[,w2019_imd_5 := NULL]
babies2[,w2019_imd_5:= NULL]
mums2[,country := NULL]
babies2[,country:= NULL]

df_mum <- rbind(mums1, mums2)
df_baby <- rbind(babies1, babies2)

write_parquet(df_mum, paste0(datafiles, "mothers_studypop_eth_dep.parquet"))
write_parquet(df_baby, paste0(datafiles, "children_studypop_eth_dep.parquet"))
