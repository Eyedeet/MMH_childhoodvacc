#Finalising the linked study population
#children's cohort was finalised in previous study see
# https://www.sciencedirect.com/science/article/pii/S0264410X2300926X?via%3Dihub


#reading in data linkage file mother baby link (MBL)gith and the children in the study
MBL <- data.table(read.delim(paste0())
cohort<- data.table(read_parquet(file=))

nrow(MBL) #904447
nrow(cohort) #1735692


###preparing the data sets for merge
MBL <- MBL %>% 
  rename(patid = babypatid) %>% 
  mutate(patid = as.character(patid),
         mumpatid = as.character(mumpatid))
cohort <- cohort %>% 
  mutate(patid = as.character(patid))

#merging the study cohort with the mothers based on patid of the children
cohort_linked <- merge(cohort, MBL, by = "patid", all= T)

#drop children without a mother
cohort_linked <- cohort_linked[!is.na(mumpatid) & !is.na(dob)]
nrow(cohort_linked) #805,490 children remaining

#any siblings?
any(duplicated(cohort_linked$mumpatid)) 


###saving txt files with patid
df_child <- unique(cohort_linked[, list(patid)]) #805,490
df_mum <- unique(cohort_linked[, list(mumpatid)]) #653,425

write.table(df_child, file=paste0(datafiles, "children_patid.txt"), 
            append = FALSE, quote=FALSE, sep = " ", dec = ".", 
            row.names=FALSE, col.names=FALSE)
write.table(df_mum, file=paste0(datafiles, "mother_patid.txt"), 
            append = FALSE, quote=FALSE, sep = " ", dec = ".", 
            row.names=FALSE, col.names=FALSE)

###adding file which indicated other CPRD data linkages
df_mum <- df_mum %>% 
  rename(patid = mumpatid)
df <- rbind(df_mum, df_child)

links <- data.table(read.table(file = ,
                               header = TRUE,
                               sep = "\t",
                               dec = "."))
links$patid <- as.character(links$patid)

#merging data sets based on patid
df_linked <- merge(df, links, by = "patid", all.x = T)
nrow(df_linked) #1458915
#filter out thoese who won't have any linkage
df_linked <- df_linked[!is.na(linkdate)]
nrow(df_linked) #1434313


write.table(df_linked, file=paste0(datafiles, "patid_linkage.txt"), 
            append = FALSE, quote=FALSE, sep = " ", dec = ".", 
            row.names=FALSE, col.names=T)
