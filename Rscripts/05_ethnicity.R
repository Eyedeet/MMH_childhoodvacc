###Ethnicity algorithm using med codes in CPRD Aurum to determine the ethnicity
####of the included mothers and children

###---preparation
#reading in the existing code list for ethnicity
eth_codes <- haven::read_dta(file = )
eth_codes <- as.data.table(eth_codes)

obs.files.mum <- list.files(path = mothers_parquet, pattern = "\\Observation")

#extracting ethnicity codes only
setwd(mothers_parquet)
df_mum <- extractor_med(list.files = obs.files.mum,
                        codelist = eth_codes)
length(unique(df_mum$patid))#data of 578,830 mothers extracted
df_mum <- df_mum[,obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df_mum <- df_mum[,enterdate := as.Date(enterdate, format = "%d/%m/%Y")]
df_mum <- df_mum[,patid := as.character(patid)]

#describing the overall given codes
janitor::tabyl(df_mum$eth5)
janitor::tabyl(df_mum$eth16)

#tagging patients with the same ethnicity entry within the same year
df_mum<- df_mum[order(patid)]
df_mum <- df_mum[, sysyear := year(obsdate)]
df_mum[, duplicates := .N, by = c("patid", "sysyear", "medcodeid")]
#keeping the first ethnicity entry per year if there is a duplicate
df_mum[duplicates > 1, first_entry := lapply(.SD, min), by = c("patid", "sysyear"), .SDcols = "obsdate"]
df_mum[duplicates >1, keep_row := obsdate == first_entry]
df_mum[duplicates == 1, keep_row := TRUE]
df_mum <- df_mum[keep_row == TRUE]

df_mum[, duplicates := NULL]
df_mum[, first_entry := NULL]
df_mum[, keep_row := NULL]

#count the observations of ethnicity by patient
df_mum <- df_mum[, by = c("patid"), eth5_count := .N]

tmp <- df_mum[, list(patid, eth5_count)]
tmp <- unique(tmp)
janitor::tabyl(tmp$eth5_count) #51.1% of patients have only one ethnicity entry, max 17

#save extra data copy
tmp <- df_mum
df_mum<- tmp


#five categories
df_mum[eth5 == 0, eth5whitecount := .N, by = "patid"]
df_mum[eth5 != 0, eth5whitecount := 0]
df_mum[, eth5whitecount := lapply(.SD, max_NA), .SDcols = c("eth5whitecount"), by="patid"]
df_mum[eth5 == 1, eth5sacount := .N, by = "patid"]
df_mum[eth5 != 1, eth5sacount := 0]
df_mum[, eth5sacount := lapply(.SD, max_NA), .SDcols = c("eth5sacount"), by="patid"]
df_mum[eth5 == 2, eth5blackcount := .N, by = "patid"]
df_mum[eth5 != 2, eth5blackcount := 0]
df_mum[, eth5blackcount := lapply(.SD, max_NA), .SDcols = c("eth5blackcount"), by="patid"]
df_mum[eth5 == 3, eth5othercount := .N, by = "patid"]
df_mum[eth5 != 3, eth5othercount := 0]
df_mum[, eth5othercount := lapply(.SD, max_NA), .SDcols = c("eth5othercount"), by="patid"]
df_mum[eth5 == 4, eth5mixedcount := .N, by = "patid"]
df_mum[eth5 != 4, eth5mixedcount := 0]
df_mum[, eth5mixedcount := lapply(.SD, max_NA), .SDcols = c("eth5mixedcount"), by="patid"]
df_mum[eth5 == 5, notstated5count := .N, by = "patid"]
df_mum[eth5 != 5, notstated5count := 0]
df_mum[, notstated5count := lapply(.SD, max_NA), .SDcols = c("notstated5count"), by="patid"]
#16 categories
df_mum[eth16 ==1, british16count := .N, by = "patid"]
df_mum[eth16 != 1, british16count := 0]
df_mum[, british16count := lapply(.SD, max_NA), .SDcols = c("british16count"), by="patid"]
df_mum[eth16 ==2, irish16count := .N, by = "patid"]
df_mum[eth16 != 2, irish16count := 0]
df_mum[, irish16count := lapply(.SD, max_NA), .SDcols = c("irish16count"), by="patid"]
df_mum[eth16 ==3, otherwhite16count := .N, by = "patid"]
df_mum[eth16 != 3, otherwhite16count := 0]
df_mum[, otherwhite16count := lapply(.SD, max_NA), .SDcols = c("otherwhite16count"), by="patid"]
df_mum[eth16 ==4, whitecarib16count := .N, by = "patid"]
df_mum[eth16 != 4, whitecarib16count := 0]
df_mum[, whitecarib16count := lapply(.SD, max_NA), .SDcols = c("whitecarib16count"), by="patid"]
df_mum[eth16 ==5, whiteaf16count := .N, by = "patid"]
df_mum[eth16 != 5, whiteaf16count := 0]
df_mum[, whiteaf16count := lapply(.SD, max_NA), .SDcols = c("whiteaf16count"), by="patid"]
df_mum[eth16 ==6, whiteasian16count := .N, by = "patid"]
df_mum[eth16 != 6, whiteasian16count := 0]
df_mum[, whiteasian16count := lapply(.SD, max_NA), .SDcols = c("whiteasian16count"), by="patid"]
df_mum[eth16 ==7, othermixed16count := .N, by = "patid"]
df_mum[eth16 != 7, othermixed16count := 0]
df_mum[, othermixed16count := lapply(.SD, max_NA), .SDcols = c("othermixed16count"), by="patid"]
df_mum[eth16 ==8, indian16count := .N, by = "patid"]
df_mum[eth16 != 8, indian16count := 0]
df_mum[, indian16count := lapply(.SD, max_NA), .SDcols = c("indian16count"), by="patid"]
df_mum[eth16 ==9, pak16count := .N, by = "patid"]
df_mum[eth16 != 9, pak16count := 0]
df_mum[, pak16count := lapply(.SD, max_NA), .SDcols = c("pak16count"), by="patid"]
df_mum[eth16 ==10, bangla16count := .N, by = "patid"]
df_mum[eth16 != 10, bangla16count := 0]
df_mum[, bangla16count := lapply(.SD, max_NA), .SDcols = c("bangla16count"), by="patid"]
df_mum[eth16 ==11, otherasian16count := .N, by = "patid"]
df_mum[eth16 != 11, otherasian16count := 0]
df_mum[, otherasian16count := lapply(.SD, max_NA), .SDcols = c("otherasian16count"), by="patid"]
df_mum[eth16 ==12, carib16count := .N, by = "patid"]
df_mum[eth16 != 12, carib16count := 0]
df_mum[, carib16count := lapply(.SD, max_NA), .SDcols = c("carib16count"), by="patid"]
df_mum[eth16 ==13, african16count := .N, by = "patid"]
df_mum[eth16 != 13, african16count := 0]
df_mum[, african16count := lapply(.SD, max_NA), .SDcols = c("african16count"), by="patid"]
df_mum[eth16 ==14, otherblack16count := .N, by = "patid"]
df_mum[eth16 != 14, otherblack16count := 0]
df_mum[, otherblack16count := lapply(.SD, max_NA), .SDcols = c("otherblack16count"), by="patid"]
df_mum[eth16 ==15, chinese16count := .N, by = "patid"]
df_mum[eth16 != 15, chinese16count := 0]
df_mum[, chinese16count := lapply(.SD, max_NA), .SDcols = c("chinese16count"), by="patid"]
df_mum[eth16 ==16, other16count := .N, by = "patid"]
df_mum[eth16 != 16, other16count := 0]
df_mum[, other16count := lapply(.SD, max_NA), .SDcols = c("other16count"), by="patid"]
df_mum[eth16 ==17, notstated16count := .N, by = "patid"]
df_mum[eth16 != 17, notstated16count := 0]
df_mum[, notstated16count := lapply(.SD, max_NA), .SDcols = c("notstated16count"), by="patid"]

#flagging those patients who only have non stated ethnicity
df_mum<- df_mum[,  nonstatedonly := (notstated5count > 0 &
                                       eth5whitecount == 0 &
                                       eth5sacount == 0 &
                                       eth5blackcount == 0 &
                                       eth5othercount == 0 &
                                       eth5mixedcount == 0)] 
janitor::tabyl(df_mum$nonstatedonly) #1.4% have non-stated only

###finding the most common ethnicity, excluding ethnicity not stated
# Get Row wise max in R
df_mum <- df_mum[,eth5max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 29:33]
df_mum <- df_mum[,eth5max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 29:33]
df_mum <- df_mum[,eth16max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 35:50]
df_mum <- df_mum[,eth16max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 35:50]


#get the rows with equally common ethnicities, excluding non stated
df_mum<- df_mum[, equaleth5 := eth5max_f != eth5max_l & notstated5count == 0 ]
df_mum<- df_mum[, equaleth16 := eth16max_f != eth16max_l & notstated16count  ==0]
janitor::tabyl(df_mum$equaleth5) #2.27% with notequally recorded most common ethnicity


###---FINAL STEP - assigning ethnicity to patid
#case 1: ethnicity is not stated - assign not stated
eth <- df_mum[nonstatedonly==T, ethnicity_5 := 5]
eth <- df_mum[nonstatedonly==T, ethnicity_16 := 17]

tmp <- eth[nonstatedonly == T]
length(unique(tmp$patid)) #12,950 mothers with no stated ethnicity



####case 2: assign the most common ethnicity
eth[equaleth5 == F & nonstatedonly == F, ethnicity_5c := eth5max_f]
eth[equaleth16 == F & nonstatedonly == F, ethnicity_16c := eth16max_f]


###case 3: if equal common
#finding the latest date of ethnicity record and using this record as eth5/16 code
eth[equaleth5 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "obsdate"]
eth[equaleth5 == T, eth_pick := obsdate == latestethdate]
eth[equaleth5 == T & eth_pick == T, ethnicity_5 := eth5]

eth[equaleth16 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "obsdate"]
eth[equaleth16 == T, eth_pick := obsdate == latestethdate]
eth[equaleth16 == T & eth_pick == T, ethnicity_16 := eth16]



#translating the character back into ethnicity numbers
eth[ethnicity_5c == "eth5whitecount", ethnicity_5 := 0]
eth[ethnicity_5c == "eth5sacount", ethnicity_5 := 1]
eth[ethnicity_5c == "eth5blackcount", ethnicity_5 := 2]
eth[ethnicity_5c == "eth5othercount", ethnicity_5 := 3]
eth[ethnicity_5c == "eth5mixedcount", ethnicity_5 := 4]

eth[ethnicity_16c ==  "british16count", ethnicity_16 := 1]
eth[ethnicity_16c ==  "irish16count", ethnicity_16 := 2]
eth[ethnicity_16c ==  "otherwhite16count", ethnicity_16 := 3]
eth[ethnicity_16c ==  "whitecarib16count", ethnicity_16 := 4]
eth[ethnicity_16c ==  "whiteaf16count", ethnicity_16 := 5]
eth[ethnicity_16c ==  "whiteasian16count", ethnicity_16 := 6]
eth[ethnicity_16c ==  "othermixed16count", ethnicity_16 := 7]
eth[ethnicity_16c ==  "indian16count", ethnicity_16 := 8]
eth[ethnicity_16c ==  "pak16count", ethnicity_16 := 9]
eth[ethnicity_16c ==  "bangla16count", ethnicity_16 := 10]
eth[ethnicity_16c ==  "otherasian16count", ethnicity_16 := 11]
eth[ethnicity_16c ==  "carib16count", ethnicity_16 := 12]
eth[ethnicity_16c ==  "african16count", ethnicity_16 := 13]
eth[ethnicity_16c ==  "otherblack16count", ethnicity_16 := 14]
eth[ethnicity_16c ==  "chinese16count", ethnicity_16 := 15]
eth[ethnicity_16c ==  "other16count", ethnicity_16 := 16]


###special cases that people received conflicting codes on the same day
#set them to mixed
eth <- unique(eth[, list(patid,  pracid,  ethnicity_5,  
                         ethnicity_16)])

eth[, same_entry := .N, by = "patid"]

###setting people to mixed when they have more than one code
eth[same_entry >1, ethnicity_5 := 4]
eth[same_entry >1, ethnicity_16 := 7]

eth <- unique(eth[, list(patid,  pracid,  ethnicity_5,  
                         ethnicity_16)])
eth[, same_entry := .N, by = "patid"]



###--- creating a clean ethnicity file
eth <- unique(eth[, list(patid, pracid, ethnicity_5, ethnicity_16)])
janitor::tabyl(eth$ethnicity_5)
hist(eth$ethnicity_5)
janitor::tabyl(eth$ethnicity_16)
hist(eth$ethnicity_16)


####now adding HES data to it
hes <-data.table(read.delim(paste0(linked_data, "hes_patient_22_001706_DM.txt")))
hes[, patid := as.character(patid)]
tabyl(hes$gen_ethnicity)
# 12593 0.008963934
# Bangladesi   15555 0.011072341
# Bl_Afric   51175 0.036427325
# Bl_Carib   14639 0.010420315
# Bl_Other   13895 0.009890722
# Chinese    9580 0.006819224
# Indian   40451 0.028793780
# Mixed   41976 0.029879304
# Oth_Asian   30542 0.021740368
# Other   38621 0.027491152
# Pakistani   39533 0.028140331
# Unknown   41524 0.029557562
#White 1054768 0.750803643

hes[gen_ethnicity == "White", ethnicity_5_hes := 0]
hes[gen_ethnicity == "Bangladesi", ethnicity_5_hes := 1]
hes[gen_ethnicity == "Indian", ethnicity_5_hes := 1]
hes[gen_ethnicity == "Bangladesi", ethnicity_5_hes := 1]
hes[gen_ethnicity == "Pakistani", ethnicity_5_hes := 1]
hes[gen_ethnicity == "Bl_Afric", ethnicity_5_hes := 2]
hes[gen_ethnicity == "Bl_Carib", ethnicity_5_hes := 2]
hes[gen_ethnicity == "Bl_Other", ethnicity_5_hes := 2]
hes[gen_ethnicity == "Oth_Asian", ethnicity_5_hes := 3]
hes[gen_ethnicity == "Other", ethnicity_5_hes := 3]
hes[gen_ethnicity == "Chinese", ethnicity_5_hes := 3]
hes[gen_ethnicity == "Mixed", ethnicity_5_hes := 4]


#read in the study pop file
mums <- read_parquet(paste0(datafiles, "mothers_studypop.parquet"))
mums <- merge(mums, eth, by = c("patid", "pracid"), all.x = T)
mum1 <- mums[!is.na(ethnicity_5)] #359662
mum2 <- mums[is.na(ethnicity_5)] #62132


mum2 <- merge(mum2, hes, by = c("patid", "pracid"), all.x = T) 
tabyl(mum2$ethnicity_5_hes)
# mum2$ethnicity_5_hes     n    percent valid_percent
# 0 70151 0.86617936    0.89782937
# 1  2807 0.03465903    0.03592546
# 2  1989 0.02455889    0.02545627
# 3  2103 0.02596649    0.02691530
# 4  1084 0.01338453    0.01387360
# NA  2855 0.03525170            NA

###hence 76,847 codes were substituted
mum2[, ethnicity_5 := ethnicity_5_hes]
mum2[, gen_ethnicity  := NULL]
mum2[, ethnicity_5_hes  := NULL]

mums <- rbind(mum1, mum2)
#correcting the FU time
mums[end_fu > as.Date("01/02/2020", format = "%d/%m/%Y"), end_fu := as.Date("01/02/2020", format = "%d/%m/%Y")]
#calculating new FU time
mums[, age_startfu := as.integer(year(start_fu))-yob]
mums[, age_endfu:= as.integer(year(end_fu))-yob]
mums[, fu_time := age_endfu-age_startfu]
mums[, gender := gender.y]
mums[, gender.y := NULL]


write_parquet(mums, paste0(datafiles, "mothers_studypop_eth.parquet"))


###############################################################################################################################

####repeating the analysis for the children
#extracting ethnicity codes only
obs.files.child <- list.files(path = children_parquet, pattern = "\\Observation")

setwd(children_parquet)
df_child <- extractor_med(list.files = obs.files.child,
                          codelist = eth_codes)
length(unique(df_child$patid))#data of 692,052 children extracted
df_child <- df_child[,obsdate := as.Date(obsdate, format = "%d/%m/%Y")]
df_child <- df_child[,enterdate := as.Date(enterdate, format = "%d/%m/%Y")]
df_child <- df_child[,patid := as.character(patid)]

#describing the overall given codes
janitor::tabyl(df_child$eth5)
janitor::tabyl(df_child$eth16)

#tagging patients with the same ethnicity entry within the same year
df_child<- df_child[order(patid)]
df_child <- df_child[, sysyear := year(obsdate)]
df_child[, duplicates := .N, by = c("patid", "sysyear", "medcodeid")]
#keeping the first ethnicity entry per year if there is a duplicate
df_child[duplicates > 1, first_entry := lapply(.SD, min), by = c("patid", "sysyear"), .SDcols = "obsdate"]
df_child[duplicates >1, keep_row := obsdate == first_entry]
df_child[duplicates == 1, keep_row := TRUE]
df_child <- df_child[keep_row == TRUE]

df_child[, duplicates := NULL]
df_child[, first_entry := NULL]
df_child[, keep_row := NULL]

#count the observations of ethnicity by patient
df_child <- df_child[, by = c("patid"), eth5_count := .N]

tmp <- df_child[, list(patid, eth5_count)]
tmp <- unique(tmp)
janitor::tabyl(tmp$eth5_count) #74.5% of patients have only one ethnicity entry, max 17

#save extra data copy
tmp <- df_child
df_child<- tmp


#five categories
df_child[eth5 == 0, eth5whitecount := .N, by = "patid"]
df_child[eth5 != 0, eth5whitecount := 0]
df_child[, eth5whitecount := lapply(.SD, max_NA), .SDcols = c("eth5whitecount"), by="patid"]
df_child[eth5 == 1, eth5sacount := .N, by = "patid"]
df_child[eth5 != 1, eth5sacount := 0]
df_child[, eth5sacount := lapply(.SD, max_NA), .SDcols = c("eth5sacount"), by="patid"]
df_child[eth5 == 2, eth5blackcount := .N, by = "patid"]
df_child[eth5 != 2, eth5blackcount := 0]
df_child[, eth5blackcount := lapply(.SD, max_NA), .SDcols = c("eth5blackcount"), by="patid"]
df_child[eth5 == 3, eth5othercount := .N, by = "patid"]
df_child[eth5 != 3, eth5othercount := 0]
df_child[, eth5othercount := lapply(.SD, max_NA), .SDcols = c("eth5othercount"), by="patid"]
df_child[eth5 == 4, eth5mixedcount := .N, by = "patid"]
df_child[eth5 != 4, eth5mixedcount := 0]
df_child[, eth5mixedcount := lapply(.SD, max_NA), .SDcols = c("eth5mixedcount"), by="patid"]
df_child[eth5 == 5, notstated5count := .N, by = "patid"]
df_child[eth5 != 5, notstated5count := 0]
df_child[, notstated5count := lapply(.SD, max_NA), .SDcols = c("notstated5count"), by="patid"]
#16 categories
df_child[eth16 ==1, british16count := .N, by = "patid"]
df_child[eth16 != 1, british16count := 0]
df_child[, british16count := lapply(.SD, max_NA), .SDcols = c("british16count"), by="patid"]
df_child[eth16 ==2, irish16count := .N, by = "patid"]
df_child[eth16 != 2, irish16count := 0]
df_child[, irish16count := lapply(.SD, max_NA), .SDcols = c("irish16count"), by="patid"]
df_child[eth16 ==3, otherwhite16count := .N, by = "patid"]
df_child[eth16 != 3, otherwhite16count := 0]
df_child[, otherwhite16count := lapply(.SD, max_NA), .SDcols = c("otherwhite16count"), by="patid"]
df_child[eth16 ==4, whitecarib16count := .N, by = "patid"]
df_child[eth16 != 4, whitecarib16count := 0]
df_child[, whitecarib16count := lapply(.SD, max_NA), .SDcols = c("whitecarib16count"), by="patid"]
df_child[eth16 ==5, whiteaf16count := .N, by = "patid"]
df_child[eth16 != 5, whiteaf16count := 0]
df_child[, whiteaf16count := lapply(.SD, max_NA), .SDcols = c("whiteaf16count"), by="patid"]
df_child[eth16 ==6, whiteasian16count := .N, by = "patid"]
df_child[eth16 != 6, whiteasian16count := 0]
df_child[, whiteasian16count := lapply(.SD, max_NA), .SDcols = c("whiteasian16count"), by="patid"]
df_child[eth16 ==7, othermixed16count := .N, by = "patid"]
df_child[eth16 != 7, othermixed16count := 0]
df_child[, othermixed16count := lapply(.SD, max_NA), .SDcols = c("othermixed16count"), by="patid"]
df_child[eth16 ==8, indian16count := .N, by = "patid"]
df_child[eth16 != 8, indian16count := 0]
df_child[, indian16count := lapply(.SD, max_NA), .SDcols = c("indian16count"), by="patid"]
df_child[eth16 ==9, pak16count := .N, by = "patid"]
df_child[eth16 != 9, pak16count := 0]
df_child[, pak16count := lapply(.SD, max_NA), .SDcols = c("pak16count"), by="patid"]
df_child[eth16 ==10, bangla16count := .N, by = "patid"]
df_child[eth16 != 10, bangla16count := 0]
df_child[, bangla16count := lapply(.SD, max_NA), .SDcols = c("bangla16count"), by="patid"]
df_child[eth16 ==11, otherasian16count := .N, by = "patid"]
df_child[eth16 != 11, otherasian16count := 0]
df_child[, otherasian16count := lapply(.SD, max_NA), .SDcols = c("otherasian16count"), by="patid"]
df_child[eth16 ==12, carib16count := .N, by = "patid"]
df_child[eth16 != 12, carib16count := 0]
df_child[, carib16count := lapply(.SD, max_NA), .SDcols = c("carib16count"), by="patid"]
df_child[eth16 ==13, african16count := .N, by = "patid"]
df_child[eth16 != 13, african16count := 0]
df_child[, african16count := lapply(.SD, max_NA), .SDcols = c("african16count"), by="patid"]
df_child[eth16 ==14, otherblack16count := .N, by = "patid"]
df_child[eth16 != 14, otherblack16count := 0]
df_child[, otherblack16count := lapply(.SD, max_NA), .SDcols = c("otherblack16count"), by="patid"]
df_child[eth16 ==15, chinese16count := .N, by = "patid"]
df_child[eth16 != 15, chinese16count := 0]
df_child[, chinese16count := lapply(.SD, max_NA), .SDcols = c("chinese16count"), by="patid"]
df_child[eth16 ==16, other16count := .N, by = "patid"]
df_child[eth16 != 16, other16count := 0]
df_child[, other16count := lapply(.SD, max_NA), .SDcols = c("other16count"), by="patid"]
df_child[eth16 ==17, notstated16count := .N, by = "patid"]
df_child[eth16 != 17, notstated16count := 0]
df_child[, notstated16count := lapply(.SD, max_NA), .SDcols = c("notstated16count"), by="patid"]

#flagging those patients who only have non stated ethnicity
df_child<- df_child[,  nonstatedonly := (notstated5count > 0 &
                                           eth5whitecount == 0 &
                                           eth5sacount == 0 &
                                           eth5blackcount == 0 &
                                           eth5othercount == 0 &
                                           eth5mixedcount == 0)] 
janitor::tabyl(df_child$nonstatedonly) #5.75% have non-stated only

###finding the most common ethnicity, excluding ethnicity not stated
# Get Row wise max in R
df_child <- df_child[,eth5max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 29:33]
df_child <- df_child[,eth5max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 29:33]
df_child <- df_child[,eth16max_f :=  names(.SD)[max.col(.SD, ties.method = "first")], .SDcols = 35:50]
df_child <- df_child[,eth16max_l :=  names(.SD)[max.col(.SD, ties.method = "last")], .SDcols = 35:50]


#get the rows with equally common ethnicities, excluding non stated
df_child<- df_child[, equaleth5 := eth5max_f != eth5max_l & notstated5count == 0 ]
df_child<- df_child[, equaleth16 := eth16max_f != eth16max_l & notstated16count  ==0]
janitor::tabyl(df_child$equaleth5) #2.09% with equally recorded most common ethnicity


###---FINAL STEP - assigning ethnicity to patid
#case 1: ethnicity is not stated - assign not stated
eth <- df_child[nonstatedonly==T, ethnicity_5 := 5]
eth <- df_child[nonstatedonly==T, ethnicity_16 := 17]

tmp <- eth[nonstatedonly == T]
length(unique(tmp$patid)) #47, 713 children with no stated ethnicity



####case 2: assign the most common ethnicity
eth[equaleth5 == F & nonstatedonly == F, ethnicity_5c := eth5max_f]
eth[equaleth16 == F & nonstatedonly == F, ethnicity_16c := eth16max_f]

write_parquet(eth,  paste0(datafiles, "ethnicity_children_prelim.parquet"))
eth <- read_parquet(paste0(datafiles, "ethnicity_children_prelim.parquet"))


###case 3: if equal common
#finding the latest date of ethnicity record and using this record as eth5/16 code
eth[equaleth5 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "obsdate"]
eth[equaleth5 == T, eth_pick := obsdate == latestethdate]
eth[equaleth5 == T & eth_pick == T, ethnicity_5 := eth5]

eth[equaleth16 ==T, latestethdate := lapply(.SD, max), by = "patid", .SDcols = "obsdate"]
eth[equaleth16 == T, eth_pick := obsdate == latestethdate]
eth[equaleth16 == T & eth_pick == T, ethnicity_16 := eth16]

eth <- eth[(equaleth5 == T & eth_pick == T) | (equaleth16 == T & eth_pick == T) |
             (equaleth5 == F & equaleth16 == F)]


#translating the character back into ethnicity numbers
eth[ethnicity_5c == "eth5whitecount", ethnicity_5 := 0]
eth[ethnicity_5c == "eth5sacount", ethnicity_5 := 1]
eth[ethnicity_5c == "eth5blackcount", ethnicity_5 := 2]
eth[ethnicity_5c == "eth5othercount", ethnicity_5 := 3]
eth[ethnicity_5c == "eth5mixedcount", ethnicity_5 := 4]

eth[ethnicity_16c ==  "british16count", ethnicity_16 := 1]
eth[ethnicity_16c ==  "irish16count", ethnicity_16 := 2]
eth[ethnicity_16c ==  "otherwhite16count", ethnicity_16 := 3]
eth[ethnicity_16c ==  "whitecarib16count", ethnicity_16 := 4]
eth[ethnicity_16c ==  "whiteaf16count", ethnicity_16 := 5]
eth[ethnicity_16c ==  "whiteasian16count", ethnicity_16 := 6]
eth[ethnicity_16c ==  "othermixed16count", ethnicity_16 := 7]
eth[ethnicity_16c ==  "indian16count", ethnicity_16 := 8]
eth[ethnicity_16c ==  "pak16count", ethnicity_16 := 9]
eth[ethnicity_16c ==  "bangla16count", ethnicity_16 := 10]
eth[ethnicity_16c ==  "otherasian16count", ethnicity_16 := 11]
eth[ethnicity_16c ==  "carib16count", ethnicity_16 := 12]
eth[ethnicity_16c ==  "african16count", ethnicity_16 := 13]
eth[ethnicity_16c ==  "otherblack16count", ethnicity_16 := 14]
eth[ethnicity_16c ==  "chinese16count", ethnicity_16 := 15]
eth[ethnicity_16c ==  "other16count", ethnicity_16 := 16]


###special cases that people received conflicting codes on the same day
#pick last row
eth <- unique(eth[, list(patid,  pracid,  ethnicity_5,  
                         ethnicity_16)])

eth[, same_entry := seq_len(.N), by = "patid"]
eth[, same_entry := rowid(patid)]

###setting people to mixed when they have more than one code
eth[same_entry >1, ethnicity_5 := 4]
eth[same_entry >1, ethnicity_16 := 7]

###--- creating a clean ethnicity file
eth <- unique(eth[, list(patid, pracid, ethnicity_5, ethnicity_16)])
janitor::tabyl(eth$ethnicity_5)
hist(eth$ethnicity_5)
janitor::tabyl(eth$ethnicity_16)
hist(eth$ethnicity_16)

####now adding HES data to it
#read in the study pop file
child <- read_parquet(paste0(datafiles, "children_studypop.parquet"))
child <- merge(child, eth, by = c("patid", "pracid"), all.x = T)
child1 <- child[!is.na(ethnicity_5)] #445,990
child2 <- child[is.na(ethnicity_5)] #91,676


child2 <- merge(child2, hes, by = c("patid", "pracid"), all.x = T) 
tabyl(child2$ethnicity_5_hes)
# 
# child2$ethnicity_5_hes     n    percent valid_percent
# 0 56548 0.61682447    0.82600058
# 1  3449 0.03762162    0.05037978
# 2  2569 0.02802260    0.03752556
# 3  2907 0.03170950    0.04246275
# 4  2987 0.03258214    0.04363132
# NA 23216 0.25323967            NA


###hence 68,460 codes were substituted
child2[, ethnicity_5 := ethnicity_5_hes]
child2[, gen_ethnicity  := NULL]
child2[, ethnicity_5_hes  := NULL]

child <- rbind(child1, child2)

write_parquet(child, paste0(datafiles, "children_studypop_eth.parquet"))



