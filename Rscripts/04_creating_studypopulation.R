####Finalising thhe final study population

###all initidal patients
all_pat <- data.table(read.delim(paste0(datafiles, "patid_linkage.txt"),
                                 header = TRUE, sep = " ", na.strings = ""))
all_pat[, patid := as.character(patid)]
nrow(all_pat) #1434313

#only keep patients who are in the MBL
#mother baby link
MBL_baby <- data.table(read.delim(paste0(linked_data, "MBL_Baby_22_001706_DM.txt")),
                       header = TRUE, sep = "\t", na.strings = "")
MBL_mum <- data.table(read.delim(paste0(linked_data, "MBL_Mum_22_001706_DM.txt")),
                      header = TRUE, sep = "\t", na.strings = "")

MBL_baby$mumpatid <- as.character(MBL_baby$mumpatid)
MBL_baby$babypatid <- as.character(MBL_baby$babypatid)
MBL_baby[, patid := babypatid]
nrow(MBL_baby) #796069
MBL_mum$mumpatid <- as.character(MBL_mum$mumpatid)
MBL_mum$babypatid <- as.character(MBL_mum$babypatid)
MBL_mum[, patid := mumpatid]
nrow(MBL_mum) #994192

mothers <- merge(MBL_mum, all_pat, by = "patid")
nrow(mothers) #994192
babies <- merge(MBL_baby, all_pat, by = "patid")
nrow(babies) #796069

#identifying the pairs which are in both datasets
baby_id <- mothers$babypatid
mum_id <- babies$mumpatid

mothers <- mothers[patid %chin% mum_id]
nrow(mothers) #993167, 1025 not in the baby  data set

babies <- babies[patid %chin% baby_id]
nrow(babies) #788379, 7690 not in the mother data set

#double checking
any(!mothers$patid %chin% babies$mumpatid)
any(!babies$patid %chin% mothers$babypatid)


mothers <- mothers[, list(patid, pracid.x, mumpatid, babypatid, deldate,
                          mumbirthyear, babybirthyear, gender, children)]

babies <- babies[, list(patid, pracid.x, mumpatid, babypatid, deldate,
                        mumbirthyear, babybirthyear, gender, children)]

mothers <- mothers[order(deldate)]
mothers[, n_children := .N, by = "patid"]
mothers[, birthorder := rowid(patid)]
mothers[, qual := n_children == children] 
#check match of number of children and count
# mothers$qual      n percent
# TRUE 993167       1

mothers[, deldate := as.Date(deldate, format = "%d/%m/%Y")]
mothers[, del_year := year(deldate)]
nrow(mothers) #993167

#mark the order of children included into the study period
mothers[, child_in_fu := del_year >= 2006 &  del_year <= 2014 ] #17.6 % of children outside of study period
mothers[child_in_fu== T, n_children_MBL := .N, by = "patid"]
mothers[child_in_fu== T, birthorder_MBL := rowid(patid)]
mothers[, pracid := pracid.x]

#merging the maternal data with general patient file
mothers_pat <-  data.table(read_parquet(paste0(mothers_parquet, (list.files(path = mothers_parquet, pattern = "\\Patient")))))
mothers_pat[, patid := as.character(patid)]
mothers_pat[, regstartdate := as.Date(regstartdate, format = "%d/%m/%Y")]
mothers_pat[, regenddate  := as.Date(regenddate , format = "%d/%m/%Y")]
mothers_pat[, cprd_ddate := as.Date(cprd_ddate, format = "%d/%m/%Y")]
mothers_pat[, emis_ddate := as.Date(emis_ddate, format = "%d/%m/%Y")]


#merging maternal file with patient file
length(unique(mothers$patid)) #638769
length(unique(mothers_pat$patid)) #653425
df <- merge(mothers, mothers_pat, by = c("patid", "pracid"), all.x = T)
length(unique(df$patid)) #638769

#only keep acceptable mums
df <- df[acceptable == 1]
length(unique(df$patid)) #638769

#keep only mums with a regstartdate
df[, start_ok := year(regstartdate) >= 1940]
any(is.na(df$regstartdate))
#only keeo children with delivery date
df[, birth_ok := year(deldate) >= 1940]
length(unique(df$patid)) #638769
nrow(df)



#for each deliver date, check whether mum was registered 15 month prior delivery
df[, enough_fu := (regstartdate+lubridate::dmonths(15)) <= deldate]
tabyl(df$enough_fu)
# df$enough_fu      n  percent
# FALSE 308868 0.310993
# TRUE 684299 0.689007
length(unique(df[enough_fu == T]$patid))


tabyl(df[enough_fu == F, birthorder_MBL])
# df[enough_fu == F, birthorder_MBL]      n      percent valid_percent
# 1 245099 7.935396e-01  8.716553e-01
# 2  33111 1.072011e-01  1.177540e-01
# 3   2752 8.909955e-03  9.787046e-03
# 4    209 6.766645e-04  7.432750e-04
# 5     15 4.856444e-05  5.334509e-05


tabyl(df[enough_fu == T, birthorder_MBL])
# df[enough_fu == T, birthorder_MBL]      n      percent valid_percent
# 1 393670 5.752895e-01  7.330407e-01
# 2 126244 1.844866e-01  2.350751e-01
# 3  15364 2.245217e-02  2.860883e-02
# 4   1556 2.273860e-03  2.897380e-03
# 5    179 2.615816e-04  3.333104e-04
# 6     19 2.776564e-05  3.537931e-05


df <- df[enough_fu == T]
length(unique(df$patid)) #447600, 191169 mothers excluded
df[, start_fu := regstartdate]

#adding correct date of death based on ONS-linkage
ons <- data.table(read.delim(paste0(linked_data, "death_patient_22_001706_DM.txt")),
                  header = TRUE, sep = "\t", na.strings = "")
ons[, patid := as.character(patid)]
ons[, dod := as.Date(dod, format = "%d/%m/%Y")]
ons <- ons[, list(patid, dod)]
df <- merge(df, ons, by = "patid", all.x = T)
tabyl(is.na(df$dod))
# is.na(df$dod)      n   percent
# FALSE   1956 0.0028584
# TRUE 682343 0.9971416
df[, death := if_else(is.na(dod), cprd_ddate, dod)]

#check that data wasn't collected after lcd
practice <- data.table(read_parquet(paste0(mothers_parquet, list.files(path = mothers_parquet, pattern = "\\Practice"))))
#1391 practices
practice <- practice[, list(pracid, lcd, region)]
df <- merge(df, practice, by = "pracid", all.x = T)
length(unique(df$patid))#447600
df[, lcd := as.Date(lcd, format = "%d/%m/%Y")]

#determining the end of follow up
df[, end_fu := min(regenddate, death, lcd, na.rm = T), by = "patid"]
df[is.na(end_fu), end_fu := as.Date("01/02/2020", format = "%d/%m/%Y")]
length(unique(df$patid)) #447600

#quality check of follow up time
df <- df[start_fu <= end_fu]
length(unique(df$patid))  #447600

#calculating the follow up time
df[, age_startfu := as.integer(year(start_fu))-yob]
df[, age_endfu:= as.integer(year(end_fu))-yob]
df[, fu_time := age_endfu-age_startfu]



########################################################################################
##only keep children within the fu
df <- df[child_in_fu == T]
length(unique(df$patid)) #422033
nrow(df)#537037 children


####completing the children subdataset
df_child <- df[, list(babypatid, deldate, babybirthyear, birthorder, birthorder_MBL)]
df_child[, patid := babypatid]
df_child[, babypatid := NULL]

list.children <- paste0(children_parquet_old, (list.files(path = children_parquet_old, pattern = "\\Patient")))
patient_df <- list()

for(i in 1:length(list.children)){
  
  tmp <- as.data.table(arrow::read_parquet(file = list.children[i]))
  tmp$regstartdate <- as.Date(tmp$regstartdate, format = "%d/%m/%Y")
  tmp$regenddate <- as.Date(tmp$regenddate, format = "%d/%m/%Y")
  tmp$yob <- as.integer(tmp$yob)
  tmp$mob <- as.integer(tmp$mob)
  tmp$accetable <- as.integer(tmp$acceptable)
  tmp$pracid <- as.integer(tmp$pracid)
  tmp$patid <- as.character(tmp$patid)
  
  patient_df[[i]] <- tmp
}

pats <- rbind(patient_df[[1]], patient_df[[2]], patient_df[[3]])
colnames(pats)
colnames(df_child)

#merging files
df_child <- merge(df_child, pats, by = c("patid"), all.x = T)
nrow(df_child) #537037

#only acceptable children
df_child <- df_child[accetable == 1]
nrow(df_child) #535953, 1084 removed

#check whether the years of birth match
df_child[, ymatch := babybirthyear == yob]
tabyl(df_child$ymatch) # all matching

#check whether all of them have a delivery date
df_child <- df_child[!is.na(deldate)]
nrow(df_child) #535953

#reducing data frame
df_child <- df_child[, list(patid, pracid, deldate, birthorder, 
                            gender, yob, mob, emis_ddate,
                            regstartdate, regenddate, cprd_ddate)]


#calculating the follow up times
df_child[, start_fu := regstartdate]
df_child <- merge(df_child, ons, by = "patid", all.x = T)
df_child <- df_child[, dod := as.Date(dod, format = "%d/%m/%Y")]
df_child <- df_child[, cprd_ddate := as.Date(cprd_ddate, format = "%d/%m/%Y")]
tabyl(is.na(df_child$dod))
# is.na(df_child$dod)      n     percent
# FALSE    548 0.001022478
# TRUE 535405 0.998977522
df_child[, death := if_else(is.na(dod), cprd_ddate, dod)]
df_child <- merge(df_child, practice, by = "pracid", all.x = T)
length(unique(df_child$patid))#535953
df_child[, lcd := as.Date(lcd, format = "%d/%m/%Y")]

#determining the end of follow up
df_child[, bday5 := deldate+dyears(5)]
df_child[, bday5 := format(as.POSIXct(bday5,format='%Y/%m/%d %H:%M:%S'),format='%d/%m/%Y')]
df_child[, bday5 := as.Date(bday5, format = "%d/%m/%Y")]
df_child[, end_fu := min(regenddate, death, lcd, bday5, na.rm = T), by = "patid"]
df_child[, end_fu := if_else(end_fu > as.Date("01/02/2020", format = "%d/%m/%Y"),
                             as.Date("01/02/2020", format = "%d/%m/%Y"), end_fu)]
df_child[is.na(end_fu), end_fu := as.Date("01/02/2020", format = "%d/%m/%Y")]
length(unique(df_child$patid)) #535953

#quality check of follow up time
df_child <- df_child[start_fu <= end_fu]
length(unique(df_child$patid))  #535,689

df_child[, age_startfu := ifelse(start_fu >= deldate, (interval(deldate, start_fu)%/%days(1)),
                                 (interval(start_fu, deldate)%/%days(1)))]
df_child[, age_endfu := interval(deldate, end_fu)%/%days(1)]
df_child[, fu_time := age_endfu-age_startfu]


###keeping only the mothers to these children
length(unique(df$patid)) #422033
df_mum <- df[babypatid %chin% df_child$patid]
length(unique(df$patid))

tabyl(df_child$birthorder)
# df_child$birthorder      n      percent
# 1 333198 6.216926e-01
# 2 158467 2.956733e-01
# 3  35170 6.562143e-02
# 4   6960 1.298621e-02
# 5   1537 2.867789e-03
# 6    408 7.612608e-04
# 7    135 2.518878e-04
# 8     45 8.396259e-05
# 9     18 3.358503e-05

colnames(df_child)
df_child <- df_child[, list(patid, pracid, deldate, birthorder, gender,
                            yob, mob, death, region,  regstartdate, regenddate, start_fu,
                            end_fu, fu_time, age_startfu, age_endfu, bday5)]

colnames(df_mum)
df_mum <- df_mum[, list(patid, pracid, babypatid, deldate, n_children, birthorder, gender.y,
                        yob, mob, death, region, regstartdate, regenddate, start_fu,
                        end_fu, fu_time)]
#saving final data
write_parquet(df_child, paste0(datafiles, "children_studypop.parquet"))
write_parquet(df_mum, paste0(datafiles, "mothers_studypop.parquet"))

