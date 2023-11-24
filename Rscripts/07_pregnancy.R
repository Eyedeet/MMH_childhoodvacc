#---Estimating the start and end dates of the pregnancies

#mothers pop
pop <- data.table(read_parquet(paste0(datafiles, "mothers_studypop_eth_dep.parquet")))
pop <- pop[, patid := as.character(patid)]
pop <- pop[, pracid := as.character(pracid)]
nrow(pop) #535904
double_babies <- which(duplicated(pop$babypatid))


preg <- data.table(read.delim(paste0(linked_data, "pregnancy_register_22_001706_DM.txt")))
preg <- preg[, patid := as.character(patid)]
preg <- preg[, pregstart := as.Date(pregstart, format = "%d/%m/%Y")]
preg <- preg[, pregend := as.Date(pregend, format = "%d/%m/%Y")]
preg <- preg[, deldate := pregend]


#matching by nearest date
pop[, join_time := deldate]
preg[, join_time := pregend]

setkey(pop, patid, join_time)
setkey(preg, patid, join_time)

df <- pop[preg, roll = Inf]
df <- df[!is.na(babypatid)]
df[, pregstart := as.Date(pregstart, format = "%d/%m/%Y")]
df[, secondtrim := as.Date(secondtrim, format = "%d/%m/%Y")]
df[, thirdtrim := as.Date(thirdtrim, format = "%d/%m/%Y")]

df[, mismatch := interval(deldate, pregend)%/%days(1)]
df[, duration := interval(pregstart, deldate) %/% weeks(1)]

df <- df[mismatch <=28]
length(setdiff(df$babypatid, matched_babies)) #56197

df <- df[, list( patid, babypatid, pracid, deldate, n_children, birthorder,
                 yob,  mob, death, region, regstartdate,regenddate, start_fu,
                 end_fu, fu_time, ethnicity_5, ethnicity_16,age_startfu,
                 age_endfu, gender, imd,  pregstart, secondtrim, thirdtrim,
                 pregend, mismatch)]


#remove duplicated children which have multiple pregnancoes matching
df[, multiple := .N, by = "babypatid"]
# df$multiple      n      percent
# 1 305712 9.296368e-01
# 2  23124 7.031756e-02
# 3     15 4.561336e-05

#closest pregnancy
df[multiple >1, min_gap := min(mismatch), by = "babypatid"]
df[multiple >1, keep := mismatch == min_gap, by = "babypatid"]
df <- df[multiple == 1 | keep ==T]
df[, multiple := .N, by = "babypatid"]
tabyl(df$multiple)
# df$multiple      n   percent
# 1 317253 0.9998361198
# 2     52 0.0001638802


#keep pregnancy with sensible length >20 weeks
df <- df[duration >20]
df[, multiple := .N, by = "babypatid"]
tabyl(df$multiple) #no multiples left


###rematching the study population
df <- df[, list( patid, babypatid, pracid, deldate, n_children, birthorder,
                 yob,  mob, death, region, regstartdate,regenddate, start_fu,
                 end_fu, fu_time, ethnicity_5, ethnicity_16,age_startfu,
                 age_endfu, gender, imd,  pregstart, secondtrim, thirdtrim,
                 pregend)]

unmatched_babies <- pop[!babypatid %chin% df$babypatid]
unmatched_babies <-unmatched_babies[, list( patid, babypatid, pracid, deldate, n_children, birthorder,
                                            yob,  mob, death, region, regstartdate,regenddate, start_fu,
                                            end_fu, fu_time, ethnicity_5, ethnicity_16,age_startfu,
                                            age_endfu, gender, imd)]

unmatched_babies[, pregstart:= deldate-weeks(40)]
unmatched_babies[, secondtrim:= as.Date(NA)]
unmatched_babies[, thirdtrim:= as.Date(NA)]
unmatched_babies[, pregend:= deldate]

all_babies <- rbind(df, unmatched_babies)

any(duplicated(all_babies$babypatid))

###writing file
write_parquet(all_babies, paste0(datafiles, "mothers_studypop_eth_dep_preg.parquet"))



###describing
nrow(all_babies[!is.na(pregstart)])/nrow(all_babies)
#57.36% of the babies have their pregnancy start from the linkage, 
#the other 43% have an estimated pregancny start date