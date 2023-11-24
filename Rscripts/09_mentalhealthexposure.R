###Script to generate maternal mental health outcomes

#--- reading in the the code lists for MMH 
csv_files <- list.files(paste0(codelists, "mental_health/"))
for(i in 1:length(csv_files)){
  csv_files[[i]] <- paste(c(codelists, "mental_health/", csv_files[[i]]),
                          collapse="")
}


#reading in the code lists
CMD_codelist <-as.data.table(read.csv(csv_files[[1]]))
CMD_codelist$medcodeid <- as.character(CMD_codelist$medcodeid)

SMI_codelist <- as.data.table(read.csv(csv_files[[2]]))
SMI_codelist[, medcodeid := MedCodeId]
SMI_codelist[, MedCodeId := NULL ]
SMI_codelist$medcodeid <- as.character(SMI_codelist$medcodeid)

SUD_codelist <- as.data.table(read.csv(csv_files[[3]]))
SUD_codelist[, medcodeid := MedCodeId]
SUD_codelist[, MedCodeId := NULL ]
SUD_codelist$medcodeid <- as.character(SUD_codelist$medcodeid)

#matching the variables for timing
# 0 : diagnosis, 1: history, 2:antenatal, 3:postnatal
SMI_codelist[history ==1, timing :=  1]
SMI_codelist[postpartum ==1, timing :=  3]
SMI_codelist[is.na(history) & is.na(postpartum), timing := 0]
tabyl(SMI_codelist$postpartum)


SUD_codelist[history ==1, timing :=  1]
SUD_codelist[medcodeid ==  "9322521000006110", timing := 2]
SUD_codelist[medcodeid ==  "9324921000006110", timing := 3]
SUD_codelist[is.na(perinatal)& is.na(history), timing := 0]

SUD_codelist[,term := Term]
SUD_codelist[,Term := NULL]
SUD_codelist <- SUD_codelist[, list(medcodeid, term, timing)]
SMI_codelist[,term := Term]
SMI_codelist[,Term := NULL]
SMI_codelist <- SMI_codelist[, list(medcodeid, term, timing)]

CMD_codelist <- CMD_codelist[, list(medcodeid, term, timing)]

#removing the duplicates in the code lists
CMD_codelist <- unique(CMD_codelist)
SUD_codelist <- unique(SUD_codelist)
SMI_codelist <- unique(SMI_codelist)


####filtering the different diagnosis
setwd(mothers_parquet)
obs.files<- list.files(path = mothers_parquet, pattern = "\\Observation")
#CMD
df_CMD <- extractor_med(list.files = obs.files, codelist = CMD_codelist)
df_CMD <- df_CMD[!is.na(medcodeid)]
length(unique(df_CMD$patid)) #268,720

df_SUD <- extractor_med(list.files = obs.files, codelist = SUD_codelist)
df_SUD <- df_SUD[!is.na(medcodeid)]
length(unique(df_SUD$patid)) #28,514

df_SMI <- extractor_med(list.files = obs.files, codelist = SMI_codelist)
df_SMI <- df_SMI[!is.na(medcodeid)]
length(unique(df_SMI$patid)) #1,204


df_CMD$patid <- as.character(df_CMD$patid)
df_SUD$patid <- as.character(df_SUD$patid)
df_SMI$patid <- as.character(df_SMI$patid)
df_CMD$pracid <- as.character(df_CMD$pracid)
df_SUD$pracid <- as.character(df_SUD$pracid)
df_SMI$pracid <- as.character(df_SMI$pracid)

df_CMD$obsdate <- as.Date(df_CMD$obsdate, format = "%d/%m/%Y")
df_SUD$obsdate <- as.Date(df_SUD$obsdate, format = "%d/%m/%Y")
df_SMI$obsdate <- as.Date(df_SMI$obsdate, format = "%d/%m/%Y")

df_CMD <- df_CMD[, list(patid, pracid, obsdate)]
df_CMD <- df_CMD[, CMD:= 1]
df_CMD <- unique(df_CMD)
df_SUD <- df_SUD[, list(patid, pracid, obsdate)]
df_SUD <- df_SUD[, SUD:= 1]
df_SUD <- unique(df_SUD)
df_SMI <- df_SMI[, list(patid, pracid, obsdate)]
df_SMI <- df_SMI[, SUD:= 1]
df_SMI <- unique(df_SMI)


###establishing whether mothers had any MHI before FU time of outcome
pop <- read_parquet(paste0(datafiles, "mothers_studypop_all_cov.parquet"))

CMD<- merge(pop, df_CMD, by = c("patid", "pracid"), all.x = T)
SMI<- merge(pop, df_SMI, by = c("patid", "pracid"), all.x = T)
SUD<- merge(pop, df_SUD, by = c("patid", "pracid"), all.x = T)

#check whether obsdate is sensible
length(unique(CMD$patid)) #421,750
CMD<- CMD[year(obsdate)>=yob| is.na(obsdate)]
length(unique(CMD$patid)) #4217,49

length(unique(SMI$patid)) #421750
SMI <- SMI[year(obsdate)>=yob| is.na(obsdate)]
length(unique(SMI$patid)) #421750

length(unique(SUD$patid)) #421750
SUD <- SUD[year(obsdate)>=yob| is.na(obsdate)]
length(unique(SUD$patid)) #421750


#FU for first vaccine, before age 8 weeks/ days
CMD[, age_vacc1 := deldate+dweeks(8)]
SMI[, age_vacc1 := deldate+dweeks(8)]
SUD[, age_vacc1 := deldate+dweeks(8)]
#FU for second vaccine, before age 1 year
CMD[, age_vacc2 := deldate+ddays(365)]
SMI[, age_vacc2 := deldate+ddays(365)]
SUD[, age_vacc2 := deldate+ddays(365)]
#FU for third vaccine, before age 18 months
CMD[, age_vacc3 := deldate+dmonths(18)]
SMI[, age_vacc3 := deldate+dmonths(18)]
SUD[, age_vacc3 := deldate+dmonths(18)]

#checking whether any diagnosis before vacc due date
CMD[!is.na(obsdate), CMD_vacc1 := obsdate <age_vacc1]
CMD[!is.na(obsdate), CMD_vacc2 := obsdate <age_vacc2]
CMD[!is.na(obsdate), CMD_vacc3 := obsdate <age_vacc3]
SMI[!is.na(obsdate), SMI_vacc1 := obsdate <age_vacc1]
SMI[!is.na(obsdate), SMI_vacc2 := obsdate <age_vacc2]
SMI[!is.na(obsdate), SMI_vacc3 := obsdate <age_vacc3]
SUD[!is.na(obsdate), SUD_vacc1 := obsdate <age_vacc1]
SUD[!is.na(obsdate), SUD_vacc2 := obsdate <age_vacc2]
SUD[!is.na(obsdate), SUD_vacc3 := obsdate <age_vacc3]

#describing the data
tabyl(CMD$CMD_vacc1) #30.07%
tabyl(CMD$CMD_vacc2) #36.59%
tabyl(CMD$CMD_vacc3) #39.72%

tabyl(SUD$SUD_vacc1) #7.00%
tabyl(SUD$SUD_vacc2) # 7.69%
tabyl(SUD$SUD_vacc3) #8.09%

tabyl(SMI$SMI_vacc1) #0.37%
tabyl(SMI$SMI_vacc2) #0.43%
tabyl(SMI$SMI_vacc3) #0.46%

#check whether there was any diagnosis before vaccination date
CMD[, vacc1_CMD := any(CMD_vacc1 == T), by = c("patid", "babypatid")]
CMD[, vacc2_CMD := any(CMD_vacc2 == T), by = c("patid", "babypatid")]
CMD[, vacc3_CMD := any(CMD_vacc3 == T), by = c("patid", "babypatid")]

SMI[, vacc1_SMI := any(SMI_vacc1 == T), by = c("patid", "babypatid")]
SMI[, vacc2_SMI := any(SMI_vacc2 == T), by = c("patid", "babypatid")]
SMI[, vacc3_SMI := any(SMI_vacc3 == T), by = c("patid", "babypatid")]

SUD[, vacc1_SUD := any(SUD_vacc1 == T), by = c("patid", "babypatid")]
SUD[, vacc2_SUD := any(SUD_vacc2 == T), by = c("patid", "babypatid")]
SUD[, vacc3_SUD := any(SUD_vacc3 == T), by = c("patid", "babypatid")]


CMD <- unique(CMD[, list(patid, babypatid, vacc1_CMD, vacc2_CMD, vacc3_CMD, obsdate)])
write_parquet(CMD, paste0(datafiles, "CMD_raw.parquet"))

SMI <- unique(SMI[, list(patid, babypatid, vacc1_SMI, vacc2_SMI, vacc3_SMI]))
write_parquet(SMI, paste0(datafiles, "SMI_raw.parquet"))
SUD <- unique(SUD[, list(patid, babypatid, vacc1_SUD, vacc2_SUD, vacc3_SUD)])
write_parquet(SUD, paste0(datafiles, "SUD_raw.parquet"))

###merging this with pop data
df <- merge(pop, CMD, by = c("patid", "babypatid"), all.x = T)
df <- merge(df, SMI, by = c("patid", "babypatid"), all.x = T )
df <- merge(df, SUD, by = c("patid", "babypatid"), all.x = T )

#categorising the MH exposure in the different combinations
#first vaccination age
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "None"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "CMD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "SMI"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "SUD"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == T) &
     (vacc1_SUD == F | is.na(vacc1_SUD)), MHI_vacc1 := "CMD & SMI"]
df[(vacc1_CMD == T) &
     (vacc1_SMI == F | is.na(vacc1_SMI))&
     (vacc1_SUD == T), MHI_vacc1 := "CMD & SUD"]
df[(vacc1_CMD == F | is.na(vacc1_CMD)) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "SMI & SUD"]
df[(vacc1_CMD == T ) &
     (vacc1_SMI == T )&
     (vacc1_SUD == T), MHI_vacc1 := "CMD & SMI & SUD"]

#second vaccination age
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "None"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "CMD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "SMI"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "SUD"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == T) &
     (vacc2_SUD == F | is.na(vacc2_SUD)), MHI_vacc2 := "CMD & SMI"]
df[(vacc2_CMD == T) &
     (vacc2_SMI == F | is.na(vacc2_SMI))&
     (vacc2_SUD == T), MHI_vacc2 := "CMD & SUD"]
df[(vacc2_CMD == F | is.na(vacc2_CMD)) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "SMI & SUD"]
df[(vacc2_CMD == T ) &
     (vacc2_SMI == T )&
     (vacc2_SUD == T), MHI_vacc2 := "CMD & SMI & SUD"]

#third vaccination age
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "None"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "CMD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "SMI"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "SUD"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == T) &
     (vacc3_SUD == F | is.na(vacc3_SUD)), MHI_vacc3 := "CMD & SMI"]
df[(vacc3_CMD == T) &
     (vacc3_SMI == F | is.na(vacc3_SMI))&
     (vacc3_SUD == T), MHI_vacc3 := "CMD & SUD"]
df[(vacc3_CMD == F | is.na(vacc3_CMD)) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "SMI & SUD"]
df[(vacc3_CMD == T ) &
     (vacc3_SMI == T )&
     (vacc3_SUD == T), MHI_vacc3 := "CMD & SMI & SUD"]


df[,vacc1_CMD := NULL ]
df[,vacc2_CMD := NULL ]
df[,vacc3_CMD := NULL ]
df[,vacc1_SMI := NULL ]
df[,vacc2_SMI := NULL ]
df[,vacc3_SMI := NULL ]
df[,vacc1_SUD := NULL ]
df[,vacc2_SUD := NULL ]
df[,vacc3_SUD := NULL ]

write_parquet(df, paste0(datafiles, "studypop_all_exposures_cat.parquet"))

#sense checking
any(df$MHI_vacc1!= "None" & df$MHI_vacc2 == "None")
any(df$MHI_vacc2!= "None" & df$MHI_vacc3 == "None")


#-------------------------------------------------------------------------------
#descrbing the vaccine journey by vaccine dose

#restructing the data set
tmp <- unique(df[, list(patid, babypatid, birthorder,  MHI_vacc1, MHI_vacc2, MHI_vacc3)])
tmp <- melt(tmp, id.vars = c("patid", "babypatid", "birthorder"))
tmp[, vaccine_dose := factor(variable, levels = c("MHI_vacc1","MHI_vacc2", "MHI_vacc3"),
                             labels = c("1st DTP", "3rd PCV", "2nd MMR"), ordered = T)]
tmp[, MHI_issue := factor(value, levels = c("None","CMD", "SUD", "SMI", "CMD & SUD",
                                            "CMD & SMI", "SMI & SUD","CMD & SMI & SUD"),
                          ordered = T)]
tmp[birthorder == 1, birth_order_cat := "First child"]
tmp[birthorder == 2, birth_order_cat := "Second child"]
tmp[birthorder == 3, birth_order_cat := "Third child"]
tmp[birthorder > 3, birth_order_cat := "Forth child or higher"]
tmp[, birth_order_cat := factor(birth_order_cat, levels = c("First child", "Second child",
                                                            "Third child", "Forth child or higher"),
                                ordered = T)]


#first child in the order
child_1 <- tmp[birthorder == 1]
is_lodes_form(data =child_1, key = vaccine_dose, value = MHI_issue, id = patid)
child1_MHI_journey <- child_1 %>%
  filter(MHI_issue != "None") %>%
  ggplot( aes(x = vaccine_dose, stratum = MHI_issue, alluvium = patid, 
              fill = MHI_issue, label = MHI_issue))+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow(alpha =0.5)+
  geom_stratum(alpha = .5)+
  xlab("Vaccine appointment")+
  ylab("Frequency")+
  labs(fill = "Maternal mental health issue")+
  geom_text(stat = "stratum", color="black",
            min.y = 800000)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 

ggsave("allu_child1_MMH_journey.pdf",
       child1_MHI_journey,
       width = 8,
       height = 6,
       bg = "white",
       path= graphfiles)


#second child in the order
child_2 <- tmp[birthorder == 2]
is_lodes_form(data =child_2, key = vaccine_dose, value = MHI_issue, id = patid)
child2_MHI_journey <- child_2 %>%
  filter(MHI_issue != "None") %>%
  ggplot( aes(x = vaccine_dose, stratum = MHI_issue, alluvium = patid, 
              fill = MHI_issue, label = MHI_issue))+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow(alpha =0.5)+
  geom_stratum(alpha = .5)+
  xlab("Vaccine appointment")+
  ylab("Frequency")+
  labs(fill = "Maternal mental health issue")+
  geom_text(stat = "stratum", color="black",
            min.y = 800000)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 

ggsave("allu_child2_MMH_journey.pdf",
       child2_MHI_journey,
       width = 8,
       height = 6,
       bg = "white",
       path= graphfiles)


#third child
child_3 <- tmp[birthorder == 3]
is_lodes_form(data =child_3, key = vaccine_dose, value = MHI_issue, id = patid)
child3_MHI_journey <- child_3 %>%
  filter(MHI_issue != "None") %>%
  ggplot( aes(x = vaccine_dose, stratum = MHI_issue, alluvium = patid, 
              fill = MHI_issue, label = MHI_issue))+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow(alpha =0.5)+
  geom_stratum(alpha = .5)+
  xlab("Vaccine appointment")+
  ylab("Frequency")+
  labs(fill = "Maternal mental health issue")+
  geom_text(stat = "stratum", color="black",
            min.y = 800000)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 

ggsave("allu_child3_MMH_journey.pdf",
       child3_MHI_journey,
       width = 8,
       height = 6,
       bg = "white",
       path= graphfiles)


####journey of MHI between children
all_children <- tmp[vaccine_dose == "1st DTP"]
is_lodes_form(data =all_children, key = birth_order_cat, value = MHI_issue, id = patid)
all_MHI_journey <- all_children %>%
  filter(birthorder <4) %>%
  ggplot( aes(x = birth_order_cat, stratum = MHI_issue, alluvium = patid, 
              fill = MHI_issue, label = MHI_issue))+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow(alpha =0.5)+
  geom_stratum(alpha = .5)+
  xlab("Number of child")+
  ylab("Frequency")+
  labs(fill = "Maternal mental health issue")+
  geom_text(stat = "stratum", color="black",
            min.y = 800000)+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 

ggsave("allu_all_children_MMH_journey_1DTP.pdf",
       all_MHI_journey,
       width = 8,
       height = 6,
       bg = "white",
       path= graphfiles)

all_babies <- length(tmp$babypatid)
tmp[, percentage := .N/all_babies, by = c("MHI_issue", "vaccine_dose")]

tiny <- unique(tmp[, list(MHI_issue, vaccine_dose, percentage)])
tiny[, percentage := rounding(percentage)]


desc1 <- tabyl(df$MHI_vacc1)
desc2  <- tabyl(df$MHI_vacc2)
desc3 <- tabyl(df$MHI_vacc3)
desc1 <- desc1 %>% mutate(percent = rounding(percent*100))
desc2 <- desc2 %>% mutate(percent = rounding(percent*100))
desc3 <- desc3 %>% mutate(percent = rounding(percent*100))

sum_MHI_exp <- cbind(desc1, desc2, desc3)

write.table(sum_MHI_exp, paste0(results, "MHI_exposure_cat.txt"), sep = "\t" )
#-------------------------------------------------------------------------------
####establishing the timing in relationship to the pregnancy
#match mother with all included children
#link each child with a pregnancy 
#establish timing for MHI in relationship to each mother baby pair
MBL <- MBL[, list(mumpatid, babypatid, deldate, mumbirthyear, babybirthyear,
                  gender, children, child_order)]
MBL[, patid := mumpatid]
MBL$patid <- as.character(MBL$patid)

#making different datasets for different number of children
max(MBL$child_order, na.rm=T) # 7
MBL_1 <- MBL[child_order == 1]
MBL_2 <- MBL[child_order == 2]
MBL_3 <- MBL[child_order == 3]
MBL_4 <- MBL[child_order == 4]
MBL_5 <- MBL[child_order == 5]
MBL_6 <- MBL[child_order == 6]
MBL_7 <- MBL[child_order == 7]

####exploring CMD
CMD_linked1 <- merge(MBL_1, df_CMD,  by = "patid", all.x = T)
CMD_linked2 <- merge(MBL_2, df_CMD,  by = "patid", all.x = T)
CMD_linked3 <- merge(MBL_3, df_CMD,  by = "patid", all.x = T)
CMD_linked4 <- merge(MBL_4, df_CMD,  by = "patid", all.x = T)
CMD_linked5 <- merge(MBL_5, df_CMD,  by = "patid", all.x = T)
CMD_linked6 <- merge(MBL_6, df_CMD,  by = "patid", all.x = T)
CMD_linked7 <- merge(MBL_7, df_CMD,  by = "patid", all.x = T)

all_CMDs <- list(CMD_linked1, CMD_linked2, CMD_linked3, CMD_linked4, 
                 CMD_linked5, CMD_linked6, CMD_linked7)

for(i in 1:length(all_CMDs)){
  
  all_CMDs[[i]][,  N_diagnoses :=  .N, by = "mumpatid"]
  all_CMDs[[i]]$deldate <-as.Date(all_CMDs[[i]]$deldate, format = "%d/%m/%Y")
  all_CMDs[[i]]$obsdate <-as.Date(all_CMDs[[i]]$obsdate, format = "%d/%m/%Y")
  all_CMDs[[i]][, rel_time_diag := interval(deldate, obsdate) %/% months(1)]
  all_CMDs[[i]][, rel_time_diag_weeks := interval(deldate, obsdate) %/% weeks(1)]
  
}

for(i in 1:length(all_CMDs)){
  
  print(max(all_CMDs[[i]]$N_diagnoses))
}

# [1] 681
# [1] 681
# [1] 681
# [1] 150
# [1] 58
# [1] 4
# [1] 1


#exploring number of diagnosis per mother
plots_N_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_N_diag[[i]] <- ggplot(all_CMDs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 700, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 700, by=50))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]], plots_N_diag[[4]],
                              plots_N_diag[[5]], plots_N_diag[[6]],
                              plots_N_diag[[7]],align = "h", 
                              labels = c("A", "B", "C", "D", "E", "F", "G"), label_size = 10,
                              ncol = 2, nrow = 4)

ggsave("N_diag_pP_CMD.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)

for(i in 1:length(all_CMDs)){
  plots_N_diag[[i]] <- ggplot(all_CMDs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 50, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 50, by=10))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]], plots_N_diag[[4]],
                              plots_N_diag[[5]], plots_N_diag[[6]],
                              plots_N_diag[[7]],align = "h", 
                              labels = c("A", "B", "C", "D", "E", "F", "G"), label_size = 10,
                              ncol = 2, nrow = 4)

ggsave("N_diag_pP_CMD_2.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)

###Calculating the timing of each diagnoses in relation to the child's bday
plots_bday5_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_bday5_diag[[i]] <- ggplot(all_CMDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-60, 60, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 60, by=10))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_5y <- plot_grid(plots_bday5_diag[[1]],plots_bday5_diag[[2]],
                                  plots_bday5_diag[[3]], plots_bday5_diag[[4]],
                                  plots_bday5_diag[[5]], plots_bday5_diag[[6]],
                                  plots_bday5_diag[[7]],align = "h", 
                                  labels = c("A", "B", "C", "D", "E", "F", "G"),
                                  label_size = 10,
                                  ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_5y_CMD.pdf",
       hist_birth_timing_5y,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)


#Calculating timing around the first bday of life
plots_bday1_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_bday1_diag[[i]] <- ggplot(all_CMDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-12, 12, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-12, 12, by=1))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_1y <- plot_grid(plots_bday1_diag[[1]],plots_bday1_diag[[2]],
                                  plots_bday1_diag[[3]], plots_bday1_diag[[4]],
                                  plots_bday1_diag[[5]], plots_bday1_diag[[6]],
                                  plots_bday1_diag[[7]],align = "h", 
                                  labels = c("A", "B", "C", "D", "E", "F", "G"),
                                  label_size = 10,
                                  ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_1y.pdf",
       hist_birth_timing_1y,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)




#Calculating timing around the second bday of life
plots_bday2_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_bday2_diag[[i]] <- ggplot(all_CMDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-24, 24, by = 2), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-24, 24, by=2))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_2y <- plot_grid(plots_bday2_diag[[1]],plots_bday2_diag[[2]],
                                  plots_bday2_diag[[3]], plots_bday2_diag[[4]],
                                  plots_bday2_diag[[5]], plots_bday2_diag[[6]],
                                  plots_bday2_diag[[7]],align = "h", 
                                  labels = c("A", "B", "C", "D", "E", "F", "G"),
                                  label_size = 10,
                                  ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_2y_CMD.pdf",
       hist_birth_timing_2y,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)



#making a graph just for the potential pregnancy time
plots_preg_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_preg_diag[[i]] <- ggplot(all_CMDs[[i]], aes(rel_time_diag_weeks)) +
    geom_histogram( breaks = seq(-60, 1, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 1, by=5))+
    xlab("Weeks before birth")+
    theme_classic()
}

hist_birth_timing_preg <- plot_grid(plots_preg_diag[[1]],plots_preg_diag[[2]],
                                    plots_preg_diag[[3]], plots_preg_diag[[4]],
                                    plots_preg_diag[[5]], plots_preg_diag[[6]],
                                    plots_preg_diag[[7]],align = "h", 
                                    labels = c("A", "B", "C", "D", "E", "F", "G"),
                                    label_size = 10,
                                    ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_preg_CMD.pdf",
       hist_birth_timing_preg,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)



####exploring the SUD data
SUD_linked1 <- merge(MBL_1, df_SUD,  by = "patid", all.x = T)
SUD_linked2 <- merge(MBL_2, df_SUD,  by = "patid", all.x = T)
SUD_linked3 <- merge(MBL_3, df_SUD,  by = "patid", all.x = T)
SUD_linked4 <- merge(MBL_4, df_SUD,  by = "patid", all.x = T)
SUD_linked5 <- merge(MBL_5, df_SUD,  by = "patid", all.x = T)
SUD_linked6 <- merge(MBL_6, df_SUD,  by = "patid", all.x = T)
SUD_linked7 <- merge(MBL_7, df_SUD,  by = "patid", all.x = T)


all_SUDs <- list(SUD_linked1, SUD_linked2, SUD_linked3, SUD_linked4, 
                 SUD_linked5, SUD_linked6, SUD_linked7)

for(i in 1:length(all_SUDs)){
  
  print(all(is.na(all_SUDs[[i]]$medcodeid)))
} # drop the last two items of list as there no diagnoses

all_SUDs <- list(SUD_linked1, SUD_linked2, SUD_linked3, SUD_linked4, 
                 SUD_linked5)

for(i in 1:length(all_SUDs)){
  
  all_SUDs[[i]][,  N_diagnoses :=  .N, by = "mumpatid"]
  all_SUDs[[i]]$deldate <-as.Date(all_SUDs[[i]]$deldate, format = "%d/%m/%Y")
  all_SUDs[[i]]$obsdate <-as.Date(all_SUDs[[i]]$obsdate, format = "%d/%m/%Y")
  all_SUDs[[i]][, rel_time_diag := interval(deldate, obsdate) %/% months(1)]
  all_SUDs[[i]][, rel_time_diag_weeks := interval(deldate, obsdate) %/% weeks(1)]
  
}

for(i in 1:length(all_SUDs)){
  
  print(max(all_SUDs[[i]]$N_diagnoses))
}

# [1] 1058
# [1] 557
# [1] 332
# [1] 11
# [1] 4


#exploring number of diagnosis per mother
plots_N_diag <- list()

for(i in 1:length(all_SUDs)){
  plots_N_diag[[i]] <- ggplot(all_SUDs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 700, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 700, by=50))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]], plots_N_diag[[4]],
                              plots_N_diag[[5]],align = "h", 
                              labels = c("A", "B", "C", "D", "E" ), label_size = 10,
                              ncol = 2, nrow = 3)

ggsave("N_diag_pP_SUD.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)




for(i in 1:length(all_SUDs)){
  plots_N_diag[[i]] <- ggplot(all_SUDs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 50, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 50, by=10))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]], plots_N_diag[[4]],
                              plots_N_diag[[5]],align = "h", 
                              labels = c("A", "B", "C", "D", "E"), label_size = 10,
                              ncol = 2, nrow = 3)

ggsave("N_diag_pP_SUD_2.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)


#number of diagnoses in relation to birth date adn5 years FU both sides
plots_bday5_diag <- list()

for(i in 1:length(all_SUDs)){
  plots_bday5_diag[[i]] <- ggplot(all_SUDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-60, 60, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 60, by=10))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_5y <- plot_grid(plots_bday5_diag[[1]],plots_bday5_diag[[2]],
                                  plots_bday5_diag[[3]], plots_bday5_diag[[4]],
                                  plots_bday5_diag[[5]],align = "h", 
                                  labels = c("A", "B", "C", "D", "E", ),
                                  label_size = 10,
                                  ncol = 2, nrow = 3)

ggsave("Diagnoses_rel_birth_5y_SUD.pdf",
       hist_birth_timing_5y,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)


#Calculating timing around the second bday of life
plots_bday2_diag <- list()

for(i in 1:length(all_SUDs)){
  plots_bday2_diag[[i]] <- ggplot(all_SUDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-24, 24, by = 2), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-24, 24, by=2))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_2y <- plot_grid(plots_bday2_diag[[1]],plots_bday2_diag[[2]],
                                  plots_bday2_diag[[3]], plots_bday2_diag[[4]],
                                  plots_bday2_diag[[5]],align = "h", 
                                  labels = c("A", "B", "C", "D", "E"),
                                  label_size = 10,
                                  ncol = 2, nrow = 3)

ggsave("Diagnoses_rel_birth_2y_SUD.pdf",
       hist_birth_timing_2y,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)


#Calculating timing around the first bday of life
plots_bday1_diag <- list()

for(i in 1:length(all_SUDs)){
  plots_bday1_diag[[i]] <- ggplot(all_SUDs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-12, 12, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-12, 12, by=1))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_1y <- plot_grid(plots_bday1_diag[[1]],plots_bday1_diag[[2]],
                                  plots_bday1_diag[[3]], plots_bday1_diag[[4]],
                                  plots_bday1_diag[[5]], align = "h", 
                                  labels = c("A", "B", "C", "D", "E"),
                                  label_size = 10,
                                  ncol = 2, nrow = 3)

ggsave("Diagnoses_rel_birth_1y_SUD.pdf",
       hist_birth_timing_1y,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)

###number of diagnoses in relationship to pregancy
plots_preg_diag <- list()

for(i in 1:length(all_SUDs)){
  plots_preg_diag[[i]] <- ggplot(all_SUDs[[i]], aes(rel_time_diag_weeks)) +
    geom_histogram( breaks = seq(-60, 1, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 1, by=5))+
    xlab("Weeks before birth")+
    theme_classic()
}

hist_birth_timing_preg <- plot_grid(plots_preg_diag[[1]],plots_preg_diag[[2]],
                                    plots_preg_diag[[3]], plots_preg_diag[[4]],
                                    plots_preg_diag[[5]], align = "h", 
                                    labels = c("A", "B", "C", "D", "E"),
                                    label_size = 10,
                                    ncol = 2, nrow = 3)

ggsave("Diagnoses_rel_birth_preg_SUD.pdf",
       hist_birth_timing_preg,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)



###exploring SMI
SMI_linked1 <- merge(MBL_1, df_SMI,  by = "patid", all.x = T)
SMI_linked2 <- merge(MBL_2, df_SMI,  by = "patid", all.x = T)
SMI_linked3 <- merge(MBL_3, df_SMI,  by = "patid", all.x = T)
SMI_linked4 <- merge(MBL_4, df_SMI,  by = "patid", all.x = T)
SMI_linked5 <- merge(MBL_5, df_SMI,  by = "patid", all.x = T)
SMI_linked6 <- merge(MBL_6, df_SMI,  by = "patid", all.x = T)
SMI_linked7 <- merge(MBL_7, df_SMI,  by = "patid", all.x = T)


all_SMIs <- list(SMI_linked1, SMI_linked2, SMI_linked3, SMI_linked4, 
                 SMI_linked5, SMI_linked6, SMI_linked7)

for(i in 1:length(all_SMIs)){
  
  print(all(is.na(all_SMIs[[i]]$medcodeid)))
} # drop the last four items of list as there no diagnoses

all_SMIs <- list(SMI_linked1, SMI_linked2, SMI_linked3)

for(i in 1:length(all_SMIs)){
  
  all_SMIs[[i]][,  N_diagnoses :=  .N, by = "mumpatid"]
  all_SMIs[[i]]$deldate <-as.Date(all_SMIs[[i]]$deldate, format = "%d/%m/%Y")
  all_SMIs[[i]]$obsdate <-as.Date(all_SMIs[[i]]$obsdate, format = "%d/%m/%Y")
  all_SMIs[[i]][, rel_time_diag := interval(deldate, obsdate) %/% months(1)]
  all_SMIs[[i]][, rel_time_diag_weeks := interval(deldate, obsdate) %/% weeks(1)]
  
}

for(i in 1:length(all_SMIs)){
  
  print(max(all_SMIs[[i]]$N_diagnoses))
}

# 1] 273
# [1] 160
# [1] 28

#exploring the number of diagnoses
plots_N_diag <- list()

for(i in 1:length(all_SMIs)){
  plots_N_diag[[i]] <- ggplot(all_SMIs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 700, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 700, by=50))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]],align = "h", 
                              labels = c("A", "B", "C"), label_size = 10,
                              ncol = 2, nrow = 2)

ggsave("N_diag_pP_SMI.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 6,
       bg = "white",
       path= graphfiles)




for(i in 1:length(all_SMIs)){
  plots_N_diag[[i]] <- ggplot(all_SMIs[[i]], aes(N_diagnoses)) +
    geom_histogram( breaks = seq(0, 30, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(0, 30, by=10))+
    xlab("Number of diagnoses per person")+
    theme_classic()
}

hist_N_diagnoses <- plot_grid(plots_N_diag[[1]],plots_N_diag[[2]],
                              plots_N_diag[[3]],align = "h", 
                              labels = c("A", "B", "C"), label_size = 10,
                              ncol = 2, nrow = 2)

ggsave("N_diag_pP_SMI_2.pdf",
       hist_N_diagnoses,
       width = 11,
       height = 9,
       bg = "white",
       path= graphfiles)




#number of diagnoses in relation to birth date adn5 years FU both sides
plots_bday5_diag <- list()

for(i in 1:length(all_SMIs)){
  plots_bday5_diag[[i]] <- ggplot(all_SMIs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-60, 60, by = 10), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 60, by=10))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_5y <- plot_grid(plots_bday5_diag[[1]],plots_bday5_diag[[2]],
                                  plots_bday5_diag[[3]], align = "h", 
                                  labels = c("A", "B", "C" ),
                                  label_size = 10,
                                  ncol = 2, nrow = 2)

ggsave("Diagnoses_rel_birth_5y_SMI.pdf",
       hist_birth_timing_5y,
       width = 11,
       height = 6,
       bg = "white",
       path= graphfiles)


#Calculating timing around the second bday of life
plots_bday2_diag <- list()

for(i in 1:length(all_SMIs)){
  plots_bday2_diag[[i]] <- ggplot(all_SMIs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-24, 24, by = 2), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-24, 24, by=2))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_2y <- plot_grid(plots_bday2_diag[[1]],plots_bday2_diag[[2]],
                                  plots_bday2_diag[[3]], align = "h", 
                                  labels = c("A", "B", "C"),
                                  label_size = 10,
                                  ncol = 2, nrow = 2)

ggsave("Diagnoses_rel_birth_2y_SMI.pdf",
       hist_birth_timing_2y,
       width = 11,
       height = 6,
       bg = "white",
       path= graphfiles)


#Calculating timing around the first bday of life
plots_bday1_diag <- list()

for(i in 1:length(all_SMIs)){
  plots_bday1_diag[[i]] <- ggplot(all_SMIs[[i]], aes(rel_time_diag)) +
    geom_histogram( breaks = seq(-12, 12, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-12, 12, by=1))+
    xlab("Months since birth")+
    theme_classic()
}

hist_birth_timing_1y <- plot_grid(plots_bday1_diag[[1]],plots_bday1_diag[[2]],
                                  plots_bday1_diag[[3]], align = "h", 
                                  labels = c("A", "B", "C"),
                                  label_size = 10,
                                  ncol = 2, nrow = 2)

ggsave("Diagnoses_rel_birth_1y_SMI.pdf",
       hist_birth_timing_1y,
       width = 11,
       height = 6,
       bg = "white",
       path= graphfiles)

###number of diagnoses in relationship to pregnancy
plots_preg_diag <- list()

for(i in 1:length(all_SMIs)){
  plots_preg_diag[[i]] <- ggplot(all_SMIs[[i]], aes(rel_time_diag_weeks)) +
    geom_histogram( breaks = seq(-60, 1, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 1, by=5))+
    xlab("Weeks before birth")+
    theme_classic()
}

hist_birth_timing_preg <- plot_grid(plots_preg_diag[[1]],plots_preg_diag[[2]],
                                    plots_preg_diag[[3]], align = "h", 
                                    labels = c("A", "B", "C"),
                                    label_size = 10,
                                    ncol = 2, nrow = 2)

ggsave("Diagnoses_rel_birth_preg_SMI.pdf",
       hist_birth_timing_preg,
       width = 11,
       height = 6,
       bg = "white",
       path= graphfiles)


###making the summary statistics for the different mental health disorders
length(unique(df_CMD$patid)) #268720
length(unique(df_SMI$patid)) #1204
length(unique(df_SUD$patid)) #28514

#describing the number of diagnses per women
df_CMD[,  N_diagnoses :=  .N, by = "patid"]
df_SUD[,  N_diagnoses :=  .N, by = "patid"]
df_SMI[,  N_diagnoses :=  .N, by = "patid"]

summary(unique(df_CMD[, list(patid, N_diagnoses)])$N_diagnoses)
summary(unique(df_SUD[, list(patid, N_diagnoses)])$N_diagnoses)
summary(unique(df_SMI[, list(patid, N_diagnoses)])$N_diagnoses)

#Defining the first diagnosis ever
for(i in 1:length(all_CMDs)){
  all_CMDs[[i]] <- all_CMDs[[i]][order(obsdate)]
  all_CMDs[[i]][, diag_order := seq_len(.N), by = "mumpatid"]
  all_CMDs[[i]]$mumbirthyear <- as.numeric(all_CMDs[[i]]$mumbirthyear)
  all_CMDs[[i]][, age_diag := year(obsdate)-mumbirthyear]
}

for(i in 1:length(all_SUDs)){
  all_SUDs[[i]] <- all_SUDs[[i]][order(obsdate)]
  all_SUDs[[i]][, diag_order := seq_len(.N), by = "mumpatid"]
  all_SUDs[[i]]$mumbirthyear <- as.numeric(all_SUDs[[i]]$mumbirthyear)
  all_SUDs[[i]][, age_diag := year(obsdate)-mumbirthyear]
}

for(i in 1:length(all_SMIs)){
  all_SMIs[[i]] <- all_SMIs[[i]][order(obsdate)]
  all_SMIs[[i]][, diag_order := seq_len(.N), by = "mumpatid"]
  all_SMIs[[i]]$mumbirthyear <- as.numeric(all_SMIs[[i]]$mumbirthyear)
  all_SMIs[[i]][, age_diag := year(obsdate)-mumbirthyear]
}



#making a graph just for the potential pregnancy time
plots_preg_diag <- list()

for(i in 1:length(all_CMDs)){
  plots_preg_diag[[i]] <- ggplot(all_CMDs[[i]][diag_order == 1], aes(rel_time_diag_weeks)) +
    geom_histogram( breaks = seq(-60, 1, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 1, by=5))+
    xlab("Weeks before birth")+
    theme_classic()
}

hist_birth_timing_preg <- plot_grid(plots_preg_diag[[1]],plots_preg_diag[[2]],
                                    plots_preg_diag[[3]], plots_preg_diag[[4]],
                                    plots_preg_diag[[5]], plots_preg_diag[[6]],
                                    plots_preg_diag[[7]],align = "h", 
                                    labels = c("A", "B", "C", "D", "E", "F", "G"),
                                    label_size = 10,
                                    ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_preg_CMD_firstever.pdf",
       hist_birth_timing_preg,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)

#timing of diagnosis by practice
all_practices <- unique(all_CMDs[[1]]$pracid)
seven_practices <- sample(all_practices, size = 7)


#making a graph just for the potential pregnancy time
plots_preg_diag <- list()

for(i in 1:length(seven_practices)){
  plots_preg_diag[[i]] <- ggplot(all_CMDs[[1]][pracid == seven_practices[[i]]], aes(rel_time_diag_weeks)) +
    geom_histogram( breaks = seq(-60, 1, by = 1), 
                    aes(fill = ..count..)) + 
    scale_x_continuous(breaks = seq(-60, 1, by=5))+
    xlab("Weeks before birth")+
    theme_classic()
}

hist_birth_timing_preg <- plot_grid(plots_preg_diag[[1]],plots_preg_diag[[2]],
                                    plots_preg_diag[[3]], plots_preg_diag[[4]],
                                    plots_preg_diag[[5]], plots_preg_diag[[6]],
                                    plots_preg_diag[[7]],align = "h", 
                                    labels = c("A", "B", "C", "D", "E", "F", "G"),
                                    label_size = 10,
                                    ncol = 2, nrow = 4)

ggsave("Diagnoses_rel_birth_preg_CMD_7practices_random3.pdf",
       hist_birth_timing_preg,
       width = 11,
       height = 12,
       bg = "white",
       path= graphfiles)



#summarising all diagnoses by practice
df_CMD[, Diag_per_prac := .N, by = pracid]
all_prac <- unique(df_CMD[, list(pracid, Diag_per_prac)])
summary(all_prac$Diag_per_prac)

df_SUD[, Diag_per_prac := .N, by = pracid]
all_prac <- unique(df_SUD[, list(pracid, Diag_per_prac)])
summary(all_prac$Diag_per_prac)

df_SMI[, Diag_per_prac := .N, by = pracid]
all_prac <- unique(df_SMI[, list(pracid, Diag_per_prac)])
summary(all_prac$Diag_per_prac)

#summarising by women
length(unique(df_CMD$patid)) #268720
length(unique(df_SUD$patid)) #28514
length(unique(df_SMI$patid)) #1204



###plotting the age of first diagnoses
plots_age_diag_CMD <- ggplot(all_CMDss[[1]], aes(age_diag)) +
  geom_histogram( breaks = seq(12, 50, by = 1), 
                  aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(12, 50, by=5))+
  xlab("Age at diagnosis")+
  theme_classic()

plots_age_diag_SUD <- ggplot(all_SUDs[[1]], aes(age_diag)) +
  geom_histogram( breaks = seq(12, 50, by = 1), 
                  aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(12, 50, by=5))+
  xlab("Age at diagnosis")+
  theme_classic()

plots_age_diag_SMI <- ggplot(all_SMIs[[1]], aes(age_diag)) +
  geom_histogram( breaks = seq(12, 50, by = 1), 
                  aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(12, 50, by=5))+
  xlab("Age at diagnosis")+
  theme_classic()

plots_age_first_diag <- plot_grid(plots_age_diag_CMD,
                                  plots_age_diag_SUD,
                                  plots_age_diag_SMI,
                                  align = "h",labels = c("A", "B", "C"),
                                  label_size = 10,
                                  ncol = 1, nrow = 3)



pat.files<- list.files(path = mothers_parquet, pattern = "\\Patient")
tmp <- read_parquet(pat.files[[1]])

df_CMD <- df_CMD[order(obsdate)]
df_CMD[,  diag_order :=  seq_len(.N), by = "patid"]
df_CMD <- df_CMD[diag_order == 1]
df_CMD[, age := years(obsdate)-yob]

#preparing the pregnancy register for joining
#looking up the pregnancy register
preg_reg <- data.table(read.delim(paste0(linked_data, 
                                         "pregnancy_register_22_001706_DM.txt")))
preg_reg$patid <- as.character(preg_reg$patid)
nrow(preg_reg) #2,159,980

#filter out only women with live-birth
preg_reg<- preg_reg[outcome == 1]
nrow(preg_reg) #1275490
#filtering approporiatae year of birth 2006-2014
preg_reg$pregend <- as.Date(preg_reg$pregend, format = "%d/%m/%Y")
preg_reg <- preg_reg[year(pregend) >= 2006 & year(pregend) <= 2014]
nrow(preg_reg) #932704
#number the child order by mother in this subset
preg_reg[,  child_order :=  seq_len(.N), by = "patid"]
max(preg_reg$child_order) #13




###----join all the datasets for different mental health issues
#stratified by child

for(i in 1:length(all_CMDs)){
  
  all_CMDs[[i]] <- all_CMDs[[i]][, diagnosis := "CMD"]
  
  if(length(all_SUDs) >= i){
    all_SUDs[[i]] <- all_SUDs[[i]][, diagnosis := "SUD"]
  }
  if(length(all_SMIs) >= i){
    all_SMIs[[i]] <- all_SMIs[[i]][, diagnosis := "SUD"]
  }
}


child_1 <- rbind(all_CMDs[[1]], all_SUDs[[1]], all_SMIs[[1]])
child_2 <- rbind(all_CMDs[[2]], all_SUDs[[2]], all_SMIs[[2]])
child_3 <- rbind(all_CMDs[[3]], all_SUDs[[3]], all_SMIs[[3]])
child_4 <- rbind(all_CMDs[[4]], all_SUDs[[4]])
child_5 <- rbind(all_CMDs[[5]], all_SUDs[[5]])
child_6 <- all_CMDs[[6]]
child_7 <- all_CMDs[[7]]


#---------SUMMARISING all CMD records in relation to pregnancy independent of childorder
CMD <- read_parquet(paste0(datafiles, "CMD_raw.parquet"))
pop <- read_parquet(paste0(datafiles, "studypop_all_exposures_cat.parquet"))
pop <- pop[, list(patid, babypatid, deldate)]

CMD <- merge(CMD, pop, by = c("patid", "babypatid"), all.x = T)
CMD[, rel_time_diag := interval(deldate, obsdate) %/% months(1)]

plot1 <- CMD %>% filter(!is.na(vacc1_CMD)) %>%
  ggplot(aes(rel_time_diag)) +
  geom_histogram(breaks = seq(-24, 24, by = 2), 
                 aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(-24, 24, by=2))+
  ggtitle("A. Number of maternal CMD codes in relation to child delivery")+
  xlab("Months since birth")+
  theme_classic()

#SUD
SUD <- read_parquet(paste0(datafiles, "SUD_raw.parquet"))
pop <- read_parquet(paste0(datafiles, "studypop_all_exposures_cat.parquet"))
pop <- pop[, list(patid, babypatid, deldate)]

SUD <- merge(SUD, pop, by = c("patid", "babypatid"), all.x = T)
SUD[, rel_time_diag := interval(deldate, obsdate) %/% months(1)]

plot2 <- SUD %>% filter(!is.na(vacc1_SUD)) %>%
  ggplot(aes(rel_time_diag)) +
  geom_histogram(breaks = seq(-24, 24, by = 2), 
                 aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(-24, 24, by=2))+
  xlab("Months since birth")+
  ggtitle("B. Number of maternal SUD codes in relation to child delivery")+
  theme_classic()


#SMI
SMI <- read_parquet(paste0(datafiles, "SMI_raw.parquet"))
pop <- read_parquet(paste0(datafiles, "studypop_all_exposures_cat.parquet"))
pop <- pop[, list(patid, babypatid, deldate)]

SMI <- merge(SMI, pop, by = c("patid", "babypatid"), all.x = T)
SMI[, rel_time_diag := interval(deldate, obsdate) %/% months(1)]

plot3 <- SMI %>% filter(!is.na(vacc1_SMI)) %>%
  ggplot(aes(rel_time_diag)) +
  geom_histogram(breaks = seq(-24, 24, by = 2), 
                 aes(fill = ..count..)) + 
  scale_x_continuous(breaks = seq(-24, 24, by=2))+
  xlab("Months since birth")+
  ggtitle("C. Number of maternal SMI codes in relation to child delivery")+
  theme_classic()


plot <- plot_grid(plot1, plot2, plot3,
                  ncol= 1, nrow = 3)


ggsave("Diagnoses_rel_birth_all.pdf",
       plot,
       width = 11,
       height = 20,
       bg = "white",
       path= graphfiles)
