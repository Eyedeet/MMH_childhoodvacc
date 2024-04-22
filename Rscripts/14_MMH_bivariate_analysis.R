#preformatting of dataset
df <- read_parquet(paste0(datafiles, "final_studypop_all_ind.parquet"))
colnames(df)




pop <- unique(df[, list(patid, babypatid, pracid, age_startfu_baby, age_endfu_baby, age_birth,
                         birthorder, n_children, ethnicity_5, region, imd, 
                         MHI_vacc1, MHI_vacc2, MHI_vacc3, flurisk)])
pop1 <- pop
####plotting uptake by MHI
results1 <- data.table(vaccine = rep("DTP", times= 25),
                       age = rep(c(1, 2, 3, 4, 5), times = 5),
                       dose = rep(c(1), times = 25),
                       MHI = c(rep("None", times = 5), rep("CMD", times = 5),
                               rep("CMD & SUD", times = 5), 
                               rep("SUD", times = 5),
                               rep("Any SMI", times = 5)),
                       coverage = rep(0, times = 25),
                       coverage_lb = rep(0, times = 25),
                       coverage_ub = rep(0, times = 25))

all_MHI <- c("None","CMD", "CMD & SUD", "SUD","Any SMI")

for(i in 1:length(all_MHI)){
  
  tmp <- df[MHI_vacc1 == all_MHI[i]]
  pop <- pop1[MHI_vacc1 == all_MHI[i]]
  results1[age == 1 & dose == 1 & MHI == all_MHI[i], coverage := rounding(coverage_1y(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 1 & dose == 1 & MHI == all_MHI[i], coverage_lb := rounding(coverage_1y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 1 & dose == 1 & MHI == all_MHI[i], coverage_ub := rounding(coverage_1y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  
  results1[age == 2 & dose == 1 & MHI == all_MHI[i], coverage := rounding(coverage_2y(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 2 & dose == 1 & MHI == all_MHI[i], coverage_lb := rounding(coverage_2y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 2 & dose == 1 & MHI == all_MHI[i], coverage_ub := rounding(coverage_2y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  
  results1[age == 3 & dose == 1 & MHI == all_MHI[i], coverage := rounding(coverage_3y(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 3 & dose == 1 & MHI == all_MHI[i], coverage_lb := rounding(coverage_3y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 3 & dose == 1 & MHI == all_MHI[i], coverage_ub := rounding(coverage_3y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  
  results1[age == 4 & dose == 1 & MHI == all_MHI[i], coverage := rounding(coverage_4y(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 4 & dose == 1 & MHI == all_MHI[i], coverage_lb := rounding(coverage_4y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 4 & dose == 1 & MHI == all_MHI[i], coverage_ub := rounding(coverage_4y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  
  results1[age == 5 & dose == 1 & MHI == all_MHI[i], coverage := rounding(coverage_5y(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 5 & dose == 1 & MHI == all_MHI[i], coverage_lb := rounding(coverage_5y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
  results1[age == 5 & dose == 1 & MHI == all_MHI[i], coverage_ub := rounding(coverage_5y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = pop))]
}


results1[, age:= factor(age, levels=  1:5, labels = c("1 year", "2 year", "3 year", "4 year", "5 year"))]
results1[, MHI := factor(MHI, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"), ordered = T)]


summary_MHI_vacc1 <- 
  results1 %>%
  ggplot(aes(x = MHI, y = coverage, fill = MHI))+
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin = coverage_lb, ymax = coverage_ub), width = 0.2,
                position = position_dodge(0.9))+
  #coord_cartesian(ylim = c(40,100))+
  scale_y_continuous(limits = c(0,100), breaks= seq(0,100, by = 10))+
  ggtitle("A. Uptake of the 1st DTP dose")+
  facet_wrap(~age, nrow=1)+

  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("uptake_vacc1_MMH.pdf",
       summary_MHI_vacc1,
       width = 12,
       height = 6,
       bg = "white",
       path= graphfiles)


#second vaccine, third PCV
results2 <- data.table(vaccine = rep("PCV", times= 25),
                       age = rep(c(1, 2, 3, 4, 5), times = 5),
                       dose = rep(c(3), times = 25),
                       MHI = c(rep("None", times = 5), rep("CMD", times = 5),
                               rep("CMD & SUD", times = 5), 
                               rep("SUD", times = 5),
                               rep("Any SMI", times = 5)),
                       coverage = rep(0, times = 25),
                       coverage_lb = rep(0, times = 25),
                       coverage_ub = rep(0, times = 25))

all_MHI <- c("None","CMD", "CMD & SUD", "SUD","Any SMI")

for(i in 1:length(all_MHI)){
  
  tmp <- df[MHI_vacc2 == all_MHI[i]]
  pop <- pop1[MHI_vacc2 == all_MHI[i]]
  results2[age == 1 & dose == 3 & MHI == all_MHI[i], coverage := rounding(coverage_1y(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 1 & dose == 3 & MHI == all_MHI[i], coverage_lb := rounding(coverage_1y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 1 & dose == 3 & MHI == all_MHI[i], coverage_ub := rounding(coverage_1y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  
  results2[age == 2 & dose == 3 & MHI == all_MHI[i], coverage := rounding(coverage_2y(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 2 & dose == 3 & MHI == all_MHI[i], coverage_lb := rounding(coverage_2y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 2 & dose == 3 & MHI == all_MHI[i], coverage_ub := rounding(coverage_2y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  
  results2[age == 3 & dose == 3 & MHI == all_MHI[i], coverage := rounding(coverage_3y(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 3 & dose == 3 & MHI == all_MHI[i], coverage_lb := rounding(coverage_3y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 3 & dose == 3 & MHI == all_MHI[i], coverage_ub := rounding(coverage_3y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  
  
  results2[age == 4 & dose == 3 & MHI == all_MHI[i], coverage := rounding(coverage_4y(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 4 & dose == 3 & MHI == all_MHI[i], coverage_lb := rounding(coverage_4y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 4 & dose == 3 & MHI == all_MHI[i], coverage_ub := rounding(coverage_4y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  
  results2[age == 5 & dose == 3 & MHI == all_MHI[i], coverage := rounding(coverage_5y(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 5 & dose == 3 & MHI == all_MHI[i], coverage_lb := rounding(coverage_5y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
  results2[age == 5 & dose == 3 & MHI == all_MHI[i], coverage_ub := rounding(coverage_5y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = pop))]
}


results2[, age:= factor(age, levels=  1:5, labels = c("1 year", "2 year", "3 year", "4 year", "5 year"))]

results2[, MHI := factor(MHI, levels = c("None","CMD", "CMD & SUD", "SUD","Any SMI"), ordered = T)]
summary_MHI_vacc2 <- 
  results2 %>%
  ggplot(aes(x = MHI, y = coverage, fill = MHI))+
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin = coverage_lb, ymax = coverage_ub), width = 0.2,
                position = position_dodge(0.9))+
  #coord_cartesian(ylim = c(40,100))+
  scale_y_continuous(limits = c(0,100), breaks= seq(0,100, by = 10))+
  facet_wrap(~age, nrow=1)+
  ggtitle("B. Uptake of the 3rd PCV dose")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("uptake_vacc2_MMH.pdf",
       summary_MHI_vacc2,
       width = 12,
       height = 6,
       bg = "white",
       path= graphfiles)

#third vaccine, second MMR
results3 <- data.table(vaccine = rep("MMR", times= 25),
                       age = rep(c(1, 2, 3, 4, 5), times = 5),
                       dose = rep(c(2), times = 25),
                       MHI = c(rep("None", times = 5), rep("CMD", times = 5),
                               rep("CMD & SUD", times = 5), 
                               rep("SUD", times = 5), 
                               rep("Any SMI",times = 5)),
                       coverage = rep(0, times = 25),
                       coverage_lb = rep(0, times = 25),
                       coverage_ub = rep(0, times = 25))

all_MHI <- c("None","CMD", "CMD & SUD", "SUD","Any SMI")

for(i in 1:length(all_MHI)){
  
  tmp <- df[MHI_vacc3 == all_MHI[i]]
  pop <- pop1[MHI_vacc3 == all_MHI[i]]
  results3[age == 1 & dose == 2 & MHI == all_MHI[i], coverage := rounding(coverage_1y(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 1 & dose == 2 & MHI == all_MHI[i], coverage_lb := rounding(coverage_1y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 1 & dose == 2 & MHI == all_MHI[i], coverage_ub := rounding(coverage_1y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  
  results3[age == 2 & dose == 2 & MHI == all_MHI[i], coverage := rounding(coverage_2y(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 2 & dose == 2 & MHI == all_MHI[i], coverage_lb := rounding(coverage_2y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 2 & dose == 2 & MHI == all_MHI[i], coverage_ub := rounding(coverage_2y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  
  results3[age == 3 & dose == 2 & MHI == all_MHI[i], coverage := rounding(coverage_3y(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 3 & dose == 2 & MHI == all_MHI[i], coverage_lb := rounding(coverage_3y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 3 & dose == 2 & MHI == all_MHI[i], coverage_ub := rounding(coverage_3y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  
  results3[age == 4 & dose == 2 & MHI == all_MHI[i], coverage := rounding(coverage_4y(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 4 & dose == 2 & MHI == all_MHI[i], coverage_lb := rounding(coverage_4y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 4 & dose == 2 & MHI == all_MHI[i], coverage_ub := rounding(coverage_4y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  
  results3[age == 5 & dose == 2 & MHI == all_MHI[i], coverage := rounding(coverage_5y(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 5 & dose == 2 & MHI == all_MHI[i], coverage_lb := rounding(coverage_5y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
  results3[age == 5 & dose == 2 & MHI == all_MHI[i], coverage_ub := rounding(coverage_5y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = pop))]
}


results3[, MHI := factor(MHI, levels = c("None","Any MHI", "CMD", "CMD & SUD", "SUD","Any SMI"), ordered = T)]
results3[, age:= factor(age, levels=  1:5, labels = c("1 year", "2 year", "3 year", "4 year", "5 year"))]


summary_MHI_vacc3 <- 
  results3 %>%
  ggplot(aes(x = MHI, y = coverage, fill = MHI))+
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin = coverage_lb, ymax = coverage_ub), width = 0.2,
                position = position_dodge(0.9))+
  #coord_cartesian(ylim = c(40,100))+
  scale_y_continuous(limits = c(0,100), breaks= seq(0,100, by = 10))+
  facet_wrap(~age, nrow=1)+
  ggtitle("C. Uptake of the 2nd MMR dose")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggsave("uptake_vacc3_MMH.pdf",
       summary_MHI_vacc3,
       width = 12,
       height = 6,
       bg = "white",
       path= graphfiles)


all_uptake <- summary_MHI_vacc1+summary_MHI_vacc2+summary_MHI_vacc3+plot_layout(nrow=3)

ggsave("uptake_all_by_MHI.pdf",
       all_uptake,
       width = 12,
       height = 12,
       bg = "white",
       path= graphfiles)

###############################################################################
#adding age of uptake plot
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot <- df[vaccine == "DTP" & (n_dose==1)]
DTP <- plot %>%
  ggplot(aes(x=age))+
  geom_histogram(color = cbbPalette[1], fill = cbbPalette[1], binwidth = 7) + 
  geom_vline(xintercept  = 56, linetype = "dashed")+
  scale_x_continuous(name="age (days)", limits=c(0, 2000), 
                     breaks = seq(from = 0, to = 2000, by = 250))+
  theme_classic()+
  ggtitle("A. Recording of the first DTP vaccine dose by age")+
  theme(plot.title = element_text(family = "Helvetica", face = "italic", size = 12, hjust = 0.5))

plot <- df[vaccine == "PCV" & n_dose == 3]
PCV <- plot %>%
  ggplot(aes(x=age))+
  geom_histogram(color = cbbPalette[3], fill = cbbPalette[3], binwidth = 7) + 
  geom_vline(xintercept  = 365, linetype = "dashed")+
  scale_x_continuous(name="age (days)", limits=c(0, 2000), 
                     breaks = seq(from = 0, to = 2000, by = 250))+
  theme_classic()+
  ggtitle("B. Recording of the third PCV vaccine dose by age")+
  theme(plot.title = element_text(family = "Helvetica", face = "italic", size = 12, hjust = 0.5))

plot <- df[vaccine == "MMR" & n_dose == 2]
MMR <- plot %>%
  ggplot(aes(x=age))+
  geom_histogram(color = cbbPalette[7], fill = cbbPalette[7], binwidth = 7) + 
  geom_vline(xintercept  = 1215, linetype = "dashed")+
  scale_x_continuous(name="age (days)", limits=c(0, 2000), 
                     breaks = seq(from = 0, to = 2000, by = 250))+
  theme_classic()+
  ggtitle("C. Recording of the second MMR vaccine dose by age")+
  theme(plot.title = element_text(family = "Helvetica", face = "italic", size = 12, hjust = 0.5))

all_vaccines <- DTP+PCV+MMR+plot_layout(nrow=3)

ggsave("age_uptake_all.pdf",
       all_vaccines,
       width = 7,
       height = 9,
       bg = "white",
       path= graphfiles)






###############################################################################
#Creating descriptive table for age of uptake

Q1 <- function(x){
  res <- quantile(x, probs = c(0,0.25,0.5,0.75,1))
  return(res[2])
}
Q3 <- function(x){
  res <- quantile(x, probs = c(0,0.25,0.5,0.75,1))
  return(res[4])
}

#first DTP dose
age_table <- data.table()
for(i in 1:length(all_MHI)){
tmp <- df[vaccine == "DTP" & n_dose == 1 & MHI_vacc1_ex == all_MHI[i] &
            age_startfu_baby <= 40, .(vaccine = "1st DTP",
                                                          MHI = all_MHI[i],
                                                           median_age = median(age),
                                                              quart_1 = Q1(age),
                                                              quart_3 = Q3(age))]
age_table <- rbind(age_table, tmp)
}

#Adding any MHI
tmp1 <- df[vaccine == "DTP" & n_dose == 1 & MHI_vacc1_any == "Any MHI" &
             age_startfu_baby <= 40, .(vaccine = "1st DTP",
                                                                          MHI = "Any MHI",
                                                                          median_age = median(age),
                                                                          quart_1 = Q1(age),
                                                                          quart_3 = Q3(age))]
age_table <- rbind(age_table, tmp1)

#third PCV dose
for(i in 1:length(all_MHI)){
  tmp <- df[vaccine == "PCV" & n_dose == 3 & MHI_vacc2_ex == all_MHI[i] &
              age_startfu_baby <= 40, .(vaccine = "3rd PCV",
                                                                           MHI = all_MHI[i],
                                                                           median_age = median(age),
                                                                           quart_1 = Q1(age),
                                                                           quart_3 = Q3(age))]
  age_table <- rbind(age_table, tmp)
}
#Adding any MHI
tmp2 <- df[vaccine == "PCV" & n_dose == 3 & MHI_vacc2_any == "Any MHI" & 
             age_startfu_baby <= 40, .(vaccine = "3rd PCV",
                                                                          MHI = "Any MHI",
                                                                          median_age = median(age),
                                                                          quart_1 = Q1(age),
                                                                          quart_3 = Q3(age))]
age_table <- rbind(age_table, tmp2)
#second MMR dose
for(i in 1:length(all_MHI)){
  tmp <- df[vaccine == "MMR" & n_dose == 2 & MHI_vacc3_ex == all_MHI[i] & 
              age_startfu_baby <= 40, .(vaccine = "2nd MMR",
                                                                           MHI = all_MHI[i],
                                                                           median_age = median(age),
                                                                           quart_1 = Q1(age),
                                                                           quart_3 = Q3(age))]
  age_table <- rbind(age_table, tmp)
}

#Adding any MHI
tmp3 <- df[vaccine == "MMR" & n_dose == 2 & MHI_vacc3_any == "Any MHI" & 
             age_startfu_baby <= 40, .(vaccine = "2nd MMR",
                                                                         MHI = "Any MHI",
                                                                         median_age = median(age),
                                                                         quart_1 = Q1(age),
                                                                         quart_3 = Q3(age))]

age_table <- rbind(age_table, tmp3)



###adding a plot of vaccine uptake by MHI
box_DTP1 <- df[vaccine == "DTP" & n_dose == 1 &  age_startfu_baby >=40] %>%
  ggplot(aes(x = MHI_vacc1_ex, y= age))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(name="Age of vaccination (days)", limits=c(0, 200), 
                     breaks = seq(from = 0, to = 200, by = 25))+
  ylab("Age of vaccination (days)")+
  xlab("Maternal mental illness")+
  ggtitle("Age of vaccination for first DTP dose")+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = 'white', color = 'grey'),
        plot.title = element_text(hjust = 0.5))    

ggsave("vac_age_DTP1_MMH.pdf",
       box_DTP1,
       width = 10,
       height = 6,
       bg = "white",
       path= graphfiles)


box_PCV3 <- df[vaccine == "PCV" & n_dose == 3 &  age_startfu_baby >=40] %>%
  ggplot(aes(x = MHI_vacc3_ex, y= age))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(name="Age of vaccination (days)", limits=c(300, 600), 
                     breaks = seq(from = 300, to = 600, by = 25))+
  ylab("Age of vaccination (days)")+
  xlab("Maternal mental illness")+
  ggtitle("Age of vaccination for third PCV dose")+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = 'white', color = 'grey'),
        plot.title = element_text(hjust = 0.5))  

ggsave("vac_age_PCV3_MMH.pdf",
       box_PCV3,
       width = 10,
       height = 6,
       bg = "white",
       path= graphfiles)


box_MMR2 <- df[vaccine == "MMR" & n_dose == 2 &  age_startfu_baby >=40] %>%
  ggplot(aes(x = MHI_vacc3_ex, y= age))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(name="Age of vaccination (days)", limits=c(1100, 1500), 
                     breaks = seq(from = 1100, to = 1500, by = 25))+
  ylab("Age of vaccination (days)")+
  xlab("Maternal mental illness")+
  ggtitle("Age of vaccination for second MMR dose")+
  theme(axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = 'white', color = 'grey'),
        plot.title = element_text(hjust = 0.5)) 

ggsave("vac_age_MMR2_MMH.pdf",
       box_MMR2,
       width = 10,
       height = 6,
       bg = "white",
       path= graphfiles)



age_table[, res := paste0(median_age, " (", round(quart_1, digits = 0), " - ",round(quart_3, digits = 0), ")")]
age_table <- age_table[, list(vaccine, MHI, res)]

write.table(age_table, paste0(results, "median_age_by_MHI.csv"), sep = "\t", row.names = T,
            dec = ".")


####making a coverage table
for(i in 1:length(all_MHI)){
  
  tmp <- df[MHI_vacc1_ex == all_MHI[i]]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov2y := rounding(coverage_2y(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov2y_lb := rounding(coverage_2y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov2y_ub := rounding(coverage_2y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov5y := rounding(coverage_5y(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov5y_lb := rounding(coverage_5y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  df[MHI_vacc1_ex == all_MHI[i] & vaccine == "DTP" & n_dose == 1, cov5y_ub := rounding(coverage_5y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
  
  
  tmp <- df[MHI_vacc2_ex == all_MHI[i]]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov2y := rounding(coverage_2y(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov2y_lb := rounding(coverage_2y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov2y_ub := rounding(coverage_2y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov5y := rounding(coverage_5y(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov5y_lb := rounding(coverage_5y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  df[MHI_vacc2_ex == all_MHI[i] & vaccine == "PCV" & n_dose == 3, cov5y_ub := rounding(coverage_5y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
  
  tmp <- df[MHI_vacc3_ex == all_MHI[i]]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov2y := rounding(coverage_2y(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov2y_lb := rounding(coverage_2y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov2y_ub := rounding(coverage_2y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov5y := rounding(coverage_5y(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov5y_lb := rounding(coverage_5y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
  df[MHI_vacc3_ex == all_MHI[i] & vaccine == "MMR" & n_dose == 2, cov5y_ub := rounding(coverage_5y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
}

all_MHI_uptake1 <- df[, list(MHI_vacc1_ex, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake1 <- unique(all_MHI_uptake1[vaccine == "DTP" & n_dose == 1])
all_MHI_uptake1[, MHI := MHI_vacc1_ex]
all_MHI_uptake1[, MHI_vacc1_ex := NULL]
all_MHI_uptake2 <- df[, list(MHI_vacc2_ex, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake2 <- unique(all_MHI_uptake2[vaccine == "PCV" & n_dose == 3])
all_MHI_uptake2[, MHI := MHI_vacc2_ex]
all_MHI_uptake2[, MHI_vacc2_ex := NULL]
all_MHI_uptake3 <- df[, list(MHI_vacc3_ex, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake3<- unique(all_MHI_uptake3[vaccine == "MMR" & n_dose == 2])
all_MHI_uptake3[, MHI := MHI_vacc3_ex]
all_MHI_uptake3[, MHI_vacc3_ex := NULL]

all_MHI_uptake <- rbind(all_MHI_uptake1, all_MHI_uptake2,  all_MHI_uptake3)

#adding any MHI
tmp <- df[MHI_vacc1_any == "Any MHI"]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov2y := rounding(coverage_2y(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov2y_lb := rounding(coverage_2y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov2y_ub := rounding(coverage_2y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov5y := rounding(coverage_5y(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov5y_lb := rounding(coverage_5y_lb(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
df[MHI_vacc1_any == "Any MHI" & vaccine == "DTP" & n_dose == 1, cov5y_ub := rounding(coverage_5y_ub(vacc = "DTP", dose = 1, df = tmp, df_p = tmp))]
tmp <- df[MHI_vacc2_any == "Any MHI"]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov2y := rounding(coverage_2y(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov2y_lb := rounding(coverage_2y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov2y_ub := rounding(coverage_2y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov5y := rounding(coverage_5y(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov5y_lb := rounding(coverage_5y_lb(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
df[MHI_vacc2_any == "Any MHI" & vaccine == "PCV" & n_dose == 3, cov5y_ub := rounding(coverage_5y_ub(vacc = "PCV", dose = 3, df = tmp, df_p = tmp))]
tmp <- df[MHI_vacc3_any == "Any MHI"]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov2y := rounding(coverage_2y(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov2y_lb := rounding(coverage_2y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov2y_ub := rounding(coverage_2y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov5y := rounding(coverage_5y(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov5y_lb := rounding(coverage_5y_lb(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]
df[MHI_vacc3_any == "Any MHI" & vaccine == "MMR" & n_dose == 2, cov5y_ub := rounding(coverage_5y_ub(vacc = "MMR", dose = 2, df = tmp, df_p = tmp))]

#narrowing down the data set
all_MHI_uptake1 <- df[, list(MHI_vacc1_any, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake1 <- unique(all_MHI_uptake1[vaccine == "DTP" & n_dose == 1])
all_MHI_uptake1[, MHI := MHI_vacc1_any]
all_MHI_uptake1[, MHI_vacc1_any := NULL]
all_MHI_uptake2 <- df[, list(MHI_vacc2_any, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake2 <- unique(all_MHI_uptake2[vaccine == "PCV" & n_dose == 3])
all_MHI_uptake2[, MHI := MHI_vacc2_any]
all_MHI_uptake2[, MHI_vacc2_any := NULL]
all_MHI_uptake3 <- df[, list(MHI_vacc3_any, vaccine, n_dose, cov2y, cov2y_lb, cov2y_ub, cov5y, cov5y_ub, cov5y_lb)]
all_MHI_uptake3<- unique(all_MHI_uptake3[vaccine == "MMR" & n_dose == 2])
all_MHI_uptake3[, MHI := MHI_vacc3_any]
all_MHI_uptake3[, MHI_vacc3_any := NULL]
all_MHI_uptake_add <- rbind(all_MHI_uptake1, all_MHI_uptake2,  all_MHI_uptake3)
all_MHI_uptake_add <- all_MHI_uptake_add[ MHI == "Any MHI"]

#joining the datasets
all_MHI_uptake <- rbind(all_MHI_uptake, all_MHI_uptake_add)

####plot them all together
res <- rbind(results1[vaccine == "DTP" & age == 1], 
             results2[vaccine == "PCV" & age == 2],
             results3[vaccine == "MMR" & age == 5])

res$vaccine <- factor(res$vaccine, levels = c("DTP", "PCV", "MMR"), 
                      labels = c("1st DTP at age 1", "3rd PCV at age 2", "2nd MMR at age 5"),
                      ordered = T)

summary_all_MHI <- 
  res %>%
  ggplot(aes(x = MHI, y = coverage, fill = MHI))+
  facet_wrap(~vaccine)+
  geom_bar(stat="identity", width = 0.8, position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin = coverage_lb, ymax = coverage_ub), width = 0.2,
                position = position_dodge(0.9))+
  coord_cartesian(ylim = c(80,100))+
  scale_y_continuous(limits = c(0,100), breaks= seq(0,100, by = 10))+
  ggtitle("Vaccine uptake by MHI")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave("Cov_outcome_by_MMH.pdf",
       summary_all_MHI,
       width = 10,
       height = 6,
       bg = "white",
       path= graphfiles)


write.table(res, paste0(results, "Outcomes_of_interest_coverage.csv"), sep = "\t", row.names = T,
            dec = ".")




all_MHI_uptake[, year2_uptake := paste0(cov2y, " (", cov2y_lb, " - ", cov2y_ub, ")")]
all_MHI_uptake[, year5_uptake := paste0(cov5y, " (", cov5y_lb, " - ", cov5y_ub, ")")]

all_MHI_uptake<- all_MHI_uptake[, list(vaccine, MHI, year2_uptake, year5_uptake)]

write.table(all_MHI_uptake, paste0(results, "coverage_age_by_MHI.csv"), sep = "\t", row.names = T,
            dec = ".")
