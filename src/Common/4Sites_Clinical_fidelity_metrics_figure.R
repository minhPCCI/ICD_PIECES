# Run KM_PHHS_WIP_ver3.R first!!

library(gridExtra)
library(ggpubr)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
load("Inputs/A1c_tally.RData")
A1c_tally_PHHS <- A1c_tally %>% select(pat_id, arm, val, Period) %>% 
  filter(Period == "Baseline" | Period == "One year follow up") %>% 
  mutate(arm = ifelse(arm == "ICD - Pieces", "ICD-Pieces", "Standard of care")) %>% 
  mutate(Period = ifelse(Period == "One year follow up", "One year post-baseline", "Baseline"))
rm(A1c_tally)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
load("Inputs/A1c_tally.RData")

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth")
load("Inputs/A1c_tally.RData")
A1c_tally_ProHealth <- A1c_tally %>% select(pat_id = study_id, arm,val, Period)
rm(A1c_tally)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA")
load("Inputs/A1c_tally.RData")
A1c_tally_VA <- A1c_tally %>% select(pat_id = study_id, arm,val, Period)
rm(A1c_tally)

# A1c_PrePost <- A1c_tally[which(A1c_tally$Period == "Baseline" | A1c_tally$Period == "One year follow up"),]
A1c_PrePost <- rbind(A1c_tally_PHHS, A1c_tally_THR, A1c_tally_ProHealth, A1c_tally_VA) %>% na.omit()
A1c_PrePost$Period <- factor(A1c_PrePost$Period, levels =c("Baseline", "One year post-baseline"))

A1c_PrePost_Control <- A1c_PrePost %>% filter(arm == "Standard of care") %>%  group_by(Period) %>% dplyr::summarise(Average = round(mean(val), 1), SD = round(sd(val), 1)) %>% 
  mutate(Category = "HbA1c, Mean +/- SD (%)",
         Arm = "Standard of care",
         A1c = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, A1c) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'A1c')

A1c_PrePost_Pieces <- A1c_PrePost %>% filter(arm == "ICD-Pieces") %>%  group_by(Period) %>% dplyr::summarise(Average = round(mean(val), 1), SD = round(sd(val), 1)) %>% 
  mutate(Category = "HbA1c, Mean +/- SD (%)",
         Arm = "ICD-Pieces",
         A1c = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, A1c) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'A1c')



A1C_ICD_pre <-A1c_PrePost[A1c_PrePost$Period == "Baseline" & A1c_PrePost$arm == "ICD-Pieces",] 
A1C_ICD_post <-A1c_PrePost[A1c_PrePost$Period == "One year post-baseline" & A1c_PrePost$arm == "ICD-Pieces",] 
A1C_ICD_delta <- inner_join(x = A1C_ICD_pre %>% select(pat_id, val) %>% rename(pre_val = val), 
                            y = A1C_ICD_post %>% select(pat_id, val) %>% rename(post_val = val),
                            by = "pat_id") %>% mutate(delta = post_val - pre_val)%>%  na.omit()

A1C_Ctrl_pre <-A1c_PrePost[A1c_PrePost$Period == "Baseline" & A1c_PrePost$arm == "Standard of care",] 
A1C_Ctrl_post <-A1c_PrePost[A1c_PrePost$Period == "One year post-baseline" & A1c_PrePost$arm == "Standard of care",] 
A1C_Ctrl_delta <- inner_join(x = A1C_Ctrl_pre %>% select(pat_id, val) %>% rename(pre_val = val), 
                            y = A1C_Ctrl_post %>% select(pat_id, val) %>% rename(post_val = val),
                            by = "pat_id") %>% mutate(delta = post_val - pre_val) %>%  na.omit()

A1C_delta <- data.frame(Category = "Change in HbA1c, Mean +/- SD (%)", 
                        `Patient count` = format(nrow(A1C_ICD_delta) + nrow(A1C_Ctrl_delta), big.mark = ","),
                        `ICD-Pieces` = paste0(round(mean(A1C_ICD_delta$delta), 1), " +/- ", round(sd(A1C_ICD_delta$delta), 1)), 
                        `Standard of care` = paste0(round(mean(A1C_Ctrl_delta$delta), 1), " +/- ", round(sd(A1C_Ctrl_delta$delta), 1)),
                        `P-value` = t.test(A1C_ICD_delta$delta, A1C_Ctrl_delta$delta)$p.value)


# breaks = c(seq(6, 16, by = 3), 7.5)

# A1c_PrePost.sum <- 
pv_A1C_ICD <- t.test(x = A1c_PrePost[A1c_PrePost$arm == "ICD-Pieces" & A1c_PrePost$Period == "Baseline", "val"],
                     y = A1c_PrePost[A1c_PrePost$arm == "ICD-Pieces" & A1c_PrePost$Period == "One year post-baseline", "val"], 
                     alternative = "two.sided")$p.value
pv_A1C_Control <- t.test(x = A1c_PrePost[A1c_PrePost$arm == "Standard of care" & A1c_PrePost$Period == "Baseline", "val"],
                     y = A1c_PrePost[A1c_PrePost$arm == "Standard of care" & A1c_PrePost$Period == "One year post-baseline", "val"], 
                     alternative = "two.sided")$p.value
pv_A1C_Pre <- t.test(x = A1c_PrePost[A1c_PrePost$arm == "ICD-Pieces" & A1c_PrePost$Period == "Baseline", "val"],
                         y = A1c_PrePost[A1c_PrePost$arm == "Standard of care" & A1c_PrePost$Period == "Baseline", "val"], 
                         alternative = "two.sided")$p.value
pv_A1C_Post <- t.test(x = A1c_PrePost[A1c_PrePost$arm == "ICD-Pieces" & A1c_PrePost$Period == "One year post-baseline", "val"],
                     y = A1c_PrePost[A1c_PrePost$arm == "Standard of care" & A1c_PrePost$Period == "One year post-baseline", "val"], 
                     alternative = "two.sided")$p.value

plot1 <- ggplot(data = A1c_PrePost, aes(x = Period, y = val, fill = arm)) +
  geom_boxplot() +
  labs(x="Evaluation period", y = "HbA1c (%)") +
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(yintercept= 7.5, linetype = "HbA1c 7.5 %"), colour= 'red') +
  scale_linetype_manual(name ="Threshold", values = c('dashed'))  +
  scale_y_continuous(limits = c(4.5, 18))  + 
  theme_classic() +labs(fill = "Arm")

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common")
pdf(file = "Outputs/4Sites_ICDPieces_Clinical_fidelity_metrics_HbA1C.pdf",   # The directory you want to save the file in
    width = 5, # The width of the plot in inches
    height = 9) # The height of the plot in inches

grid.arrange(plot1, ncol=1)

dev.off()


########### Blood Pressure ########### 
setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
load("Inputs/BP_tally_PHHS.RData")
BP_tally_PHHS <- BP_tally_PHHS %>% select(pat_id, arm,sbp, dbp, Period) 

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
load("Inputs/BP_tally.RData")
head(BP_tally_THR)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth")
load("Inputs/BP_tally.RData")
BP_tally_ProHealth <- BP_tally %>% select(pat_id = study_id, arm,sbp, dbp, Period)
rm(BP_tally)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA")
load("Inputs/BP_tally.RData")
BP_tally_VA <- BP_tally %>% select(pat_id, arm, sbp = SBP, dbp = DBP, Period)
rm(BP_tally)


BP_PrePost <- rbind(BP_tally_PHHS, BP_tally_THR, BP_tally_ProHealth, BP_tally_VA) %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp)) %>% na.omit()

BP_PrePost$Period <- factor(BP_PrePost$Period, levels =c("Baseline", "One year post-baseline"))

BP_PrePost_sbp_Control <- BP_PrePost %>% filter(arm == "Standard of care") %>%  
  group_by(Period) %>% 
  dplyr::summarise(Average = round(mean(sbp), 1), 
                   SD = round(sd(sbp), 1)) %>% 
  mutate(Category = "Mean Systolic Blood Pressure +/- SD (mmHg)",
         Arm = "Standard of care",
         SBP = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, SBP) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'SBP')

BP_PrePost_sbp_Pieces <- BP_PrePost %>% filter(arm == "ICD-Pieces") %>%  
  group_by(Period) %>% 
  dplyr::summarise(Average = round(mean(sbp), 1), 
                   SD = round(sd(sbp), 1)) %>% 
  mutate(Category = "Mean Systolic Blood Pressure +/- SD (mmHg)",
         Arm = "ICD-Pieces",
         SBP = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, SBP) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'SBP')

BP_PrePost_dbp_Control <- BP_PrePost %>% filter(arm == "Standard of care") %>%  
  group_by(Period) %>% 
  dplyr::summarise(Average = round(mean(dbp), 1), 
                   SD = round(sd(dbp), 1)) %>% 
  mutate(Category = "Mean Diastolic Blood Pressure +/- SD (mmHg)",
         Arm = "Standard of care",
         DBP = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, DBP) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'DBP')

BP_PrePost_dbp_Pieces <- BP_PrePost %>% filter(arm == "ICD-Pieces") %>%  
  group_by(Period) %>% 
  dplyr::summarise(Average = round(mean(dbp), 1), 
                   SD = round(sd(dbp), 1)) %>% 
  mutate(Category = "Mean Diastolic Blood Pressure +/- SD (mmHg)",
         Arm = "ICD-Pieces",
         DBP = paste0(Average, " +/- ", SD)) %>% 
  select(Category, Arm, Period, DBP) %>% 
  reshape2::dcast(Category + Arm ~ Period, value.var = 'DBP')


bp_ICD_pre <-BP_PrePost[BP_PrePost$Period == "Baseline" & BP_PrePost$arm == "ICD-Pieces",] 
bp_ICD_post <-BP_PrePost[BP_PrePost$Period == "One year post-baseline" & BP_PrePost$arm == "ICD-Pieces",] 
bp_ICD_delta <- inner_join(x = bp_ICD_pre %>% select(pat_id, sbp, dbp) %>% rename(pre_sbp = sbp, pre_dbp = dbp), 
                            y = bp_ICD_post %>% select(pat_id, sbp, dbp) %>% rename(post_sbp = sbp, post_dbp = dbp),
                            by = "pat_id") %>% mutate(delta_sbp = post_sbp - pre_sbp, delta_dbp = post_dbp - pre_dbp)%>%  na.omit()

bp_Ctrl_pre <-BP_PrePost[BP_PrePost$Period == "Baseline" & BP_PrePost$arm == "Standard of care",] 
bp_Ctrl_post <-BP_PrePost[BP_PrePost$Period == "One year post-baseline" & BP_PrePost$arm == "Standard of care",] 
bp_Ctrl_delta <- inner_join(x = bp_Ctrl_pre %>% select(pat_id, sbp, dbp) %>% rename(pre_sbp = sbp, pre_dbp = dbp),
                            y = bp_Ctrl_post %>% select(pat_id, sbp, dbp) %>% rename(post_sbp = sbp, post_dbp = dbp),
                            by = "pat_id") %>% mutate(delta_sbp = post_sbp - pre_sbp, delta_dbp = post_dbp - pre_dbp)%>%  na.omit()

sbp_delta <- data.frame(Category = "Change in SBP, Mean +/- SD (mmHg)", 
                        `Patient count` = format(nrow(bp_ICD_delta) + nrow(bp_Ctrl_delta), big.mark = ","),
                        `ICD-Pieces` = paste0(round(mean(bp_ICD_delta$delta_sbp),1), " +/- ",  round(sd(bp_ICD_delta$delta_sbp),1)), 
                        `Standard of care` = paste0(round(mean(bp_Ctrl_delta$delta_sbp), 1), " +/- ", round(sd(bp_Ctrl_delta$delta_sbp), 1))   , 
                        `P-value` = t.test(bp_ICD_delta$delta_sbp, bp_Ctrl_delta$delta_sbp)$p.value)
dbp_delta <- data.frame(Category = "Change in DBP, Mean +/- SD (mmHg)", 
                        `Patient count` = format(nrow(bp_ICD_delta) + nrow(bp_Ctrl_delta), big.mark = ","),
                        `ICD-Pieces` =paste0( round(mean(bp_ICD_delta$delta_dbp), 1), " +/- ",  round(sd(bp_ICD_delta$delta_dbp), 1) ), 
                        `Standard of care` = paste0(round(mean(bp_Ctrl_delta$delta_dbp), 1), " +/- ", round(sd(bp_Ctrl_delta$delta_dbp),1)), 
                        `P-value` = t.test(bp_ICD_delta$delta_dbp, bp_Ctrl_delta$delta_dbp)$p.value)
# break2 = c(seq(0, 125, by = 25), 90)

plot2 <- ggplot(data = BP_PrePost, aes(x = Period, y = dbp, fill = arm)) +
  geom_boxplot() +
  labs(x="Evaluation period", y = "Diastolic Blood Pressure (mmHg)") +
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(yintercept= 90, linetype = "DBP 90 mmHg"), colour= 'red') +
  scale_linetype_manual(name ="Threshold", values = c('dashed'))  +
  scale_y_continuous(limits = c(0, 250))  +
  theme_classic() +labs(fill = "Arm")
  # theme(axis.text.x = element_text(angle = -30, hjust = 0))+
  # stat_compare_means(method = "t.test") + labs(fill = "Arm")#+  theme(legend.position = "none")

# break3 = c(seq(50, 225, by = 50), 140)

plot3 <- ggplot(data = BP_PrePost, aes(x = Period, y = sbp, fill = arm)) +
  geom_boxplot() +
  labs(x="Evaluation period", y = "Systolic Blood Pressure (mmHg)") +
  scale_color_brewer(palette="Dark2") +
  geom_hline(aes(yintercept= 140, linetype = "SBP 140 mmHg"), colour= 'red') +
  scale_linetype_manual(name ="Threshold", values = c('dashed'))  +
  scale_y_continuous(limits = c(0, 250))  +
  theme_classic()+labs(fill = "Arm")
  # theme(axis.text.x = element_text(angle = -30, hjust = 0))+
  # stat_compare_means(method = "t.test")+ labs(fill = "Arm")

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common")

pdf(file = "Outputs/4Sites_ICDPieces_Clinical_fidelity_metrics_BP.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 9) # The height of the plot in inches

grid.arrange(plot3, plot2, ncol=2)

dev.off()

delta <- rbind(A1C_delta,
               sbp_delta,
               dbp_delta)
colnames(delta) <- c("Category", "Patient count", "ICD-Pieces", "Standard of care","P-value")

Delta_table <- kable(delta,
      style = 'html',
      booktabs = T,
      caption = 'All sites - The comparison in changes in HbA1c, systolic blood pressure, and diastolic blood pressure after one year post-baseline between two arms.') %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F, html_font = 'Times New Roman') %>%
  row_spec(0, bold = T) %>% 
  column_spec(1, '3in') %>%
  column_spec(c(2, 3, 4, 5), extra_css = "text-align:center;") %>% 
  collapse_rows(1) %>%
  kable_styling(latex_options = "scale_down", font_size = 16) %>%
  row_spec(0, background = "lightskyblue")

Delta_table

############### ace
setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
load(file = 'Inputs/ace_tally_PHHS.RData') %>% as.data.frame()
ace_tally_PHHR <- ace_tally %>% 
  mutate(Period = ifelse(Period == "One Year follow up",  "One year post-baseline", "Baseline")) %>% 
  select(pat_id, arm, had_meds, Period)

rm(ace_tally)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
load(file = 'Inputs/ace_tally_THR.RData') %>% as.data.frame()
ace_tally_THR <- ace_tally_simp %>% mutate(arm = unfactor(arm)) %>% 
  select(pat_id, arm, had_meds, Period)

rm(ace_tally_simp)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth")
load(file = 'Inputs/ace_tally.RData') %>% as.data.frame()
ace_tally_ProHealth <- ace_tally_simp %>% mutate(pat_id = as.character(Study_id)) %>% 
  select(pat_id, arm, had_meds = had_med, Period)
rm(ace_tally_simp)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA")
load(file = 'Inputs/ace_tally.RData') %>% as.data.frame()
ace_tally_VA <- ace_tally_simp %>% 
  select(pat_id = Column1, arm = Arm, had_meds = ACE_ARB, Period)
rm(ace_tally_simp)

ace_tally <- dplyr::bind_rows(ace_tally_PHHR, ace_tally_THR, ace_tally_ProHealth, ace_tally_VA)
# ace_tally[which(ace_tally$Period == "One Year follow up"), "Period"] <- "One year post-baseline"
ace_tally$Period <- factor(ace_tally$Period, levels =c("Baseline", "One year post-baseline"))


# ace_tally_Pieces <- ace_tally %>% filter(arm == "ICD-Pieces") %>%  
#   group_by(Period) %>% 
#   dplyr::summarise(Average = round(mean(dbp), 1), 
#                    SD = round(sd(dbp), 1)) %>% 
#   mutate(Category = "Mean Diastolic  Blood Pressure +/- SD (mmHg)",
#          Arm = "ICD-Pieces",
#          DBP = paste0(Average, " +/- ", SD)) %>% 
#   select(Category, Arm, Period, DBP) %>% 
#   reshape2::dcast(Category + Arm ~ Period, value.var = 'DBP')


data = ace_tally %>%
  group_by(arm, Period, had_meds ) %>%
  tally %>%
  mutate(percent = round(n*100/sum(n),1)) %>% 
  filter(had_meds == "Yes")

ace_Control <- data %>% filter(arm == "Standard of care") %>%  
    mutate(Category = "ACEi/ARB e-prescribe, Count",
           ACEi = paste0(n, " (", percent, "%)")) %>%
    select(Category, Arm = arm, Period, ACEi) %>%
    reshape2::dcast(Category + Arm ~ Period, value.var = 'ACEi')

ace_Pieces <- data %>% filter(arm == "ICD-Pieces") %>%  
  mutate(Category = "ACEi/ARB e-prescribe, Count",
         ACEi = paste0(n, " (", percent, "%)")) %>%
  select(Category, Arm = arm, Period, ACEi) %>%
  reshape2::dcast(Category + Arm ~ Period, value.var = 'ACEi')

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common")

pdf(file = "Outputs/4Sites_ICDPieces_Clinical_fidelity_metrics_ACEi_v2.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 5) # The height of the plot in inches
ggplot(data = data ,
       aes(x = arm, y = percent, fill = arm)) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab('Arm') + ylab('Percentage of patients') +
  ggtitle('All sites - ACE Inhibitors active prescription (by arm)') +
  # scale_y_continuous(breaks = seq(0,4000,500))+
  facet_wrap(~ Period, nrow = 1) +
  labs(fill ='Arm') + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position="none") +
  geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 4, position = position_stack(vjust = 1 + 0.02))+
  geom_text(aes(x = arm, label = paste0("n = ", format(n, big.mark = ",") )),color = 'black',size = 4, position =  position_stack(vjust = 0.5)) 
dev.off()


ace_statin_table <- ace_tally %>% group_by(arm, Period, had_med) %>%
  dplyr::summarise(count = n()) %>%
  mutate(percent = round((count/sum(count))*100,2)) %>%
  mutate(Characteristics = 'Clinical Fidelity Metrics',
         Category = 'ACEI') %>% as.data.table() %>%
  data.table::dcast.data.table(Characteristics + Category + Period + had_med  ~ arm , value.var = c('percent', 'count')) %>% as.data.frame() %>%
  mutate_all( ~lapply(.,as.character))

#STATIN
setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
load("Inputs/statin_tally_PHHS.RData")
statin_tally_PHHS <- statin_tally %>% 
  mutate(Period = ifelse(Period == "One Year follow up",  "One year post-baseline", "Baseline"))
rm(statin_tally)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
load(file = 'Inputs/statin_tally_THR.RData') %>% as.data.frame()
statin_tally_THR <- statin_tally_simp %>% mutate(arm = unfactor(arm)) %>% 
  select(pat_id, arm, had_meds, Period)

rm(statin_tally_simp)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth")
load(file = 'Inputs/statin_tally.RData') %>% as.data.frame()
statin_tally_ProHealth <- statin_tally_simp %>% mutate(pat_id = as.character(Study_id)) %>% 
  select(pat_id, arm, had_meds = had_med, Period)
rm(statin_tally_simp)

setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA")
load(file = 'Inputs/statin_tally.RData') %>% as.data.frame()
statin_tally_VA <- statin_tally_simp %>% 
  select(pat_id = Column1, arm = Arm, had_meds = Statin, Period)
rm(statin_tally_simp)

statin_tally <- dplyr::bind_rows(statin_tally_PHHS, statin_tally_THR, statin_tally_ProHealth, statin_tally_VA)

# statin_tally[which(statin_tally$Period == "One Year follow up"), "Period"] <- "One year post-baseline"
statin_tally$Period <- factor(statin_tally$Period, levels =c("Baseline", "One year post-baseline"))
data = statin_tally %>%
  group_by(arm, Period, had_meds ) %>%
  tally %>%
  mutate(percent = round(n*100/sum(n),1)) %>% 
  filter(had_meds == "Yes")


statin_Control <- data %>% filter(arm == "Standard of care") %>%  
  mutate(Category = "Statin e-prescribe, Count",
         Statin = paste0(n, " (", percent, "%)")) %>%
  select(Category, Arm = arm, Period, Statin) %>%
  reshape2::dcast(Category + Arm ~ Period, value.var = 'Statin')

statin_Pieces <- data %>% filter(arm == "ICD-Pieces") %>%  
  mutate(Category = "Statin e-prescribe, Count",
         Statin = paste0(n, " (", percent, "%)")) %>%
  select(Category, Arm = arm, Period, Statin) %>%
  reshape2::dcast(Category + Arm ~ Period, value.var = 'Statin')


setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common")

pdf(file = "Outputs/4Sites_ICDPieces_Clinical_fidelity_metrics_Statin_v2.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 5) # The height of the plot in inches
ggplot(data ,
       aes(x = arm, y = percent, fill = arm)) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab('Arm') + ylab('Percentage of patients') +
  ggtitle('All Sites - Statin active prescription (by arm)') +
  # scale_y_continuous(breaks = seq(0,3000,500))+
  geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 4, position = position_stack(vjust = 1 + 0.02))+ 
  facet_wrap(~ Period, nrow = 1) +
  labs(fill ='Arm') + theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))  +
  theme(legend.position="none") +
  # geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 3, position = position_stack(vjust = 0.5))+ 
  geom_text(aes(x = arm, label = paste0("n = ", format(n, big.mark = ","))),color = 'black',size = 4, position = position_stack(vjust = 0.5)) 
dev.off()


#Table
sum <- rbind(A1c_PrePost_Control, A1c_PrePost_Pieces, 
             BP_PrePost_sbp_Control, BP_PrePost_sbp_Pieces, 
             BP_PrePost_dbp_Control, BP_PrePost_dbp_Pieces, 
             ace_Control, ace_Pieces, 
             statin_Control, statin_Pieces) %>% 
  kable(style = 'html',
        booktabs = T,
        col.names = c('Category','Arm','Baseline','One year post-baseline'),
        caption = '4 Sites - Fidelity metrics summary') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  collapse_rows(c(1)) %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  column_spec(3, '2in') %>%
  column_spec(2, '2in') %>%
  kable_styling(latex_options = "scale_down", font_size = 18) %>% 
  save_kable('/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/4Sites Fidelity metrics table.pdf')



A1c_PrePost.sum <- left_join(x = A1c_PrePost %>% na.omit() %>%
                               group_by(arm, Period, val > 7.5) %>% 
                               dplyr::summarise(countSub = n()) ,
                             y = A1c_PrePost %>% na.omit() %>%
                               group_by(arm, Period) %>% 
                               dplyr::summarise(total = n()),
                             by = c("arm", "Period")) %>% 
  mutate(percent = paste0(countSub, " (", round(countSub*100/total, 1), "%)")) %>% 
  filter(`val > 7.5` == TRUE) %>% 
  mutate(Characteristics = "HbA1c > 7.5") %>% 
  select(Characteristics, arm, Period, percent) %>% 
  reshape2::dcast(Characteristics ~ Period + arm, value.var = 'percent')

SBP_PrePost.sum <- left_join(x = BP_PrePost %>% na.omit() %>%
                               group_by(arm, Period, sbp > 140) %>% 
                               dplyr::summarise(countSub = n()) ,
                             y = BP_PrePost %>% na.omit() %>%
                               group_by(arm, Period) %>% 
                               dplyr::summarise(total = n()),
                             by = c("arm", "Period")) %>% 
  mutate(percent = paste0(countSub, " (", round(countSub*100/total, 1), "%)")) %>% 
  filter(`sbp > 140` == FALSE) %>% 
  mutate(Characteristics = "SBP < 140") %>% 
  select(Characteristics, arm, Period, percent) %>% 
  reshape2::dcast(Characteristics ~ Period + arm, value.var = 'percent')

DBP_PrePost.sum <- left_join(x = BP_PrePost %>% na.omit() %>%
                               group_by(arm, Period, dbp > 90) %>% 
                               dplyr::summarise(countSub = n()) ,
                             y = BP_PrePost %>% na.omit() %>%
                               group_by(arm, Period) %>% 
                               dplyr::summarise(total = n()),
                             by = c("arm", "Period")) %>% 
  mutate(percent = paste0(countSub, " (", round(countSub*100/total, 1), "%)")) %>% 
  filter(`dbp > 90` == FALSE) %>% 
  mutate(Characteristics = "DBP < 90") %>% 
  select(Characteristics, arm, Period, percent) %>% 
  reshape2::dcast(Characteristics ~ Period + arm, value.var = 'percent')
# c("Characteristics", "Standard care", "Standard care", "ICD-Pieces", "ICD-Pieces")

tb <- rbind(A1c_PrePost.sum, 
            SBP_PrePost.sum, 
            DBP_PrePost.sum)
colnames(tb) <- c("Characteristics", "Baseline<br>Standard care", "Baseline<br>ICD-Pieces",
                  "One year post-baseline<br>Standard care", "One year post-baseline<br>ICD-Pieces")

tb_arm <- kable(tb,
                  style = 'html', escape = F, align = "c",
                  caption = 'Percentage of patients in both arms at baseline and one year post-baseline having HbA1c > 7.5, SBP < 140, and DBP < 90.',
                  booktabs = T) %>%
  kable_classic_2(full_width = F,
                  html_font = 'Montserrat') %>%
  kable_styling(latex_options = "scale_down", font_size = 15) %>%
  row_spec(0, background = "lightskyblue") #%>% 
tb_arm
