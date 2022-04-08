#Medical Chart review

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(htmltools)
library(pdftools)

setwd('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/')
load("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Masterdemos.RData")

phhs_cr <- read.csv(file = '~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Inputs/PHHS_Ten_Percent_completed_manual chart reviews_v2.csv',
                    stringsAsFactors = F)
thr_cr <-  read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Inputs/ICD Pieces Chartreview THR_Ten_Percent_Final Aug 2020_VS analysis_9_2_2020 .xlsm', sheet = 'THR_Ten_Percent')
names(thr_cr) <- names(phhs_cr)

phhs_cr <- phhs_cr %>% mutate(Values.eGFR..60.ml...min..0.1.2..2. = ifelse(Values.eGFR..60.ml...min..0.1.2..2. >2, ">2", Values.eGFR..60.ml...min..0.1.2..2.),
                              Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. = ifelse(Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. >2, ">2", Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2.),
                              Proteinuria..0.1.2..2. = ifelse(Proteinuria..0.1.2..2. >2, ">2", Proteinuria..0.1.2..2.)) %>% 
  mutate(Values.eGFR..60.ml...min..0.1.2..2. = as.character(Values.eGFR..60.ml...min..0.1.2..2.),
         Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. = as.character(Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2.),
         Proteinuria..0.1.2..2. = as.character(Proteinuria..0.1.2..2.))

cr_PHHS_THR <- rbind(phhs_cr, thr_cr) 
new_col_names <- c('pat_id',
                   'patient_mrn',
                   'pat_last_name',
                   'pat_first_name',
                   'birth_date',
                   'enroll_date',
                   'enroll_year',
                   'location',
                   'is_85',
                   'problem_list_htn',
                   'problem_list_dm',
                   'problem_list_ckd',
                   'egfr_60',
                   'albuminuria',
                   'proteinuria',
                   'glucose',
                   'hba1c_7.5_pre',
                   'hypoglycemic_agents',
                   'sbp_140_2_pre',
                   'dbp_90_2_pre',
                   'antihypertensive_agents',
                   'problem_list_post',
                   'htn_bp_goal_set_post',
                   'sbp_140_post',
                   'dbp_90_post',
                   'acei_arb_post',
                   'statin_post',
                   'dm_hba1c_goal_set_post',
                   'hba1c_7.5_post',
                   'htn_education_post',
                   'dm_education_post',
                   'ckd_education_post',
                   'lipid_education_post',
                   'immunization_post',
                   'pf_followup',
                   'pf_recommendation',
                   'pf_acceptance',
                   'unplanned_hospitalization_post',
                   'death',
                   'arm')
names(cr_PHHS_THR) <- new_col_names
cr_PHHS_THR <- as.data.frame(cr_PHHS_THR) %>% mutate(location = "PHHS + THR")
cr_PHHS_THR <- cr_PHHS_THR[,c("pat_id", "arm", "location", "pf_followup", "pf_recommendation", "pf_acceptance")]

#Add two hospital systems -- TODO
prohealth_cr1 <-  read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/Inputs/chartreviewsDEIDENTIFIED_v2.xlsm', sheet = 'Pro_Ten_Percent')
masterdemo_ProHealth = masterdemo_ProHealth %>% mutate(arm = ifelse(arm == "Standard of care", "CONTROL", "PIECES"))

prohealth_cr1 <- left_join(x = prohealth_cr1 %>% select(-ARM),
                           y = masterdemo_ProHealth %>% select(Study_id = pat_id, arm),
                           by = "Study_id") %>% 
  filter(!is.na(arm))

names(prohealth_cr1) <- c("pat_id", "enroll_date", "enroll_year", "location",
                          'is_85',
                          'problem_list_htn',
                          'problem_list_dm',
                          'problem_list_ckd',
                          'egfr_60',
                          'albuminuria',
                          'proteinuria',
                          'glucose',
                          'hba1c_7.5_pre',
                          'hypoglycemic_agents',
                          'sbp_140_2_pre',
                          'dbp_90_2_pre',
                          'antihypertensive_agents',
                          'problem_list_post',
                          'htn_bp_goal_set_post',
                          'sbp_140_post',
                          'dbp_90_post',
                          'acei_arb_post',
                          'statin_post',
                          'dm_hba1c_goal_set_post',
                          'hba1c_7.5_post',
                          'htn_education_post',
                          'dm_education_post',
                          'ckd_education_post',
                          'lipid_education_post',
                          'immunization_post',
                          'pf_DTP_identification',
                          'pf_recommendation',
                          'pf_acceptance',
                          'pf_followup',
                          'unplanned_hospitalization_post',
                          'death',
                          'notes',
                          'enrollment_issue',
                          'Acei_ABR',
                          'statin', "arm")

prohealth_cr1 <- prohealth_cr1 %>% 
  mutate(pf_followup = case_when(pf_followup == "1" | pf_followup == "2" ~ "Yes", 
                                 TRUE ~"No")) %>% 
  mutate(pf_DTP_identification = case_when(is.na(pf_DTP_identification) | pf_DTP_identification == "" | pf_DTP_identification == "None" | pf_DTP_identification == "none" ~"No",
                                           TRUE ~ "Yes")) %>% 
  mutate(pf_recommendation = case_when(is.na(pf_recommendation) | pf_recommendation == "None" ~ "None",
                                       TRUE ~ pf_recommendation))

#VA cr
VA_cr <- read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Inputs/VA_Ten_Percent_2_24_21_completed.xlsm', sheet = 'VA_Ten_Percent')

names(VA_cr) <- c("pat_id", "enroll_date", "enroll_year", "location",
                  'is_85',
                  'problem_list_htn',
                  'problem_list_dm',
                  'problem_list_ckd',
                  'egfr_60',
                  'albuminuria',
                  'proteinuria',
                  'glucose',
                  'hba1c_7.5_pre',
                  'hypoglycemic_agents',
                  'sbp_140_2_pre',
                  'dbp_90_2_pre',
                  'antihypertensive_agents',
                  'problem_list_post',
                  'htn_bp_goal_set_post',
                  'sbp_140_post',
                  'dbp_90_post',
                  'acei_arb_post',
                  'statin_post',
                  'dm_hba1c_goal_set_post',
                  'hba1c_7.5_post',
                  'htn_education_post',
                  'dm_education_post',
                  'ckd_education_post',
                  'lipid_education_post',
                  'immunization_post',
                  'pf_DTP_identification',
                  'pf_recommendation',
                  'pf_acceptance',
                  'pf_followup',
                  'unplanned_hospitalization_post',
                  'death',
                  'notes')

#data cleaning for VA_cr
VA_cr <- inner_join(x = masterdemo_VA %>% select(pat_id, arm), 
                    y = VA_cr,
                    by = "pat_id")
VA_cr <- VA_cr %>% 
  mutate(pf_followup = case_when(pf_followup == "1" | pf_followup == "2" | pf_followup == ">2" ~ "Yes", 
                                 TRUE ~"No")) %>% 
  mutate(pf_DTP_identification = case_when(pf_DTP_identification == "none"| pf_DTP_identification == "None" | is.na(pf_DTP_identification) ~ "No",
                                           TRUE ~ "Yes")) %>% 
  mutate(pf_recommendation = case_when(pf_recommendation == "none" | pf_recommendation == "" | is.na(pf_recommendation) ~ "None",
                                       TRUE ~ pf_recommendation)) %>% 
  mutate(arm = case_when(arm == "ICD-Pieces" ~ "PIECES",
                         TRUE ~ "CONTROL"))

colselect <- c("pat_id", "arm", "location", 'pf_DTP_identification', 'pf_recommendation', 'pf_acceptance', 'pf_followup')

cr_ProHealthVA <- rbind(prohealth_cr1[, colselect], VA_cr[, colselect])
cr_ProHealthVA <- as.data.frame(cr_ProHealthVA) %>% mutate(location = "ProHealth + VA")

#Care manager PHHS + THR
pf_followups <- cr_PHHS_THR %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  mutate(Characteristics = 'Registered nurse',
         Category = 'Follow ups (patient and provider contact)') %>% 
  reshape2::dcast(Characteristics + Category + pf_followup ~ arm , value.var = 'n') %>%
  filter(pf_followup == 'Yes') %>% 
  select(- pf_followup)


pf_ICD_recomm <- cr_PHHS_THR %>% 
  filter(pf_followup == 'Yes' & pf_recommendation != 'None') %>% 
  group_by( arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'Registered nurse',
         Category = 'Recommendations made') %>% 
  reshape2::dcast(Characteristics + Category ~  arm , value.var = 'n')

pf_ICD_recomm_acceptance <- cr_PHHS_THR %>% 
  filter(pf_followup == 'Yes' & pf_recommendation != 'None' & !pf_acceptance %in% c('None','Unknown')) %>% 
  group_by( arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'Registered nurse',
         Category = 'Recommendations accepted') %>% 
  reshape2::dcast(Characteristics + Category ~  arm , value.var = 'n')

patient_count_PHHS_THR <- cr_PHHS_THR %>% 
  group_by( arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'PHHS + THR',
         Category = '') %>% 
  reshape2::dcast(Characteristics + Category ~  arm, value.var = 'n') %>%
  mutate_all(as.character())

#ProHealth + VA

pf_DTP_identification_PrVA <- cr_ProHealthVA %>% 
  group_by(arm, pf_DTP_identification) %>% 
  tally() %>% 
  filter(pf_DTP_identification == 'Yes') %>% 
  mutate(Characteristics = 'Pharmacist',
         Category = 'Prescriptions reviews issues identified') %>% 
  reshape2::dcast(Characteristics + Category + pf_DTP_identification ~ arm , value.var = 'n') %>%
  select(- pf_DTP_identification)

pf_followups_PrVA <- cr_ProHealthVA %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  filter(pf_followup == 'Yes') %>% 
  mutate(Characteristics = 'Pharmacist',
         Category = 'Follow ups (pharmacists encounters)') %>% 
  reshape2::dcast(Characteristics + Category + pf_followup ~ arm , value.var = 'n') %>%
  select(- pf_followup)

pf_ICD_recomm_PrVA <- cr_ProHealthVA %>% 
  filter(pf_recommendation != 'None') %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'Pharmacist',
         Category = 'Recommendations made') %>% 
  reshape2::dcast(Characteristics + Category ~ arm , value.var = 'n')

pf_ICD_recomm_acceptance_PrVA <- cr_ProHealthVA %>% 
  filter(pf_recommendation != 'None' & !pf_acceptance %in% c('none', 'None','Unknown')) %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'Pharmacist',
         Category = 'Recommendations accepted') %>% 
  reshape2::dcast(Characteristics + Category ~ arm , value.var = 'n')

patient_count_PrVA <- cr_ProHealthVA %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Characteristics = 'ProHealth + VA',
         Category = '') %>% 
  reshape2::dcast(Characteristics + Category ~ arm, value.var = 'n') %>%
  mutate_all(as.character())


medical_chart_audit_PHHSTHR <- bind_rows(patient_count_PHHS_THR,
                                       pf_followups,
                                       pf_ICD_recomm,
                                       pf_ICD_recomm_acceptance,) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  
  # Follow the workflow, add two more hospital systems -- TODO  
  mutate(CONTROL_PER = ifelse(Characteristics == 'PHHS + THR',
                                   CONTROL,
                                   paste0(round((CONTROL/cr_PHHS_THR %>%
                                                   group_by( arm) %>%
                                                   tally() %>%
                                                   ungroup() %>%
                                                   filter(arm == 'CONTROL') %>%
                                                   select(n) %>% as.numeric()
                                                 )*100,0),'%')), #%>%
                                                   # first() %>%
                                                   # pull(n)
                                   # )*100,0),'%')),
         PIECES_PER = ifelse(Characteristics == 'PHHS + THR',
                                  PIECES,
                                  paste0(round((PIECES/cr_PHHS_THR %>%
                                                  group_by(arm) %>%
                                                  tally() %>%
                                                  ungroup()%>%
                                                  filter(arm == 'PIECES') %>%
                                                  select(n) %>%  as.numeric())*100,0),'%'))) 

medical_chart_audit_PrVA <- bind_rows(patient_count_PrVA,
                                      pf_followups_PrVA,
                                         pf_DTP_identification_PrVA,
                                         pf_ICD_recomm_PrVA,
                                         pf_ICD_recomm_acceptance_PrVA) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  
  # Follow the workflow, add two more hospital systems -- TODO  
  mutate(CONTROL_PER = ifelse(Characteristics == 'ProHealth + VA',
                              CONTROL,
                              paste0(round((CONTROL/cr_ProHealthVA %>%
                                              group_by( arm) %>%
                                              tally() %>%
                                              ungroup() %>%
                                              filter(arm == 'CONTROL') %>%
                                              select(n) %>% as.numeric()
                              )*100,0),'%')), #%>%
         # first() %>%
         # pull(n)
         # )*100,0),'%')),
         PIECES_PER = ifelse(Characteristics == 'ProHealth + VA',
                             PIECES,
                             paste0(round((PIECES/cr_ProHealthVA %>%
                                             group_by(arm) %>%
                                             tally() %>%
                                             ungroup()%>%
                                             filter(arm == 'PIECES') %>%
                                             select(n) %>%  as.numeric())*100,0),'%'))) 

PHHSTHR <- medical_chart_audit_PHHSTHR %>%
        # select(Characteristics, Category, contains('THR')) %>%
        filter(Characteristics != 'PHHS + THR') %>%
        mutate(CONTROL_NOT = medical_chart_audit_PHHSTHR %>%
                                  filter(Characteristics == 'PHHS + THR') %>%
                                  select(CONTROL) %>%
                                  first() %>% as.numeric()
                                  -
                                  CONTROL,
               PIECES_NOT  = medical_chart_audit_PHHSTHR %>%
                                  filter(Characteristics == 'PHHS + THR') %>%
                                  select(PIECES) %>%
                                  first() %>% as.numeric()
                                  - 
                                  PIECES)

PrVA <- medical_chart_audit_PrVA %>%
  # select(Characteristics, Category, contains('THR')) %>%
  filter(Characteristics != 'ProHealth + VA') %>%
  mutate(CONTROL_NOT = medical_chart_audit_PrVA %>%
           filter(Characteristics == 'ProHealth + VA') %>%
           select(CONTROL) %>%
           first() %>% as.numeric()
         -
           CONTROL,
         PIECES_NOT  = medical_chart_audit_PrVA %>%
           filter(Characteristics == 'ProHealth + VA') %>%
           select(PIECES) %>%
           first() %>% as.numeric()
         - 
           PIECES)


chisq_test   <- PHHSTHR %>% filter(CONTROL >= 5 & CONTROL_NOT >= 5 & PIECES >=5 &PIECES_NOT >= 5)
fishers_test <- PHHSTHR %>% filter(CONTROL < 5 | CONTROL_NOT < 5 | PIECES <5 |PIECES_NOT < 5)

options(scipen = 999) #Puts p-value to decimal format (non scientific)

# chisq_pval   <- chisq_test %>%
#                      rowwise() %>%
#                      mutate(p.val = chisq.test(matrix(c(CONTROL ,CONTROL_NOT, PIECES, PIECES_NOT), nrow = 2))$p.value)

fishers_pval <- fishers_test %>%
                     rowwise() %>%
                     mutate(p.val = fisher.test(matrix(c(CONTROL ,CONTROL_NOT, PIECES, PIECES_NOT), nrow = 2))$p.value)

options(scipen = 0)
# merged_PHHSTHR           <- rbind(chisq_pval,fishers_pval) %>% select(Characteristics,Category,CONTROL_PER,PIECES_PER,p.val) %>%
merged_PHHSTHR <- fishers_pval %>% select(Characteristics,Category,CONTROL_PER,PIECES_PER,p.val) %>%
  select(Characteristics,
         Category,
         p.val)

medical_chart_audit_with_pval_PHHSTHR <- left_join( x = medical_chart_audit_PHHSTHR,
                                            y = merged_PHHSTHR,
                                            by = c('Characteristics'  = 'Characteristics', 'Category' = 'Category')) %>% 
  mutate(PIECES = ifelse(Characteristics == "PHHS + THR", PIECES, paste0(PIECES, " (", PIECES_PER, ")")),
         CONTROL = ifelse(Characteristics == "PHHS + THR", CONTROL, paste0(CONTROL, " (", CONTROL_PER, ")")),
         p.val = round(p.val, 2)) %>% 
  select(`Site/Practice Facilitator` = Characteristics, `ICD-Pieces Activities` = Category, `ICD-Pieces` = PIECES, `Standard of care` = CONTROL,p.val )

chisq_test   <- PrVA %>% filter(CONTROL >= 5 & CONTROL_NOT >= 5 & PIECES >=5 &PIECES_NOT >= 5)
fishers_test <- PrVA %>% filter(CONTROL < 5 | CONTROL_NOT < 5 | PIECES <5 |PIECES_NOT < 5)

options(scipen = 999) #Puts p-value to decimal format (non scientific)

chisq_pval   <- chisq_test %>%
  rowwise() %>%
  mutate(p.val = chisq.test(matrix(c(CONTROL ,CONTROL_NOT, PIECES, PIECES_NOT), nrow = 2))$p.value)

fishers_pval <- fishers_test %>%
  rowwise() %>%
  mutate(p.val = fisher.test(matrix(c(CONTROL ,CONTROL_NOT, PIECES, PIECES_NOT), nrow = 2))$p.value)

options(scipen = 0)

merged_PrVA <- rbind(chisq_pval,fishers_pval) %>% select(Characteristics,Category,CONTROL_PER,PIECES_PER,p.val) %>%
  select(Characteristics,
         Category,
         p.val)

medical_chart_audit_with_pval_PrVA <- left_join( x = medical_chart_audit_PrVA,
                                                    y = merged_PrVA,
                                                    by = c('Characteristics'  = 'Characteristics', 'Category' = 'Category'))  %>% 
  mutate(PIECES = ifelse(Characteristics == "ProHealth + VA", PIECES, paste0(PIECES, " (", PIECES_PER, ")")),
         CONTROL = ifelse(Characteristics == "ProHealth + VA", CONTROL, paste0(CONTROL, " (", CONTROL_PER, ")")),
         p.val = round(p.val, 2)) %>% 
  select(`Site/Practice Facilitator` = Characteristics, `ICD-Pieces Activities` = Category, `ICD-Pieces` = PIECES, `Standard of care` = CONTROL, p.val )


combined_cr <- rbind(medical_chart_audit_with_pval_PHHSTHR,
                     medical_chart_audit_with_pval_PrVA) %>% 
  mutate(`p.val` = ifelse(`p.val` < 0.001, "<0.01", `p.val`)) %>% 
  select(-p.val)
# Broken to smaller tables

# DoubleLine <- c(4)

#Manual chart review table by location and arms
mcr_arm <- kable(combined_cr ,
                 style = 'html',
                 booktabs = T,
                 col.names = c('Site/Practice Facilitator','ICD-Pieces Activities','ICD-Pieces','Standard of care'),
                 caption = '4 Sites - Medical chart review for care manager outlining the proportion of subjects that received each ICD-Pieces intervention') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  # column_spec(1, '2in') %>%
  # column_spec(5, '1in') %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = "scale_down", font_size = 18)

mcr_arm

save_kable(mcr_arm, 'Outputs/4Sites Medical Chart Review (by arm) _ Care manager_v6.pdf')



######## Appendix

load("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Masterdemos.RData")


# ProHealth
prohealth_cr1 <-  read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/Inputs/chartreviewsDEIDENTIFIED_v2.xlsm', sheet = 'Pro_Ten_Percent')
masterdemo_ProHealth = masterdemo_ProHealth %>% mutate(arm = ifelse(arm == "Standard of care", "CONTROL", "PIECES"))

prohealth_cr1 <- left_join(x = prohealth_cr1 %>% select(-ARM),
                           y = masterdemo_ProHealth %>% select(Study_id = pat_id, arm),
                           by = "Study_id") %>% 
  filter(!is.na(arm))

names(prohealth_cr1) <- c("pat_id", "enroll_date", "enroll_year", "location",
                          'is_85',
                          'problem_list_htn',
                          'problem_list_dm',
                          'problem_list_ckd',
                          'egfr_60',
                          'albuminuria',
                          'proteinuria',
                          'glucose',
                          'hba1c_7.5_pre',
                          'hypoglycemic_agents',
                          'sbp_140_2_pre',
                          'dbp_90_2_pre',
                          'antihypertensive_agents',
                          'problem_list_post',
                          'htn_bp_goal_set_post',
                          'sbp_140_post',
                          'dbp_90_post',
                          'acei_arb_post',
                          'statin_post',
                          'dm_hba1c_goal_set_post',
                          'hba1c_7.5_post',
                          'htn_education_post',
                          'dm_education_post',
                          'ckd_education_post',
                          'lipid_education_post',
                          'immunization_post',
                          'pf_DTP_identification',
                          'pf_recommendation',
                          'pf_acceptance',
                          'pf_followup',
                          'unplanned_hospitalization_post',
                          'death',
                          'notes',
                          'enrollment_issue',
                          'Acei_ABR',
                          'statin', "arm")

VA_cr <- read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Inputs/VA_Ten_Percent_2_24_21_completed.xlsm', sheet = 'VA_Ten_Percent')

names(VA_cr) <- c("pat_id", "enroll_date", "enroll_year", "location",
                  'is_85',
                  'problem_list_htn',
                  'problem_list_dm',
                  'problem_list_ckd',
                  'egfr_60',
                  'albuminuria',
                  'proteinuria',
                  'glucose',
                  'hba1c_7.5_pre',
                  'hypoglycemic_agents',
                  'sbp_140_2_pre',
                  'dbp_90_2_pre',
                  'antihypertensive_agents',
                  'problem_list_post',
                  'htn_bp_goal_set_post',
                  'sbp_140_post',
                  'dbp_90_post',
                  'acei_arb_post',
                  'statin_post',
                  'dm_hba1c_goal_set_post',
                  'hba1c_7.5_post',
                  'htn_education_post',
                  'dm_education_post',
                  'ckd_education_post',
                  'lipid_education_post',
                  'immunization_post',
                  'pf_DTP_identification',
                  'pf_recommendation',
                  'pf_acceptance',
                  'pf_followup',
                  'unplanned_hospitalization_post',
                  'death',
                  'notes')

#data cleaning for VA_cr
masterdemo_VA <- masterdemo_VA %>% mutate(arm = ifelse(arm == "Standard of care", "CONTROL", "PIECES"))
VA_cr <- inner_join(x = masterdemo_VA %>% select(pat_id, arm), 
                    y = VA_cr ,
                    by = "pat_id")

pf_DTP_identification_Pr <- prohealth_cr1 %>% 
  mutate(pf_DTP_identification = ifelse(grepl(",", pf_DTP_identification), "Combination", pf_DTP_identification)) %>% 
  mutate(pf_DTP_identification = case_when(pf_DTP_identification == "none" | is.na(pf_DTP_identification) ~ "None",
                                           pf_DTP_identification == "safety" ~ "Safety",
                                           pf_DTP_identification == "efficacy" ~ "Efficacy",
                                           pf_DTP_identification == "safety" ~ "Safety",
                                           pf_DTP_identification == "indication" ~ "Indication",
                                           TRUE ~ pf_DTP_identification)) %>% 
  mutate(pf_DTP_identification = factor(pf_DTP_identification, 
                                        levels = c("None", "Indication", "Efficacy", "Compliance", "Safety", "Combination"))) %>% 
  group_by(arm, pf_DTP_identification) %>% 
  tally() %>% 
  mutate(Site = 'ProHealth',
         `ICD-Pieces Activities` = 'Prescriptions reviews issues identified') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_DTP_identification ~ arm , value.var = 'n') %>%
  rename(Category = pf_DTP_identification)

pf_DTP_identification_VA <- VA_cr %>% 
  mutate(pf_DTP_identification = ifelse(grepl(",", pf_DTP_identification), "Combination", pf_DTP_identification)) %>% 
  mutate(pf_DTP_identification = case_when(pf_DTP_identification == "none" | is.na(pf_DTP_identification) ~ "None",
                                           pf_DTP_identification == "safety" ~ "Safety",
                                           pf_DTP_identification == "efficacy" ~ "Efficacy",
                                           pf_DTP_identification == "safety" ~ "Safety",
                                           pf_DTP_identification == "indication" ~ "Indication",
                                           TRUE ~ pf_DTP_identification)) %>% 
  mutate(pf_DTP_identification = factor(pf_DTP_identification, 
                                        levels = c("None", "Indication", "Efficacy", "Compliance", "Safety", "Combination"))) %>% 
  
  group_by(arm, pf_DTP_identification) %>% 
  tally() %>% 
  mutate(Site = 'VA',
         `ICD-Pieces Activities` = 'Prescriptions reviews issues identified') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_DTP_identification ~ arm , value.var = 'n') %>%
  rename(Category = pf_DTP_identification)

pf_followups_Pr <- prohealth_cr1 %>% 
  mutate(pf_followup = case_when(pf_followup == "None" | is.na(pf_followup) ~ "0",
                                 TRUE ~ pf_followup)) %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  mutate(Site = 'ProHealth',
         `ICD-Pieces Activities` = 'Number of pharmacist encounters during follow ups') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_followup ~ arm , value.var = 'n') %>%
  rename(Category = pf_followup)
  
pf_followups_VA <- VA_cr %>% 
  mutate(pf_followup = case_when(pf_followup == "None" | is.na(pf_followup) ~ "0",
                                 TRUE ~ pf_followup)) %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  mutate(Site = 'VA',
         `ICD-Pieces Activities` = 'Number of pharmacist encounters during follow ups') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_followup ~ arm , value.var = 'n') %>%
  rename(Category = pf_followup)

pf_ICD_recomm_Pr <- prohealth_cr1 %>% 
  mutate(pf_recommendation = case_when(pf_recommendation == "none" | pf_recommendation == "None" | is.na(pf_recommendation) ~ "None",
                                       pf_recommendation == "efficacy" ~ "Efficacy",
                                       pf_recommendation == "indication" ~ "Indication",
                                       TRUE ~ pf_recommendation)) %>% 
  mutate(pf_recommendation = factor(pf_recommendation, levels = c("None", "Indication", "Efficacy", "Compliance", "Safety", "Combination"))) %>% 
  group_by(arm, pf_recommendation) %>% 
  tally() %>% 
  mutate(Site = 'ProHealth',
         `ICD-Pieces Activities` = 'Recommendations made category') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_recommendation ~ arm , value.var = 'n') %>%
  rename(Category = pf_recommendation)

pf_ICD_recomm_VA <- VA_cr %>% 
  mutate(pf_recommendation = ifelse(grepl(",", pf_recommendation), "Combination", pf_recommendation)) %>% 
  mutate(pf_recommendation = case_when(pf_recommendation == "none" | pf_recommendation == "" | is.na(pf_recommendation) ~ "None",
                                       pf_recommendation == "efficacy" ~ "Efficacy",
                                       pf_recommendation == "indication" ~ "Indication",
                                       TRUE ~ pf_recommendation)) %>% 
  mutate(pf_recommendation = factor(pf_recommendation, levels = c("None", "Indication", "Efficacy", "Specialist Consultation", "Combination"))) %>% 
  group_by(arm, pf_recommendation) %>% 
  tally() %>% 
  mutate(Site = 'VA',
         `ICD-Pieces Activities` = 'Recommendations made category') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_recommendation ~ arm , value.var = 'n') %>%
  rename(Category = pf_recommendation)


pf_ICD_recomm_acceptance_Pr <- prohealth_cr1 %>% 
  mutate(pf_acceptance = case_when(pf_acceptance == "none" | pf_acceptance == "None" | is.na(pf_acceptance) ~ "No recommendation/acceptance",
                                   pf_acceptance == "efficacy" ~ "Efficacy",
                                   TRUE ~ pf_acceptance)) %>% 
  mutate(pf_acceptance = factor(pf_acceptance, levels = c("No recommendation/acceptance", "Indication", "Efficacy", "Compliance", "Safety", "Combination"))) %>% 
  group_by(arm, pf_acceptance) %>% 
  tally() %>% 
  mutate(Site = 'ProHealth',
         `ICD-Pieces Activities` = 'Recommendations accepted') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_acceptance ~ arm , value.var = 'n') %>%
  rename(Category = pf_acceptance)


pf_ICD_recomm_acceptance_VA <- VA_cr %>% 
  mutate(pf_acceptance = ifelse(grepl(",", pf_acceptance), "Combination", pf_acceptance)) %>%
  mutate(pf_acceptance = ifelse(pf_acceptance == "None", "No recommendation/acceptance", pf_acceptance)) %>% 
  mutate(pf_acceptance = factor(pf_acceptance, levels = c("No recommendation/acceptance", "Indication", "Efficacy", "Compliance", "Safety", "Combination"))) %>% 
  filter(pf_followup != 'None' & !is.na(pf_followup)) %>% 
  group_by(arm, pf_acceptance) %>% 
  tally() %>% 
  mutate(Site = 'VA',
         `ICD-Pieces Activities` = 'Recommendations accepted') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_acceptance ~ arm , value.var = 'n') %>%
  rename(Category = pf_acceptance)

patient_count_Pr <- prohealth_cr1 %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Site = 'ProHealth',
         `ICD-Pieces Activities` = '',
         Category = 'Patient count') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + Category ~ arm, value.var = 'n') %>%
  mutate_all(as.character())

patient_count_VA <- VA_cr %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Site = 'VA',
         `ICD-Pieces Activities` = '',
         Category = 'Patient count') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + Category ~ arm, value.var = 'n') %>%
  mutate_all(as.character())


medical_chart_audit_Pr <- bind_rows(patient_count_Pr,
                                      pf_followups_Pr,
                                      pf_DTP_identification_Pr,
                                      pf_ICD_recomm_Pr,
                                      pf_ICD_recomm_acceptance_Pr) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(CONTROL_PER = ifelse(Category == 'Patient count',
                              CONTROL,
                              paste0(round((CONTROL/patient_count_Pr$CONTROL)*100,0),'%')), 
         PIECES_PER = ifelse(Category == 'Patient count',
                             PIECES,
                             paste0(round((PIECES/patient_count_Pr$PIECES)*100,0),'%'))) %>% 
  mutate(`ICD-Pieces` = ifelse(Category == 'Patient count',
                               PIECES,
                               paste0(PIECES, " (", PIECES_PER, ")")),
         `Standard of care` = ifelse(Category == 'Patient count',
                                     CONTROL,
                                     paste0(CONTROL, " (", CONTROL_PER, ")")))%>% 
  select(Site, `ICD-Pieces Activities`, Category, `ICD-Pieces`, `Standard of care`)

mcr_Pr <- kable(medical_chart_audit_Pr ,
                 style = 'html',
                 booktabs = T,
                 col.names = c('Site', 'ICD-Pieces Activities', 'Category', 'ICD-Pieces', 'Standard of care'),
                 caption = 'ProHealth') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  # column_spec(1, '2in') %>%
  column_spec(2, '3in') %>%
  collapse_rows(c(1, 2)) %>%
  kable_styling(latex_options = "scale_down", font_size = 18)

mcr_Pr
save_kable(mcr_Pr, 'Outputs/ProHealth_Care manager.pdf')

medical_chart_audit_VA <- bind_rows(patient_count_VA,
                                    pf_followups_VA,
                                    pf_DTP_identification_VA,
                                    pf_ICD_recomm_VA,
                                    pf_ICD_recomm_acceptance_VA) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(CONTROL_PER = ifelse(Category == 'Patient count',
                              CONTROL,
                              paste0(round((CONTROL/patient_count_VA$CONTROL)*100,0),'%')), 
         PIECES_PER = ifelse(Category == 'Patient count',
                             PIECES,
                             paste0(round((PIECES/patient_count_VA$PIECES)*100,0),'%'))) %>% 
  mutate(`ICD-Pieces` = ifelse(Category == 'Patient count',
                               PIECES,
                               paste0(PIECES, " (", PIECES_PER, ")")),
         `Standard of care` = ifelse(Category == 'Patient count',
                                     CONTROL,
                                     paste0(CONTROL, " (", CONTROL_PER, ")")))%>% 
  select(Site, `ICD-Pieces Activities`, Category, `ICD-Pieces`, `Standard of care`)

mcr_VA <- kable(medical_chart_audit_VA ,
                style = 'html',
                booktabs = T,
                col.names = c('Site', 'ICD-Pieces Activities', 'Category', 'ICD-Pieces', 'Standard of care'),
                caption = 'VA') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  # column_spec(1, '2in') %>%
  column_spec(2, '3in') %>%
  collapse_rows(c(1, 2)) %>%
  kable_styling(latex_options = "scale_down", font_size = 18)

mcr_VA
save_kable(mcr_VA, 'Outputs/VA_Care manager.pdf')

#PHHS
# load("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Masterdemos.RData")

phhs_cr <- read.csv(file = '~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Inputs/PHHS_Ten_Percent_completed_manual chart reviews_v2.csv',
                    stringsAsFactors = F)

phhs_cr <- phhs_cr %>% mutate(Values.eGFR..60.ml...min..0.1.2..2. = ifelse(Values.eGFR..60.ml...min..0.1.2..2. >2, ">2", Values.eGFR..60.ml...min..0.1.2..2.),
                              Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. = ifelse(Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. >2, ">2", Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2.),
                              Proteinuria..0.1.2..2. = ifelse(Proteinuria..0.1.2..2. >2, ">2", Proteinuria..0.1.2..2.)) %>% 
  mutate(Values.eGFR..60.ml...min..0.1.2..2. = as.character(Values.eGFR..60.ml...min..0.1.2..2.),
         Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2. = as.character(Values.Albuminuria.as.UACR...30.or..microalbumin....30..0.1.2..2.),
         Proteinuria..0.1.2..2. = as.character(Proteinuria..0.1.2..2.))

new_col_names <- c('pat_id',
                   'patient_mrn',
                   'pat_last_name',
                   'pat_first_name',
                   'birth_date',
                   'enroll_date',
                   'enroll_year',
                   'location',
                   'is_85',
                   'problem_list_htn',
                   'problem_list_dm',
                   'problem_list_ckd',
                   'egfr_60',
                   'albuminuria',
                   'proteinuria',
                   'glucose',
                   'hba1c_7.5_pre',
                   'hypoglycemic_agents',
                   'sbp_140_2_pre',
                   'dbp_90_2_pre',
                   'antihypertensive_agents',
                   'problem_list_post',
                   'htn_bp_goal_set_post',
                   'sbp_140_post',
                   'dbp_90_post',
                   'acei_arb_post',
                   'statin_post',
                   'dm_hba1c_goal_set_post',
                   'hba1c_7.5_post',
                   'htn_education_post',
                   'dm_education_post',
                   'ckd_education_post',
                   'lipid_education_post',
                   'immunization_post',
                   'pf_followup',
                   'pf_recommendation',
                   'pf_acceptance',
                   'unplanned_hospitalization_post',
                   'death',
                   'arm')
names(phhs_cr) <- new_col_names
phhs_cr <- as.data.frame(phhs_cr) %>% mutate(location = "PHHS")
phhs_cr <- phhs_cr[,c("pat_id", "arm", "location", "pf_followup", "pf_recommendation", "pf_acceptance")]

pf_followups_PHHS <- phhs_cr %>% 
  mutate(pf_followup = factor(pf_followup, levels = c("Yes", "No"))) %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  mutate(Site = "PHHS",
         `ICD-Pieces Activities` = 'Follow ups (patient and provider contact)') %>% 
  reshape2::dcast(Site +  `ICD-Pieces Activities` + pf_followup ~ arm , value.var = 'n') %>%
  rename(Category = pf_followup)


pf_ICD_recomm_PHHS <- phhs_cr %>% 
  mutate(pf_recommendation = ifelse(pf_followup == "No", "None", pf_recommendation)) %>% 
  mutate(pf_recommendation = factor(pf_recommendation, 
                                    levels = c("None", "Orderset requested", "Help with appointments", "Drug Therapy" , "CKD management" , "Unknown"))) %>% 
  
  group_by( arm, pf_recommendation) %>% 
  tally() %>% 
  mutate(Site = "PHHS",
         `ICD-Pieces Activities` = 'Recommendations made category') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_recommendation ~  arm , value.var = 'n') %>% 
  rename(Category = pf_recommendation)


pf_ICD_recomm_acceptance_PHHS <- phhs_cr %>% 
  mutate(pf_acceptance = ifelse(pf_followup == "No", "None", pf_acceptance)) %>% 
  mutate(pf_acceptance = factor(pf_acceptance, 
                                levels = c("None","Orderset accepted","Compliance","Successful appointments", "DM management","CKD management", "Unknown"  ))) %>% 
  group_by( arm, pf_acceptance) %>% 
  tally() %>% 
  mutate(Site = "PHHS",
         `ICD-Pieces Activities`  = 'Recommendations accepted') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_acceptance ~  arm , value.var = 'n') %>% 
  rename(Category = pf_acceptance)
    

patient_count_PHHS <- phhs_cr %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Site = 'PHHS',
         `ICD-Pieces Activities` = '',
         Category = 'Patient count') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + Category ~ arm, value.var = 'n') %>%
  mutate_all(as.character())

medical_chart_audit_PHHS <- bind_rows(patient_count_PHHS,
                                    pf_followups_PHHS,
                                    pf_ICD_recomm_PHHS,
                                    pf_ICD_recomm_acceptance_PHHS) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(CONTROL_PER = ifelse(Category == 'Patient count',
                              CONTROL,
                              paste0(round((CONTROL/patient_count_PHHS$CONTROL)*100,0),'%')), 
         PIECES_PER = ifelse(Category == 'Patient count',
                             PIECES,
                             paste0(round((PIECES/patient_count_PHHS$PIECES)*100,0),'%'))) %>% 
  mutate(`ICD-Pieces` = ifelse(Category == 'Patient count',
                               PIECES,
                               paste0(PIECES, " (", PIECES_PER, ")")),
         `Standard of care` = ifelse(Category == 'Patient count',
                                     CONTROL,
                                     paste0(CONTROL, " (", CONTROL_PER, ")")))%>% 
  select(Site, `ICD-Pieces Activities`, Category, `ICD-Pieces`, `Standard of care`)

mcr_PHHS <- kable(medical_chart_audit_PHHS ,
                style = 'html',
                booktabs = T,
                col.names = c('Site', 'ICD-Pieces Activities', 'Category', 'ICD-Pieces', 'Standard of care'),
                caption = 'PHHS') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  # column_spec(1, '2in') %>%
  column_spec(2, '3in') %>%
  collapse_rows(c(1, 2)) %>%
  kable_styling(latex_options = "scale_down", font_size = 18)

mcr_PHHS
save_kable(mcr_PHHS, 'Outputs/PHHS_Care manager.pdf')

#THR

thr_cr <-  read_excel('~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Inputs/ICD Pieces Chartreview THR_Ten_Percent_Final Aug 2020_VS analysis_9_2_2020 .xlsm', sheet = 'THR_Ten_Percent')

new_col_names <- c('pat_id',
                   'patient_mrn',
                   'pat_last_name',
                   'pat_first_name',
                   'birth_date',
                   'enroll_date',
                   'enroll_year',
                   'location',
                   'is_85',
                   'problem_list_htn',
                   'problem_list_dm',
                   'problem_list_ckd',
                   'egfr_60',
                   'albuminuria',
                   'proteinuria',
                   'glucose',
                   'hba1c_7.5_pre',
                   'hypoglycemic_agents',
                   'sbp_140_2_pre',
                   'dbp_90_2_pre',
                   'antihypertensive_agents',
                   'problem_list_post',
                   'htn_bp_goal_set_post',
                   'sbp_140_post',
                   'dbp_90_post',
                   'acei_arb_post',
                   'statin_post',
                   'dm_hba1c_goal_set_post',
                   'hba1c_7.5_post',
                   'htn_education_post',
                   'dm_education_post',
                   'ckd_education_post',
                   'lipid_education_post',
                   'immunization_post',
                   'pf_followup',
                   'pf_recommendation',
                   'pf_acceptance',
                   'unplanned_hospitalization_post',
                   'death',
                   'arm')
names(thr_cr) <- new_col_names
thr_cr <- as.data.frame(thr_cr) %>% mutate(location = "THR")
THR_cr <- thr_cr[,c("pat_id", "arm", "location", "pf_followup", "pf_recommendation", "pf_acceptance")]

pf_followups_THR <- THR_cr %>% 
  mutate(pf_followup = factor(pf_followup, levels = c("Yes", "No", "Unknown"))) %>% 
  group_by(arm, pf_followup) %>% 
  tally() %>% 
  mutate(Site = "THR",
         `ICD-Pieces Activities` = 'Follow ups (patient and provider contact)') %>% 
  reshape2::dcast(Site +  `ICD-Pieces Activities` + pf_followup ~ arm , value.var = 'n') %>%
  rename(Category = pf_followup)


pf_ICD_recomm_THR <- THR_cr %>% 
  mutate(pf_recommendation = ifelse(grepl(",", pf_recommendation), "Combination", pf_recommendation)) %>% 
  mutate(pf_recommendation = ifelse(pf_followup == "No", "None", pf_recommendation)) %>% 
  mutate(pf_recommendation = factor(pf_recommendation, 
                                    levels = c("None", "Orderset requested", "Help with appointments",
                                               "CKD management" , "DM management", "Combination", "Unknown"))) %>% 
  
  group_by( arm, pf_recommendation) %>% 
  tally() %>% 
  mutate(Site = "THR",
         `ICD-Pieces Activities` = 'Recommendations made category') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_recommendation ~  arm , value.var = 'n') %>% 
  rename(Category = pf_recommendation)


pf_ICD_recomm_acceptance_THR <- THR_cr %>% 
  mutate(pf_acceptance = ifelse(grepl(",", pf_acceptance), "Combination", pf_acceptance)) %>% 
  mutate(pf_acceptance = ifelse(pf_followup == "No", "None", pf_acceptance)) %>% 
  mutate(pf_acceptance = factor(pf_acceptance, 
                                levels = c("None", "Compliance", "Successful appointments", 
                                           "DM management" ,"CKD management","HTN management", "Combination", "Unknown"))) %>% 
  group_by( arm, pf_acceptance) %>% 
  tally() %>% 
  mutate(Site = "THR",
         `ICD-Pieces Activities`  = 'Recommendations accepted') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + pf_acceptance ~  arm , value.var = 'n') %>% 
  rename(Category = pf_acceptance)


patient_count_THR <- THR_cr %>% 
  group_by(arm) %>% 
  tally() %>% 
  mutate(Site = 'THR',
         `ICD-Pieces Activities` = '',
         Category = 'Patient count') %>% 
  reshape2::dcast(Site + `ICD-Pieces Activities` + Category ~ arm, value.var = 'n') %>%
  mutate_all(as.character())

medical_chart_audit_THR <- bind_rows(patient_count_THR,
                                      pf_followups_THR,
                                      pf_ICD_recomm_THR,
                                      pf_ICD_recomm_acceptance_THR) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(CONTROL_PER = ifelse(Category == 'Patient count',
                              CONTROL,
                              paste0(round((CONTROL/patient_count_THR$CONTROL)*100,0),'%')), 
         PIECES_PER = ifelse(Category == 'Patient count',
                             PIECES,
                             paste0(round((PIECES/patient_count_THR$PIECES)*100,0),'%'))) %>% 
  mutate(`ICD-Pieces` = ifelse(Category == 'Patient count',
                               PIECES,
                               paste0(PIECES, " (", PIECES_PER, ")")),
         `Standard of care` = ifelse(Category == 'Patient count',
                                     CONTROL,
                                     paste0(CONTROL, " (", CONTROL_PER, ")")))%>% 
  select(Site, `ICD-Pieces Activities`, Category, `ICD-Pieces`, `Standard of care`)

mcr_THR <- kable(medical_chart_audit_THR ,
                  style = 'html',
                  booktabs = T,
                  col.names = c('Site', 'ICD-Pieces Activities', 'Category', 'ICD-Pieces', 'Standard of care'),
                  caption = 'THR') %>%
  # row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F,html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  # column_spec(1, '2in') %>%
  column_spec(2, '3in') %>%
  collapse_rows(c(1, 2)) %>%
  kable_styling(latex_options = "scale_down", font_size = 18)

mcr_THR
save_kable(mcr_THR, 'Outputs/THR_Care manager.pdf')




