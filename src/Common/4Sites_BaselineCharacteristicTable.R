# set date parameters
enrollcens  = as.Date('2019-06-28')   #last day qualifying
enrollstart = as.Date('2016-07-06')
fucens      = as.Date('2020-06-30')
startdate   = as.Date('2015-07-01')  #start of study (used to look back 12 months prior to start of study)
enddate     = fucens  

#Join masterdemo
# setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")

load(file  = '~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Inputs/masterdemo-20190701.RData') %>% data.frame()

# Fix masterdemo
masterdemo_PHHS <- masterdemo %>% as.data.frame() %>% 
  mutate(patient_mrn_new = if_else(grepl('^[A-Za-z]', patient_mrn) | grepl('^[0+]', patient_mrn) ==  TRUE , #if patient_mrn starts with a letter or a zero, swap values with pat_id
                                   pat_id,
                                   NULL),
         pat_id_new = if_else(grepl('^[A-Za-z]', patient_mrn)| grepl('^[0+]', patient_mrn)  == TRUE,
                              patient_mrn,
                              NULL)) %>% 
  mutate(pat_id_final = coalesce(pat_id_new, pat_id),
         patient_mrn_final = coalesce(patient_mrn_new, patient_mrn),
         Ethnicity = case_when(ethnic_group_c_name == 'HISPANIC' ~ 'Hispanic or Latino',
                               ethnic_group_c_name == 'NON HISPANIC' ~ 'Not Hispanic or Latino',
                               TRUE ~ 'Unknown'),
         Race = case_when(patient_race == 'White' ~ 'White',
                          patient_race == 'Black' ~ 'Black or African American',
                          patient_race == 'Asian' ~ 'Asian',
                          patient_race == 'Unknown' | patient_race == 'Unknown/Not Reported/Declined' ~ 'Unknown',
                          TRUE ~ 'Other'),
         arm = case_when(arm == 'CONTROL' ~ 'Standard of care',
                         TRUE ~ 'ICD-Pieces')) %>% 
  select(-pat_id, -patient_mrn) %>% 
  dplyr::rename(pat_id = pat_id_final,
                patient_mrn = patient_mrn_final,
                Gender = sex_c_name ) %>% 
  select(pat_id, patient_mrn, everything()) %>% 
  mutate(Gender = unfactor(Gender)) %>%
  filter(!duplicated(patient_mrn))

masterdemo_PHHS1 <- masterdemo_PHHS %>% select(pat_id, patient_mrn, enroll_date, arm, pcp_id, createdt, BIRTH_DATE, 
                                              Gender, age, Ethnicity, Race)  
masterdemo_PHHS <- masterdemo_PHHS %>% select(pat_id, enroll_date, arm, pcp_id, 
                                     Gender, age, Ethnicity, Race) %>% 
  mutate(pat_id = unfactor(pat_id))

### THR masterdemo
load(file  = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Inputs/adv_icd_named.Rdata')  # ICD codes for adverse events, saved as a list

load(file = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Inputs/masterdemo_widewnewid-upto20190921_round2_nooptout.RData') # masterdemo_widewnewid
masterdemo_THR <- masterdemo_widewnewid %>%
  as.data.frame() %>%
  mutate(enumber = coalesce(enumber_new, enumber),
         pat_id = coalesce(pat_id_new, pat_id)) %>% 
  select(-enumber_new, -pat_id_new) %>%
  separate(PAT_NAME, into = c('Lastname','Firstname'), sep ="\\,", extra = 'merge') %>% # splits after comma delimeter --  lastname, firstname middlename format to Lastname and Firstname column
  mutate(Firstname = str_extract(Firstname,'\\w*')) %>% #extracts first name only (Removing middle name and suffixes)
  mutate(ID = paste(Firstname,Lastname,BIRTH_DATE)) %>% 
  mutate(arm = case_when(arm == 'CONTROL' ~ 'Standard of care', TRUE ~ 'ICD-Pieces'), # A = control, B = ICD
         Gender = case_when(SEX_NAME == 'Male' ~ 'MALE', TRUE ~ 'FEMALE'),
         Ethnicity = case_when(eth_col == 'HISPANIC' ~ 'Hispanic or Latino',
                               eth_col == 'NON HISPANIC' ~ 'Not Hispanic or Latino',
                               TRUE ~ 'Unknown'),
         Race = case_when(race_col == 'WHITE' ~ 'White',
                          race_col == 'BLACK' ~ 'Black or African American',
                          race_col == 'ASIAN' ~ 'Asian',
                          race_col == 'NATIVE AMERICAN' | race_col == 'OTHER PACIFIC' | race_col == 'TWO OR MORE RACES'  ~ 'Other',
                          TRUE ~ 'Unknown')) %>% 
  select(-eth_col, -race_col, -SEX_NAME)

masterdemo_THR1 <- masterdemo_THR %>% select(pat_id, enumber, ID,enroll_date, arm, pcp_id = ClusterKey, createdt, 
                                            Gender, age, Ethnicity, Race) 
masterdemo_THR <- masterdemo_THR %>% select(pat_id = enumber, enroll_date, arm, pcp_id = ClusterKey, 
                                             Gender, age, Ethnicity, Race) 

## ProHealth Masterdemo load(file = "Outputs/Archive/Masterdemo_v3.RData")
load(file = "/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/Outputs/Final_version/Masterdemo_v3(3865pat).RData")
masterdemo_ProHealth = masterdemo %>% 
  select(pat_id = Study_id, enroll_date = enroll_date_prefill, arm, pcp_id = clustergroup, Gender, age, Ethnicity, Race)

## VA masterdemo
demo <- read.xlsx("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Inputs/Tables_graphs for reports_2020 - DCRI_may 2020.xlsx",
                  sheet = "KM")#,

masterdemo_VA <- demo[demo$Location == "VA",] %>% mutate(start_dt = as.Date(start_dt, origin="1899-12-30"),
                                                      Censor_date = as.Date(Censor_date, origin = "1899-12-30"),
                                                      Hosp_date = as.Date(Hosp_date, origin = "1899-12-30"),
                                                      Death_date = as.Date(Death_date, origin = "1899-12-30"),
                                                      close.date = as.Date(close.date, origin = "1899-12-30"),
                                                      `enrolldate.("w"-365)`= as.Date(`enrolldate.("w"-365)`, origin = "1899-12-30")) %>% 
  mutate(Arm = case_when(Arm == "CONTROL" ~ "Standard of care",
                         TRUE ~ "ICD-Pieces")) %>% 
  mutate(Sex = case_when(Sex == "M" ~ "MALE",
                         TRUE ~ "FEMALE")) %>%
  mutate(Race = ifelse(Race == "WHITE", "White", ifelse(Race == "ASIAN", "Asian", Race) )) %>% 
  dplyr::rename(age = Age, Gender = Sex) %>%
  mutate(Ethnicity = case_when(Ethnicity == "NON HISPANIC" ~ "Not Hispanic or Latino",
                               Ethnicity == "HISPANIC" ~ "Hispanic or Latino",
                               TRUE ~ "Unknown")) %>% 
  select(pat_id = Column1, enroll_date = start_dt, arm = Arm, pcp_id = clustergroup, Gender, age, Ethnicity, Race) %>% 
  filter(pat_id != "ABC36577")



#Combine all masterdemo
masterdemo <- rbind(masterdemo_PHHS, masterdemo_THR, masterdemo_ProHealth, masterdemo_VA)
masterdemo <- masterdemo %>% mutate(Ethnicity = ifelse(Ethnicity == "Not Hispanic or Latino", "Non-Hispanic or Latino", Ethnicity),
                                    Race = case_when(Race == 'Other Pacific' ~ "Other",
                                                     Race == 'Black' ~ "Black or African American",
                                                     Race == 'American Indian' ~ "Other",
                                                     TRUE ~ as.character(Race)))

#Join km file

km_PHHS <- read.xlsx("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Outputs/Final_version/PHHS_KM_v11.xlsx", sheet = 1) 
km_PHHS <- km_PHHS %>%  mutate_at(vars(Arm), ~replace(., Arm == "Control","Standard of care")) %>% # for patients that did not have any medication info  
mutate(start_dt = openxlsx::convertToDate(start_dt),
       Censor_date = openxlsx::convertToDate(Censor_date),
       Death_date = openxlsx::convertToDate(Death_date),
       Hosp_date = openxlsx::convertToDate(Hosp_date),
       Dial_date = openxlsx::convertToDate(Dial_date),
       Admit_Date_Observation = openxlsx::convertToDate(Admit_Date_Observation))

km_THR <- read.xlsx("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Outputs/THR_KM_v3.xlsx", sheet = 1) %>% 
  mutate(start_dt = openxlsx::convertToDate(start_dt),
         Censor_date = openxlsx::convertToDate(Censor_date),
         Death_date = openxlsx::convertToDate(Death_date),
         Hosp_date = openxlsx::convertToDate(Hosp_date),
         Dial_date = openxlsx::convertToDate(Dial_date),
         Admit_Date_Observation = openxlsx::convertToDate(Admit_Date_Observation))%>% 
  mutate_at(vars(Arm), ~replace(., Arm == "Control","Standard of care")) %>%  # for patients that did not have any medication info
  rename(pcp_id = ClusterKey, Sex = Gender)

km_ProHealth <- read.xlsx("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/Outputs/Final_version/ProHealth_KM_v7.xlsx", sheet = 1) %>% 
  mutate(start_dt = openxlsx::convertToDate(start_dt),
         Censor_date = openxlsx::convertToDate(Censor_date),
         Death_date = openxlsx::convertToDate(Death_date),
         Hosp_date = openxlsx::convertToDate(Hosp_date),
         Dial_date = openxlsx::convertToDate(Dial_date),
         Admit_Date_Observation = openxlsx::convertToDate(Admit_Date_Observation))%>% 
  mutate_at(vars(Arm), ~replace(., Arm == "Control","Standard of care")) %>% 
  rename(pcp_id = clustergroup, Sex = Gender)

km_VA <- read.xlsx("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Outputs/Final_version/VA_KM_v7.xlsx", sheet = 1) %>% 
  mutate(start_dt = openxlsx::convertToDate(start_dt),
         Censor_date = openxlsx::convertToDate(Censor_date),
         Death_date = openxlsx::convertToDate(Death_date),
         Hosp_date = openxlsx::convertToDate(Hosp_date),
         Dial_date = openxlsx::convertToDate(Dial_date),
         Admit_Date_Observation = openxlsx::convertToDate(Admit_Date_Observation))%>% 
  mutate_at(vars(Arm), ~replace(., Arm == "Control","Standard of care")) %>% 
  rename(Age = age, Sex = Gender)


kmColnames <- intersect(intersect(colnames(km_PHHS), colnames(km_THR)),
          intersect(colnames(km_ProHealth), colnames(km_VA)))

km_complete_final <- rbind(km_PHHS[,kmColnames], km_THR[,kmColnames], km_ProHealth[,kmColnames], km_VA[,kmColnames]) %>% 
  mutate_at(vars(HospY), ~replace(., HospY == "1","Y")) %>% 
  mutate_at(vars(HospY), ~replace(., is.na(HospY),"N")) %>% 
  mutate_at(vars(DeathYN), ~replace(., DeathYN == "1","Y")) %>% 
  mutate_at(vars(DeathYN), ~replace(., DeathYN == "0","N")) %>% 
  mutate_at(vars(CV_Event), ~replace(., is.na(CV_Event),0)) %>% 
  mutate_at(vars(CV_Event), ~replace(., is.na(CV_Event),0)) %>% 
  mutate(Race = case_when(Race == 'White' ~ 'White',
                          Race == 'Black' ~ 'Black or African American',
                          Race == 'Asian' ~ 'Asian',
                          Race == 'Unknown' | Race == 'Unknown/Not Reported/Declined' ~ 'Unknown',
                   TRUE ~ 'Other'),
         Ethnicity = case_when(Ethnicity == 'HISPANIC' | Ethnicity == "Hispanic or Latino" ~ 'Hispanic or Latino',
                               Ethnicity == 'NON HISPANIC' | Ethnicity == "Not Hispanic or Latino" ~ 'Not Hispanic or Latino',
                               TRUE ~ 'Unknown'),
         CAD = ifelse(is.na(CAD), 0, CAD),
         CHF = ifelse(is.na(CHF), 0, CHF),
         CV_procedure = ifelse(is.na(CV_procedure), 0, CV_procedure),
         `HbA1C_lt7.5` = ifelse(is.na(`HbA1C_lt7.5`), "Unknown", `HbA1C_lt7.5`),
         Death_1yr = ifelse(Death_1yr == 0, "N", ifelse(Death_1yr == 1, "Y", Death_1yr)),
         `DBP_lt90` = ifelse(is.na(`DBP_lt90`), "Unknown", `DBP_lt90`),
         `SBP_lt140` = ifelse(is.na(`SBP_lt140`), "Unknown", `SBP_lt140`)) %>% 
  mutate(CV_procedure = ifelse(HospY == "N", 0, CV_procedure))
  
  
write.xlsx(km_complete_final, 
           file = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Aggregate_KM_v6.xlsx")

#Table 1
Count  <- masterdemo %>%
  group_by (arm) %>%
  mutate(Count = n()) %>%
  ungroup() %>% 
  mutate(All = n()) %>% 
  select(arm, Count, All) %>% 
  unique() %>%
  mutate(Characteristics = 'Count',
         Category = '') %>% 
  reshape2::dcast(Characteristics + Category + All ~ arm, value.var = 'Count') %>% 
  arrange(desc(`ICD-Pieces`))%>%
  mutate_all( ~lapply(.,as.character)) %>% 
  mutate_all(~lapply(., as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Age_Mean   <- masterdemo %>%
  group_by (arm) %>%
  mutate(Age = paste0(round(mean(age),1), " +/- ", round(sd(age), 1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(age),1), " +/- ", round(sd(age), 1))) %>%
  select(arm, Age, All) %>% 
  unique() %>%
  mutate(Characteristics = '<b>Age</b>, Mean +/- SD (years)') %>% 
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'Age') %>% #converting long format to wide format
  arrange(desc(`ICD-Pieces`)) %>% 
  mutate_all(~lapply(., as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Male <- left_join(x = masterdemo  %>%
                    group_by(arm, Gender) %>% #by arm
                    dplyr::summarise(n = n()) %>% 
                    mutate(percent = paste0(n, " (", round(n/sum(n)*100,1), "%)")) %>% 
                    filter(Gender == 'MALE'), #%>% select(-n),
                  y = masterdemo  %>%
                    group_by(Gender) %>%  # overall
                    dplyr::summarise(n = n()) %>% 
                    mutate(All = paste0(n, " (", round(n/sum(n)*100,1), "%)") ) %>% 
                    filter(Gender == 'MALE'),# %>% select(-n),
                  by = 'Gender') %>% 
  mutate(Characteristics = '<b>Male</b> (%)') %>% 
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'percent') %>% 
  mutate_all(., ~lapply(., as.character))  %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Ethnicity <- left_join(x = masterdemo  %>%
                         group_by(arm, Ethnicity, ) %>% #by arm
                         dplyr::summarise(n = n()) %>% 
                         mutate(percent = paste0(n, " (", round(n/sum(n)*100,1), "%)")) %>% 
                         select(-n),
                       y = masterdemo  %>%
                         group_by(Ethnicity) %>%  # overall
                         dplyr::summarise(n = n()) %>% 
                         mutate(All = paste0(n, " (", round(n/sum(n)*100,1), "%)"),
                                p = round(n/sum(n)*100,1)) %>% 
                         select(-n),
                       by = 'Ethnicity') %>% 
  mutate(Characteristics = Ethnicity) %>% 
  reshape2::dcast(Characteristics + All + p ~ arm, value.var = 'percent') %>%
  arrange(desc(p)) %>% 
  select(-p) %>%
  mutate_all(., ~lapply(., as.character))  %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Race <- left_join(x = masterdemo  %>%
                    group_by(arm, Race, ) %>% #by arm
                    dplyr::summarise(n = n()) %>% 
                    mutate(percent = paste0(n , " (", round(n/sum(n)*100,1), "%)") ) %>% 
                    select(-n),
                  y = masterdemo  %>%
                    group_by(Race) %>%  # overall
                    dplyr::summarise(n = n()) %>% 
                    mutate(All = paste0(n, " (", round(n/sum(n)*100,1), "%)"),
                           p = round(n/sum(n)*100,1)) %>% 
                    select(-n),
                  by = 'Race') %>% 
  mutate(Characteristics = Race) %>% 
  reshape2::dcast(Characteristics + All + p ~ arm, value.var = 'percent') %>%
  arrange(desc(as.numeric(p))) %>%
  select(-p) %>%
  mutate_all(., ~lapply(., as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


#Weight and BMI
load(file = '~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Inputs/bmi.RData') %>% as.data.frame()
bmi <- bmi %>%
  select(pat_id = PatientEpicId,
         csn = EncounterEpicCsn,
         patient_mrn = PrimaryMrn,
         DateValue,
         Height = HeightInInches,
         Weight = WeightInOunces,
         BMI = BodyMassIndex
  )
weight_bmi_bsln_PHHS <- left_join(x = masterdemo_PHHS %>% as.data.frame() %>% select(pat_id, enroll_date, arm),
                             y = bmi %>% as.data.frame(),
                             by = 'pat_id') %>% 
  filter(DateValue <= enroll_date) %>% 
  select(pat_id, patient_mrn, arm,  enroll_date, DateValue, Weight, Height, BMI) %>% 
  unique() %>%
  mutate(Weight = round((Weight*0.0283495),2),
         BMI = ifelse(BMI > 500 | is.na(BMI), mean(BMI, na.rm = T), BMI)) %>% #imputing 7 patients
  arrange(pat_id, desc(DateValue)) %>% 
  filter(!duplicated(pat_id)) %>% 
  select(pat_id, arm, Weight_KG = Weight, BMI) 

#THR
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/"
clinical_encounter_new = readr::read_delim(paste0(path, 'Inputs/Sept 2020 snapshot/clinenc_vitals_Jun_1_2019_to_Aug_26_2020.txt'), '|', escape_double = FALSE, trim_ws = TRUE)
clinical_encounter_old = readr::read_delim(paste0(path, 'Inputs/Source Files for DSMB2020Jan/clinenc_bkfil_july2015_nov212019.txt'), '|', escape_double = FALSE, trim_ws = TRUE)
clinical_encounter <- rbind(clinical_encounter_new, clinical_encounter_old)

#1. Weight and BMI
weight_bmi <-clinical_encounter %>%
  as.data.frame() %>%
  select(pat_id = PAT_ID,
         enumber = ENUMBER,
         HEIGHT,
         WEIGHT,
         hw_date = APPT_TIME) %>%  #considering appointment date as date of vitals
  mutate(hw_date = anytime::anydate(hw_date)) %>% 
  separate(HEIGHT, c('Feet','Inches'), "'", convert = T) %>% 
  mutate(Inches = gsub("[^[:digit:].]", "", Inches)) %>% 
  filter(!is.na(Inches)) %>%
  mutate(Height_M = ((12*as.numeric(Feet) + as.numeric(Inches)) * 2.54)/100,
         Weight_KG = as.numeric(WEIGHT)/35.274) %>% 
  mutate(BMI = Weight_KG/(Height_M^2)) %>% 
  filter(!is.na(BMI))


weight_bmi_bsln_THR <- left_join(  x = masterdemo_THR %>% as.data.frame() %>% select(pat_id, arm), 
                               y = left_join( x  = masterdemo_THR %>% as.data.frame() %>% select(pat_id, enroll_date),
                                              y  = weight_bmi %>% select(pat_id = enumber, Weight_KG, BMI, hw_date),
                                              by = 'pat_id') %>%
                                 filter(hw_date <= enroll_date & enroll_date - hw_date <=365) %>%
                                 arrange(pat_id, desc(hw_date)) %>% 
                                 filter(!duplicated(pat_id)) %>%
                                 select(pat_id, Weight_KG, BMI) %>% 
                                 unique(),
                               by = 'pat_id')  # 14 patients do not have Height/weight information during baseline


#ProHealth BMI
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/"
load(paste0(path,"Inputs/KM_claims_enrolled-20181231.RData"))
ckd_claims <- readr::read_delim(paste0(path,'Inputs/ckd_claims.txt'), '\t', escape_double = FALSE, trim_ws = TRUE)
ckd_vitals_ProHealth <- readr::read_delim(paste0(path,'Inputs/ckd_vitals.txt'), '\t', escape_double = FALSE, trim_ws = TRUE)
ckd_vitals_ProHealth <- ckd_vitals_ProHealth %>% mutate(rand_DatePerformed = as.Date(strptime(rand_DatePerformed,"%b %d %Y %H:%M%p")))
#1. Weight and BMI
bmi_raw <-ckd_vitals_ProHealth %>%
  as.data.frame() %>%
  filter(subtest_name == "BMI Calculated") %>% 
  select(study_id,
         BMI = firstfindingvalue,
         hw_date = rand_DatePerformed)


bmi_bsln_ProHealth <- left_join(  x = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, arm), 
                        y = left_join( x  = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, enroll_date),
                                       y  = bmi_raw %>% rename(pat_id = study_id),
                                       by = 'pat_id') %>%
                          filter(hw_date <= enroll_date & enroll_date - hw_date <=365) %>%
                          arrange(pat_id, enroll_date, desc(hw_date)) %>% 
                          filter(!duplicated(pat_id, enroll_date)) %>%
                          select(pat_id, BMI) %>% 
                          unique(),
                        by = 'pat_id')  %>% 
  # filter(pat_id, enroll_date != "17510") %>% 
  na.omit()

weigth_raw <-ckd_vitals_ProHealth %>%
  as.data.frame() %>%
  filter(subtest_name == "Weight") %>% 
  mutate(Weight_KG = 0.453592 * firstfindingvalue,
         Weight = firstfindingvalue) %>% 
  select(study_id,
         Weight,
         Weight_KG,
         hw_date = rand_DatePerformed)

weight_bsln_ProHealth <- left_join(  x = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, arm), 
                           y = left_join( x  = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, enroll_date),
                                          y  = weigth_raw %>% rename(pat_id = study_id),
                                          by = 'pat_id') %>%
                             filter(hw_date <= enroll_date & enroll_date - hw_date <=365) %>%
                             arrange(pat_id, desc(hw_date)) %>% 
                             filter(!duplicated(pat_id)) %>%
                             select(pat_id, Weight_KG) %>% 
                             unique(),
                           by = 'pat_id')  %>% 
  na.omit()

weight_bmi_bsln_ProHealth <- merge(x = bmi_bsln_ProHealth, y = weight_bsln_ProHealth, by = c("pat_id", "arm"))


# VA BMI and Weight
BMI_VA <- read.csv("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Inputs/VA_BMI.csv", stringsAsFactors = F) %>% select(-X)
Weight_VA <- read.csv("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Inputs/VAWeight.csv", stringsAsFactors = F) %>% 
  mutate(NumResult = 0.453592*NumResult)

weight_bmi_bsln_VA <- left_join(x = masterdemo_VA %>% select(pat_id, arm), 
                                y = merge(x = BMI_VA %>% select(ABC, BMI), 
                                          y = Weight_VA %>% select(ABC, Weight_KG = NumResult), 
                                          by = "ABC", all = T) %>% 
                                  rename(pat_id = ABC),
                                by = "pat_id")


weight_bmi_bsln <- rbind(weight_bmi_bsln_PHHS, weight_bmi_bsln_THR, weight_bmi_bsln_ProHealth, weight_bmi_bsln_VA)


BMI    <- weight_bmi_bsln %>% 
  group_by(arm) %>% 
  mutate(Mean_BMI = paste(eval(round(mean(BMI, na.rm = T),1)),'+/-',eval(round(sd(BMI, na.rm = T),1)))) %>%
  ungroup() %>% 
  mutate(All = paste(eval(round(mean(BMI, na.rm = T),1)),'+/-',eval(round(sd(BMI, na.rm = T),1)))) %>% 
  select(arm, Mean_BMI, All) %>% 
  unique() %>% 
  mutate(Characteristics = '<b>BMI</b>, Mean +/- SD (kg/m2)') %>% 
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'Mean_BMI') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


Weight    <- weight_bmi_bsln %>% 
  group_by(arm) %>%
  mutate(Mean_Weight = paste(eval(round(mean(Weight_KG, na.rm = T),1)),'+/-',eval(round(sd(Weight_KG, na.rm = T),1)))) %>%
  ungroup() %>% 
  mutate(All = paste(eval(round(mean(Weight_KG, na.rm = T),1)),'+/-',eval(round(sd(Weight_KG, na.rm = T),1)))) %>% 
  select(arm, Mean_Weight, All) %>% 
  unique() %>% 
  mutate(Characteristics ='<b>Weight</b>, Mean +/- SD (kg)') %>% 
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'Mean_Weight') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


# PHHS Blood pressure
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/"

load(file = paste0(path, 'Inputs/match_bp.RData')) %>% as.data.frame()
load("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/Shelley Handoff/PHHS SHELLEY/flowsheet_sql_20190227.RData")
bp_sql1 <- left_join(x = masterdemo_PHHS %>% select(pat_id,enroll_date),
                     y = bp_sql,
                     by = "pat_id") %>% 
  filter(contact_date < enroll_date & contact_date > (enroll_date-365) & bp_systolic != 0 & bp_diastolic != 0) %>% 
  select(pat_id, pat_enc_csn_id, bp_systolic, bp_diastolic, contact_date) %>% 
  mutate(contact_date = as.Date(contact_date)) %>% 
  dplyr::rename(bp_d = contact_date, sbp = bp_systolic, dbp = bp_diastolic, csn = pat_enc_csn_id)


# missing_bp = unfactor(bp_bsln[is.na(bp_bsln$sbp),"pat_id"])
# missing_bp <- left_join(x = missing_bp)

bp <- match_bp %>%
  as.data.frame() %>%
  separate(DTValue, into = c('sbp', 'dbp'), sep = '\\/', extra = 'merge') %>%
  select(pat_id, csn, sbp, dbp, bp_d) %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp),
         pat_id = as.factor(pat_id)) %>% 
  filter(sbp != 0 | !is.na(sbp)) %>%
  arrange(pat_id, bp_d)

bp_PHHS <- rbind(bp, bp_sql1) %>%  unique()
rm(bp)

bp_bsln_PHHS <- left_join(  x = masterdemo_PHHS %>% as.data.frame() %>% select(pat_id, arm), 
                       y = left_join( x  = masterdemo_PHHS %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y  = bp_PHHS,
                                      by = 'pat_id') %>%
                         filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                         arrange(pat_id, desc(bp_d)) %>% 
                         filter(!duplicated(pat_id)) %>%
                         select(pat_id, enroll_date, sbp, dbp) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp)) %>% 
  select(pat_id, arm, sbp, dbp)#195 Patients do not have BP records during baseline

# THR bp
setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")

clinical_encounter_new = readr::read_delim('Inputs/Sept 2020 snapshot/clinenc_vitals_Jun_1_2019_to_Aug_26_2020.txt', '|', escape_double = FALSE, trim_ws = TRUE)
clinical_encounter_old = readr::read_delim('Inputs/Source Files for DSMB2020Jan/clinenc_bkfil_july2015_nov212019.txt', '|', escape_double = FALSE, trim_ws = TRUE)
clinical_encounter <- rbind(clinical_encounter_new, clinical_encounter_old)


bp_THR <- clinical_encounter %>%
  as.data.frame() %>%
  filter(!is.na(BP_DIASTOLIC) | !is.na(BP_SYSTOLIC)) %>% 
  select(pat_id = ENUMBER,
         csn = PAT_ENC_CSN_ID,
         sbp = BP_SYSTOLIC,
         dbp = BP_DIASTOLIC,
         bp_d = APPT_TIME) %>%  #considering appointment date as res_d ( result date)
  mutate(bp_d = anytime::anydate(bp_d),
         sbp = as.numeric(sbp),
         dbp = as.numeric(dbp))

bp_bsln_THR <- left_join(  x = masterdemo_THR %>% as.data.frame() %>% select(pat_id, arm), 
                       y = left_join( x  = masterdemo_THR %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y  = bp_THR,
                                      by = 'pat_id') %>%
                         filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                         arrange(pat_id, desc(bp_d)) %>% 
                         filter(!duplicated(pat_id)) %>%
                         select(pat_id, enroll_date, sbp, dbp) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp)) %>% 
  select(pat_id, arm, sbp, dbp)

# ProHealth bp

bp_raw_ProHealth <- ckd_vitals_ProHealth %>%
  as.data.frame() %>%
  filter(subtest_name == "Systolic/Diastolic") %>% 
  mutate(SBP = firstfindingvalue,
         DBP = secondfindingvalue) %>% 
  select(pat_id = study_id,
         sbp = SBP,
         dbp = DBP,
         bp_d = rand_DatePerformed)

bp_bsln_ProHealth <- left_join(  x = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id , arm), 
                            y = left_join( x  = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, enroll_date),
                                           y  = bp_raw_ProHealth,
                                           by = 'pat_id') %>%
                              filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                              arrange(pat_id, desc(bp_d)) %>% 
                              filter(!duplicated(pat_id)) %>%
                              select(pat_id, enroll_date, sbp, dbp) %>% 
                              unique(),
                            by = 'pat_id')  %>% 
  na.omit() %>% 
  select(pat_id, arm, sbp, dbp)


# VA bp
path = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/'
FinalBloodPressuresExport <- read.csv(paste0(path, "Inputs/FinalBloodPressuresExport.csv"), stringsAsFactors = F) %>% 
  mutate(day_diff_enroll = as.numeric(day_diff_enroll),
         Diastolic = as.numeric(Diastolic),
         resulte_date = as.Date(strptime(resulte_date, "%m/%d/%Y"))) %>% na.omit() %>% 
  rename(DayDiffEnroll = day_diff_enroll,
         SBP = Systolic,
         DBP = Diastolic)
  
bp_VA <- FinalBloodPressuresExport %>% 
  na.omit() %>% 
  filter(DayDiffEnroll <= 0 & DayDiffEnroll > -365) %>% 
  arrange(ABC_ID, desc(DayDiffEnroll)) %>% 
  filter(!duplicated(ABC_ID)) %>% 
  select(pat_id = ABC_ID, sbp = SBP, dbp = DBP, resulte_date)

bp_bsln_VA <- left_join(  x = masterdemo_VA %>% as.data.frame() %>% select(pat_id, arm), 
                            y = left_join( x  = masterdemo_VA %>% as.data.frame() %>% select(pat_id, enroll_date),
                                           y  = bp_VA,
                                           by = 'pat_id') %>%
                            filter(resulte_date <= enroll_date & enroll_date - resulte_date <=365) %>%
                            arrange(pat_id, desc(resulte_date)) %>% 
                            filter(!duplicated(pat_id)) %>%
                            select(pat_id, enroll_date, sbp, dbp) %>% 
                            unique(),
                            by = 'pat_id')  %>% 
  na.omit() %>% 
  select(pat_id, arm, sbp, dbp)

bp_bsln <- rbind(bp_bsln_PHHS, bp_bsln_THR, bp_bsln_ProHealth, bp_bsln_VA)

Bp_mean_SBP  <- bp_bsln %>% 
  group_by(arm) %>%
  mutate(bp_median = paste0(round(mean(sbp, na.rm = T),1), " +/- ", round(sd(sbp, na.rm = T), 1))) %>% 
  ungroup() %>%
  mutate(All = paste0(round(mean(sbp, na.rm = T),1)," +/- ", round(sd(sbp, na.rm = T), 1))) %>%
  select(arm, bp_median, All) %>%
  unique() %>% 
  mutate(Characteristics = 'Mean Systolic Blood Pressure +/- SD (mmHg)') %>%
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'bp_median') %>%
  mutate_all( ~lapply(.,as.character))  %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Bp_mean_DBP  <- bp_bsln %>% 
  group_by(arm) %>%
  mutate(bp_median = paste0(round(mean(dbp, na.rm = T),1), " +/- ", round(sd(dbp, na.rm = T), 1))) %>% 
  ungroup() %>%
  mutate(All = paste0(round(mean(dbp, na.rm = T),1), " +/- ", round(sd(dbp, na.rm = T), 1))) %>%
  select(arm, bp_median, All) %>%
  unique() %>% 
  mutate(Characteristics = 'Mean Diastolic Blood Pressure +/- SD (mmHg)') %>%
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'bp_median') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


# A1c
#PHHS
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/"

load(file = paste0(path, 'Inputs/a1c.RData')) %>% as.data.frame()
a1c = a1c %>% select(pat_id = PatientEpicId,
                     csn = EncounterEpicCsn,
                     order_proc_id = PrimaryMrn,
                     component_id = LabComponentEpicId,
                     ord_value = LabResultValue,
                     ord_num_value = NumericValue,
                     res_d, 
                     enc_d)

a1c_bsln_PHHS <- left_join(  x = masterdemo_PHHS %>% as.data.frame(), 
                        y = left_join( x  = masterdemo_PHHS %>% as.data.frame() %>% select(pat_id, enroll_date),
                                       y  = a1c %>% as.data.frame() %>% filter(component_id %in% c(316664, 21328827, 1558024, 21489210)),
                                       by = 'pat_id') %>%
                          filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                          select(pat_id, ord_value, ord_num_value, res_d) %>% 
                          unique(),
                        by = 'pat_id') %>%  
  mutate(a1c = ifelse(!is.na(ord_num_value), 1, 0),
         val = as.numeric(gsub('>|<|%|\\+|/[0-9]{,3}','', ord_value))) %>% 
  arrange(pat_id, desc(res_d)) %>% 
  filter(!duplicated(pat_id)) %>% 
  select(pat_id, arm, val)   

path = "/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/"

lab_new = readr::read_delim(paste0(path, 'Inputs/Sept 2020 snapshot/lab_June_1_2019_to_Aug_26_2020.txt'), '|', escape_double = FALSE, trim_ws = TRUE)
lab_old = readr::read_delim(paste0(path,'Inputs/Source Files for DSMB2020Jan/lab_bkfil_july2015_nov212019.txt'), '|', escape_double = FALSE, trim_ws = TRUE)
lab_THR <- rbind(lab_new,lab_old) %>% mutate(res_d = anytime::anydate(RESULT_TIME))

a1c_THR = lab_THR %>% 
  filter(COMPONENT_NAME %in% c( 'HEMOGLOBIN A1' , 'A1C' , 'HEMOGLOBIN A1C %' , 'HEMOGLOBIN A1C', 'POC')) %>% 
  select(pat_id = ENUMBER,
         csn = PAT_ENC_CSN_ID,
         order_proc_id = ORDER_PROC_ID,
         component_id = COMPONENT_ID,
         ord_value = ORD_VALUE,
         ord_num_value = ORD_NUM_VALUE,
         res_d)


a1c_bsln_THR <- left_join(  x = masterdemo_THR %>% as.data.frame(), 
                        y = left_join( x  = masterdemo_THR %>% as.data.frame() %>% select(pat_id, enroll_date),
                                       y  = a1c_THR %>% as.data.frame(),
                                       by = 'pat_id') %>%
                          filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                          select(pat_id, ord_value, ord_num_value, res_d) %>% 
                          unique(),
                        by = 'pat_id') %>%  
  mutate(a1c = ifelse(!is.na(ord_num_value), 1, 0),
         val = as.numeric(gsub('>|<|%|\\+|/[0-9]{,3}','', ord_value))) %>% 
  arrange(pat_id, desc(res_d)) %>% 
  filter(!duplicated(pat_id)) %>% 
  select(pat_id, arm, val) #%>% 
  # mutate(lt_7.5 = case_when(val < 7.5 ~ 'Yes',
  #                           val >= 7.5 ~ 'No',
  #                           is.na(val) == TRUE ~ 'Unknown')) # There are 304 patients for whom we do not have HbA1c values during baseline


# ProHealth a1c
#Labs
load("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/Shelley Handoff/Albert Analysis/Prohealth_A1c_Prior_AK.RData")

a1c_bsln_ProHealth <- a1cprior2_prohealth %>% 
  mutate(arm = ifelse(arm == "PIECES", "ICD-Pieces", "Standard of care")) %>% 
  select(pat_id = study_id, arm, val = maxa1cval)

# VA a1c
path = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/'
FinalLabsExport <- read.csv(paste0(path, "Inputs/FinalLabsExport.csv"), stringsAsFactors = F) %>% 
  mutate(date_diff = as.numeric(date_diff))%>% na.omit()

HBA1C_VA <- FinalLabsExport %>% filter(grepl("A1C", LabChemTestName)) %>% 
  rename(val = LabChemResultValue) %>% 
  mutate(val = ifelse(val == ">15.0", 15.1, ifelse(val == ">16.9", 17, as.numeric(val)))) %>% 
  mutate(date_diff = as.numeric(date_diff)) %>% 
  select(pat_id = ABC_ID, date_diff, val)

a1c_bsln_VA <- left_join(x = masterdemo_VA %>% select(pat_id, arm), 
                        y = HBA1C_VA %>% 
                          filter(date_diff <=0 & date_diff > -365) %>% 
                          arrange(pat_id, desc(date_diff)) %>% 
                          filter(!duplicated(pat_id)) %>% 
                          select(-date_diff), 
                        by = "pat_id")


#Combine a1c
a1c_bsln <- rbind(a1c_bsln_PHHS, a1c_bsln_THR, a1c_bsln_ProHealth, a1c_bsln_VA)

a1c_mean <- a1c_bsln %>% 
  group_by(arm) %>%
  mutate(hba1c_mean = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>% 
  select(arm, hba1c_mean, All) %>% 
  unique() %>% 
  mutate(Characteristics = '<b>HbA1c</b>, Mean +/- SD (%)') %>%  
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'hba1c_mean') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`) 


#EGFR
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/"

load(file = paste0(path, 'Inputs/egfr.RData')) %>% as.data.frame()

egfr_PHHS = egfr %>% select(pat_id = PatientEpicId,
                       csn = EncounterEpicCsn,
                       order_proc_id = PrimaryMrn,
                       component_id = LabComponentEpicId,
                       ord_value = LabResultValue,
                       ord_num_value = NumericValue,
                       res_d, 
                       enc_d)
rm(egfr)

egfr_bsln_PHHS <- left_join(x = masterdemo_PHHS %>% as.data.frame(), 
                       y = left_join(x = masterdemo_PHHS %>%
                                       as.data.frame() %>%
                                       select(pat_id, enroll_date),
                                     y = egfr_PHHS %>%
                                       as.data.frame() %>%
                                       filter(component_id %in% c(6578054, 6578053, 27645213, 27645211)),
                                     by = 'pat_id') %>%
                         filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                         select(pat_id, ord_value, ord_num_value, res_d) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(val = as.numeric(gsub('>|<','', ord_value))) %>% 
  mutate(val = ifelse(is.na(val), 0, val)) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>% 
  select(pat_id, arm, val)
  

load("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/Shelley Handoff/THR SHELLEY/match_labs_egfr.RData")

egfr_THR = labs_egfr %>% select(pat_id = enumber,
                            csn,
                            component_id,
                            ord_num_value = val,
                            res_d)

egfr_bsln_THR <- left_join(x = masterdemo_THR %>% as.data.frame(), 
                       y = left_join(x = masterdemo_THR %>%
                                       as.data.frame() %>%
                                       select(pat_id, enroll_date),
                                     y = egfr_THR %>%
                                       as.data.frame(),
                                     by = 'pat_id') %>%
                         filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                         select(pat_id, ord_num_value, res_d) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(val = as.numeric(ord_num_value)) %>% 
  mutate(val = ifelse(is.na(val), 0, val)) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>%
  select(pat_id, arm, val)
  

#ProHealth
path = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/'

ckd_lab <- readr::read_delim(paste0(path, 'Inputs/ckd_lab.txt'), '\t', escape_double = FALSE, trim_ws = TRUE)

egfr_ProHealth <-  ckd_lab %>% filter(grepl("GFR", subtestname )) %>% select(pat_id = study_id,
                                                                    resultvalue, subtestname,
                                                                    res_d = rand_ResultDatetime)

egfr_bsln_ProHealth <- left_join(x = masterdemo_ProHealth %>% as.data.frame() %>% select(pat_id, arm) , 
                       y = left_join(x = masterdemo_ProHealth %>%
                                       as.data.frame() %>%
                                       select(pat_id, enroll_date),
                                     y = egfr_ProHealth %>%
                                       as.data.frame(),
                                     by = 'pat_id') %>%
                         filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                         select(pat_id, resultvalue, res_d) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  rename(val = resultvalue) %>% 
  mutate(val = ifelse(is.na(val), 0, ifelse(val == ">60" | val == ">60.00", 61, val))) %>% 
  mutate(val = as.numeric(val)) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(val != 0) %>%
  filter(!duplicated(pat_id)) %>%
  select(pat_id, arm, val)
  
#VA 

egfr_VA <- FinalLabsExport %>%
  filter(date_diff <= 0 & date_diff > -365) %>% 
  filter(grepl("eGFR", LabChemTestName)) %>% 
  mutate(LabChemResultValue = ifelse(LabChemResultValue == ">60", 61,
                                     ifelse(LabChemResultValue == "<10", 9, as.numeric(LabChemResultValue)))) %>% 
  arrange(ABC_ID, desc(date_diff)) %>%
  filter(!duplicated(ABC_ID)) %>% na.omit() %>% 
  select(pat_id = ABC_ID, val = LabChemResultValue) 

egfr_bsln_VA <- left_join(x = masterdemo_VA %>% select(pat_id, arm),
                          y = egfr_VA, 
                          by = 'pat_id')



egfr_bsln <- rbind(egfr_bsln_PHHS, egfr_bsln_THR, egfr_bsln_ProHealth, egfr_bsln_VA)

egfr_mean <- egfr_bsln %>% 
  group_by(arm) %>%
  mutate(egfr = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>% 
  select(arm, egfr, All) %>% 
  unique() %>% 
  mutate(Characteristics = paste0('<b>Estimated GFR</b>, Mean +/- SD (ml/min/1.73m<sup>2</sup>)')) %>%  
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'egfr') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


#Cholesterol
setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")

load(file = 'Inputs/cholest.RData')
cholesterol <- cholest %>%
  select(pat_id = PatientEpicId,
         csn = EncounterEpicCsn,
         patient_mrn = PrimaryMrn,
         enc_d,
         res_d,
         component_id = LabComponentEpicId,
         labname = LabName,
         val = NumericValue)

cholest_tot_PHHS <- left_join(x = masterdemo_PHHS %>% as.data.frame(), 
                         y = left_join(x = masterdemo_PHHS %>%
                                         as.data.frame() %>%
                                         select(pat_id, enroll_date),
                                       y = cholesterol %>%
                                         as.data.frame() %>%
                                         filter(labname %in% c('Cholesterol','CHOLESTEROL')),
                                       by = 'pat_id') %>%
                           filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                           select(pat_id, val, res_d) %>% 
                           unique(),
                         by = 'pat_id') %>% 
  select(pat_id, arm, res_d, val) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>% select(-res_d)

cholest_ldl_PHHS <- left_join(x = masterdemo_PHHS %>% as.data.frame(), 
                         y = left_join(x = masterdemo_PHHS %>%
                                         as.data.frame() %>%
                                         select(pat_id, enroll_date),
                                       y = cholesterol %>%
                                         as.data.frame() %>%
                                         filter(labname %in% c('Non-HDL Cholesterol')),
                                       by = 'pat_id') %>%
                           filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                           select(pat_id, val, res_d) %>% 
                           unique(),
                         by = 'pat_id') %>% 
  select(pat_id, arm,res_d, val) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>% select(-res_d)

setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")

cholesterol <-  read.delim("Inputs/lab_July2015_July2020_cholesterol_panel.txt", sep = "|")
cholesterol1 <- cholesterol %>%
  select(pat_id = PAT_ID,
         enumber = ENUMBER,
         csn = PAT_ENC_CSN_ID,
         enc_d = RESULT_TIME,
         res_d = RESULT_TIME,
         component_id = COMPONENT_ID,
         labname = COMPONENT_NAME,
         val = ORD_VALUE) %>% 
  mutate(enc_d = as.Date(as.POSIXct(res_d, format = "%m/%d/%Y %H:%M:%S")),
         res_d = as.Date(as.POSIXct(res_d, format = "%m/%d/%Y %H:%M:%S")),
         val = as.numeric(unfactor(val)))

cholest_tot_THR <- left_join(x = masterdemo_THR %>% as.data.frame(), 
                         y = left_join(x = masterdemo_THR %>%
                                         as.data.frame() %>%
                                         select(pat_id, enroll_date),
                                       y = cholesterol1 %>%
                                         as.data.frame() %>%
                                         filter(labname == 'CHOLESTEROL, TOTAL'),
                                       by = 'pat_id') %>% na.omit() %>% 
                           filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                           select(pat_id, val, res_d) %>% 
                           unique(),
                         by = 'pat_id') %>% 
  select(pat_id, arm, res_d, val) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>% select(-res_d)

cholest_ldl_THR <- left_join(x = masterdemo_THR %>% as.data.frame(), 
                         y = left_join(x = masterdemo_THR %>%
                                         as.data.frame() %>%
                                         select(pat_id, enroll_date),
                                       y = cholesterol1 %>%
                                         as.data.frame() %>%
                                         filter(labname == 'HDL CHOLESTEROL'),
                                       by = 'pat_id') %>%
                           filter(res_d <= enroll_date & enroll_date - res_d <=365) %>%
                           select(pat_id, val, res_d) %>% 
                           unique(),
                         by = 'pat_id') %>% 
  select(pat_id, arm, res_d, val) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>% select(-res_d)

#ProHealth
setwd('/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/')
ckd_lab <- readr::read_delim('Inputs/ckd_lab.txt', '\t', escape_double = FALSE, trim_ws = TRUE)

cholesterol1 <- left_join(x = masterdemo_ProHealth %>% select(study_id = pat_id, enroll_date, arm), 
                          y = ckd_lab[ckd_lab$subtestname %in% c("CHOL/HDL RATIO-O", "CHOL/HDLC RATIO-Q", "CHOLESTEROL",
                                                                 "CHOLESTEROL, TOTAL-Q", "CHOLESTEROL-O", "Cholesterol",
                                                                 "Cholesterol, Total-L", "HDL CHOLESTEROL-Q", "HDL Cholesterol-L",
                                                                 "LDL-CHOLESTEROL-Q", "NON-HDL CHOLESTEROL"),] %>% 
                            select(study_id, subtestname, resultvalue, rand_ResultDatetime),
                          by = "study_id") %>% 
  filter(rand_ResultDatetime < enroll_date & rand_ResultDatetime > enroll_date - 365)

cholest_tot_ProHealth <- cholesterol1 %>% filter(subtestname %in% c("CHOLESTEROL-O", "HDL CHOLESTEROL-Q", "CHOLESTEROL, TOTAL-Q")) %>% 
  mutate(resultvalue = as.numeric(resultvalue)) %>% 
  arrange(study_id, desc(rand_ResultDatetime)) %>%  
  filter(!duplicated(study_id)) %>% 
  select(pat_id = study_id, arm, val = resultvalue)

cholest_nonHDL_O <- inner_join(x = cholesterol1 %>% filter(subtestname == "CHOLESTEROL-O") %>% select(study_id, resultvalue_total = resultvalue, rand_ResultDatetime, arm),
                               y = cholesterol1 %>% filter(subtestname == "CHOL/HDL RATIO-O")%>% select(study_id, resultvalue_ratio = resultvalue, rand_ResultDatetime, arm),
                               by = c("study_id", "rand_ResultDatetime", "arm")) %>% 
  arrange(study_id, desc(rand_ResultDatetime)) %>%  
  filter(!duplicated(study_id)) %>%
  mutate(resultvalue_total = as.numeric(resultvalue_total),
         resultvalue_ratio = as.numeric(resultvalue_ratio),
         Non_HDL = resultvalue_total*(1-1/resultvalue_ratio)) %>% 
  select(-resultvalue_ratio, -resultvalue_total)

cholest_nonHDL_Q <- inner_join(x = cholesterol1 %>% filter(subtestname == "CHOLESTEROL, TOTAL-Q") %>% select(study_id, resultvalue_total = resultvalue, rand_ResultDatetime, arm),
                               y = cholesterol1 %>% filter(subtestname == "CHOL/HDLC RATIO-Q")%>% select(study_id, resultvalue_ratio = resultvalue, rand_ResultDatetime, arm),
                               by = c("study_id", "rand_ResultDatetime", "arm")) %>% 
  arrange(study_id, desc(rand_ResultDatetime)) %>%  
  filter(!duplicated(study_id)) %>%
  mutate(resultvalue_total = as.numeric(resultvalue_total),
         resultvalue_ratio = as.numeric(resultvalue_ratio),
         Non_HDL = resultvalue_total*(1-1/resultvalue_ratio))  %>% 
  select(-resultvalue_ratio, -resultvalue_total)

cholest_nonHDL_Q_2 = cholesterol1 %>% filter(subtestname == "LDL-CHOLESTEROL-Q") %>% select(study_id, resultvalue_total = resultvalue, rand_ResultDatetime, arm) %>% 
  mutate(Non_HDL = as.numeric(resultvalue_total)) %>% na.omit() %>% 
  select(study_id, rand_ResultDatetime, arm, Non_HDL) 

cholest_ldl_ProHealth <- rbind(cholest_nonHDL_Q, cholest_nonHDL_Q_2, cholest_nonHDL_O) %>% 
  select(pat_id = study_id, arm, val = Non_HDL)

# VA Cholesterol
path = '/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/'
VA_lab<-read_csv(paste0(path, "Inputs/FinalLabs.v2Export.csv" ))
VA_lab$LabChemSpecimenDateTime <- as.Date(mdy_hm(VA_lab$LabChemSpecimenDateTime))

Cholesterol <- left_join(x = masterdemo_VA %>% select(Column1 = pat_id, arm, start_dt = enroll_date),
                         y = VA_lab%>%
                           filter(LabChemTestName=='HDL'|LabChemTestName=='CHOLESTEROL') %>% select (ABC,LabChemSpecimenDateTime,LabChemTestName,LabChemResultNumericValue) %>%
                           mutate(NumericValue= as.numeric(LabChemResultNumericValue), date = as.Date(LabChemSpecimenDateTime)) %>%
                           select( ABC,date,NumericValue,LabChemTestName) %>% 
                           pivot_wider(id_cols=c(ABC,date),values_from = NumericValue,names_from = LabChemTestName,
                                       values_fn = min ) %>%
                           group_by(ABC,date) %>%
                           mutate(NHD_CHOL = CHOLESTEROL-HDL) %>% 
                           rename(Column1 = ABC),
                         by = "Column1") %>% 
  filter(date <= start_dt & start_dt - date <= 365) %>% 
  arrange(Column1, date) %>% 
  filter(!duplicated(Column1))

cholest_tot_prop <- Cholesterol %>% 
  group_by(Arm) %>%
  mutate(cholest = paste0(round(mean(NHD_CHOL, na.rm = T),1), " +/- ", round(sd(NHD_CHOL, na.rm = T),1))) %>%
  ungroup() %>% 
  select(Arm, cholest) %>% 
  unique() %>% 
  mutate(Characteristics ='<b>Total Cholesterol</b>, Mean +/- SD (mg/dL)') %>%  
  reshape2::dcast(Characteristics ~ Arm, value.var = 'cholest') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

Cholesterol <- left_join(x = masterdemo %>% select(Column1 = Column2, Arm, start_dt),
                         y = VA_lab%>%
                           filter(LabChemTestName=='HDL'|LabChemTestName=='CHOLESTEROL') %>% select (ABC,LabChemSpecimenDateTime,LabChemTestName,LabChemResultNumericValue) %>%
                           mutate(NumericValue= as.numeric(LabChemResultNumericValue), date = as.Date(LabChemSpecimenDateTime)) %>%
                           select( ABC,date,NumericValue,LabChemTestName) %>% 
                           pivot_wider(id_cols=c(ABC,date),values_from = NumericValue,names_from = LabChemTestName,
                                       values_fn = min ) %>%
                           group_by(ABC,date) %>%
                           rename(Column1 = ABC),
                         by = "Column1") %>% 
  filter(date <= start_dt & start_dt - date <= 365) %>% 
  arrange(Column1, date) %>% 
  filter(!duplicated(Column1))

cholest_tot_VA <- Cholesterol %>% select(pat_id = Column1, arm, val = CHOLESTEROL)
cholest_ldl_VA <- Cholesterol %>% select(pat_id = Column1, arm, val = NHD_CHOL)

cholest_tot <- rbind(cholest_tot_PHHS, cholest_tot_THR, cholest_tot_ProHealth, cholest_tot_VA)
cholest_ldl <- rbind(cholest_ldl_PHHS, cholest_ldl_THR, cholest_ldl_ProHealth, cholest_ldl_VA)


cholest_tot_prop <- cholest_tot %>% 
  group_by(arm) %>%
  mutate(cholest = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>% 
  select(arm, cholest, All) %>% 
  unique() %>% 
  mutate(Characteristics ='<b>Total Cholesterol</b>, Mean +/- SD (mg/dL)') %>%  
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'cholest') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

cholest_ldl_prop <- cholest_ldl %>% 
  group_by(arm) %>%
  mutate(ldl = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>% 
  select(arm, ldl, All) %>% 
  unique() %>% 
  mutate(Characteristics = '<b>Non HDL Cholesterol</b>, Mean +/- SD (mg/dL)') %>%  
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'ldl') %>%
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


#Proteinuria
#PHHS
yesICD = 1035 +  397 + 281  + 504 
yesCon = 983 + 466 + 228 + 388
noICD = 239 + 414 + 160 + 190
noCon = 210 +505 +129 + 174
UnkICD = 153 +599 + 1512
UnkCon = 240 +440 + 1555
paste0(yesICD, " (", 100*yesICD/5508, "%)")
paste0(noICD, " (", 100*noICD/5508, "%)")
paste0(UnkICD, " (", 100*UnkICD/5508, "%)")

paste0(yesCon, " (", 100*yesCon/5492, "%)")
paste0(noCon, " (", 100*noCon/5492, "%)")
paste0(UnkCon, " (", 100*UnkCon/5492, "%)")

proteinuria <- rbind( data.frame("Characteristics" = c("Yes"), 
                                 "ICD-Pieces" = c("2217( 40.3%)"), 
                                 "Standard of care" = c("2065 (37.6%)"), stringsAsFactors = F,  check.names = FALSE),
       data.frame("Characteristics" = c("No"), 
                  "ICD-Pieces" = c("1003 (18.2%)"), 
                  "Standard of care" = c("1018 (18.5%)"), stringsAsFactors = F,  check.names = FALSE),
       data.frame("Characteristics" = c("Unknown"), 
                  "ICD-Pieces" = c("2264 (41.1%)"), 
                  "Standard of care" = c(" 2235 (40.7%)"), stringsAsFactors = F,  check.names = FALSE))

#THR
load("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/THR/Inputs/proteinuriaCat.RData")

proteinuriaCat <- left_join(x = masterdemo_THR %>% select(pat_id, arm),
                             y = proteinuriaCat %>% select(-enumber), 
                             by = "pat_id") %>% 
  mutate(protenuria = ifelse(is.na(protenuria), "Unknown", protenuria)) %>% unique() %>%
  arrange(pat_id, protenuria) %>% 
  dplyr::filter(!duplicated(pat_id))


#Medication

load(file = '~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Inputs/match_med_generic.RData') %>% as.data.frame()

med_PHHS <- match_med %>% 
  select(pat_id = PatientEpicId,
         pat_enc_csn_id = EncounterEpicCsn,
         order_d = DateValue,
         description = Name,
         PharmaceuticalClass,
         PharmaceuticalSubclass,
         TherapeuticClass) %>% 
  mutate(acetemp         = as.numeric(PharmaceuticalSubclass %in% c('ACE Inhibitors','Angiotensin II Receptor Blockers (ARBs)')),
         statintemp      = as.numeric(PharmaceuticalSubclass %in% c('Antihyperlipidemic - HMG CoA Reductase Inhibitors (statins)')),
         Diureticstemp   = as.numeric(PharmaceuticalClass %in% c('LOOP DIURETICS','POTASSIUM SPARING DIURETICS')),
         betablockertemp = as.numeric(grepl('BETA-ADRENERGIC BLOCKING AGENTS', PharmaceuticalClass) & TherapeuticClass == 'CARDIOVASCULAR' & PharmaceuticalSubclass != 'Antiarrhythmic - Class II'),
         insulintemp     = as.numeric(PharmaceuticalClass %in% ('INSULINS')),
         SGLT2temp       = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('SGLT-2',PharmaceuticalSubclass)),
         GLP1temp        = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('GLP-1',PharmaceuticalSubclass)),
         Noninsulintemp  = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & !grepl('SGLT-2|GLP-1',PharmaceuticalSubclass)))

#prior
prior_meds <- left_join(x = masterdemo_PHHS %>% as.data.frame(),
                        y = left_join(x = masterdemo_PHHS %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y = med_PHHS %>% as.data.frame(),
                                      by = 'pat_id') %>%
                          filter(order_d <= fucens & order_d < enroll_date & enroll_date - order_d <= 365) %>% #-- prior
                          unique(),
                        by = c('pat_id' = 'pat_id', 'enroll_date' = 'enroll_date')) %>%
  mutate_at(vars(ends_with('temp')), ~replace(., is.na(.),0)) %>% # for patients that did not have any medication info
  group_by(pat_id) %>% 
  mutate(ACE          = max(acetemp, na.rm = T),
         STATIN       = max(statintemp, na.rm = T),
         DIURETICS    = max(Diureticstemp, na.rm = T),
         BETABLOCKERS = max(Diureticstemp, na.rm = T),
         INSULIN      = max(insulintemp, na.rm = T),
         SGLT2        = max(SGLT2temp, na.rm = T),
         GLP1         = max(GLP1temp, na.rm = T),
         NONINSULIN   = max(Noninsulintemp, na.rm = T))%>% 
  ungroup() %>% 
  select(pat_id, enroll_date, arm, ACE, STATIN,DIURETICS, BETABLOCKERS, INSULIN, SGLT2, GLP1, NONINSULIN, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>% 
  unique()

med_acei_arb_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                             y = prior_meds %>% 
                               select(pat_id, arm, PharmaceuticalSubclass) %>%
                               filter(grepl('ACE Inhibitor|Angiotensin II', PharmaceuticalSubclass)) %>%
                               unique() %>% 
                               mutate(ACEI_ARB = 'Yes') %>% 
                               select(-PharmaceuticalSubclass) %>% 
                               unique(),
                             by = c('pat_id','arm')) %>% 
  mutate_at(vars(ACEI_ARB), ~replace(., is.na(.),'No')) #if the patient did not have any acei/arb during baseline, mark them no

med_statin_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                           y = prior_meds %>% 
                             select(pat_id, arm, PharmaceuticalSubclass) %>%
                             filter(PharmaceuticalSubclass %in% c('Antihyperlipidemic - HMG CoA Reductase Inhibitors (statins)')) %>% 
                             unique() %>% 
                             mutate(STATIN = 'Yes') %>% 
                             select(-PharmaceuticalSubclass) %>% 
                             unique(),
                           by = c('pat_id','arm')) %>% 
  mutate_at(vars(STATIN), ~replace(., is.na(.),'No'))

med_dieuretics_0_PHHS <-  left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                                     y = prior_meds %>% 
                                       select(pat_id, arm, PharmaceuticalClass) %>%
                                       filter(grepl('DIURETICS', PharmaceuticalClass)) %>% 
                                       unique() %>% 
                                       group_by(pat_id) %>% 
                                       dplyr::mutate(n = n()) %>% 
                                       ungroup() %>% 
                                       mutate(DIURETICS = case_when(n == 1 ~ PharmaceuticalClass, TRUE ~ 'Two or more')) %>% 
                                       select(-PharmaceuticalClass) %>% 
                                       unique(),
                                     by = c('pat_id','arm')) %>% 
  mutate(DIURETICS = ifelse(DIURETICS == 'BETA-BLOCKERS AND THIAZIDE,THIAZIDE-LIKE DIURETICS' |
                              DIURETICS =="POTASSIUM SPARING DIURETICS IN COMBINATION", 'THIAZIDE AND RELATED DIURETICS', DIURETICS)) %>% #Combine those two into one category
  mutate_at(vars(DIURETICS), ~replace(., is.na(.),'NONE')) %>% #if the patient did not have any diuretics, mark them none
  filter(DIURETICS != "OSMOTIC DIURETICS") %>% 
  mutate(DIURETICS = ifelse(DIURETICS == 'NONE', 'No', "Yes")) %>% select(-n)


med_bb_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                       y = prior_meds %>% 
                         select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                         filter(grepl('BETA-ADRENERGIC BLOCKING AGENTS', PharmaceuticalClass) & 
                                  TherapeuticClass == 'CARDIOVASCULAR' & 
                                  PharmaceuticalSubclass != 'Antiarrhythmic - Class II') %>% 
                         unique() %>% 
                         group_by(pat_id) %>% 
                         dplyr::mutate(n = n()) %>% 
                         ungroup() %>% 
                         mutate(BB = case_when(n == 1 ~ PharmaceuticalSubclass, TRUE ~ 'Two or More Beta Blockers')) %>% 
                         select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                         unique(),
                       by = c('pat_id','arm')) %>% 
  mutate_at(vars(BB), ~replace(., is.na(.),'None')) %>% #if the patient did not have any diuretics, mark them none
  mutate(BB = ifelse(BB == "None", "No", "Yes")) %>% select(-n)

med_insulin_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                            y = prior_meds %>% 
                              select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                              filter(PharmaceuticalClass %in% ('INSULINS')) %>% 
                              unique() %>% 
                              mutate(INSULIN = 'Yes') %>% 
                              select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                              unique(),
                            by = c('pat_id','arm')) %>% 
  mutate_at(vars(INSULIN), ~replace(., is.na(.),'No')) 

med_SGLT2_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                          y = prior_meds %>% 
                            select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                            filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('SGLT-2',PharmaceuticalSubclass)) %>% 
                            unique() %>% 
                            mutate(SGLT2 = 'Yes') %>% 
                            select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                            unique(),
                          by = c('pat_id','arm')) %>% 
  mutate_at(vars(SGLT2), ~replace(., is.na(.),'No'))

med_GLP1_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                         y = prior_meds %>% 
                           select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                           filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('GLP-1',PharmaceuticalSubclass)) %>% 
                           unique() %>% 
                           mutate(GLP1 = 'Yes') %>% 
                           select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                           unique(),
                         by = c('pat_id','arm')) %>% 
  mutate_at(vars(GLP1), ~replace(., is.na(.),'No'))

med_othr_agents_diab_0_PHHS <- left_join( x = masterdemo_PHHS %>% select(pat_id, arm),
                                     y = prior_meds %>% 
                                       select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                                       filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & !grepl('SGLT-2|GLP-1',PharmaceuticalSubclass)) %>% 
                                       unique() %>% 
                                       mutate(OtherAgents = 'Yes') %>% 
                                       select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                                       unique(),
                                     by = c('pat_id','arm')) %>% 
  mutate_at(vars(OtherAgents), ~replace(., is.na(.),'No')) #if the patient did not have any insulin, mark them no

# Med THR
setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")


prior_meds = readr::read_delim('Inputs/med.txt', '|', escape_double = FALSE, trim_ws = TRUE)

med_recs <- read.delim(file = "Inputs/med_recs.txt", sep = "|", header = T, fill = T, quote = "", stringsAsFactors = FALSE)

# med_new = readr::read_delim('Inputs/Sept 2020 snapshot/med_June_1_2019_Aug_26_2020.txt', '|', escape_double = FALSE, trim_ws = TRUE)
# med_old = readr::read_delim('Inputs/Source Files for DSMB2020Jan/med_bkfil_july2015_nov212019.txt', '|', escape_double = FALSE, trim_ws = TRUE)
# med <- rbind(med_new, med_old) %>% mutate(order_d = anytime::anydate(ORDERING_DATE))

med_recs1 <- left_join(x = masterdemo_THR1 %>% select(pat_id, enumber, enroll_date, arm),
                       y = med_recs %>% rename(pat_id = PAT_ID),
                       by = "pat_id") %>% select(-ENROLL_DATE) %>% 
  select(-pat_id) %>% 
  rename(pat_id = enumber)

med2 <- med_recs1 %>% dplyr::rename(pat_enc_csn_id = PAT_ENC_CSN_ID, description = DESCRIPTION, order_d = CONTACT_DATE, PharmaceuticalClass = TITLE.1, 
                                    PharmaceuticalSubclass = TITLE.2, TherapeuticClass = TITLE, Taking_YN = TAKING_YN) %>% 
  mutate(acetemp1 = as.numeric(grepl("TRIBENZOR|OLMESARTAN|ZESTRIL|ALTACE|AZILSARTAN|EDARBI|EDARBYCLOR|MOEXIPRIL|UNIRETIC|UNIVASC|ENALAPRIL|EPANED|AVAPRO|
                                       CANDESARTAN|PERINDOPRIL|PRESTALIA|VALSARTAN|ENTRESTO|CAPTOPRIL |LISINOPRIL|RAMIPRIL|TRANDOLAPRIL|CILAZAPRIL|IRBESARTAN|TELMISARTAN|
                                       PRINIVIL|ACCUPRIL|PRINZIDE|MAVIK|MICARDIS|TEVETEN|CAPTOPRIL|EPROSARTAN|QUINAPRIL|DIOVAN|BENICAR|TEVETEN HCT|ATACAND|EXFORGE|AZOR|
                                       MICARDIS-HCT|ATACAND HCT|DIOVAN HCT|ACCURETIC|AVALIDE|ZESTORETIC|BENICAR HCT|EXFORGE HCT|ACEON|FOSINOPRIL|MONOPRIL|VASOTEC|VASERETIC|
                                       TWYNSTA|TARKA|BENAZEPRIL|LOTREL|LOTENSIN HCT|LOTENSIN|LOSARTAN|HYZAAR|COZAAR", description))) %>% 
  
  mutate(acetemp         = as.numeric(PharmaceuticalSubclass %in% toupper(c('ACE INHIBITOR', "ACE INHIBITORS",
                                                                            "ACE INHIBITOR AND DIURETIC COMBINATIONS",
                                                                            "ACE INHIBITOR AND CALCIUM CHANNEL BLOCKER COMBINATIONS",
                                                                            "ANGIOTENSIN II RECEPTOR BLOCKER (ARB)-DIURETIC COMBINATIONS",
                                                                            "ANGIOTENSIN II RECEPTOR BLOCKERS (ARBS)",                            
                                                                            "ANGIOTENSIN II RECEPTOR BLOCKER-NEPRILYSIN INHIBITOR COMB. (ARNI)",
                                                                            "ANGIOTENSIN II RECEPTOR BLOCKER (ARB)-CALCIUM CHANNEL BLOCKER COMB.",
                                                                            "ANGIOTENSIN II RECEPTOR BLOCKER (ARB)-CALCIUM CHANNEL BLOCKER-DIURETIC"))),
         statintemp      = as.numeric(PharmaceuticalSubclass %in% toupper(c('ANTIHYPERLIPIDEMIC - HMG COA REDUCTASE INHIBITORS (STATINS)', 
                                                                            "ANTIHYPERLIPIDEMIC HMG COA REDUCT INHIB AND CALCIUM CHANNEL BLOCKER",
                                                                            "ANTIHYPERLIPIDEMIC - HMG COA REDUCTASE INHIBITOR AND NIACIN COMB"))),
         Diureticstemp   = as.numeric(PharmaceuticalClass %in% c('LOOP DIURETICS','POTASSIUM SPARING DIURETICS',
                                                                 "ACE INHIBITOR AND DIURETIC COMBINATIONS")),
         betablockertemp = as.numeric(grepl('BETA-ADRENERGIC BLOCKING AGENTS', PharmaceuticalClass) & TherapeuticClass == 'CARDIOVASCULAR' & PharmaceuticalSubclass != 'Antiarrhythmic - Class II'),
         insulintemp     = as.numeric(PharmaceuticalClass %in% ('INSULINS')),
         SGLT2temp       = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('SGLT-2',PharmaceuticalSubclass)),
         GLP1temp        = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('GLP-1',PharmaceuticalSubclass)),
         Noninsulintemp  = as.numeric(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & !grepl('SGLT-2|GLP-1',PharmaceuticalSubclass)))%>% 
  mutate(Taking_YN = "Y") %>% 
  mutate(order_d = as.Date(parse_date_time(order_d, orders = "mdyHMS"))) %>%
  mutate(diff = difftime(order_d, enroll_date, units = "days")) %>% 
  select(-arm)

med_statin_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                               y = med2 %>% filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                 select(pat_id, statintemp) %>%
                                 filter(statintemp == 1) %>% 
                                 unique() %>% 
                                 mutate(STATIN = 'Yes') %>% 
                                 select(-statintemp) %>% 
                                 unique(),
                               by = c('pat_id')) %>% 
  mutate_at(vars(STATIN), ~replace(., is.na(.),'No'))
  

med_acei_arb_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                                 y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                   select(pat_id, acetemp) %>%
                                   filter(acetemp == 1) %>% 
                                   unique() %>% 
                                   mutate(ACEI_ARB = 'Yes') %>% 
                                   select(-acetemp) %>% 
                                   unique(),
                                 by = c('pat_id')) %>% 
  mutate_at(vars(ACEI_ARB), ~replace(., is.na(.),'No'))


med_dieuretics_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                                   y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                     select(pat_id, Diureticstemp) %>%
                                     filter(Diureticstemp == 1) %>% 
                                     unique() %>% 
                                     mutate(DIURETICS = 'Yes') %>% 
                                     select(-Diureticstemp) %>% 
                                     unique(),
                                   by = c('pat_id')) %>% 
  mutate_at(vars(DIURETICS), ~replace(., is.na(.),'No'))


med_bb_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                           y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                             select(pat_id, betablockertemp) %>%
                             filter(betablockertemp == 1) %>% 
                             unique() %>% 
                             mutate(BB = 'Yes') %>% 
                             select(-betablockertemp) %>% 
                             unique(),
                           by = c('pat_id')) %>% 
  mutate_at(vars(BB), ~replace(., is.na(.),'No'))

med_insulin_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                                y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                  select(pat_id, insulintemp) %>%
                                  filter(insulintemp == 1) %>% 
                                  unique() %>% 
                                  mutate(INSULIN = 'Yes') %>% 
                                  select(-insulintemp) %>% 
                                  unique(),
                                by = c('pat_id')) %>% 
  mutate_at(vars(INSULIN), ~replace(., is.na(.),'No'))

med_SGLT2_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                              y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                select(pat_id, SGLT2temp) %>%
                                filter(SGLT2temp == 1) %>% 
                                unique() %>% 
                                mutate(SGLT2 = 'Yes') %>% 
                                select(-SGLT2temp) %>% 
                                unique(),
                              by = c('pat_id')) %>% 
  mutate_at(vars(SGLT2), ~replace(., is.na(.),'No'))

med_GLP1_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                             y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                               select(pat_id, GLP1temp) %>%
                               filter(GLP1temp == 1) %>% 
                               unique() %>% 
                               mutate(GLP1 = 'Yes') %>% 
                               select(-GLP1temp) %>% 
                               unique(),
                             by = c('pat_id')) %>% 
  mutate_at(vars(GLP1), ~replace(., is.na(.),'No'))

med_othr_agents_diab_0_THR <- left_join( x = masterdemo_THR %>% select(pat_id, arm),
                                         y = med2 %>%  filter(order_d < enroll_date & enroll_date - order_d <= 365) %>% 
                                           select(pat_id, Noninsulintemp) %>%
                                           filter(Noninsulintemp == 1) %>% 
                                           unique() %>% 
                                           mutate(OtherAgents = 'Yes') %>% 
                                           select(-Noninsulintemp) %>% 
                                           unique(),
                                         by = c('pat_id')) %>% 
  mutate_at(vars(OtherAgents), ~replace(., is.na(.),'No'))


#ProHealth 
setwd('/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/')
load("Inputs/Med_baseline.RData")

med_statin_0_ProHealth <- Statin_baseline %>% 
  mutate(STATIN = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, STATIN)

med_acei_arb_0_ProHealth <- ACEARB_baseline %>% 
  mutate(ACEI_ARB = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, ACEI_ARB)

med_dieuretics_0_ProHealth <- Duiretics_baseline %>% 
  mutate(DIURETICS = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, DIURETICS)

med_bb_0_ProHealth <- bb_baseline %>% 
  mutate(BB = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, BB)

med_insulin_0_ProHealth <- Insulin_baseline %>% 
  mutate(INSULIN = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, INSULIN)

med_SGLT2_0_ProHealth <- SLGT2_baseline %>% 
  mutate(SGLT2 = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, SGLT2)

med_GLP1_0_ProHealth <- GLP1_baseline %>% 
  mutate(GLP1 = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, GLP1)

med_othr_agents_diab_0_ProHealth <- NonIns_baseline %>% 
  mutate(OtherAgents = ifelse(Taking_YN == "Y", "Yes", "No")) %>% select(pat_id = Study_id, arm, OtherAgents)

# VA
setwd('/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/')
# save(Statin, acei_arb, dieuretics, bb, ins, SGLT2, GLP, NonIns,
#      file = "Inputs/Med_baseline.RData")
load("Inputs/Med_baseline.RData")


med_statin_0_VA <- Statin %>% 
  mutate(STATIN = ifelse(statin == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, STATIN)

med_acei_arb_0_VA <- acei_arb %>% 
  mutate(ACEI_ARB = ifelse(ACE_ARB == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, ACEI_ARB)

med_dieuretics_0_VA <- dieuretics %>% 
  mutate(DIURETICS = ifelse(Diuretic == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, DIURETICS)

med_bb_0_VA <- bb %>% 
  mutate(BB = ifelse(BB == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, BB)

med_insulin_0_VA <- ins %>% 
  mutate(INSULIN = ifelse(INS == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, INSULIN)

med_SGLT2_0_VA <- SGLT2 %>% 
  mutate(SGLT2 = ifelse(SGLT2 == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, SGLT2)

med_GLP1_0_VA <- GLP %>% 
  mutate(GLP1 = ifelse(GLP == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, GLP1)

med_othr_agents_diab_0_VA <- NonIns %>% 
  mutate(OtherAgents = ifelse(NonIns == 1, "Yes", "No")) %>% select(pat_id = Column1, arm = Arm, OtherAgents)


#Combine 

med_statin_0 <- rbind(med_statin_0_PHHS, med_statin_0_THR, med_statin_0_ProHealth, med_statin_0_VA)
med_statin <- med_statin_0 %>%
  select(pat_id, arm, STATIN) %>% 
  unique() %>% 
  group_by(arm, STATIN) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(STATIN != 'No') %>% 
  select(-n)%>% 
  mutate(Characteristics = 'Statin') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))%>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_acei_arb_0 <- rbind(med_acei_arb_0_PHHS, med_acei_arb_0_THR, med_acei_arb_0_ProHealth, med_acei_arb_0_VA)
med_acei_arb <- med_acei_arb_0 %>%
  select(pat_id, arm, ACEI_ARB) %>% 
  unique() %>% 
  group_by(arm, ACEI_ARB) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(ACEI_ARB == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'ACEi/ARB') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_dieuretics_0 <- rbind(med_dieuretics_0_PHHS , med_dieuretics_0_THR, med_dieuretics_0_ProHealth, med_dieuretics_0_VA)
med_dieuretics <- med_dieuretics_0 %>% # medication prescription during baseline
  select(pat_id, arm, DIURETICS) %>% 
  unique() %>% 
  group_by(arm, DIURETICS) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(DIURETICS == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'Any Diuretics') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)


med_bb_0 <- rbind(med_bb_0_PHHS , med_bb_0_THR, med_bb_0_ProHealth, med_bb_0_VA)
med_bb<- med_bb_0 %>% # medication prescription during baseline
  select(pat_id, arm, BB) %>% 
  unique() %>% 
  group_by(arm, BB) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(BB == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'Any Beta Blockers') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_insulin_0 <- rbind(med_insulin_0_PHHS , med_insulin_0_THR, med_insulin_0_ProHealth, med_insulin_0_VA)
med_insulin <- med_insulin_0 %>% # medication prescription during baseline
  select(pat_id, arm, INSULIN) %>% 
  unique() %>% 
  group_by(arm, INSULIN) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(INSULIN == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'Insulin') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_SGLT2_0 <- rbind(med_SGLT2_0_PHHS , med_SGLT2_0_THR, med_SGLT2_0_ProHealth, med_SGLT2_0_VA)
med_SGLT2 <- med_SGLT2_0 %>% # medication prescription during baseline
  select(pat_id, arm, SGLT2) %>% 
  unique() %>% 
  group_by(arm, SGLT2) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(SGLT2 == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'SGLT-2 Inhibitor') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_GLP1_0 <- rbind(med_GLP1_0_PHHS , med_GLP1_0_THR, med_GLP1_0_ProHealth, med_GLP1_0_VA)
med_GLP1 <- med_GLP1_0 %>% # medication prescription during baseline
  select(pat_id, arm, GLP1) %>% 
  unique() %>% 
  group_by(arm, GLP1) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(GLP1 == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'GLP-1 Receptor Agonist') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

med_othr_agents_diab_0 <- rbind(med_othr_agents_diab_0_PHHS , med_othr_agents_diab_0_THR, med_othr_agents_diab_0_ProHealth, med_othr_agents_diab_0_VA)
med_othr_agents_diab <- med_othr_agents_diab_0 %>% # medication prescription during baseline
  select(pat_id, arm, OtherAgents) %>% 
  unique() %>% 
  group_by(arm, OtherAgents) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(OtherAgents == 'Yes') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'Other non-insulin agents for Diabetes') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

# Comorbidities
comorbid_list = list(Myocardial_Infarction = comorbidity::icd10cm_2018 %>%
                       as.data.frame() %>%
                       filter(grepl('^I21|^I22|^I252', Code)) %>%
                       select(Code) %>% 
                       unlist() %>%
                       str_trim(., side = 'both') %>% # removes leading and trailing white spaces
                       unname(),
                     
                     Heart_Failure        =  comorbidity::icd10cm_2018 %>%
                       as.data.frame() %>%
                       filter(grepl('^I099|^I110|^I130|^I132|^I255|^I420|^I425|^I426|^I427|^I428|^I429|^I43|^I50|^P290', Code)) %>%
                       select(Code) %>% 
                       unlist() %>%
                       str_trim(., side = 'both') %>%  # removes leading and trailing white spaces
                       unname()
)

#problem_list
path = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/"

load(file = paste0(path, 'Inputs/pl.RData')) %>% as.data.frame()

problem_list_PHHS <- left_join(x = masterdemo_PHHS %>% select(pat_id, enroll_date, arm),
                                        y = pl %>% select(pat_id = PatientEpicId,
                                                          Diag_date = DateValue,
                                                          Code = DTValue),
                                        by = 'pat_id') %>% 
                            filter(Diag_date <= enroll_date) %>% 
                            select(pat_id, Code) %>%
                            mutate(Code = gsub(Code, pattern = '\\.',replacement = ''))

rm(pl)
# setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
load(file= '~/T-Drive/NIH Chronic Disease Management/Data/Shelley Handoff/THR SHELLEY/match_pl-upto20190921.RData')

problem_list_THR <- left_join(x = masterdemo_THR %>% select(pat_id, enroll_date, arm),
                                        y = pl %>% select(-pat_id) %>% rename(pat_id = enumber) %>%  select(pat_id,
                                                          Diag_date = date_of_entry,
                                                          Code = icd10_code),
                                        by = 'pat_id') %>% na.omit() %>% 
                            filter(Diag_date <= enroll_date) %>% 
                            select(pat_id, Code) %>%
                            mutate(Code = gsub(Code, pattern = '\\.',replacement = ''))

s <- strsplit(problem_list_THR$Code, split = ",")
problem_list_THR <- data.frame(pat_id = rep(problem_list_THR$pat_id, sapply(s, length)), Code = unlist(s)) %>% 
  na.omit() %>% unique()

rm(pl)

# ProHealth problem list
ckd_problems <- readr::read_delim('/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/ProHealth/Inputs/ckd_problems.txt', '\t', escape_double = FALSE, trim_ws = TRUE)

problem_list_ProHealth <- left_join(x = masterdemo_ProHealth %>% select(pat_id, enroll_date, arm),
                                        y = ckd_problems %>% select(pat_id = study_id,
                                                                    Diag_date = rand_CreateDate,
                                                                    Code = ICD10DiagnosisCode) %>% 
                                          mutate(Diag_date = as.Date(strptime(Diag_date,"%b %d %Y %H:%M%p"))),
                                        by = 'pat_id') %>% 
                            filter(Diag_date <= enroll_date) %>% 
                            select(pat_id, Code) %>%
                            mutate(Code = gsub(Code, pattern = '\\.',replacement = ''))
                          
s <- strsplit(problem_list_ProHealth$Code, split = ",")
problem_list_ProHealth <- data.frame(pat_id = rep(problem_list_ProHealth$pat_id, sapply(s, length)), Code = unlist(s)) %>% 
  na.omit() %>% unique()

#VA problem list
FinalProblemListsExport <- read.csv("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/ICD_Pieces/VA/Inputs/FinalProblemListsExport.csv", stringsAsFactors = F)

pl_9 <- FinalProblemListsExport %>% select(ABC_ID, ICD9code) %>% na.omit() %>% 
  mutate(ICD10code = icd_map(ICD9code)) %>% 
  mutate(ICD10code = strsplit(as.character(ICD10code), ",")) %>% 
  unnest(ICD10code)
# mutate_all(~ gsub(., pattern = '\\.', replacement = '')) #%>% 


pl_10 <- FinalProblemListsExport %>% select(ABC_ID, ICD10code) %>%filter(ICD10code != "NULL") %>% 
  mutate_all(~ gsub(., pattern = '\\.', replacement = ''))

problem_list_VA <- rbind(x = pl_9 %>% select(ABC_ID, ICD10code), y = pl_10) %>% select(pat_id = ABC_ID, Code = ICD10code)


# Combine problem list
problem_list <- rbind(problem_list_PHHS, problem_list_THR, problem_list_ProHealth, problem_list_VA)

charlson10 <- left_join( y = comorbidity(x = problem_list, #computes charlson comorbidity score
                                         id = 'pat_id',
                                         code = 'Code',
                                         score = 'charlson',
                                         icd = 'icd10',
                                         assign0 = FALSE),
                         x = masterdemo %>% select(pat_id, arm), #to retrieve arms of all patients
                         by = 'pat_id')

charlson10[is.na(charlson10)] <- 0

charlson10_AgeAdj <- left_join(x = charlson10, y = masterdemo %>% select(pat_id, age),
                               by = 'pat_id') %>% 
  mutate(ageAdj = case_when(age >= 50 & age < 60 ~ 1,
                            age >= 60 & age < 70 ~ 2,
                            age >= 70 & age < 80 ~ 3,
                            age >= 80 ~ 4,
                            TRUE ~ 0)) %>%
  mutate(ageAdjScore = wscore + ageAdj) %>% 
  select(-c(arm, age, ageAdj))

CAD <- problem_list %>% mutate(Code = ifelse(grepl("^I21|^I22|^I23|^I25", Code), "1", "0")) %>% 
  group_by(pat_id)  %>% 
  summarise_each(max) 

CAD_prop <- left_join(x = left_join(x = masterdemo %>% select(pat_id, arm),
                                    y = CAD,
                                    by = "pat_id") %>%
                        group_by(arm, Code) %>%
                        dplyr::summarise(n = n()),
                      y = left_join(x = masterdemo %>% select(pat_id, arm),
                                    y = CAD,
                                    by = "pat_id") %>%
                        group_by(arm) %>%
                        dplyr::summarise(total = n()),
                      by = "arm") %>% 
  mutate(percentage = paste0(n, " (", round(100*n/total, 1), "%)")) %>% 
  filter(Code == 1) %>% 
  mutate(Characteristics = 'Coronary artery disease') %>% 
  reshape2::dcast(Characteristics ~ arm, value.var = "percentage")

comorb_prop <- inner_join( x = charlson10 %>% 
                             group_by(arm) %>%
                             dplyr::summarise_at(vars(ami:aids), ~ (paste0(sum(.), " (", round((sum(.)/length(.))*100,1), "%)"))) %>% 
                             select(ami:aids) %>% 
                             t() %>%
                             as.data.table(keep.rownames = T) %>%
                             as.data.frame() %>% 
                             dplyr::rename(Disease = rn,
                                           A = V1,
                                           B = V2),
                           y = charlson10 %>% 
                             select(ami:aids) %>%
                             summarise_all(~(paste0(sum(.), " (", round((sum(.)/length(.))*100,1), "%)"))) %>% 
                             select(ami:aids) %>% 
                             t() %>% 
                             as.data.table(keep.rownames = T) %>%
                             as.data.frame() %>% 
                             dplyr::rename(Disease = rn,
                                           All = V1),
                           by = 'Disease') %>% 
  mutate(Disease = case_when(Disease == 'ami' ~ 'Myocardial infarction',
                             Disease == 'chf' ~ 'Congestive heart failure',
                             Disease == 'pvd' ~ 'Peripheral vascular disease',
                             Disease == 'cevd' ~ 'Cerebrovascular disease',
                             Disease == 'dementia' ~ 'Dementia',
                             Disease == 'copd' ~ 'Chronic obstructive pulmonary disease ',
                             Disease == 'rheumd' ~ 'Rheumatoid disease',
                             Disease == 'pud' ~ 'Peptic ulcer disease',
                             Disease == 'mld' ~ 'Mild liver disease',
                             Disease == 'msld' ~ 'Moderate or severe liver disease',
                             Disease == 'diab' ~ 'Diabetes without chronic complications',
                             Disease == 'diabwc' ~ 'Diabetes with chronic complications',
                             Disease == 'hp' ~ 'Hemiplegia or paraplegia',
                             Disease == 'rend' ~ 'Renal disease',
                             Disease == 'canc' ~ 'Cancer (any malignancy)',
                             Disease == 'metacanc' ~ 'Metastatic solid tumour',
                             Disease == 'aids' ~ 'AIDS/HIV')) %>% 
  dplyr::rename(Characteristics = Disease, `Standard of care` = A, `ICD-Pieces` = B) %>% 
  mutate_all(., ~lapply(.,as.character)) %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

comorb_prop <- comorb_prop[c(2, 3, 4),]

comorb_score <-  left_join(x = masterdemo %>% select(pat_id, arm),
                           y = charlson10_AgeAdj,
                           by = "pat_id")%>% 
  group_by(arm) %>%
  mutate(score_median = paste0(round(mean(ageAdjScore, na.rm = T),1), " +/- " , round(sd(ageAdjScore), 1))) %>% 
  ungroup() %>% 
  mutate(All = paste0(round(mean(ageAdjScore, na.rm = T),1), " +/- " , round(sd(ageAdjScore), 1)))  %>%
  select(arm, score_median, All) %>% 
  unique() %>% 
  mutate(Characteristics ='Age adjusted Charlson Comorbidity Score (Mean +/- SD)') %>%
  reshape2::dcast(Characteristics + All ~ arm , value.var = 'score_median') %>% 
  mutate_all(., ~lapply(., as.character))  %>% 
  select(Characteristics, `ICD-Pieces`, `Standard of care`)

baseline_characteristics <- rbind(Age_Mean,
                                  Male,
                                  data.frame("Characteristics" = c("<b>Ethnicity</b>"), 
                                             "ICD-Pieces" = c(""), 
                                             "Standard of care" = c(""), stringsAsFactors = F,  check.names = FALSE),
                                  Ethnicity,
                                  data.frame("Characteristics" = c("<b>Race</b>"), 
                                             "ICD-Pieces" = c(""), 
                                             "Standard of care" = c(""), stringsAsFactors = F,  check.names = FALSE),
                                  Race,
                                  data.frame("Characteristics" = c("<b>Blood Pressure</b>"), 
                                             "ICD-Pieces" = c(""), 
                                             "Standard of care" = c(""), stringsAsFactors = F,  check.names = FALSE),
                                  Bp_mean_SBP,
                                  Bp_mean_DBP,
                                  a1c_mean,
                                  egfr_mean,
                                  BMI,
                                  Weight,
                                  cholest_tot_prop,
                                  cholest_ldl_prop,
                                  data.frame("Characteristics" = c("<b>Proteinuria</b>"),
                                             "ICD-Pieces" = c(""),
                                             "Standard of care" = c(""),stringsAsFactors = F,  check.names = FALSE),
                                  proteinuria,
                                  data.frame("Characteristics" = c("<b>Medication</b>"),
                                             "ICD-Pieces" = c(""),
                                             "Standard of care" = c(""),stringsAsFactors = F,  check.names = FALSE),
                                  med_statin,
                                  med_acei_arb,
                                  med_dieuretics,
                                  med_bb,
                                  med_insulin,
                                  med_SGLT2,
                                  med_GLP1,
                                  med_othr_agents_diab,
                                  data.frame("Characteristics" = c("<b>Comorbidities</b>"), 
                                             "ICD-Pieces" = c(""), 
                                             "Standard of care" = c(""),stringsAsFactors = F,  check.names = FALSE),
                                  comorb_score,
                                  CAD_prop,
                                  comorb_prop) 
colnames(baseline_characteristics) <- c("Characteristics", paste0("ICD-Pieces <br>intervention <br>(n = ", format(as.numeric(Count$`ICD-Pieces`), big.mark=",") , ")"),
                                        paste0("Standard care <br>(n = ", format(as.numeric(Count$`Standard of care`),  big.mark=","), ")"))

DoubleLine <- c(1, 2, 6, 12, 15, 16:21, 25, 34)
# DoubleLine <- c(1, 2, 6, 12, 15, 16)


bsln_arm <- kable(baseline_characteristics,
                  style = 'html', escape = F, align = "c",
                  caption = 'Baseline Characteristics: aggregated date from all 4 sites',
                  booktabs = T,
                  row.names = NA) %>%
  kable_classic_2(full_width = F,
                  html_font = 'Montserrat') %>%
  row_spec(0, bold = T, extra_css = "text-align:left; border-right: solid; border-left: solid;") %>%
  row_spec(DoubleLine, extra_css = "border-bottom-style: double; background-color: white;") %>%
  column_spec(c(1, 2, 3, 4),  extra_css = "border-right: solid; border-left: solid;") %>%
  column_spec(2, extra_css = "text-align:left;") %>%
  collapse_rows(1) %>%   
  kable_styling(latex_options = "scale_down", font_size = 15) %>%
  row_spec(0, extra_css = "vertical-align: middle;") %>% 
  row_spec(0, background = "lightskyblue")

bsln_arm <- remove_column(bsln_arm, 1)
bsln_arm

save(masterdemo, masterdemo_PHHS, masterdemo_THR, masterdemo_ProHealth, masterdemo_VA,
     weight_bmi_bsln, bp_bsln, a1c_bsln, egfr_bsln, 
     cholest_tot, cholest_ldl, 
     med_statin_0, med_acei_arb_0, med_dieuretics_0, 
     med_bb_0, med_insulin_0, med_SGLT2_0, med_GLP1_0, 
     med_othr_agents_diab_0, problem_list,
     Count, 
     Age_Mean,
     Male,
     Ethnicity,
     Race,
     Bp_mean_SBP,
     Bp_mean_DBP,
     a1c_mean,
     egfr_mean,
     BMI,
     Weight,
     cholest_tot_prop,
     cholest_ldl_prop,
     proteinuria,
     med_statin,
     med_acei_arb,
     med_dieuretics,
     med_bb,
     med_insulin,
     med_SGLT2,
     med_GLP1,
     med_othr_agents_diab,
     comorb_score,
     CAD_prop,
     comorb_prop,
     file = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Table1Components_v5.RData")

save(masterdemo, masterdemo_PHHS, masterdemo_THR, masterdemo_THR1, masterdemo_ProHealth, masterdemo_VA, 
     file = "~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Outputs/Masterdemos.RData")
# 
# 
# # Adverse Events THR
# setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/THR")
# 
# enr = data.table(data.frame(masterdemo_widewnewid))
# 
# raw0 = data.table(read.delim('Inputs/NIH_CONTROL_GROUP_RD03212019_addendum9missingpatients.tab', header = T, sep = '\t') %>% data.table)
# raw1 = data.table(read.delim('Inputs/NIH_CONTROL_GROUP_RD03062019.tab', header = T, sep = '\t') %>% data.table)
# raw2 = data.table(read.delim('Inputs/NIH_CONTROL_GROUP_RD02192019.tab', header = T, sep = '\t') %>% data.table)
# raw3 = data.table(read.delim('Inputs/NIH_ENCOUNTERS_USE_08082018.tab', header = T, sep = '\t') %>% data.table)
# raw4 = data.table(read.delim('Inputs/NIH_FILE_11052019_rd11132019_part1.tab', header = T, sep = '\t') %>% data.table)
# raw5 = data.table(read.delim('Inputs/NIH_FILE_11212019_rd11262019_part2.tab', header = T, sep = '\t') %>% data.table)
# raw6 = data.table(read.delim('Inputs/NIH_THR_12_2019_RD10262020.tab', header = T, sep = '\t') %>% data.table)
# raw7 = readxl::read_excel('Inputs/THR_DFWHC_MatchResults_2021-01-13.xlsx') %>%  #DFWHC Data
#   filter(CMS_DRG != "NULL")
# 
# #combine files (THR Hosp data)
# 
# names(raw0)<- names(raw3)
# names(raw1)<- names(raw3)
# names(raw2)<- names(raw3)
# names(raw4)<- names(raw3)
# names(raw5)<- names(raw3)
# names(raw6)<- names(raw3)
# 
# rf_thr <- unique(data.table(rbind(raw0, raw1, raw2, raw3 , raw4, raw5, raw6))) %>%
#   as.data.frame() %>%  
#   mutate(ADMISSION_DATE = anytime::anydate(ADMISSIONDATE),
#          DISCHARGE_DATE = anytime::anydate(DISCHARGEDATE)) %>% 
#   select(ENUMBER,
#          PAT_NAME,
#          ADMISSION_DATE,
#          DISCHARGE_DATE,
#          DISCHARGEDISPOSITION,
#          DISCHARGEDISPOSITION_DESC,
#          contains('DX'),
#          contains('PX')) %>%
#   unique() %>% 
#   arrange(ENUMBER, PAT_NAME, ADMISSION_DATE, DISCHARGE_DATE) %>% 
#   mutate(X17_ICD10PXCODE = NA, #since these column does not exist, we need to add these to properly merge(row_bind)
#          X18_ICD10PXCODE = NA,
#          X19_ICD10PXCODE = NA,
#          X20_ICD10PXCODE = NA,
#          X21_ICD10PXCODE = NA,
#          X22_ICD10PXCODE = NA,
#          X23_ICD10PXCODE = NA,
#          X24_ICD10PXCODE = NA,
#          X25_ICD10PXCODE = NA ) %>% 
#   mutate(across(where(is.character), ~na_if(., "$null$")))
# 
# 
# rf_dfw <- raw7 %>% as.data.frame() %>% 
#   mutate(ADMISSION_DATE = lubridate::as_date(Admit_Date),
#          DISCHARGE_DATE = lubridate::as_date(Discharge_Date),
#          PAT_NAME = paste0(ProvidedLastName,',', ProvidedFirstName),
#          DISCHARGEDISPOSITION = NA) %>% #since this column does not exist, we need to add this to properly merge(row_bind)
#   rename(DISCHARGEDISPOSITION_DESC = DischStatus,
#          ENUMBER = THR_MRN) %>% 
#   select(ENUMBER,
#          PAT_NAME,
#          ADMISSION_DATE,
#          DISCHARGE_DATE,
#          DISCHARGEDISPOSITION,
#          DISCHARGEDISPOSITION_DESC,
#          PrinDx1,
#          contains('Diag'),
#          Admitcode,
#          ECode01,
#          ECode02,
#          ECode03,
#          ECode04,
#          ECode05,
#          ECode06,
#          ECode07,
#          contains('Proc')) %>%
#   filter(!is.na(ENUMBER)) %>% 
#   unique() %>% 
#   mutate(across(where(is.character), ~na_if(., "NULL")))
# 
# names(rf_dfw) <- names(rf_thr)
# rf <- rbind(rf_thr, rf_dfw) %>% unique()
# 
# re <- inner_join(x  = masterdemo_THR,
#                  y  = rf,
#                  by = c('pat_id' = 'ENUMBER')) %>% 
#   mutate(DISCHARGEDISPOSITION_DESC = case_when(DISCHARGEDISPOSITION == '01'  ~  'DISCHARGED TO HOME OR SELF CARE' ,
#                                                DISCHARGEDISPOSITION == '02'  ~  'DISCHARGED/TRANSFERRED TO OTHER FACILITY' ,
#                                                DISCHARGEDISPOSITION == '03'  ~  'DISCHARGED/TRANSFERRED TO SNF' ,
#                                                DISCHARGEDISPOSITION == '04'  ~  'DISCHARGED/TRANSFERRED TO ICF' ,
#                                                DISCHARGEDISPOSITION == '05'  ~  'DSCHRD/XFERED CANCER CTR/CHILDRN HOSP',
#                                                DISCHARGEDISPOSITION == '06'  ~  'DISCHARGED TO HOME HEALTH ORG.',
#                                                DISCHARGEDISPOSITION == '07'  ~  'LEFT AGAINST MEDICAL ADVICE',
#                                                DISCHARGEDISPOSITION == '20'  ~  'EXPIRED',
#                                                DISCHARGEDISPOSITION == '21'  ~  'DISCH/TRANS TO COURT/LAW ENFORCE',
#                                                DISCHARGEDISPOSITION == '43'  ~  'DISCHARGED/TRANSFERRED TO FEDERAL HOSP',
#                                                DISCHARGEDISPOSITION == '50'  ~  'DISCHARGED TO HOSPICE-HOME',
#                                                DISCHARGEDISPOSITION == '51'  ~  'DISCHARGED TO HOSPICE-MEDICAL FACILITY',
#                                                DISCHARGEDISPOSITION == '61'  ~  'DISCHRGD/TRANSFRD TO SWING BED',
#                                                DISCHARGEDISPOSITION == '62'  ~  'DSCHRGD/TRNSFRD TO ANOTHER REHAB FACILTY',
#                                                DISCHARGEDISPOSITION == '63'  ~  'DSCHRGD/TRNSFRD TO A LTC HOSPITAL',
#                                                DISCHARGEDISPOSITION == '64'  ~  'DISCHRGD/TRNSFRD TO A NURSING FACILITY M',
#                                                DISCHARGEDISPOSITION == '65'  ~  'DISCHARGED/TRANSFERRED TO PSYCH HOSP',
#                                                TRUE ~ as.character(DISCHARGEDISPOSITION_DESC))) %>% 
#   mutate( DISCHARGEDISPOSITION_DESC =  str_to_upper(DISCHARGEDISPOSITION_DESC))
# 
# 
# claims_enrolled = re
# claims_enrolled_wAdmitOnly = re %>% filter(!is.na(ADMISSION_DATE))  
# 
# rec_THR <- claims_enrolled_wAdmitOnly %>% 
#   filter(ADMISSION_DATE >= enroll_date & ADMISSION_DATE <= fucens & ADMISSION_DATE <= enroll_date + 365) %>% 
#   mutate(Complete = ifelse(enroll_date + 365 <= fucens, 1 , 0)) %>% 
#   as.data.table()
# 
# colrec <- c(colnames(rec_THR)[rec_THR[, str_detect(names(rec_THR),"DX")]]) # get columns that contains 'DX' as column names
# rec_THR[,(colrec) := lapply(.SD, function(x) {gsub("\\.", "", x)}), .SDcols = colrec] # #remove periods
# 
# for ( i in seq_along(names(adv_icd_named))) { #adv_icd_named is saved both as a list and a csv file in working directory 
#   print(names(adv_icd_named)[i])
#   res = rec_THR[, lapply(.SD, grepl, pattern = paste(adv_icd_named[[i]], collapse = '|')), .SDcols = c(colrec)]
#   res_max = apply(res, 1, max)  # rowwise maximum
#   rec_THR[ , (names(adv_icd_named)[i]) :=  res_max  ]
# }
# 
# rs_THR <- as.data.frame(rec_THR) %>% 
#   select(pat_id,
#          enroll_date,
#          arm,
#          ADMISSION_DATE,
#          Discharge.Date = DISCHARGE_DATE,
#          names(adv_icd_named)) %>%    # adverse events      
#   unique() %>% 
#   arrange(pat_id, Discharge.Date)
# 
# dedup_THR <- rs_THR %>%
#   group_by(pat_id, Discharge.Date) %>%
#   mutate(hyperkalemia_max    = max(hyperkalemia)   ,
#          hyponatremia_max    = max(hyponatremia)   ,
#          syncope_max         = max(syncope)        ,
#          hypotension_max     = max(hypotension)    ,
#          drugtoxicity_max    = max(drugtoxicity)   ,
#          AKI_max             = max(AKI)            ,
#          hypoglycemia_max    = max(hypoglycemia)   ,
#          rhabdomyolysis_max  = max(rhabdomyolysis) ,
#          myositis_max        = max(myositis)       ,
#          fluidoverload_max   = max(fluidoverload)  ,
#          excluded_max        = max(excluded)       ,
#          cellulitis_max      = max(cellulitis)     ,
#          sepicshock_max      = max(sepicshock)     ,
#          stroke_max          = max(stroke)         ,
#          dialysis_max        = max(dialysis)) %>% 
#   ungroup() %>% 
#   arrange(pat_id, Discharge.Date) %>%
#   filter(!duplicated(cbind(pat_id, Discharge.Date)))
# dedup_THR <- dedup_THR %>% 
#   select(-ADMISSION_DATE, -enroll_date, -Discharge.Date, -hyperkalemia, -hyponatremia, -syncope, -hypotension, -drugtoxicity, -AKI, 
#          hypoglycemia, -rhabdomyolysis, -myositis, -fluidoverload, -excluded, -cellulitis, -sepicshock, -stroke, -dialysis, -hypoglycemia)
# #Adverse Events PHHS
# setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
# 
# # Load data sets: Hospbody, DX_flat, PX_flat, Deaths, Masterdemo, ICD-10 codes for Adverse Events
# load(file  = 'Inputs/ahrq_phhs_HospBody_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>% as.data.frame()
# load(file  = 'Inputs/ahrq_phhs_DX_flat_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
# load(file  = 'Inputs/ahrq_phhs_PX_flat_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
# load(file  = 'Inputs/ahrq_phhs_Deaths_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
# 
# enrollcens  = as.Date('2019-06-28')   #last day qualifying
# enrollstart = as.Date('2016-07-06')
# fucens      = as.Date('2020-06-30')
# startdate   = as.Date('2015-07-01')  #start of study (used to look back 12 months prior to start of study)
# enddate     = fucens                 #time of outcomes follow-up censor
# 
# 
# 
# #DFWHC data. Pull All Pat_MRN_IDs that are also in masterdemo
# 
# # dfhc <- readxl::read_excel('Inputs/Parkland_DFWHC_MatchResults_2021-01-19.xlsx') %>%  #DFWHC Data
# dfhc <- readxl::read_excel('~/T-Drive/NIH Chronic Disease Management/Data/DSMB data/OneDrive_1_5-11-2021/Parkland_DFWHC_MatchResults_2021-05-10.xlsx') %>%  #DFWHC Data
#   as.data.frame() %>% 
#   filter(Pat_MRN_ID %in% masterdemo_PHHS1$patient_mrn) %>% 
#   filter(CMS_DRG != "NULL") # filter just inpatients
# 
# dfhc_hosp <- dfhc %>%
#   select( Pat_MRN_ID, Birth_Date, DischargeAge, Ptsex, RaceCodeDesc, Discharge_Quarter, LOS, PayCodeDesc1,  Patient_Zip_Code, CMS_DRG , Admit_Source,
#           DischargeStatus, Discharge_Date, Admit_Date ,
#           PProc01, Proc02, Proc03, Proc04, Proc05, Proc06, Proc07, Proc08, Proc09, Proc10, Proc11, Proc12, Proc13, Proc14, Proc15, Proc16, Proc17, Proc18, Proc19, 
#           Proc20, Proc21, Proc22, Proc23, Proc24, Proc25, PrinDx1, Diag02, Diag03, Diag04, Diag05, Diag06, Diag07, Diag08, Diag09, Diag10, Diag11, Diag12, Diag13, 
#           Diag14, Diag15, Diag16, Diag17, Diag18, Diag19, Diag20, Diag21, Diag22, Diag23, Diag24, Diag25, Admitcode, ECode01, ECode02, ECode03, ECode04, Is_observation_room) %>% 
#   mutate( Discharge_Date = as.Date(Discharge_Date, format = '%m/%d/%Y'),
#           Admit_Date     = as.Date(Admit_Date,     format = '%m/%d/%Y'),
#           Birth_Date     = as.Date(Birth_Date,     format = '%m/%d/%Y'))
# 
# nam <- c( 'patient_mrn', 'BIRTH_DATE', 'AGE_YRS', 'SEX', 'PATIENT_RACE_C', 'DQTR', 'LOS',  'RPT_GRP_SIX_NAME',  'HOSPSTCO', 'MS_DRG', 'ADM_SOURCE_NAME',  'DISCH_DISP_NAME',  'DISCH_DATE_TIME',  'ADM_DATE_TIME',  														
#           'PR_1','PR_2','PR_3','PR_4', 'PR_5', 'PR_6', 'PR_7', 'PR_8', 'PR_9', 'PR_10', 'PR_11', 'PR_12', 'PR_13', 'PR_14', 'PR_15', 'PR_16', 'PR_17', 'PR_18', 'PR_19', 'PR_20', 'PR_21', 'PR_22', 'PR_23', 'PR_24', 'PR_25',  					
#           'DX_1','DX_2','DX_3','DX_4', 'DX_5', 'DX_6', 'DX_7', 'DX_8', 'DX_9', 'DX_10', 'DX_11', 'DX_12', 'DX_13', 'DX_14', 'DX_15', 'DX_16', 'DX_17', 'DX_18', 'DX_19', 'DX_20', 'DX_21', 'DX_22', 'DX_23', 'DX_24', 'DX_25', 
#           'DX_26', 'DX_27', 'DX_28', 'DX_29', 'DX_30', 'Is_observation_room')
# 
# 
# # nam <- c( 'patient_mrn', 'BIRTH_DATE', 'AGE_YRS', 'SEX', 'PATIENT_RACE_C', 'DQTR',  'YAR',  'LOS',  'RPT_GRP_SIX_NAME',  'HOSPSTCO', 'MS_DRG', 'ADM_SOURCE_NAME',  'DISCH_DISP_NAME',  'DISCH_DATE_TIME',  'ADM_DATE_TIME',  														
# # 'PR_1','PR_2','PR_3','PR_4', 'PR_5', 'PR_6', 'PR_7', 'PR_8', 'PR_9', 'PR_10', 'PR_11', 'PR_12', 'PR_13', 'PR_14', 'PR_15', 'PR_16', 'PR_17', 'PR_18', 'PR_19', 'PR_20', 'PR_21', 'PR_22', 'PR_23', 'PR_24', 'PR_25',  					
# # 'DX_1','DX_2','DX_3','DX_4', 'DX_5', 'DX_6', 'DX_7', 'DX_8', 'DX_9', 'DX_10', 'DX_11', 'DX_12', 'DX_13', 'DX_14', 'DX_15', 'DX_16', 'DX_17', 'DX_18', 'DX_19', 'DX_20', 'DX_21', 'DX_22', 'DX_23', 'DX_24', 'DX_25', 'DX_26', 'DX_27', 'DX_28', 'DX_29', 'DX_30')
# 
# dfhc_hosp <- dfhc_hosp %>%
#   setnames( old = dfhc_hosp %>% names(),
#             new = nam) %>%   # rename column names of dfhc data
#   as.data.table()
# 
# # Remove periods (.) from ICD codes for DX/PR codes
# DX = dfhc_hosp %>%
#   lazy_dt() %>% 
#   select( contains('DX')) %>% as.data.frame() %>% 
#   names() 
# 
# PR = dfhc_hosp %>%
#   lazy_dt() %>% 
#   select( contains('PR')) %>% as.data.frame() %>% 
#   names() 
# 
# dfhc_hosp[, c(DX) := lapply(.SD, gsub, pattern = '\\.' , replacement =  ''), .SDcols = c(DX)]
# dfhc_hosp[, c(PR) := lapply(.SD, gsub, pattern = '\\.' , replacement =  ''), .SDcols = c(PR)]
# 
# dfhc_re <- dfhc_hosp %>%
#   as.data.frame() %>% 
#   select(patient_mrn,
#          ADM_DATE_TIME,
#          DISCH_DATE_TIME,
#          contains('DX'),
#          contains('PR')) %>% 
#   mutate(patient_mrn = as.character(patient_mrn))
# 
# re_dfwhc <- inner_join( x = dfhc_re, 
#                         y = masterdemo_PHHS1 %>% 
#                           as.data.frame() %>%
#                           filter(enroll_date <= enrollcens) %>%
#                           mutate(patient_mrn = as.character(patient_mrn)),
#                         by = 'patient_mrn') %>% 
#   select(patient_mrn,
#          enroll_date,
#          arm,
#          ADM_DATE_TIME,
#          DISCH_DATE_TIME,
#          contains('DX'),
#          contains('PR')) %>% 
#   mutate_at(vars(starts_with('DX')| starts_with('PR')), na_if, 'NULL') # Replace 'NULL' values with NA for DX and PR columns
# 
# # start working on PHHS data here --  join hospbody with dx and px
# ahrq_phhs_HospBody <- ahrq_phhs_HospBody %>% filter(!is.na(MSDRG))
# 
# hosp <- ahrq_phhs_HospBody %>%
#   as.data.frame() %>%
#   mutate(Admission.Date = as.Date(ADM_Date_TIME),
#          StudyID        = as.character(PATIENT_ID),
#          PAT_ENC_CSN_ID = as.character(PAT_ENC_CSN_ID)) %>% 
#   distinct()
# 
# 
# d <- full_join(x   = ahrq_phhs_PX_flat %>% as.data.frame(),
#                y   = ahrq_phhs_DX_flat %>% as.data.frame() %>% select(PAT_ENC_CSN_ID, contains("DX_")),
#                by  = 'PAT_ENC_CSN_ID',
#                all = TRUE ) %>% 
#   unique() %>%
#   select(PAT_MRN_ID,
#          PAT_ENC_CSN_ID,
#          contains("DX_"),
#          contains("PR_"))
# 
# 
# clar <- left_join(x  = hosp,
#                   y  = d,
#                   by = 'PAT_ENC_CSN_ID') %>% 
#   unique() %>% 
#   dplyr::rename(patient_mrn   = PATIENT_ID,  #new_colname = old_colname
#                 ADM_DATE_TIME = ADM_Date_TIME) %>% 
#   mutate(patient_mrn   = as.character(patient_mrn))
# 
# 
# enr <- masterdemo_PHHS1 %>%
#   as.data.frame() %>% 
#   select(patient_mrn,
#          enroll_date,
#          arm)
# 
# re_phhs <- left_join(x  = enr %>% mutate(patient_mrn = as.character(patient_mrn)),
#                      y  = clar,
#                      by = 'patient_mrn') %>% 
#   select(patient_mrn,
#          enroll_date,
#          arm,
#          ADM_DATE_TIME,
#          DISCH_DATE_TIME,
#          contains('DX_'),
#          contains('PR_'))
# 
# re <- bind_rows(re_phhs, re_dfwhc) %>%  #combine phhs and dfwhc data
#   unique() %>%
#   as.data.frame()
# 
# 
# rec_PHHS <- re %>% 
#   mutate(enroll_date = as.Date(enroll_date),
#          ADM_DATE_TIME = as.Date(ADM_DATE_TIME),
#          DISCH_DATE_TIME = as.Date(DISCH_DATE_TIME)) %>% 
#   filter(ADM_DATE_TIME >= enroll_date & ADM_DATE_TIME <= fucens & ADM_DATE_TIME <= enroll_date + 365) %>% 
#   mutate(Complete = ifelse(enroll_date + 365 <= fucens, 1 , 0)) %>% 
#   as.data.table()
# 
# colrec <- c(colnames(rec_PHHS)[rec_PHHS[, str_detect(names(rec_PHHS),"DX")]]) # get columns that contains 'DX' as column names
# 
# for ( i in seq_along(names(adv_icd_named))) { #adv_icd_named is saved both as a list and a csv file in working directory 
#   print(names(adv_icd_named)[i])
#   res = rec_PHHS[, lapply(.SD, grepl, pattern = paste(adv_icd_named[[i]], collapse = '|')), .SDcols = c(colrec)]
#   res_max = apply(res, 1, max)  # rowwise maximum
#   rec_PHHS[ , (names(adv_icd_named)[i]) :=  res_max  ]
# }
# 
# rs_PHHS <- as.data.frame(rec_PHHS) %>% 
#   select(patient_mrn,
#          enroll_date,
#          arm,
#          ADM_DATE_TIME,
#          Discharge.Date = DISCH_DATE_TIME,
#          names(adv_icd_named)) %>%    # adverse events      
#   unique() %>% 
#   arrange(patient_mrn, Discharge.Date) %>% 
#   filter()
# 
# 
# # limit 1 per discharge date
# dedup_PHHS <- rs_PHHS %>% group_by(patient_mrn) %>% 
#   dplyr::summarise(across(where(is.numeric), list(max = max))) %>% ungroup()
# 
# dedup_PHHS <- left_join(x = dedup_PHHS, y = rs_PHHS %>% select(patient_mrn, arm) %>%  unique(), by = "patient_mrn") %>% 
#   rename(pat_id = patient_mrn) %>% select(colnames(dedup_THR))
# 
# dedup <- rbind(dedup_THR, dedup_PHHS)
# 
# adv_evnts <- dedup %>% 
#   select (arm, contains('max')) %>% 
#   group_by(arm) %>% 
#   summarise_all(list(max = sum)) %>% 
#   ungroup() %>% 
#   gather( key = 'Adverse Events' , value = Count, contains('max')) %>% 
#   spread( arm, Count) %>% 
#   mutate( Category = str_remove_all(`Adverse Events`, '_max'),
#           Domain = 'Adverse Events') %>% 
#   filter( `Adverse Events` != 'excluded') %>% 
#   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# 
# 
# dem_totl <- masterdemo %>% 
#   select(arm) %>% 
#   group_by(arm) %>% 
#   tally() %>% 
#   spread(arm, n) %>% 
#   mutate(Domain = 'Total Enrollment',
#          Category = '') %>%
#   select(Domain, Category,`ICD-Pieces`, `Standard of care`)
# 
# Total_enrollment <- dem_totl #%>% mutate(location = 'PHHS + THR')
# #save(Total_enrollment_THR, file = 'T:/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Inputs/THR Total Enrollment.RData')
# # 
# # dem_gndr <- km_complete_final %>%
# #   select(Arm, Gender) %>% 
# #   group_by(Arm, Gender) %>% 
# #   tally() %>% 
# #   spread(Arm, n) %>% 
# #   mutate(Domain = 'Gender') %>% 
# #   rename(Category = Gender) %>% 
# #   select(Domain, Category,`ICD-Pieces`, `Standard of care`)
# # 
# # dem_race <- km_complete_final %>%
# #   select(Arm, Race ) %>% 
# #   group_by(Arm, Race) %>% 
# #   tally() %>% 
# #   spread(Arm, n) %>% 
# #   mutate(Domain = 'Race') %>% 
# #   rename(Category = Race) %>% 
# #   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# # 
# # dem_ethn <- km_complete_final %>%
# #   select(Arm, Ethnicity ) %>% 
# #   group_by(Arm, Ethnicity) %>% 
# #   tally() %>% 
# #   spread(Arm, n) %>% 
# #   mutate(Domain = 'Ethnicity') %>% 
# #   rename(Category = Ethnicity) %>% 
# #   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# 
# # N hospitalization total - all pt- event within 1 yr           
# # hospitalizations_1 <- dedup %>%
# #   select(arm, pat_id) %>%
# #   group_by(arm) %>% 
# #   tally() %>% 
# #   spread(arm, n) %>% 
# #   mutate(Domain = 'Hospitalization',
# #          Category = 'Number of Hospitalizations (within 1 year)') %>% 
# #   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# 
# 
# # N hosp patients - all  pt - event within 1 year
# hospitalizations_2 <- dedup %>%
#   select(arm, pat_id) %>%
#   group_by(arm) %>% 
#   summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>% 
#   spread(arm, Count ) %>% 
#   mutate(Domain = 'Hospitalization',
#          Category = 'Number of Patients Hospitalized (Period after enrollment began; within 1 year)') %>% 
#   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# 
# 
# #death in hospital within 1 year--must be in hospital death
# hosp_death <- km_complete_final %>%
#   mutate(end_dt = as.Date(start_dt) + 365,
#          AnyDeathAfterEnroll = ifelse(Death_date >= start_dt & DeathYN == 'Y','Y','N'),
#          AnyDeathAfterEnrollin1YR = ifelse(Death_date >= start_dt & Death_date <= Censor_date & DeathYN =='Y' & Death_date <= end_dt, 'Y','N'))%>% 
#   # filter(start_dt <= enrollcens) %>%
#   group_by(Arm, AnyDeathAfterEnrollin1YR) %>%
#   tally()%>% 
#   filter(AnyDeathAfterEnrollin1YR == 'Y') %>% 
#   ungroup() %>% 
#   select(Arm, Count = n) %>% 
#   spread(Arm, Count) %>% 
#   mutate(Domain = 'Adverse Events',
#          Category = 'Number of death in hospital within one year') %>% 
#   select(Domain, Category, `ICD-Pieces`, `Standard of care`)
# 
# 
# #proc code for dialysis  
# ae_dx_dialysis <-('Z992')
# ae_proc_dialysis <- c('5A1D00Z','5A1D60Z','3E1M39Z')
# load(file  = 'Inputs/adv_icd_named.Rdata')  # ICD codes for adverse events, saved as a list
# 
# colrec_THR <- c(colnames(rec_THR)[rec_THR[, str_detect(names(rec_THR), "PX")]] )
# colrec_PHHS <- c(colnames(rec_PHHS)[rec_PHHS[, str_detect(names(rec_PHHS), "PR")]] )
# 
# res_THR = rec_THR[, lapply(.SD, grepl, pattern = paste(ae_proc_dialysis, collapse = '|')), .SDcols = c(colrec_THR) ]
# res_max_THR = apply(res_THR, 1, max)  # along  rows
# rec_THR[ , ae_proc_dialysis :=  res_max_THR  ]
# rec_THR <- rec_THR %>% select(pat_id, enroll_date, arm, Discharge.Date = DISCHARGE_DATE, ae_proc_dialysis)
# 
# res_PHHS = rec_PHHS[, lapply(.SD, grepl, pattern = paste(ae_proc_dialysis, collapse = '|')), .SDcols = c(colrec_PHHS) ]
# res_max_PHHS = apply(res_PHHS, 1, max)  # along  rows
# rec_PHHS[ , ae_proc_dialysis :=  res_max_PHHS  ]
# rec_PHHS <- rec_PHHS %>% select(pat_id = patient_mrn, enroll_date, arm, Discharge.Date = DISCH_DATE_TIME, ae_proc_dialysis)
# 
# rec <- rbind(rec_PHHS, rec_THR)
# # rec <- rbind(rec_PHHS)
# 
# 
# dialysis_proc <- as.data.frame(rec) %>%    # adverse events      
#   unique() %>% 
#   arrange(pat_id, Discharge.Date) %>% 
#   group_by(pat_id, Discharge.Date) %>% 
#   mutate(ae_proc_dialysis_max = max(ae_proc_dialysis)) %>% 
#   ungroup() %>% 
#   arrange(pat_id, Discharge.Date) %>% 
#   filter(!duplicated(cbind(pat_id, Discharge.Date))) %>%
#   group_by(arm) %>% 
#   summarise(Count = sum(ae_proc_dialysis_max), .groups = 'drop') %>%
#   spread(arm, Count ) %>% 
#   mutate(Domain = 'Adverse Events',
#          Category = 'Procedure code for dialysis') %>% 
#   select(Domain, Category,`ICD-Pieces`, `Standard of care`)
# 
# 
# #Put together Enrollment, Demographics, Hospitalization and Adverse Events (diseases, dialysis proc, hosp death) together
# # combined <- rbind(dem_totl,
# #                   dem_gndr, 
# #                   dem_race, 
# #                   dem_ethn,
# #                   hospitalizations_1,
# #                   hospitalizations_2,
# #                   adv_evnts,
# #                   dialysis_proc,
# #                   hosp_death)
# 
# Adverse_events <- rbind(adv_evnts,
#                             dialysis_proc,
#                             hosp_death)
# 
# Adverse_events <- rbind(data.frame("Domains" = c("Total enrollment"), "Standard of care" = c(nrow(masterdemo[masterdemo$arm == "Standard of care",])), 
#                                        "ICD-Pieces" = c(nrow(masterdemo[masterdemo$arm == "ICD-Pieces",])),stringsAsFactors = F,  check.names = FALSE),
#                             data.frame("Domains" = c("Adverse Events"), "Standard of care" = c(""), 
#                                        "ICD-Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
#                             adv_evnts %>% dplyr::rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`),
#                             dialysis_proc %>% dplyr::rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`),
#                             hosp_death %>% dplyr::rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`)) %>% 
#   # dplyr::rename(`Standard of Care` = Control) %>% 
#   mutate(Domains = str_to_title(Domains)) %>% 
#   mutate(Domains = case_when(Domains == "Drugtoxicity" ~ "Drug toxicity",
#                              Domains == "Aki" ~ "AKI",
#                              Domains == "Fluidoverload" ~ "Fluid overload",
#                              Domains == "Sepicshock" ~ "Sepic shock",
#                              Domains == "Dialysis" ~ "Dialysis (ICD10-CM = Z99.2)",
#                              TRUE ~ Domains))
# 
# DoubleLine <- c(1, 2)
# 
# Adverse_events_Table <- kable(Adverse_events,
#                                   style = 'html', escape = F, align = "c",
#                                   caption = 'Adverse Events at PHHS and THR',
#                                   booktabs = T, row.names = F) %>%
#   kable_classic_2(full_width = F,
#                   html_font = 'Montserrat') %>%
#   row_spec(DoubleLine,
#            extra_css = "border-bottom-style: double; background-color: white;") %>% 
#   column_spec(1, extra_css = "text-align:left;") %>%
#   collapse_rows(1) %>%
#   kable_styling(latex_options = "scale_down", font_size = 15) %>%
#   row_spec(0, background = "lightskyblue")
# 
# Adverse_events_Table 
# 
# Adverse_events_simp <- rbind(adv_evnts %>% rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`),
#                                  dialysis_proc %>% dplyr::rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`),
#                                  hosp_death %>% dplyr::rename(Domains = Category) %>% select(Domains, `Standard of care`, `ICD-Pieces`)) %>% 
#   rename(A = `Standard of care`, B = `ICD-Pieces`) %>%
#   mutate(Domains = str_to_title(Domains)) %>% 
#   mutate(Domains = case_when(Domains == "Drugtoxicity" ~ "Drug Toxicity",
#                              Domains == "Aki" ~ "AKI",
#                              Domains == "Fluidoverload" ~ "Fluid Overload",
#                              Domains == "Sepicshock" ~ "Septic Shock",
#                              Domains == "Number Of Death In Hospital Within One Year" ~ "Number of Death within One Year",
#                              Domains == "Procedure Code For Dialysis" ~ "Procedure Code for Dialysis",
#                              Domains == "Dialysis" ~ "Dialysis (ICD10-CM = Z99.2)",
#                              TRUE ~ Domains)) %>% 
#   filter(Domains != "Excluded") 
# 
# enrollment <- masterdemo %>%
#   group_by (arm) %>%
#   dplyr::mutate(Count = n()) %>%
#   ungroup() %>% 
#   dplyr::mutate(All = n()) %>% 
#   select(arm, Count, All) %>% 
#   unique() %>%
#   mutate(Characteristics = 'Count',
#          Category = '') %>% 
#   reshape2::dcast(Characteristics + Category + All ~ arm, value.var = 'Count') %>% 
#   arrange(desc(`Standard of care`))%>%
#   mutate_all( ~lapply(.,as.character)) %>% 
#   select(Characteristics, `Standard of care`, `ICD-Pieces`) %>% rename(A = `Standard of care`, B = `ICD-Pieces`, Domains = Characteristics)
# 
# table1 <- Adverse_events_simp %>%
#   mutate(A_PER =  paste(A, ' (', eval(round((A/as.numeric(enrollment$A))*100,2)),'%)', sep = ''),
#          B_PER =  paste(B, ' (', eval(round((B/as.numeric(enrollment$B))*100,2)),'%)', sep = '')) %>%
#   mutate(A_NOT = as.numeric(enrollment$A) - A,
#          B_NOT = as.numeric(enrollment$B) - B)
# 
# chisq_test   <- table1 %>% filter(A >= 5 & A_NOT >= 5 & B >=5 & B_NOT >= 5)
# fishers_test <- table1 %>% filter(A < 5 | A_NOT < 5 | B <5 |B_NOT < 5)
# 
# options(scipen = 999) #Puts p-value to decimal format (non scientific)
# 
# chisq_pval   <- chisq_test %>%
#   rowwise() %>%
#   mutate(p.val = chisq.test(matrix(c(A ,A_NOT, B, B_NOT), nrow = 2))$p.value)
# 
# fishers_pval <- fishers_test %>%
#   rowwise() %>%
#   mutate(p.val = fisher.test(matrix(c(A, A_NOT, B,B_NOT), nrow = 2))$p.value)
# 
# options(scipen = 0)
# 
# merged <- chisq_pval %>% select(Domains, A_PER, B_PER, p.val) %>% 
#   mutate(`Events` = Domains,
#          `Standard of Care` = A_PER,
#          `ICD-Pieces` = B_PER,
#          `P-value` = round(p.val,3)) %>% 
#   select(`Events`, `ICD-Pieces`, `Standard of Care`, `P-value` )
# 
# adverse_event_all <- Adverse_events_simp %>% 
#   mutate(`Number of patients` = A+B,
#          Percent = round(100*(A+B)/nrow(masterdemo), 1)) %>% 
#   filter(Percent >= 2) %>% 
#   mutate(`Number of patients` = paste0(`Number of patients`, " (", Percent, "%)")) %>% 
#   select(Domains, `Number of patients`)
# 
# merged <- merged %>% filter(Events %in% adverse_event_all$Domains) 
# 
# colnames(merged) = c(paste0("Events<br> (Total Enrollment = ", format(as.numeric(enrollment$A) + as.numeric(enrollment$B), big.mark=","), ")"), 
#                      paste0("ICD-Pieces<br> Intervention<br>(N = ", format(as.numeric(enrollment$B), big.mark=","), ")"),
#                      paste0("Standard of Care<br> (N = ", format(as.numeric(enrollment$A), big.mark=","), ")"),
#                      "P-Value")
# 
# adverse_event_merged_table <- kable(merged,
#                                     style = 'html', escape = F, align = "c",
#                                     caption = 'Adverse Events at PHHS and THR Cohort',
#                                     booktabs = T, row.names = F) %>%
#   kable_classic_2(full_width = F,
#                   html_font = 'Montserrat') %>%
#   # row_spec(DoubleLine,
#   #          extra_css = "border-bottom-style: double; background-color: white;") %>% 
#   collapse_rows(1) %>%
#   kable_styling(latex_options = "scale_down", font_size = 15) %>%
#   row_spec(0,bold = T, background = "lightskyblue") %>% 
#   column_spec(1, extra_css = "text-align:left;")
# 
# 
# colnames(adverse_event_all) = c("Events", 
#                                 paste0("Number of patients<br> (Total Enrollment = ", 
#                                        format(as.numeric(enrollment$A) + as.numeric(enrollment$B), big.mark=","), ")"))
# 
# adverse_event_all_merged_table <- kable(adverse_event_all,
#                                         style = 'html', escape = F, align = "c",
#                                         caption = 'Adverse Events at PHHS and THR Cohort',
#                                         booktabs = T, row.names = F) %>%
#   kable_classic_2(full_width = F,
#                   html_font = 'Times New Roman') %>%
#   collapse_rows(1) %>%
#   kable_styling(latex_options = "scale_down", font_size = 15) %>%
#   row_spec(0,bold = T, background = "lightskyblue") %>% 
#   column_spec(1, extra_css = "text-align:left;")
# adverse_event_all_merged_table
# 
# 
# ############## 
# #Complete table
# 
# merged <- chisq_pval %>% select(Domains, A_PER, B_PER, p.val) %>% 
#   mutate(`Events` = Domains,
#          `Standard of Care` = A_PER,
#          `ICD-Pieces` = B_PER,
#          `P-value` = round(p.val,3)) %>% 
#   select(`Events`, `ICD-Pieces`, `Standard of Care`, `P-value` )
# 
# adverse_event_all <- Adverse_events_simp %>% 
#   mutate(`Number of patients` = A+B,
#          Percent = round(100*(A+B)/nrow(masterdemo), 1)) %>% 
#   # filter(Percent >= 2) %>% 
#   mutate(`Number of patients` = paste0(`Number of patients`, " (", Percent, "%)")) %>% 
#   select(Domains, `Number of patients`)
# 
# # merged <- merged %>% filter(Events %in% adverse_event_all$Domains) 
# 
# colnames(merged) = c(paste0("Events<br> (Total Enrollment = ", format(as.numeric(enrollment$A) + as.numeric(enrollment$B), big.mark=","), ")"), 
#                      paste0("ICD-Pieces<br> Intervention<br>(N = ", format(as.numeric(enrollment$B), big.mark=","), ")"),
#                      paste0("Standard of Care<br> (N = ", format(as.numeric(enrollment$A), big.mark=","), ")"),
#                      "P-Value")
# 
# adverse_event_merged_table <- kable(merged,
#                                     style = 'html', escape = F, align = "c",
#                                     caption = 'Adverse Events at PHHS and THR Cohort',
#                                     booktabs = T, row.names = F) %>%
#   kable_classic_2(full_width = F,
#                   html_font = 'Montserrat') %>%
#   # row_spec(DoubleLine,
#   #          extra_css = "border-bottom-style: double; background-color: white;") %>% 
#   collapse_rows(1) %>%
#   kable_styling(latex_options = "scale_down", font_size = 15) %>%
#   row_spec(0,bold = T, background = "lightskyblue") %>% 
#   column_spec(1, extra_css = "text-align:left;")
# 
# 
# colnames(adverse_event_all) = c("Events", 
#                                 paste0("Number of patients<br> (Total Enrollment = ", 
#                                        format(as.numeric(enrollment$A) + as.numeric(enrollment$B), big.mark=","), ")"))
# adverse_event_merged_table
# 
# adverse_event_all_merged_table <- kable(adverse_event_all,
#                                         style = 'html', escape = F, align = "c",
#                                         caption = 'Adverse Events at PHHS and THR Cohort',
#                                         booktabs = T, row.names = F) %>%
#   kable_classic_2(full_width = F,
#                   html_font = 'Times New Roman') %>%
#   collapse_rows(1) %>%
#   kable_styling(latex_options = "scale_down", font_size = 15) %>%
#   row_spec(0,bold = T, background = "lightskyblue") %>% 
#   column_spec(1, extra_css = "text-align:left;")
# adverse_event_all_merged_table



