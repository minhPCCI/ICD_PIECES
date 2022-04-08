library(stringi)
library(lubridate)
library(stringr)
library(tidyverse)
# library(plyr)
library(dplyr)
library(dtplyr)
library(data.table)
library(RODBC)
library(RMySQL)
library(sqldf)
library(ggplot2)
library(matrixStats)
library(formattable)
library(kableExtra)
library(janitor)
library(htmltools)
# library(pdftools) # ERROR
library(gt)
library(devtools)
library(openxlsx)
#library(magick) # ERROR
library(comorbidity)
library(varhandle)
library(huxtable)
library(lubridate)
require(data.table)
detach("package:plyr", unload = TRUE)
options(sqldf.drive="SQLite")
options(gsubfn.engine = "R")
options(encoding = "UTF-8")
options("scipen" = 10)
setwd("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS")
# pull data using the most recent R program. Currently used: PHHS_DataPull20200930.R

# Load data sets: Hospbody, DX_flat, PX_flat, Deaths, Masterdemo, ICD-10 codes for Adverse Events
load(file  = 'Inputs/ahrq_phhs_HospBody_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>% as.data.frame()
load(file  = 'Inputs/ahrq_phhs_DX_flat_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
load(file  = 'Inputs/ahrq_phhs_PX_flat_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
load(file  = 'Inputs/ahrq_phhs_Deaths_ptmatchedto20190628_202009DSMB-2015-07-01w30lkbkto2020-06-30.RData') %>%  as.data.frame()
load(file  = 'Inputs/masterdemo-20190701.RData') %>% data.frame()
load(file  = 'Inputs/adv_icd_named.Rdata')  # ICD codes for adverse events, saved as a list
AHRQ <- read.xlsx("Inputs/AHRQ_subsets_v2.xlsx")

# Fix masterdemo
masterdemo <- masterdemo %>% as.data.frame() %>% 
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
                     arm = case_when(arm == 'CONTROL' ~ 'Control',
                                     TRUE ~ 'ICD - Pieces')) %>% 
              select(-pat_id, -patient_mrn) %>% 
              dplyr::rename(pat_id = pat_id_final,
                     patient_mrn = patient_mrn_final,
                     Gender = sex_c_name ) %>% 
              select(pat_id, patient_mrn, everything()) %>% 
              mutate(Gender = unfactor(Gender)) %>%
              filter(!duplicated(patient_mrn))

# Opt out patients that have EGFR less than 15
# Opt_out = c("000000854650", "000001850404", "Z2658975", "Z2726898", "Z3044413")
# masterdemo <- masterdemo %>% filter(!(pat_id%in%Opt_out))

# set date parameters
enrollcens  = as.Date('2019-06-28')   #last day qualifying
enrollstart = as.Date('2016-07-06')
fucens      = as.Date('2020-06-30')
startdate   = as.Date('2015-07-01')  #start of study (used to look back 12 months prior to start of study)
enddate     = fucens                 #time of outcomes follow-up censor



#DFWHC data. Pull All Pat_MRN_IDs that are also in masterdemo

# dfhc <- readxl::read_excel('Inputs/Parkland_DFWHC_MatchResults_2021-01-19.xlsx') %>%  #DFWHC Data
dfhc <- readxl::read_excel('~/T-Drive/NIH Chronic Disease Management/Data/DSMB data/OneDrive_1_5-11-2021/Parkland_DFWHC_MatchResults_2021-05-10.xlsx') %>%  #DFWHC Data
        as.data.frame() %>% 
        filter(Pat_MRN_ID %in% masterdemo$patient_mrn) %>% 
        filter(CMS_DRG != "NULL") # filter just inpatients

dfhc_hosp <- dfhc %>%
  select( Pat_MRN_ID, Birth_Date, DischargeAge, Ptsex, RaceCodeDesc, Discharge_Quarter, LOS, PayCodeDesc1,  Patient_Zip_Code, CMS_DRG , Admit_Source,
          DischargeStatus, Discharge_Date, Admit_Date ,
          PProc01, Proc02, Proc03, Proc04, Proc05, Proc06, Proc07, Proc08, Proc09, Proc10, Proc11, Proc12, Proc13, Proc14, Proc15, Proc16, Proc17, Proc18, Proc19, 
          Proc20, Proc21, Proc22, Proc23, Proc24, Proc25, PrinDx1, Diag02, Diag03, Diag04, Diag05, Diag06, Diag07, Diag08, Diag09, Diag10, Diag11, Diag12, Diag13, 
          Diag14, Diag15, Diag16, Diag17, Diag18, Diag19, Diag20, Diag21, Diag22, Diag23, Diag24, Diag25, Admitcode, ECode01, ECode02, ECode03, ECode04, Is_observation_room) %>% 
  mutate( Discharge_Date = as.Date(Discharge_Date, format = '%m/%d/%Y'),
          Admit_Date     = as.Date(Admit_Date,     format = '%m/%d/%Y'),
          Birth_Date     = as.Date(Birth_Date,     format = '%m/%d/%Y'))

nam <- c( 'patient_mrn', 'BIRTH_DATE', 'AGE_YRS', 'SEX', 'PATIENT_RACE_C', 'DQTR', 'LOS',  'RPT_GRP_SIX_NAME',  'HOSPSTCO', 'MS_DRG', 'ADM_SOURCE_NAME',  'DISCH_DISP_NAME',  'DISCH_DATE_TIME',  'ADM_DATE_TIME',  														
          'PR_1','PR_2','PR_3','PR_4', 'PR_5', 'PR_6', 'PR_7', 'PR_8', 'PR_9', 'PR_10', 'PR_11', 'PR_12', 'PR_13', 'PR_14', 'PR_15', 'PR_16', 'PR_17', 'PR_18', 'PR_19', 'PR_20', 'PR_21', 'PR_22', 'PR_23', 'PR_24', 'PR_25',  					
          'DX_1','DX_2','DX_3','DX_4', 'DX_5', 'DX_6', 'DX_7', 'DX_8', 'DX_9', 'DX_10', 'DX_11', 'DX_12', 'DX_13', 'DX_14', 'DX_15', 'DX_16', 'DX_17', 'DX_18', 'DX_19', 'DX_20', 'DX_21', 'DX_22', 'DX_23', 'DX_24', 'DX_25', 
          'DX_26', 'DX_27', 'DX_28', 'DX_29', 'DX_30', 'Is_observation_room')


# nam <- c( 'patient_mrn', 'BIRTH_DATE', 'AGE_YRS', 'SEX', 'PATIENT_RACE_C', 'DQTR',  'YAR',  'LOS',  'RPT_GRP_SIX_NAME',  'HOSPSTCO', 'MS_DRG', 'ADM_SOURCE_NAME',  'DISCH_DISP_NAME',  'DISCH_DATE_TIME',  'ADM_DATE_TIME',  														
          # 'PR_1','PR_2','PR_3','PR_4', 'PR_5', 'PR_6', 'PR_7', 'PR_8', 'PR_9', 'PR_10', 'PR_11', 'PR_12', 'PR_13', 'PR_14', 'PR_15', 'PR_16', 'PR_17', 'PR_18', 'PR_19', 'PR_20', 'PR_21', 'PR_22', 'PR_23', 'PR_24', 'PR_25',  					
          # 'DX_1','DX_2','DX_3','DX_4', 'DX_5', 'DX_6', 'DX_7', 'DX_8', 'DX_9', 'DX_10', 'DX_11', 'DX_12', 'DX_13', 'DX_14', 'DX_15', 'DX_16', 'DX_17', 'DX_18', 'DX_19', 'DX_20', 'DX_21', 'DX_22', 'DX_23', 'DX_24', 'DX_25', 'DX_26', 'DX_27', 'DX_28', 'DX_29', 'DX_30')

dfhc_hosp <- dfhc_hosp %>%
  setnames( old = dfhc_hosp %>% names(),
            new = nam) %>%   # rename column names of dfhc data
  as.data.table()

# Remove periods (.) from ICD codes for DX/PR codes
DX = dfhc_hosp %>%
  lazy_dt() %>% 
  select( contains('DX')) %>% as.data.frame() %>% 
  names() 

PR = dfhc_hosp %>%
  lazy_dt() %>% 
  select( contains('PR')) %>% as.data.frame() %>% 
  names() 

dfhc_hosp[, c(DX) := lapply(.SD, gsub, pattern = '\\.' , replacement =  ''), .SDcols = c(DX)]
dfhc_hosp[, c(PR) := lapply(.SD, gsub, pattern = '\\.' , replacement =  ''), .SDcols = c(PR)]

dfhc_re <- dfhc_hosp %>%
  as.data.frame() %>% 
  select(patient_mrn,
         ADM_DATE_TIME,
         DISCH_DATE_TIME,
         contains('DX'),
         contains('PR')) %>% 
  mutate(patient_mrn = as.character(patient_mrn))

re_dfwhc <- inner_join( x = dfhc_re, 
                        y = masterdemo %>% 
                          as.data.frame() %>%
                          filter(enroll_date <= enrollcens) %>%
                          mutate(patient_mrn = as.character(patient_mrn)),
                        by = 'patient_mrn') %>% 
  select(patient_mrn,
         enroll_date,
         arm,
         ADM_DATE_TIME,
         DISCH_DATE_TIME,
         contains('DX'),
         contains('PR')) %>% 
  mutate_at(vars(starts_with('DX')| starts_with('PR')), na_if, 'NULL') # Replace 'NULL' values with NA for DX and PR columns

# start working on PHHS data here --  join hospbody with dx and px
ahrq_phhs_HospBody <- ahrq_phhs_HospBody %>% filter(!is.na(MSDRG))

hosp <- ahrq_phhs_HospBody %>%
  as.data.frame() %>%
  mutate(Admission.Date = as.Date(ADM_Date_TIME),
         StudyID        = as.character(PATIENT_ID),
         PAT_ENC_CSN_ID = as.character(PAT_ENC_CSN_ID)) %>% 
  distinct()


d <- full_join(x   = ahrq_phhs_PX_flat %>% as.data.frame(),
               y   = ahrq_phhs_DX_flat %>% as.data.frame() %>% select(PAT_ENC_CSN_ID, contains("DX_")),
               by  = 'PAT_ENC_CSN_ID',
               all = TRUE ) %>% 
  unique() %>%
  select(PAT_MRN_ID,
         PAT_ENC_CSN_ID,
         contains("DX_"),
         contains("PR_"))


clar <- left_join(x  = hosp,
                  y  = d,
                  by = 'PAT_ENC_CSN_ID') %>% 
  unique() %>% 
  dplyr::rename(patient_mrn   = PATIENT_ID,  #new_colname = old_colname
         ADM_DATE_TIME = ADM_Date_TIME) %>% 
  mutate(patient_mrn   = as.character(patient_mrn))


enr <- masterdemo %>%
  as.data.frame() %>% 
  select(patient_mrn,
         enroll_date,
         arm)

re_phhs <- left_join(x  = enr %>% mutate(patient_mrn = as.character(patient_mrn)),
                     y  = clar,
                     by = 'patient_mrn') %>% 
  select(patient_mrn,
         enroll_date,
         arm,
         ADM_DATE_TIME,
         DISCH_DATE_TIME,
         contains('DX_'),
         contains('PR_'))

re <- bind_rows(re_phhs, re_dfwhc) %>%  #combine phhs and dfwhc data
  unique() %>%
  as.data.frame()

km6 <- masterdemo %>%
  as.data.frame() %>% 
  mutate(enroll_date  = as.Date(enroll_date)) %>% 
  filter(enroll_date  <= as.Date('2019-06-28')) %>%  #enrollcens
  mutate(patient_mrn  = as.character(patient_mrn)) %>% 
  dplyr::rename(study_id     = patient_mrn) %>% 
  dplyr::rename(StudyID      = study_id,
         start_dt     = enroll_date,
         Arm          = arm) %>% 
  mutate(Censor_date  = fucens,
         Location     = 'PHHS',
         end_dt       = start_dt + 365) %>% 
  mutate(Complete     = case_when(end_dt <= Censor_date ~ 1,
                                  end_dt >  Censor_date ~ 0))

# Obtain Death information - from both Hosp_phhs and Hosp_dfhc
dds_phhs <- hosp %>%
  mutate(DISCH_DATE_TIME = as.Date(DISCH_DATE_TIME),
         PATIENT_ID      = as.character(PATIENT_ID)) %>%
  filter(DISCH_DISP_C %in% c(14, 8, 40, 41, 42)) %>% 
  select(PATIENT_ID,
         DISCH_DATE_TIME) %>%  # The Discharge date time is actually death date because we filtered DISCH_DISP_C = 14
  unique()

dds_dfwc <- dfhc_hosp %>%
  as.data.frame() %>% 
  filter(DISCH_DISP_NAME == 'Expired') %>% 
  select(patient_mrn,
         DISCH_DATE_TIME) %>% 
  dplyr::rename( PATIENT_ID = patient_mrn) %>% 
  mutate(PATIENT_ID = as.character(PATIENT_ID)) %>% 
  unique()


dds <- rbind(dds_phhs, dds_dfwc) %>%
  unique() %>% 
  arrange(PATIENT_ID, desc(DISCH_DATE_TIME)) %>%  # order by patient_id and then descending order of their date of death(s)
  filter(!duplicated(PATIENT_ID)) # select the first date of death per patient



rd2 <- left_join(x = km6,
                 y = dds,
                 by = c('StudyID' = 'PATIENT_ID')) %>% 
  unique() %>% 
  mutate(Death_date = as.Date(DISCH_DATE_TIME)) %>% 
  mutate(Death_date = replace(Death_date,!is.na(Death_date) & Death_date > Censor_date, NA)) %>%
  mutate(DeathYN    = ifelse(!is.na(Death_date), 'Y','N'),
         HOSPDEATH  = ifelse(!is.na(Death_date), 'Y','N'),
         Death_1yr  = ifelse(!is.na(Death_date) & Death_date <= end_dt & Death_date >= start_dt, 'Y','N'))

# Deaths

dth <- ahrq_phhs_Deaths %>%
  as.data.frame() %>% 
  dplyr::rename(StudyID = PAT_MRN_ID) %>% 
  mutate(StudyID = as.character(StudyID)) %>% 
  filter(PAT_STATUS_C != 1 & !is.na(death_date)) %>% 
  select(StudyID, death_date)

km7b <- left_join(x  = rd2,
                  y  = dth,
                  by = 'StudyID') %>%
  mutate(death_date = ifelse(!is.na(death_date) & death_date > Censor_date | death_date < start_dt, NA, death_date)) %>%
  mutate(Death_date = ifelse(is.na(Death_date) & !is.na(death_date), death_date, Death_date)) %>% 
  mutate(Death_date = ifelse(!is.na(Death_date) & !is.na(death_date) & death_date < Death_date, death_date, Death_date)) %>% 
  mutate(death_date = as.Date(death_date, origin = '1970-01-01',tz = 'UTC'),
         Death_date = as.Date(Death_date, origin = '1970-01-01',tz = 'UTC')) %>% 
  mutate(death_date = NULL,
         DeathYN    = ifelse(!is.na(Death_date), 'Y', DeathYN),
         Death_1yr  = ifelse(!is.na(Death_date) & Death_date <= end_dt & Death_date >= start_dt, 'Y', Death_1yr),
         HOSPDEATH  = ifelse(!is.na(Death_date) & is.na(HOSPDEATH), 'N', HOSPDEATH)) %>% 
  unique()


km8<- left_join(x  = km7b,
                y  = rbind(hosp %>%
                             filter(PATIENT_ID %in% km7b$StudyID & Admission.Date <= fucens) %>%
                             select(Admission.Date, 
                                    StudyID,
                                    PATIENT_ID) %>% 
                             unique()
                           ,
                           dfhc_hosp %>% 
                             as.data.frame() %>% 
                             mutate(StudyID    = as.character(patient_mrn),
                                    PATIENT_ID = as.character(patient_mrn)) %>%
                             dplyr::rename(Admission.Date = ADM_DATE_TIME) %>% 
                             filter(PATIENT_ID %in% km7b$StudyID & Admission.Date <= fucens) %>% 
                             select(Admission.Date,
                                    StudyID,
                                    PATIENT_ID) %>% 
                             unique()),
                by = 'StudyID' )

km9 <- km8 %>%
  filter(!is.na(Admission.Date) & Admission.Date >= start_dt) %>% 
  arrange(StudyID, Admission.Date) %>% 
  unique() %>% 
  filter(!duplicated(StudyID)) %>%  # selects first row for each StudyID
  unique()

km10 <- left_join(x  = km7b,
                  y  = km9 %>% select(pat_id, StudyID, Admission.Date),
                  by = 'StudyID')

km10b <- km10 %>% 
  dplyr::rename(Hosp_date = Admission.Date) %>% 
  mutate(Hosp_date = ifelse(!is.na(Death_date) & !is.na(Hosp_date) & Death_date < Hosp_date, Death_date, Hosp_date)) %>%
  mutate(Hosp_date = ifelse(!is.na(Hosp_date) & Hosp_date > Censor_date, NA, Hosp_date)) %>% 
  mutate(Hosp_date = as.Date(Hosp_date, origin = '1970-01-01', tz = 'UTC')) %>% 
  mutate(HospY     = ifelse(!is.na(Hosp_date) & Hosp_date >= start_dt & Hosp_date <= end_dt, 'Y', 'N'))

ae_dx_dialysis   = c('Z992')
ae_proc_dialysis = c('5A1D00Z','5A1D60Z','3E1M39Z')


km_dial_re <- re %>% 
  mutate(ae_proc_dialysis = re %>% select(contains('PR_')) %>% 
           mutate_all(~ ifelse(grepl(., pattern = paste(ae_proc_dialysis, collapse = '|')),1 ,0)) %>%
           mutate(had_proc = matrixStats::rowMaxs(as.matrix(.,))) %>% 
           select(had_proc),
         
         ae_dx_dialysis  =  re %>% select(contains('DX_')) %>% 
           mutate_all(~ ifelse(grepl(., pattern = paste(ae_dx_dialysis, collapse = '|')),1 ,0)) %>%
           mutate(had_dx =  matrixStats::rowMaxs(as.matrix(.,))) %>% 
           select(had_dx))

km_dial_re <- km_dial_re %>%
  mutate(sums = ae_proc_dialysis + ae_dx_dialysis)

km_dial_re <- km_dial_re %>%
  group_by(patient_mrn) %>%
  mutate(km_dial = max(sums, na.rm = T)) %>%
  ungroup() %>%
  filter(sums > 0)

km_dial_1 <- km_dial_re %>%
  mutate(ADM_DATE_TIME  = as.Date(ADM_DATE_TIME)) %>% 
  filter(ADM_DATE_TIME >= enroll_date & ADM_DATE_TIME <= fucens) %>% 
  arrange(patient_mrn, ADM_DATE_TIME, DISCH_DATE_TIME) %>%
  filter(!duplicated(patient_mrn)) %>%   # selects first row for each patient_mrn
  select(patient_mrn, ADM_DATE_TIME, DISCH_DATE_TIME)

km_dial_final <- km_dial_1 %>%
  unique() %>%
  filter(!duplicated(patient_mrn))   # selects first row for each patient_mrn

km11<- left_join(km10b, km_dial_final, by = c('StudyID' = 'patient_mrn')) %>% 
  mutate(DialysisYN = 'N') %>% 
  dplyr::rename(Dial_date  = ADM_DATE_TIME) %>% 
  mutate(Dial_date  = ifelse(!is.na(Death_date) & !is.na(Dial_date) & Death_date < Dial_date, Death_date, Dial_date)) %>% 
  mutate(Dial_date  = ifelse(!is.na(Dial_date) & (Dial_date > Censor_date | Dial_date < start_dt), NA, Dial_date)) %>%
  mutate(DialysisYN = ifelse(!is.na(Dial_date) & Dial_date >= start_dt & Dial_date <= end_dt, 'Y', DialysisYN)) %>% 
  mutate(Dial_date  = as.Date(Dial_date, origin = '1970-01-01', tz = 'UTC'),
         Age        = as.integer(trunc(age, 0)),
         Sex        = Gender,
         Race       = patient_race,
         Ethnicity  = ethnic_group_c_name)

km_complete_final <- km11 %>%
  select(StudyID,
         start_dt, 
         Censor_date,
         Arm,
         Location,
         HospY,
         Hosp_date,
         DialysisYN,
         Dial_date,
         DeathYN,
         Death_date,
         Death_1yr,
         pcp_id,
         Age,
         Sex,
         Race,
         Ethnicity,
         Complete)

nrow(km_complete_final[km_complete_final$Death_1yr == "Y",])

dfhc_obs <- readxl::read_excel('~/T-Drive/NIH Chronic Disease Management/Data/DSMB data/OneDrive_1_5-11-2021/Parkland_DFWHC_MatchResults_2021-05-10.xlsx') %>%  #DFWHC Data
  as.data.frame() %>% 
  filter(Pat_MRN_ID %in% masterdemo$patient_mrn) 

dfhc_Ipatient_obs <- inner_join(x = dfhc_obs %>% mutate(patient_mrn = Pat_MRN_ID),
                            y = masterdemo %>% mutate(patient_mrn = unfactor(patient_mrn)),
                            by = 'patient_mrn') %>% 
  mutate(diff = as.Date(Admit_Date) - as.Date(enroll_date)) %>% 
  select(patient_mrn, Submission_Purpose, Is_observation_room, enroll_date, Admit_Date, diff) %>% 
  filter(diff > 0 & diff < 365) %>% 
  filter(Is_observation_room == 1 & Submission_Purpose == "O") %>% 
  rename(Admit_Date_Observation = Admit_Date) %>% 
  rename(StudyID = patient_mrn) %>% 
  mutate(StudyID = as.character(StudyID)) %>% 
  group_by(StudyID, Is_observation_room, enroll_date, Submission_Purpose) %>% 
  summarise(Admit_Date_Observation = min(Admit_Date_Observation)) %>% 
  mutate(observation_flag = 1) %>% ungroup() %>% 
  select(StudyID, observation_flag, Admit_Date_Observation) %>% 
  mutate(Admit_Date_Observation = as.Date(Admit_Date_Observation))

km_complete_final_obs <- left_join(x = km_complete_final, y = dfhc_Ipatient_obs, by = "StudyID")

# write.xlsx(km_complete_final_obs, 'Outputs/PHHS_KM_v7.xlsx')
# km_complete_final_obs <- read.xlsx('Outputs/Final_version/PHHS_KM_v10.xlsx')

# write.xlsx(km_complete_final_observation1, 'Outputs/PHHS_KM_v5.xlsx')
km_full_final <- km11

#-----------------------------------------------------------------------------------------------------------------------------------------------------------#

# Adverse Events

rec <- re %>% 
  mutate(enroll_date = as.Date(enroll_date),
         ADM_DATE_TIME = as.Date(ADM_DATE_TIME),
         DISCH_DATE_TIME = as.Date(DISCH_DATE_TIME)) %>% 
  filter(ADM_DATE_TIME >= enroll_date & ADM_DATE_TIME <= fucens & ADM_DATE_TIME <= enroll_date + 365) %>% 
  mutate(Complete = ifelse(enroll_date + 365 <= fucens, 1 , 0)) %>% 
  as.data.table()

colrec <- c(colnames(rec)[rec[, str_detect(names(rec),"DX")]]) # get columns that contains 'DX' as column names

for ( i in seq_along(names(adv_icd_named))) { #adv_icd_named is saved both as a list and a csv file in working directory 
  print(names(adv_icd_named)[i])
  res = rec[, lapply(.SD, grepl, pattern = paste(adv_icd_named[[i]], collapse = '|')), .SDcols = c(colrec)]
  res_max = apply(res, 1, max)  # rowwise maximum
  rec[ , (names(adv_icd_named)[i]) :=  res_max  ]
}

rs <- as.data.frame(rec) %>% 
  select(patient_mrn,
         enroll_date,
         arm,
         ADM_DATE_TIME,
         Discharge.Date = DISCH_DATE_TIME,
         names(adv_icd_named)) %>%    # adverse events      
  unique() %>% 
  arrange(patient_mrn, Discharge.Date) %>% 
  filter()

# limit 1 per discharge date
dedup <- rs %>% group_by(patient_mrn) %>% 
  dplyr::summarise(across(where(is.numeric), list(max = max))) %>% ungroup()

dedup <- left_join(x = dedup, y = rs %>% select(patient_mrn, arm) %>%  unique(), by = "patient_mrn")  

adv_evnts_dial <- re %>% 
  mutate(ae_proc_dialysis = re %>% select(contains('PR_')) %>% 
           mutate_all(~ ifelse(grepl(., pattern = paste(ae_proc_dialysis, collapse = '|')),1 ,0)) %>%
           mutate(had_proc = matrixStats::rowMaxs(as.matrix(.,))) %>% 
           select(had_proc)) %>% 
  filter(ADM_DATE_TIME > enroll_date & ADM_DATE_TIME <= enroll_date + 365) %>% 
  filter(ae_proc_dialysis == 1) 

adv_evnts <- dedup %>% 
  select (arm, contains('max')) %>% 
  group_by(arm) %>% 
  summarise_all(list(max = sum)) %>% 
  ungroup() %>% 
  gather( key = 'Adverse Events' , value = Count, contains('max')) %>% 
  spread( arm, Count) %>% 
  mutate( Category = str_remove_all(`Adverse Events`, '_max'),
          Domain = 'Adverse Events') %>% 
  filter( `Adverse Events` != 'excluded') %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

dem_totl <- km_complete_final %>% 
  select(Arm) %>% 
  group_by(Arm) %>% 
  tally() %>% 
  spread(Arm, n) %>% 
  mutate(Domain = 'Total Enrollment',
         Category = '') %>%
  select(Domain, Category, Control, `ICD - Pieces`)

Total_enrollment_PHHS <- dem_totl %>% mutate(location = 'PHHS')
#save(Total_enrollment_PHHS, file = 'T:/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Inputs/PHHS Total Enrollment.RData')

dem_gndr <- km_complete_final %>%
  select(Arm, Gender = Sex ) %>% 
  group_by(Arm, Gender) %>% 
  tally() %>% 
  spread(Arm, n) %>% 
  mutate(Domain = 'Gender') %>% 
  dplyr::rename(Category = Gender) %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

dem_race <- km_complete_final %>%
  select(Arm, Race ) %>% 
  group_by(Arm, Race) %>% 
  tally() %>% 
  spread(Arm, n) %>% 
  mutate(Domain = 'Race') %>% 
  dplyr::rename(Category = Race) %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

dem_ethn <- km_complete_final %>%
  select(Arm, Ethnicity ) %>% 
  group_by(Arm, Ethnicity) %>% 
  tally() %>% 
  spread(Arm, n) %>% 
  mutate(Domain = 'Ethnicity') %>% 
  dplyr::rename(Category = Ethnicity) %>% 
  select(Domain, Category, Control, `ICD - Pieces`)


# N hospitalization total - all pt- event within 1 yr           
hospitalizations_1 <- dedup %>%
  select(arm, patient_mrn) %>%
  group_by(arm) %>% 
  tally() %>% 
  spread(arm, n) %>% 
  mutate(Domain = 'Hospitalization',
         Category = 'Number of Hospitalizations (within 1 year)') %>% 
  select(Domain, Category, Control, `ICD - Pieces`)


# N hosp patients - all  pt - event within 1 year
hospitalizations_2 <- dedup %>%
  select(arm, patient_mrn) %>%
  group_by(arm) %>% 
  dplyr::summarise(Count  = n_distinct(patient_mrn), .groups = 'drop') %>% 
  spread(arm, Count ) %>% 
  mutate(Domain = 'Hospitalization',
         Category = 'Number of Patients Hospitalized (Period after enrollment began; within 1 year)') %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

#death in hospital within 1 year--must be in hospital death

hosp_death <- km_full_final %>%
  group_by(Arm, Death_1yr) %>%
  tally()%>% 
  filter(Death_1yr == 'Y') %>% 
  ungroup() %>% 
  select(Arm, Count = n) %>% 
  spread(Arm, Count) %>% 
  mutate(Domain = 'Adverse Events',
         Category = 'Number of death in hospital within one year') %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

#proc code for dialysis        

colrec <- c(colnames(rec)[rec[, str_detect(names(rec), "PR")]] )
res = rec[, lapply(.SD, grepl, pattern = paste(ae_proc_dialysis, collapse = '|')), .SDcols = c(colrec) ]
res_max = apply(res, 1, max)  # along  rows
rec[ , ae_proc_dialysis :=  res_max  ]


dialysis_proc <- as.data.frame(rec) %>% 
  select(patient_mrn,
         enroll_date,
         arm,
         ADM_DATE_TIME,
         Discharge.Date = DISCH_DATE_TIME,
         ae_proc_dialysis) %>%    # adverse events      
  unique() %>% 
  arrange(patient_mrn, Discharge.Date) %>% 
  group_by(patient_mrn, Discharge.Date) %>% 
  mutate(ae_proc_dialysis_max = max(ae_proc_dialysis)) %>% 
  ungroup() %>% 
  arrange(patient_mrn, Discharge.Date) %>% 
  filter(!duplicated(cbind(patient_mrn, Discharge.Date))) %>%
  group_by(arm) %>% 
  dplyr::summarise(Count = sum(ae_proc_dialysis_max), .groups = 'drop') %>%
  spread(arm, Count ) %>% 
  mutate(Domain = 'Adverse Events',
         Category = 'Procedure code for dialysis') %>% 
  select(Domain, Category, Control, `ICD - Pieces`)

#Put together Enrollment, Demographics, Hospitalization and Adverse Events (diseases, dialysis proc, hosp death) together
combined <- rbind(dem_totl,
                  dem_gndr, 
                  dem_race, 
                  dem_ethn,
                  hospitalizations_1,
                  hospitalizations_2,
                  adv_evnts,
                  dialysis_proc,
                  hosp_death)

Adverse_events_PHHS <- rbind(data.frame("Domains" = c("Total enrollment"), "Control" = c(nrow(masterdemo[masterdemo$arm == "Control",])), 
                                        "ICD-Pieces" = c(nrow(masterdemo[masterdemo$arm == "ICD - Pieces",])),stringsAsFactors = F,  check.names = FALSE),
                             data.frame("Domains" = c("Adverse Events"), "Control" = c(""), 
                                        "ICD-Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
                             adv_evnts %>% dplyr::rename(Domains = Category) %>% select(Domains, Control, `ICD-Pieces` = `ICD - Pieces`),
                             dialysis_proc %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`),
                             hosp_death %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`)) %>% 
  dplyr::rename(`Standard of Care` = Control) %>% 
  mutate(Domains = str_to_title(Domains)) %>% 
  mutate(Domains = case_when(Domains == "Drugtoxicity" ~ "Drug toxicity",
                             Domains == "Aki" ~ "AKI",
                             Domains == "Fluidoverload" ~ "Fluid overload",
                             Domains == "Sepicshock" ~ "Sepic shock",
                             Domains == "Dialysis" ~ "Dialysis (ICD10-CM = Z99.2)",
                             TRUE ~ Domains))

# Adverse_events_PHHS <- rbind(data.frame("Domains" = c("Total enrollment"), "Control" = c(nrow(masterdemo[masterdemo$arm == "Control",])), 
#                                         "ICD-Pieces" = c(nrow(masterdemo[masterdemo$arm == "ICD - Pieces",])),stringsAsFactors = F,  check.names = FALSE),
#                              adv_evnts %>% dplyr::rename(Domains = Category) %>% select(Domains, Control, `ICD-Pieces` = `ICD - Pieces`),
#                              dialysis_proc %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`),
#                              hosp_death %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`)) %>% 
#   dplyr::rename(`Standard of Care` = Control) %>% 
#   mutate(Domains = str_to_title(Domains)) %>% 
#   mutate(Domains = case_when(Domains == "Drugtoxicity" ~ "Drug toxicity",
#                              Domains == "Aki" ~ "AKI",
#                              Domains == "Fluidoverload" ~ "Fluid overload",
#                              Domains == "Sepicshock" ~ "Sepic shock",
#                              Domains == "Dialysis" ~ "Dialysis (ICD10-CM = Z99.2)",
#                              TRUE ~ Domains))
# 
# save(Adverse_events_PHHS, file = "Outputs/Adverse_events_PHHS.RData")

DoubleLine <- c(1, 2)

Adverse_events_PHHS_Table <- kable(Adverse_events_PHHS,
            style = 'html', escape = F, align = "c",
            caption = 'Adverse Events at PHHS',
            booktabs = T, row.names = F) %>%
        kable_classic_2(full_width = F,
                        html_font = 'Montserrat') %>%
        row_spec(DoubleLine,
           extra_css = "border-bottom-style: double; background-color: white;") %>% 
        column_spec(1, extra_css = "text-align:left;") %>%
        collapse_rows(1) %>%
        kable_styling(latex_options = "scale_down", font_size = 15) %>%
        row_spec(0, background = "lightskyblue")

Adverse_events_PHHS_Table      

Adverse_events_PHHS_simp <- rbind(adv_evnts %>% rename(Domains = Category) %>% select(Domains, Control, `ICD-Pieces` = `ICD - Pieces`),
                                  dialysis_proc %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`),
                                  hosp_death %>% dplyr::rename(Domains = Category, `ICD-Pieces` = `ICD - Pieces`) %>% select(Domains, Control, `ICD-Pieces`)) %>% 
  rename(A = Control, B = `ICD-Pieces`) %>% 
  mutate(Domains = str_to_title(Domains)) %>% 
  mutate(Domains = case_when(Domains == "Drugtoxicity" ~ "Drug Toxicity",
                             Domains == "Aki" ~ "AKI",
                             Domains == "Fluidoverload" ~ "Fluid Overload",
                             Domains == "Sepicshock" ~ "Septic Shock",
                             Domains == "Number Of Death In Hospital Within One Year" ~ "Number of Death within One Year",
                             Domains == "Procedure Code For Dialysis" ~ "Procedure Code for Dialysis",
                             Domains == "Dialysis" ~ "Dialysis (ICD10-CM = Z99.2)",
                             TRUE ~ Domains)) %>% 
  filter(Domains != "Excluded") 

PHHS_enrollment <- masterdemo %>%
  group_by (arm) %>%
  dplyr::mutate(Count = n()) %>%
  ungroup() %>% 
  dplyr::mutate(All = n()) %>% 
  select(arm, Count, All) %>% 
  unique() %>%
  mutate(Characteristics = 'Count',
         Category = '') %>% 
  reshape2::dcast(Characteristics + Category + All ~ arm, value.var = 'Count') %>% 
  arrange(desc(Control))%>%
  mutate_all( ~lapply(.,as.character)) %>% 
  mutate(Control = Control) %>%
  mutate(`ICD - Pieces` = `ICD - Pieces`) %>% 
  mutate_all(~lapply(., as.character)) %>% 
  select(Characteristics, Control, `ICD - Pieces`) %>% rename(A = Control, B = `ICD - Pieces`, Domains = Characteristics)

table1 <- Adverse_events_PHHS_simp %>% 
  mutate(A_PER =  paste(A, ' (', eval(round((A/as.numeric(PHHS_enrollment$A))*100,2)),'%)', sep = ''),
         B_PER =  paste(B, ' (', eval(round((B/as.numeric(PHHS_enrollment$B))*100,2)),'%)', sep = '')) %>% 
  mutate(A_NOT = as.numeric(PHHS_enrollment$A) - A,
         B_NOT = as.numeric(PHHS_enrollment$B) - B)

chisq_test   <- table1 %>% filter(A >= 5 & A_NOT >= 5 & B >=5 & B_NOT >= 5)
fishers_test <- table1 %>% filter(A < 5 | A_NOT < 5 | B <5 |B_NOT < 5)

options(scipen = 999) #Puts p-value to decimal format (non scientific)

chisq_pval   <- chisq_test %>%
  rowwise() %>%
  mutate(p.val = chisq.test(matrix(c(A ,A_NOT, B, B_NOT), nrow = 2))$p.value)

fishers_pval <- fishers_test %>%
  rowwise() %>%
  mutate(p.val = fisher.test(matrix(c(A, A_NOT, B,B_NOT), nrow = 2))$p.value)

options(scipen = 0)

merged <- rbind(chisq_pval, fishers_pval) %>% select(Domains, A_PER, B_PER, p.val) %>% 
  mutate(`Events` = Domains,
         `Standard of Care` = A_PER,
         `ICD-Pieces` = B_PER,
         `P-value` = round(p.val,3)) %>% 
  select(`Events`, `ICD-Pieces`, `Standard of Care`, `P-value` )

colnames(merged) = c(paste0("Events<br> (Total Enrollment = ", format(as.numeric(PHHS_enrollment$A) + as.numeric(PHHS_enrollment$B), big.mark=","), ")"), 
                     paste0("ICD-Pieces<br> Intervention<br>(N = ", format(as.numeric(PHHS_enrollment$B), big.mark=","), ")"),
                     paste0("Standard of Care<br> (N = ", format(as.numeric(PHHS_enrollment$A), big.mark=","), ")"),
                     "P-Value")

adverse_event_merged_table <- kable(merged,
                      style = 'html', escape = F, align = "c",
                      caption = 'Adverse Events at Parkland Health and Hospital System Cohort',
                      booktabs = T, row.names = F) %>%
  kable_classic_2(full_width = F,
                  html_font = 'Montserrat') %>%
  # row_spec(DoubleLine,
  #          extra_css = "border-bottom-style: double; background-color: white;") %>% 
  collapse_rows(1) %>%
  kable_styling(latex_options = "scale_down", font_size = 15) %>%
  row_spec(0,bold = T, background = "lightskyblue") %>% 
  column_spec(1, extra_css = "text-align:left;")
  
adverse_event_merged_table

adverse_event_all <- Adverse_events_PHHS_simp %>% 
  mutate(`Number of patients` = paste0(A+B, " (", round(100*(A+B)/nrow(masterdemo), 1), "%)")) %>% 
  select(Domains, `Number of patients`)

colnames(adverse_event_all) = c("Events", 
                     paste0("Number of patients<br> (Total Enrollment = ", format(as.numeric(PHHS_enrollment$A) + as.numeric(PHHS_enrollment$B), big.mark=","), ")"))

adverse_event_all_merged_table <- kable(adverse_event_all,
                                    style = 'html', escape = F, align = "c",
                                    caption = 'Adverse Events at Parkland Health and Hospital System Cohort',
                                    booktabs = T, row.names = F) %>%
  kable_classic_2(full_width = F,
                  html_font = 'Times New Roman') %>%
  collapse_rows(1) %>%
  kable_styling(latex_options = "scale_down", font_size = 15) %>%
  row_spec(0,bold = T, background = "lightskyblue") %>% 
  column_spec(1, extra_css = "text-align:left;")

adverse_event_all_merged_table
#save(Adverse_events_PHHS, file = 'T:/NIH Chronic Disease Management/Data/ICD_Pieces/Common/Inputs/PHHS Adverse events.RData')


prelim <- combined %>% 
  mutate(Control = ifelse(Category != 'Number of Hospitalizations (within 1 year)',
                          paste(Control, ' (', eval(round((Control/as.numeric(dem_totl$Control))*100,2)),'%)', sep = ''),
                          Control), 
         `ICD - Pieces` = ifelse(Category != 'Number of Hospitalizations (within 1 year)',
                         paste(`ICD - Pieces`, ' (', eval(round((`ICD - Pieces`/as.numeric(dem_totl$`ICD - Pieces`))*100,2)),'%)', sep = ''),
                         `ICD - Pieces`),
         Category = str_to_title(Category)) %>%
  mutate (Category = ifelse(Category == 'Aki','AKI', Category)) %>%
  filter (Category != 'Excluded')

prelim_table <- kable(prelim, 
                      style = 'html',
                      booktabs = T,
                      caption = 'Preliminary') %>%
  collapse_rows(columns = 1:2, valign = 'middle') %>% 
  kable_styling('striped','bordered') %>% 
  kable_classic_2(full_width = F, 
                  html_font = 'Montserrat') %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1:2, width_max = '3.5in') %>% 
  column_spec(3, width_max = '1.5in')

prelim_table
#prelim_table %>% save_kable('Outputs/Prelims.pdf')
#---------------------------------------------------------------------------------------------------------------------------------#

#Fidelity information -

# BP - match_bp table
# ACEI/STATIN - match_med table
# A1C - match_lab table


#BP 
load(file = 'Inputs/match_bp.RData') %>% as.data.frame()
load("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/Shelley Handoff/PHHS SHELLEY/flowsheet_sql_20190227.RData")
bp_sql1 <- left_join(x = masterdemo %>% select(pat_id,enroll_date),
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

bp <- rbind(bp, bp_sql1) %>%  unique()

#BP - prior 12 months of enrollment  
bpprior <- left_join(x  = masterdemo,
                    y  = bp,
                    by = 'pat_id') %>%
          filter( bp_d <= enroll_date & bp_d >= enroll_date - 365) %>% 
          group_by(pat_id) %>%
          mutate(maxdias = max(dbp),
                 maxsys = max(sbp)) %>%
          ungroup() %>% 
          select(names(masterdemo), maxdias, maxsys) %>%
          unique()


bpprior_all <- left_join(masterdemo %>% as.data.frame(),
                         bpprior %>% as.data.frame() %>% select(pat_id,
                                                                maxdias,
                                                                maxsys),
                         by = 'pat_id') %>% 
               # 0s from match_bp are patients with a visit but no BP measured will consider it no bp measured.
               # NA's are from masterdemo with no corresponding visit from labs file; this will also consider no bp measured.
               
               mutate(maxdias = ifelse(maxdias == 0, NA, maxdias),
                      maxsys  = ifelse(maxsys  == 0, NA, maxsys)) %>% 
               mutate(HiBP = ifelse(!is.na(maxdias) & maxdias >= 90 | (!is.na(maxsys) & maxsys  >= 140 ), 1, NA)) %>% 
               mutate(HiBP = ifelse(!is.na(pat_id)  & is.na(HiBP), 0, HiBP))

prior_any_bp <- bpprior_all %>%
                filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys))) %>%
                select(pat_id, arm) %>%
                unique %>%
                group_by(arm) %>%
                tally() %>% 
                spread(arm, n) %>%
                mutate(Domain = 'BP',
                       Category = '# with BP checked in prior 12 months')

prior_bp_ovr_140_90 <- bpprior_all %>%
                       filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys)) & HiBP == 1) %>%
                       select(pat_id, arm) %>%
                       unique %>%
                       group_by(arm) %>%
                       tally() %>% 
                       spread(arm, n) %>%
                       mutate(Domain = 'BP',
                              Category = 'BP in prior 12 months over 140/90')
                       

prior_bp_udr_140_90 <- bpprior_all %>%
                       filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys)) & HiBP == 0) %>%
                       select(pat_id, arm) %>%
                       unique %>%
                       group_by(arm) %>%
                       tally() %>%
                       spread(arm, n) %>%
                       mutate(Domain = 'BP',
                              Category = 'BP in prior 12 months less 140/90')


#BP - post 12 months of enrollment

bppost <- left_join(x  = masterdemo,
                    y  = bp,
                    by = 'pat_id') %>%
          filter( bp_d > enroll_date & bp_d < enroll_date + 365) %>% 
          group_by(pat_id) %>%
          mutate(maxdias = max(dbp),
                 maxsys = max(sbp)) %>%
          ungroup() %>% 
          select(names(masterdemo), maxdias, maxsys) %>%
          unique()



bppost_all <- left_join(masterdemo %>% as.data.frame(),
                        bppost %>% as.data.frame() %>% select(pat_id, maxdias, maxsys),
                        by = 'pat_id') %>% 
              # 0s from match_bp are patients with a visit but no BP measured will consider it no bp measured.
              # NA's are from masterdemo with no corresponding visit from labs file; this will also consider no bp measured.
              mutate(maxdias = ifelse(maxdias == 0, NA, maxdias),
                     maxsys  = ifelse(maxsys  == 0, NA, maxsys)) %>% 
              mutate(HiBP = ifelse(!is.na(maxdias) & maxdias >= 90 | (!is.na(maxsys) & maxsys  >= 140 ), 1, NA)) %>% 
              mutate(HiBP = ifelse(!is.na(pat_id)  & is.na(HiBP), 0, HiBP))


post_any_bp <- bppost_all %>%
               filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys))) %>%
               select(pat_id, arm) %>%
               unique %>%
               group_by(arm) %>%
               tally() %>%
               spread(arm, n) %>%
               mutate(Domain = 'BP',
                      Category = '# with BP Checked in next 12 months')

post_bp_ovr_140_90 <- bppost_all %>%
                      filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys)) & HiBP == 1) %>%
                      select(pat_id, arm) %>%
                      unique %>%
                      group_by(arm) %>%
                      tally()%>% 
                      spread(arm, n) %>%
                      mutate(Domain = 'BP',
                             Category = 'BP in next 12 months over 140/90') 

post_bp_udr_140_90 <- bppost_all %>%
                      filter(enroll_date <= enrollcens & (!is.na(maxdias) |!is.na(maxsys)) & HiBP == 0) %>%
                      select(pat_id, arm) %>%
                      unique %>%
                      group_by(arm) %>%
                      tally()%>% 
                      spread(arm, n) %>%
                      mutate(Domain = 'BP',
                             Category = 'BP in next 12 months less 140/90')

BP <- rbind(prior_any_bp,
            prior_bp_udr_140_90,
            prior_bp_ovr_140_90,
            post_any_bp,
            post_bp_udr_140_90,
            post_bp_ovr_140_90) %>% 
      select(Domain, Category, everything())


# A1c
load(file = 'Inputs/a1c.RData') %>% as.data.frame()
a1c = a1c %>% select(pat_id = PatientEpicId,
                     csn = EncounterEpicCsn,
                     order_proc_id = PrimaryMrn,
                     component_id = LabComponentEpicId,
                     ord_value = LabResultValue,
                     ord_num_value = NumericValue,
                     res_d, 
                     enc_d)

#prior
a1c_prior <- left_join(x = masterdemo %>% as.data.frame(), 
                         y = left_join(masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                   a1c %>% as.data.frame() %>% filter(component_id %in% c(316664, 21328827, 1558024, 21489210)),
                                   by = 'pat_id') %>%
                                filter(res_d <= enroll_date & enroll_date - res_d <= 365) %>% 
                                unique() %>%
                                select(pat_id, ord_value, ord_num_value, res_d),
                         by = 'pat_id') %>% 
              mutate(a1c = ifelse(!is.na(ord_num_value), 1, 0),
                      val = as.numeric(gsub('>|<|%|\\+|/[0-9]{,3}','', ord_value))) %>% 
              mutate(val = ifelse(is.na(val), 0, val)) %>% 
              group_by(pat_id) %>% 
              mutate(a1c_present = max(a1c, na.rm = T),
                       maxa1cval = max(val, na.rm = T)) %>% 
              ungroup() %>% 
              mutate(maxa1cval = ifelse(maxa1cval == 0, NA, maxa1cval),
                           val= ifelse(val == 0, NA, val)) %>%
              unique() %>%
              as.data.frame()

prior_any_a1c <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & a1c_present == 1) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = '# with A1c lab in prior 12 months') %>%
                 select(Domain, Category, everything())%>%
                 as.data.frame()
  
prior_blw_6.5 <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & maxa1cval <= 6.5) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = 'A1C lab in prior 12 months <= 6.5%') %>%
                 select(Domain, Category, everything()) %>%
                 as.data.frame()

prior_ovr_6.5 <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & maxa1cval > 6.5) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = 'A1C lab in prior 12 months > 6.5%') %>%
                 select(Domain, Category, everything()) %>%
                 as.data.frame()

prior_blw_9.0 <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & maxa1cval <= 9) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = 'A1C lab in prior 12 months <= 9%') %>%
                 select(Domain, Category, everything()) %>%
                 as.data.frame()

prior_ovr_9.0 <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & maxa1cval > 9) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = 'A1C lab in prior 12 months > 9%') %>%
                 select(Domain, Category, everything()) %>%
                 as.data.frame()
           

# Post
a1c_post <- left_join(x = masterdemo %>% as.data.frame(),
                      y = left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                    y = a1c %>% as.data.frame() %>% filter(component_id %in% c(316664, 21328827, 1558024, 21489210)),
                                    by = 'pat_id') %>%
                                filter(res_d > enroll_date & res_d - enroll_date <= 365) %>% 
                                unique() %>%
                                select(pat_id, ord_value, ord_num_value, res_d),
                      by = 'pat_id') %>% 
              mutate(a1c = ifelse(!is.na(ord_num_value), 1, 0),
                     val = as.numeric(gsub('>|<|%|\\+|/[0-9]{,3}','', ord_value)))  %>% 
              mutate(val = ifelse(is.na(val), 0, val)) %>% 
              group_by(pat_id) %>% 
              mutate(a1c_present = max(a1c, na.rm = T),
                     maxa1cval   = max(val, na.rm = T)) %>% 
              ungroup() %>% 
              mutate(maxa1cval = ifelse(maxa1cval == 0, NA, maxa1cval),
                     val       = ifelse(val == 0, NA, val)) %>% unique() %>% as.data.frame()

post_any_a1c <- a1c_post %>%
                filter(enroll_date <= enrollcens & a1c_present == 1) %>%
                group_by(arm) %>%
                dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                spread(arm, Count) %>%
                mutate(Domain =  'A1C', Category = '# with A1C lab in 12 months after enrollment date') %>%
                select(Domain, Category, everything())%>%
                as.data.frame()

post_blw_6.5 <- a1c_post %>%
                filter(enroll_date <= enrollcens & maxa1cval <= 6.5) %>%
                group_by(arm) %>%
                dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                spread(arm, Count) %>%
                mutate(Domain =  'A1C', Category = 'A1C lab in  12 months after enrollment date <= 6.5%') %>%
                select(Domain, Category, everything()) %>%
                as.data.frame()

post_ovr_6.5 <- a1c_post %>%
                filter(enroll_date <= enrollcens & maxa1cval > 6.5) %>%
                group_by(arm) %>%
                dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                spread(arm, Count) %>%
                mutate(Domain =  'A1C', Category = 'A1C lab in  12 months after enrollment date > 6.5%') %>%
                select(Domain, Category, everything()) %>%
                as.data.frame()

post_blw_9.0 <- a1c_post %>%
                filter(enroll_date <= enrollcens & maxa1cval <= 9) %>%
                group_by(arm) %>%
                dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                spread(arm, Count) %>%
                mutate(Domain =  'A1C', Category = 'A1C lab in  12 months after enrollment date <= 9%') %>%
                select(Domain, Category, everything()) %>%
                as.data.frame()

post_ovr_9.0 <- a1c_post %>%
                filter(enroll_date <= enrollcens & maxa1cval > 9) %>%
                group_by(arm) %>%
                dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                spread(arm, Count) %>%
                mutate(Domain =  'A1C', Category = 'A1C lab in  12 months after enrollment date > 9%') %>%
                select(Domain, Category, everything()) %>%
                as.data.frame()


A1C <- rbind(prior_any_a1c,
             prior_blw_6.5,
             prior_ovr_6.5,
             prior_blw_9.0,
             prior_ovr_9.0,
             post_any_a1c,
             post_blw_6.5,
             post_ovr_6.5,
             post_blw_9.0,
             post_ovr_9.0)


# ACEI/STATIN
load(file = 'Inputs/match_med_generic.RData') %>% as.data.frame()

med <- match_med %>% 
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
prior_meds <- left_join(x = masterdemo %>% as.data.frame(),
                        y = left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y = med %>% as.data.frame(),
                                      by = 'pat_id') %>%
                          filter(order_d <= fucens & order_d < enroll_date & enroll_date - order_d <= 365) %>% #-- prior
                          # filter(order_d <= fucens & order_d <= enroll_date & enroll_date - order_d <= 365) %>% #-- prior
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

prior_acei <- prior_meds %>%
  select(pat_id, arm, ACE) %>% 
  unique()%>%
  group_by(arm) %>% 
  dplyr::summarise(n = sum(ACE), .groups = 'drop') %>% 
  spread(arm, n) %>% 
  mutate(Domain = 'ACEI',
         Category = 'With ACEI before enrollment')

prior_statin <- prior_meds %>%
  select(pat_id, arm, STATIN) %>% 
  unique()%>%
  group_by(arm) %>% 
  dplyr::summarise(n = sum(STATIN), .groups = 'drop') %>% 
  spread(arm, n) %>% 
  mutate(Domain = 'STATIN',
         Category = 'With STATIN before enrollment')

prior_ace_statin <- rbind(prior_acei, prior_statin) %>%
  select(Domain, Category, everything())


#post
post_meds <- left_join(x = masterdemo %>% as.data.frame(),
                       y = left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                     y = med %>% as.data.frame(),
                                     by = 'pat_id') %>%
                         filter(order_d <= fucens & order_d >= enroll_date & order_d - enroll_date <= 365) %>% #-- post
                         # filter(order_d <= fucens & order_d > enroll_date & order_d - enroll_date <= 365) %>% #-- post
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
  select(pat_id, enroll_date, arm, ACE, STATIN,DIURETICS, BETABLOCKERS, INSULIN, SGLT2, GLP1, NONINSULIN, PharmaceuticalClass,PharmaceuticalSubclass, TherapeuticClass) %>% 
  unique() 


post_acei <- post_meds %>%
  select(pat_id, arm, ACE) %>% 
  unique()%>%
  group_by(arm) %>% 
  dplyr::summarise(n = sum(ACE), .groups = 'drop') %>% 
  spread(arm, n) %>% 
  mutate(Domain = 'ACEI',
         Category = 'With documented new ACEI post enrollment')

post_statin <- post_meds%>%
  select(pat_id, arm, STATIN) %>% 
  unique()%>%
  group_by(arm) %>% 
  dplyr::summarise(n = sum(STATIN), .groups = 'drop') %>% 
  spread(arm, n) %>% 
  mutate(Domain = 'STATIN',
         Category = 'With documented new STATIN post enrollment')

post_ace_statin <- rbind(post_acei, post_statin) %>%
  select(Domain, Category, everything())


# Combine BP, Meds and A1C information to populate fidelity table
fidelity <- rbind(BP, prior_ace_statin, A1C) %>%
  mutate(A = paste(A, ' (', eval(round((A/as.numeric(dem_totl$A))*100,2)),'%)', sep = ''), 
         B  = paste(B, ' (', eval(round((B/as.numeric(dem_totl$B))*100,2)),'%)', sep = '')) %>%
  select(-Domain) 



fidelity_table <- kable(fidelity, 
                        style = 'html',
                        booktabs = T,
                        caption = 'Fidelity Markers') %>%
  #collapse_rows(columns = 1:2, valign = 'middle') %>% 
  kable_styling() %>% 
  kable_classic_2(full_width = F, 
                  html_font = "Montserrat") %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1, bold = T, width = '4in') %>% 
  column_spec(2, width = '1.5in')


fidelity_table
#fidelity_table %>% save_kable('Outputs/Fidelity.pdf')
#--------------


##A1C.ARB 
prior_ovr_8.0 <- a1c_prior %>%
                 filter(enroll_date <= enrollcens & maxa1cval >= 8) %>%
                 group_by(arm) %>%
                 dplyr::summarise(Count  = n_distinct(pat_id), .groups = 'drop') %>%
                 spread(arm, Count) %>%
                 mutate(Domain =  'A1C', Category = 'A1C lab in prior 12 months >= 8%') %>%
                 select(Domain, Category, everything()) %>%
                 as.data.frame()

ovr_8.0  <- a1c_prior %>%
            filter(enroll_date <= enrollcens & maxa1cval >= 8) %>% 
            select(pat_id, arm, maxa1cpre = maxa1cval) %>% 
            unique()

blw_8.0  <- a1c_post %>%
            filter(enroll_date <= enrollcens & maxa1cval < 8) %>% 
            select(pat_id, maxa1c_post = maxa1cval) %>% 
            unique()

A1C_delta <- inner_join(x = ovr_8.0, 
                        y = blw_8.0,
                        by = 'pat_id') %>%
             mutate(delta = maxa1c_post - maxa1cpre) %>%
             group_by(arm) %>%  
             dplyr::summarise(count = n_distinct(pat_id), .groups ='drop') %>% 
             ungroup() %>% 
             spread(arm, count) %>% 
             mutate(Domain = 'A1C',
                    Category ='#A1C changed from >= 8% to < 8%') %>% 
             select(Domain, Category, everything())


A1C_delta_mean <- inner_join(x = ovr_8.0, 
                             y = blw_8.0,
                             by = 'pat_id') %>%
                  mutate(delta = maxa1c_post - maxa1cpre) %>%
                  group_by(arm) %>% 
                  dplyr::summarise(Mean = round(mean(delta),2), .groups = 'drop') %>% 
                  ungroup() %>% 
                  spread(arm, Mean) %>%
                  mutate(Domain = 'A1C',
                         Category ='#A1C delta (mean of A1Cs from >= 8% to < 8%') %>% 
                  select(Domain, Category, everything())

A1C_delta_median <- inner_join(x = ovr_8.0, 
                               y = blw_8.0,
                               by = 'pat_id') %>%
                  mutate(delta = maxa1c_post - maxa1cpre) %>%
                  group_by(arm) %>% 
                  dplyr::summarise(Median = round(median(delta),2), .groups = 'drop') %>% 
                  ungroup() %>% 
                  spread(arm, Median) %>%
                  mutate(Domain = 'A1C',
                         Category ='#A1C delta (median of A1Cs from >= 8% to < 8%') %>% 
                  select(Domain, Category, everything())

     
A1C.ARB_1 <- rbind(post_ace_statin, prior_ovr_8.0, A1C_delta) %>% 
             mutate(Control = paste(Control, ' (', eval(round((Control/as.numeric(dem_totl$Control))*100,2)),'%)', sep = ''), 
                    `ICD - Pieces`  = paste(`ICD - Pieces`, ' (', eval(round((`ICD - Pieces`/as.numeric(dem_totl$`ICD - Pieces`))*100,2)),'%)', sep = '')) %>%
             select(-Domain) 

A1C.ARB_2 <- rbind(A1C_delta_mean, A1C_delta_median) %>%
             select(-Domain) 
             
A1C.ARB <- rbind(A1C.ARB_1, A1C.ARB_2)


A1C.ARB_table <- kable(A1C.ARB, 
                       style = 'html',
                       booktabs = T,
                       caption = 'A1C.ARB') %>%
                 #collapse_rows(columns = 1:2, valign = 'middle') %>% 
                 kable_styling() %>% 
                 kable_classic_2(full_width = F, 
                                 html_font = "Montserrat") %>% 
                 row_spec(0, bold = T) %>% 
                 column_spec(1, bold = T, width = '4in') %>% 
                 column_spec(2, width = '1.5in')

A1C.ARB_table
#A1C.ARB_table %>% save_kable('Outputs/A1C.ARB.pdf')

#---------------------------------------------------------------------------------------------------------------

#Enrollment Rate by month

enroll_rate_by_month <- masterdemo %>% 
                        as.data.frame() %>% 
                        mutate(Year =  year(enroll_date),
                               Month = month.abb[month(enroll_date)],
                               Week = lubridate::week(ymd(enroll_date))) %>% #lubridate:ymd
                        group_by(arm, Year, Month) %>% 
                        tally() %>% 
                        ungroup %>% 
                        dplyr::rename(Arm = arm,
                               N = n) %>% 
                        reshape2::dcast(Arm + Year ~ Month , value.var = 'N') %>%
                        arrange(Year, Arm) %>%
                        select(Year, Arm, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov,Dec) %>% 
                        mutate(across(everything(), ~replace_na(.,0)))%>% 
                        adorn_totals(fill = '',where = c('row','col')) %>% # row totals and col totals
                        mutate_if(is.numeric, ~formattable::comma(.,digits = 0)) # format numeric columns to thousand separated
                         

month <- kable(enroll_rate_by_month, 
               style = 'html',
               booktabs = T,
               caption = 'Enrollment by Year/Month') %>%
         kable_styling('striped','bordered') %>%
         add_header_above(c('','', 'N' = 13), bold = T) %>%
         kable_classic_2(full_width = F, 
                         html_font = "Montserrat") %>%
         row_spec(0, bold = T) %>%
         row_spec(nrow(enroll_rate_by_month), bold = T) %>% 
         column_spec(1, '1in') %>% 
         collapse_rows(1)
  
month 

#month %>% save_kable('Outputs/Enrollment by Year and Month.pdf')
#Enrollment Rate by quarter

enroll_rate_by_qtr <- masterdemo %>% 
                      as.data.frame() %>% 
                      mutate(Year =  year(enroll_date),
                             Quarter = quarters(enroll_date)) %>% 
                      group_by(arm, Year, Quarter) %>% 
                      tally() %>% 
                      ungroup %>%
                      dplyr::rename(Arm = arm,
                             N = n) %>%
                      reshape2::dcast(Arm + Year ~ Quarter, value.var = 'N') %>% 
                      arrange(Year,Arm) %>% 
                      select(Year, Arm, everything()) %>% 
                      mutate(across(everything(), ~replace_na(.,0))) %>% 
                      adorn_totals(fill = '',where = c('row','col')) %>% 
                      mutate_if(is.numeric, ~formattable::comma(.,digits = 0)) # format numeric columns to thousand separated



qtr <- kable(enroll_rate_by_qtr, 
             style = 'html',
             booktabs = T,
             caption = 'Enrollment by Quarter') %>%
       kable_styling('striped','bordered') %>% 
       add_header_above(c('','', 'N' = 5), bold = T) %>% 
       kable_classic_2(full_width = F, 
                       html_font = "Montserrat") %>%
       row_spec(0, bold = T) %>%
       row_spec(nrow(enroll_rate_by_qtr), bold = T) %>% 
       column_spec(1, '1in') %>% 
       collapse_rows(1)

qtr

#qtr %>% save_kable('Outputs/Enrollment by Quarter.pdf')

#Enrollment Rate by cluster/pcp_id

enroll_rate_by_cluster <- masterdemo %>% 
                          as.data.frame() %>% 
                          group_by(arm, pcp_id) %>% 
                          tally() %>% 
                          ungroup %>%
                          dplyr::rename(Arm = arm,
                                 N = n,
                                 'PCP ID' = pcp_id) %>%
                          mutate(Arm = str_to_title(Arm)) %>% 
                          reshape2::dcast( `PCP ID` ~ Arm, value.var = 'N') %>%
                          mutate(across(everything(), ~replace_na(., ''))) %>% 
                          mutate_if(is.numeric, ~formattable::comma(.,digits = 0)) # format numeric columns to thousand separated

                          
pcp <- kable(enroll_rate_by_cluster,
             style = 'html',
             booktabs = T,
             caption = 'Enrollment by PCP') %>%
       kable_styling('striped','bordered') %>%
       add_header_above(c('', 'N' = 2), bold = T) %>% 
       kable_classic_2(full_width = F, 
                       html_font = 'Montserrat') %>%
       row_spec(0, bold = T) %>% 
       column_spec(1, '1in') %>% 
       collapse_rows(1)

pcp
#pcp %>% save_kable('Outputs/Enrollment by PCP.pdf')




########################################################################################################################
######################## ----------- Baseline Characteristics (per new shell tables) -------------- ####################


# Count  <- masterdemo %>%
#           group_by (arm) %>%
#           dplyr::mutate(Count = n()) %>%
#           ungroup() %>% 
#           dplyr::mutate(All = n()) %>% 
#           select(arm, Count, All) %>% 
#           unique() %>%
#           mutate(Characteristics = 'Count',
#                  Category = '') %>% 
#           reshape2::dcast(Characteristics + Category + All ~ arm, value.var = 'Count') %>% 
#           arrange(desc(Control))%>%
#           mutate_all( ~lapply(.,as.character)) %>% 
#           mutate(Control = paste0(Control, " (", round(100*as.numeric(Control)/as.numeric(All), 1), "%)")) %>%
#           mutate(`ICD - Pieces` = paste0(`ICD - Pieces`, " (", round(100*as.numeric(`ICD - Pieces`)/as.numeric(All), 1), "%)")) %>% 
#           mutate_all(~lapply(., as.character)) %>% 
#           select(Characteristics, Control, `ICD - Pieces`)

Count  <- masterdemo %>%
  group_by (arm) %>%
  dplyr::mutate(Count = n()) %>%
  ungroup() %>% 
  dplyr::mutate(All = n()) %>% 
  select(arm, Count, All) %>% 
  unique() %>%
  mutate(Characteristics = 'Count',
         Category = '') %>% 
  reshape2::dcast(Characteristics + Category + All ~ arm, value.var = 'Count') %>% 
  arrange(desc(Control))%>%
  mutate_all( ~lapply(.,as.character)) %>% 
  mutate(Control = Control) %>%
  mutate(`ICD - Pieces` = `ICD - Pieces`) %>% 
  mutate_all(~lapply(., as.character)) %>% 
  select(Characteristics, Control, `ICD - Pieces`)


Age_Median   <- masterdemo %>%
          group_by (arm) %>%
          mutate(Age = paste0(round(median(age),1), " (", round(quantile(age)[2], 1), ", ", round(quantile(age)[4], 1), ")")) %>%
          ungroup() %>% 
          mutate(All = paste0(round(median(age),1), " (", round(quantile(age)[2], 1), ", ", round(quantile(age)[4], 1), ")")) %>%
          select(arm, Age, All) %>% 
          unique() %>%
          mutate(Characteristics = 'Median (Interquartile range)') %>% 
          reshape2::dcast(Characteristics + All ~ arm, value.var = 'Age') %>% #converting long format to wide format
          arrange(desc(Control)) %>% 
          mutate_all(~lapply(., as.character)) %>% 
          select(Characteristics, Control, `ICD - Pieces`)

Age_Mean   <- masterdemo %>%
  group_by (arm) %>%
  mutate(Age = paste0(round(mean(age),1), " +/- ", round(sd(age), 1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(age),1), " +/- ", round(sd(age), 1))) %>%
  select(arm, Age, All) %>% 
  unique() %>%
  mutate(Characteristics = '<b>Age</b>, Mean +/- SD (years)') %>% 
  reshape2::dcast(Characteristics + All ~ arm, value.var = 'Age') %>% #converting long format to wide format
  arrange(desc(Control)) %>% 
  mutate_all(~lapply(., as.character)) %>% 
  select(Characteristics, Control, `ICD - Pieces`)

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
        select(Characteristics, Control, `ICD - Pieces`)
                   
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
              select(Characteristics, Control, `ICD - Pieces`)
        
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
             select(Characteristics, Control, `ICD - Pieces`)

#BP
bp_bsln <- left_join(  x = masterdemo %>% as.data.frame() %>% select(pat_id, arm), 
                       y = left_join( x  = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y  = bp,
                                      by = 'pat_id') %>%
                           filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                           arrange(pat_id, desc(bp_d)) %>% 
                           filter(!duplicated(pat_id)) %>%
                           select(pat_id, enroll_date, sbp, dbp) %>% 
                           unique(),
                      by = 'pat_id') %>% 
            mutate(sbp = as.numeric(sbp),
                   dbp = as.numeric(dbp)) %>% 
            mutate(lt_140_90 = case_when(sbp < 140 & dbp < 90 ~ 'Yes',
                                         sbp >= 140 | dbp >= 90 ~ 'No',
                                         is.na(sbp) == TRUE ~ 'Unknown')) #195 Patients do not have BP records during baseline


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
  select(Characteristics, Control, `ICD - Pieces`)

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
  select(Characteristics, Control, `ICD - Pieces`)

sbp_lt140 <- left_join(  x = masterdemo %>% as.data.frame() %>% select(pat_id, arm), 
                       y = left_join( x  = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                      y  = bp,
                                      by = 'pat_id') %>%
                         filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                         arrange(pat_id, desc(bp_d)) %>% 
                         filter(!duplicated(pat_id)) %>%
                         select(pat_id, enroll_date, sbp, dbp) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp)) %>% 
  mutate(SBP_lt140 = case_when(sbp < 140 ~ 'Yes',
                               sbp >= 140 ~ 'No',
                               is.na(sbp) == TRUE ~ 'Unknown'))

dbp_lt90 <- left_join(  x = masterdemo %>% as.data.frame() %>% select(pat_id, arm), 
                         y = left_join( x  = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                        y  = bp,
                                        by = 'pat_id') %>%
                           filter(bp_d <= enroll_date & enroll_date - bp_d <=365) %>%
                           arrange(pat_id, desc(bp_d)) %>% 
                           filter(!duplicated(pat_id)) %>%
                           select(pat_id, enroll_date, sbp, dbp) %>% 
                           unique(),
                         by = 'pat_id') %>% 
  mutate(sbp = as.numeric(sbp),
         dbp = as.numeric(dbp)) %>% 
  mutate(DBP_lt90 = case_when(dbp < 90 ~ 'Yes',
                               dbp >= 90 ~ 'No',
                               is.na(dbp) == TRUE ~ 'Unknown'))



#A1c
# A1c
load(file = 'Inputs/a1c.RData') %>% as.data.frame()
a1c = a1c %>% select(pat_id = PatientEpicId,
                     csn = EncounterEpicCsn,
                     order_proc_id = PrimaryMrn,
                     component_id = LabComponentEpicId,
                     ord_value = LabResultValue,
                     ord_num_value = NumericValue,
                     res_d, 
                     enc_d)

a1c_bsln <- left_join(  x = masterdemo %>% as.data.frame(), 
                        y = left_join( x  = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
                                       y  = a1c %>% as.data.frame() %>% filter(component_id %in% c(316664, 21328827, 1558024, 21489210)),
                                       by = 'pat_id') %>%
                             filter(res_d < enroll_date & enroll_date - res_d <=365) %>%
                             select(pat_id, ord_value, ord_num_value, res_d) %>% 
                             unique(),
                         by = 'pat_id') %>%  
              mutate(a1c = ifelse(!is.na(ord_num_value), 1, 0),
                      val = as.numeric(gsub('>|<|%|\\+|/[0-9]{,3}','', ord_value))) %>% 
              select(pat_id, arm, res_d, val) %>% 
              arrange(pat_id, desc(res_d)) %>% 
              filter(!duplicated(pat_id)) %>% 
              mutate(lt_7.5 = case_when(val <= 7.5 ~ 'Yes',
                                        val > 7.5 ~ 'No',
                                        is.na(val) == TRUE ~ 'Unknown'))



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
              select(Characteristics, Control, `ICD - Pieces`) %>% 
              select(Characteristics, Control, `ICD - Pieces`)
              

#eGFR
load(file = 'Inputs/egfr.RData') %>% as.data.frame()
egfr = egfr %>% select(pat_id = PatientEpicId,
                       csn = EncounterEpicCsn,
                       order_proc_id = PrimaryMrn,
                       component_id = LabComponentEpicId,
                       ord_value = LabResultValue,
                       ord_num_value = NumericValue,
                       res_d, 
                       enc_d)

egfr_bsln <- left_join(x = masterdemo %>% as.data.frame(), 
                       y = left_join(x = masterdemo %>%
                                       as.data.frame() %>%
                                       select(pat_id, enroll_date),
                                     y = egfr %>%
                                       as.data.frame() %>%
                                       filter(component_id %in% c(6578054, 6578053, 27645213, 27645211)),
                                     by = 'pat_id') %>%
                         filter(res_d < enroll_date & enroll_date - res_d <=365) %>%
                         select(pat_id, ord_value, ord_num_value, res_d) %>% 
                         unique(),
                       by = 'pat_id') %>% 
  mutate(val = as.numeric(gsub('>|<','', ord_value))) %>% 
  mutate(val = ifelse(is.na(val), 0, val)) %>% 
  select(pat_id, arm, res_d, ord_value,val) %>% 
  arrange(pat_id, desc(res_d)) %>%
  filter(!duplicated(pat_id)) %>%
  mutate(egfr_val = case_when(val >= 60 ~ '>= 60',
                              val >= 45 & val < 60 ~ '45 to < 60',
                              val >= 30 & val < 45 ~ '30 to < 45',
                              val >= 15 & val < 30 ~ '15 to < 30',
                              val >  0  & val < 15 ~ '0 to < 15',
                              TRUE ~ 'Unknown'))


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
                    select(Characteristics, Control, `ICD - Pieces`)

#BMI & weight
load(file = 'Inputs/bmi.RData') %>% as.data.frame()
bmi <- bmi %>%
       select(pat_id = PatientEpicId,
              csn = EncounterEpicCsn,
              patient_mrn = PrimaryMrn,
              DateValue,
              Height = HeightInInches,
              Weight = WeightInOunces,
              BMI = BodyMassIndex
              )
weight_bmi_bsln <- left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date, arm),
                             y = bmi %>% as.data.frame(),
                             by = 'pat_id') %>% 
                   filter(DateValue <= enroll_date) %>% 
                   select(pat_id, patient_mrn, arm,  enroll_date, DateValue, Weight, Height, BMI) %>% 
                   unique() %>%
                   mutate(Weight = round((Weight*0.0283495),2),
                          BMI = ifelse(BMI > 500 | is.na(BMI), mean(BMI, na.rm = T), BMI)) %>% #imputing 7 patients
                   arrange(pat_id, desc(DateValue)) %>% 
                   filter(!duplicated(pat_id)) 
#BMI
BMI    <- weight_bmi_bsln %>% 
          group_by(arm) %>% 
          mutate(Mean_BMI = paste(eval(round(mean(BMI, na.rm = T),1)),'+/-',eval(round(sd(BMI, na.rm = T),1)))) %>%
          ungroup() %>% 
          mutate(All = paste(eval(round(mean(BMI, na.rm = T),1)),'+/-',eval(round(sd(BMI, na.rm = T),1)))) %>% 
          select(arm, Mean_BMI, All) %>% 
          unique() %>% 
          mutate(Characteristics = '<b>BMI</b>, Mean +/- SD (kg/m<sup>2</sup>)') %>% 
          reshape2::dcast(Characteristics + All ~ arm, value.var = 'Mean_BMI') %>% 
          mutate_all( ~lapply(.,as.character)) %>% 
          select(Characteristics, Control, `ICD - Pieces`)

#Weight
Weight    <- weight_bmi_bsln %>% 
          group_by(arm) %>%
          mutate(Mean_Weight = paste(eval(round(mean(Weight, na.rm = T),1)),'+/-',eval(round(sd(Weight, na.rm = T),1)))) %>%
          ungroup() %>% 
          mutate(All = paste(eval(round(mean(Weight, na.rm = T),1)),'+/-',eval(round(sd(Weight, na.rm = T),1)))) %>% 
          select(arm, Mean_Weight, All) %>% 
          unique() %>% 
          mutate(Characteristics ='<b>Weight</b>, Mean +/- SD (kg)') %>% 
          reshape2::dcast(Characteristics + All ~ arm, value.var = 'Mean_Weight') %>% 
          mutate_all( ~lapply(.,as.character)) %>% 
          select(Characteristics, Control, `ICD - Pieces`)

#Cholesterol

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

cholest_tot <- left_join(x = masterdemo %>% as.data.frame(), 
                         y = left_join(x = masterdemo %>%
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
               filter(!duplicated(pat_id)) 

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
                select(Characteristics, Control, `ICD - Pieces`)


cholest_ldl <- left_join(x = masterdemo %>% as.data.frame(), 
                         y = left_join(x = masterdemo %>%
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
               select(pat_id, arm, res_d, val) %>% 
               arrange(pat_id, desc(res_d)) %>%
               filter(!duplicated(pat_id)) %>% 
               mutate(ldl_val = case_when(val >= 160  ~ 'High (>=160 mg/dL)',
                                           val >= 95 & val < 160 ~ 'Normal (95 to < 160 mg/dL)',
                                           val >= 0 & val < 95 ~ 'Low (0 to < 95 mg/dL)',
                                           TRUE ~ 'Unknown'))

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
              select(Characteristics, Control, `ICD - Pieces`)




#Medication - acei/arb, statin, diuretics, beta blockers, insulin, SGLT-2 inhibitors, GLP-1 receptor agonist, Other agents for Diabetes (non-insulin)

med_acei_arb_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                             y = prior_meds %>% 
                               select(pat_id, arm, PharmaceuticalSubclass) %>%
                               filter(grepl('ACE Inhibitor|Angiotensin II', PharmaceuticalSubclass)) %>%
                               unique() %>% 
                               mutate(ACEI_ARB = 'Yes') %>% 
                               select(-PharmaceuticalSubclass) %>% 
                               unique(),
                             by = c('pat_id','arm')) %>% 
  mutate_at(vars(ACEI_ARB), ~replace(., is.na(.),'No')) #if the patient did not have any acei/arb during baseline, mark them no


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
  select(Characteristics, Control, `ICD - Pieces`)

med_statin_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                           y = prior_meds %>% 
                             select(pat_id, arm, PharmaceuticalSubclass) %>%
                             filter(PharmaceuticalSubclass %in% c('Antihyperlipidemic - HMG CoA Reductase Inhibitors (statins)')) %>% 
                             unique() %>% 
                             mutate(STATIN = 'Yes') %>% 
                             select(-PharmaceuticalSubclass) %>% 
                             unique(),
                           by = c('pat_id','arm')) %>% 
  mutate_at(vars(STATIN), ~replace(., is.na(.),'No')) #if the patient did not have any statin, mark them no


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
  select(Characteristics, Control, `ICD - Pieces`)

med_dieuretics_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
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
  filter(DIURETICS != "OSMOTIC DIURETICS")


med_dieuretics <- med_dieuretics_0 %>% # medication prescription during baseline
  select(pat_id, arm, DIURETICS) %>% 
  mutate(DIURETICS = ifelse(DIURETICS == 'NONE', 'NONE', "Any diuretics")) %>% 
  unique() %>% 
  group_by(arm, DIURETICS) %>% 
  dplyr::summarise(n = n())%>%
  mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
  filter(DIURETICS != 'NONE') %>% 
  select(-n) %>% 
  mutate(Characteristics = 'Any Diuretics') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all(~lapply(., as.character))


med_bb_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
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
  mutate(BB = ifelse(BB == "None", "None", "Any Beta Blockers"))


med_bb <- left_join(x = med_bb_0 %>%
                      select(pat_id, arm, BB) %>% 
                      unique() %>% 
                      group_by(arm, BB) %>% 
                      dplyr::summarise(n = n())%>%
                      mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)") ) %>% 
                      filter(BB != 'None') %>% 
                      select(-n),
                    
                    y = med_bb_0 %>%
                      select(pat_id, BB) %>% 
                      unique() %>% 
                      group_by(BB) %>% 
                      dplyr::summarise(n = n())%>%
                      mutate(All = paste0(n, " (", round(100*n/sum(n),2), "%)")) %>% 
                      filter(BB != 'None') %>% 
                      select(-n),
                    by = 'BB') %>% 
  mutate(Characteristics = 'Any Beta Blockers') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))


med_insulin_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                            y = prior_meds %>% 
                              select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                              filter(PharmaceuticalClass %in% ('INSULINS')) %>% 
                              unique() %>% 
                              mutate(INSULIN = 'Yes') %>% 
                              select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                              unique(),
                            by = c('pat_id','arm')) %>% 
  mutate_at(vars(INSULIN), ~replace(., is.na(.),'No')) 


med_insulin <- left_join(x = med_insulin_0 %>% 
                           select(pat_id, arm, INSULIN) %>% 
                           unique() %>% 
                           group_by(arm, INSULIN) %>% 
                           dplyr::summarise(n = n())%>%
                           mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)") ) %>% 
                           filter(INSULIN != 'No') %>% 
                           select(-n),
                         
                         y = med_insulin_0 %>% 
                           select(pat_id, INSULIN) %>% 
                           unique() %>% 
                           group_by(INSULIN) %>% 
                           dplyr::summarise(n = n())%>%
                           mutate(All = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
                           filter(INSULIN != 'No') %>% 
                           select(-n),
                         by = 'INSULIN') %>% 
  mutate(Characteristics = 'Insulin') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))

med_SGLT2_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                          y = prior_meds %>% 
                            select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                            filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('SGLT-2',PharmaceuticalSubclass)) %>% 
                            unique() %>% 
                            mutate(SGLT2 = 'Yes') %>% 
                            select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                            unique(),
                          by = c('pat_id','arm')) %>% 
  mutate_at(vars(SGLT2), ~replace(., is.na(.),'No'))


med_SGLT2 <- left_join(x = med_SGLT2_0 %>%
                         select(pat_id, arm, SGLT2) %>% 
                         unique() %>% 
                         group_by(arm, SGLT2) %>% 
                         dplyr::summarise(n = n())%>%
                         mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
                         filter(SGLT2 != 'No') %>% 
                         select(-n),
                       
                       y = med_SGLT2_0 %>% 
                         select(pat_id, SGLT2) %>% 
                         unique() %>% 
                         group_by(SGLT2) %>% 
                         dplyr::summarise(n = n())%>%
                         mutate(All = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
                         filter(SGLT2 != 'No') %>% 
                         select(-n),
                       by = 'SGLT2') %>% 
  mutate(Characteristics = 'SLGT-2 Inhibitor',
         Category = '') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))

med_GLP1_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                         y = prior_meds %>% 
                           select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                           filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & grepl('GLP-1',PharmaceuticalSubclass)) %>% 
                           unique() %>% 
                           mutate(GLP1 = 'Yes') %>% 
                           select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                           unique(),
                         by = c('pat_id','arm')) %>% 
  mutate_at(vars(GLP1), ~replace(., is.na(.),'No'))


med_GLP1 <- med_GLP1_0 %>% 
                        select(pat_id, arm, GLP1) %>% 
                        unique() %>% 
                        group_by(arm, GLP1) %>% 
                        dplyr::summarise(n = n())%>%
                        mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
                        filter(GLP1 != 'No') %>% 
                        select(-n) %>% 
  mutate(Characteristics = 'GLP-1 Receptor Agonist') %>% 
  reshape2::dcast(Characteristics  ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))

med_othr_agents_diab_0 <- left_join( x = masterdemo %>% select(pat_id, arm),
                                     y = prior_meds %>% 
                                       select(pat_id, arm, PharmaceuticalClass, PharmaceuticalSubclass, TherapeuticClass) %>%
                                       filter(!PharmaceuticalClass %in% ('INSULINS') & TherapeuticClass %in% ('ANTIHYPERGLYCEMICS') & !grepl('SGLT-2|GLP-1',PharmaceuticalSubclass)) %>% 
                                       unique() %>% 
                                       mutate(OtherAgents = 'Yes') %>% 
                                       select(-PharmaceuticalClass, - PharmaceuticalSubclass, -TherapeuticClass) %>% 
                                       unique(),
                                     by = c('pat_id','arm')) %>% 
  mutate_at(vars(OtherAgents), ~replace(., is.na(.),'No')) #if the patient did not have any insulin, mark them no


med_othr_agents_diab <- med_othr_agents_diab_0 %>% # medication prescription during baseline
                                    select(pat_id, arm, OtherAgents) %>% 
                                    unique() %>% 
                                    group_by(arm, OtherAgents) %>% 
                                    dplyr::summarise(n = n())%>%
                                    mutate(percent = paste0(n, " (", round((n/sum(n))*100,2), "%)")) %>% 
                                    filter(OtherAgents != 'No') %>% 
                                    select(-n)%>% 
  mutate(Characteristics = 'Other non-insulin agents for Diabetes') %>% 
  reshape2::dcast(Characteristics ~ arm , value.var = 'percent') %>% 
  mutate_all( ~lapply(.,as.character))

# load("Inputs/protein.RData")
# km_forProtein <- readxl::read_excel("~/T-Drive/NIH Chronic Disease Management/Data/ICD_Pieces/PHHS/Outputs/PHHS_KM_v10.xlsx")
load("~/T-Drive/NIH Chronic Disease Management/Data/Shelley Handoff/PHHS SHELLEY/lab_backfill_syc_final-20181215to20190701.RData")
load("~/T-Drive/NIH Chronic Disease Management/Data/Shelley Handoff/PHHS SHELLEY/lab_cumulative-upto20181231.RData")
lab_cumulative <- lab_cumulative %>% mutate(pat_id = unfactor(pat_id),
                                            result_date = as.Date(result_date),
                                            ord_value = unfactor(ord_value))  
lab_cumulative <- lab_cumulative %>% filter(component_id %in%  c(1551412, 1552111, 1557742, 33418450))
lab_backfill_syc_final <- lab_backfill_syc_final %>% mutate(result_date = as.Date(result_date)) %>% filter(component_id %in%  c(1551412, 1552111, 1557742, 33418450))
regexp <- "[[:digit:]]+"
protein_lab = rbind(x = lab_backfill_syc_final %>% select(pat_id, result_date, component_id, ord_value, ord_num_value),
                     y = lab_cumulative %>% select(pat_id, result_date, component_id, ord_value, ord_num_value)) %>% 
  filter(!grepl('^[A-Za-z]', ord_value)) %>% 
  mutate(NumericValue = ifelse(component_id %in% c(1551412, 1552111, 1557742) & grepl('^>', ord_value), 
                               as.numeric(str_extract(ord_value, regexp)) + 1,
                               ifelse(component_id %in% c(1551412, 1552111, 1557742) & (ord_value == "NEGATIVE" | ord_value == "Negative") ,
                                      -1,
                                      ifelse(component_id %in% c(33418450) & grepl('^>', ord_value), 
                                             as.numeric(str_extract(ord_value, regexp)) + 0.1,
                                             as.numeric(ord_value)))))

# most recent protein data before enrollment
prtn <- inner_join(x = protein_lab, 
                   y = masterdemo %>% select(pat_id, enroll_date),
                   b = "pat_id") %>% 
  filter(result_date <= enroll_date & enroll_date - result_date < 540) %>% # 540 days = 18 months
  arrange(pat_id, desc(result_date)) %>% 
  filter(!duplicated(pat_id)) %>% 
  mutate(category = ifelse((component_id %in% c(1551412, 1552111, 1557742) & NumericValue < 30) | (component_id %in% c(33418450) & NumericValue < 0.15),
                           "No",
                           ifelse((component_id %in% c(1551412, 1552111, 1557742) & NumericValue >= 30 & NumericValue < 300) | 
                                    (component_id %in% c(33418450) & NumericValue >= 0.15 & NumericValue < 0.2),
                                  "Yes",
                                  "Yes"))) #it used to have 3 categories, hence 2 ifelse's. Now there're only 2 and one can delete the second ifelse

prtn1 <- left_join(x = masterdemo %>% select(pat_id, arm),
                  y = prtn, 
                  by = "pat_id") %>% 
  mutate(category = ifelse(is.na(category), "Unknown", category))
  
proteinuria <- prtn1  %>%
  group_by(arm, category) %>% #by arm
  dplyr::summarise(n = n()) %>% 
  mutate(percent = paste0(n , " (", round(n/sum(n)*100,1), "%)") ) %>% na.omit() %>% 
  select(-n) %>% 
  mutate(Characteristics = category) %>% 
  reshape2::dcast(Characteristics ~ arm, value.var = 'percent') %>%
  mutate_all(., ~lapply(., as.character)) %>% 
  select(Characteristics, Control, `ICD - Pieces`)# %>% 
  # rename(`ICD-Pieces` = `ICD - Pieces`, `Standard of care` = Control)

#comorbidities. ICD-10 Codes are coming from R-package called comorbidity
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
load(file = 'Inputs/pl.RData') %>% as.data.frame()
problem_list <- left_join(x = masterdemo %>% select(pat_id),
                          y = left_join(x = masterdemo %>% select(pat_id, enroll_date, arm),
                                        y = pl %>% select(pat_id = PatientEpicId,
                                                          Diag_date = DateValue,
                                                          Code = DTValue),
                                        by = 'pat_id') %>% 
                              filter(Diag_date <= enroll_date) %>% 
                              select(pat_id, Code) %>%
                              mutate(Code = gsub(Code, pattern = '\\.',replacement = '')), #remove periods from  ICD-10 Codes
                          by = 'pat_id') # to retrieve list of all patients
                           
charlson10 <- left_join( x = comorbidity(x = problem_list, #computes charlson comorbidity score
                                         id = 'pat_id',
                                         code = 'Code',
                                         score = 'charlson',
                                         icd = 'icd10',
                                         assign0 = FALSE),
                         y = masterdemo %>% select(pat_id, arm), #to retrieve arms of all patients
                         by = 'pat_id')
              



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
                               summarise_at(vars(ami:aids), ~ (paste0(sum(.), " (", round((sum(.)/length(.))*100,1), "%)"))) %>% 
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
               dplyr::rename(Characteristics = Disease, Control = A, `ICD - Pieces` = B) %>% 
               mutate_all(., ~lapply(.,as.character)) %>% 
               select(Characteristics, Control, `ICD - Pieces`)
              
comorb_prop <- comorb_prop[c(2, 3, 4),] # rearrange the rows, also remove "Renal disease"

# comorb_prop <- comorb_prop[c(1:9, 15, 10, 11, 12, 14, 16, 17),] # rearrange the rows, also remove "Renal disease"
 
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
                 select(Characteristics, Control, `ICD - Pieces`)

save(Count, Age_Mean,
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
     file = "Outputs/Table1Components.RData")

load("Outputs/Table1Components.RData")

baseline_characteristics <- rbind( 
      # data.frame("Characteristics" = c("<b>Age</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      Age_Mean,
      Male,
      data.frame("Characteristics" = c("<b>Ethnicity</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      Ethnicity,
      data.frame("Characteristics" = c("<b>Race</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      Race,
      data.frame("Characteristics" = c("<b>Blood Pressure</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      Bp_mean_SBP,
      Bp_mean_DBP,
      a1c_mean,
      egfr_mean,
      BMI,
      Weight,
      cholest_tot_prop,
      cholest_ldl_prop,
      data.frame("Characteristics" = c("<b>Proteinuria</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      proteinuria,
      data.frame("Characteristics" = c("<b>Medication</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      med_statin,
      med_acei_arb,
      med_dieuretics,
      med_bb,
      med_insulin,
      med_SGLT2,
      med_GLP1,
      med_othr_agents_diab,
      data.frame("Characteristics" = c("<b>Comorbidities</b>"), "Control" = c(""), "ICD - Pieces" = c(""),stringsAsFactors = F,  check.names = FALSE),
      comorb_score,
      CAD_prop,
      comorb_prop
      ) 
colnames(baseline_characteristics) <- c("Characteristics", paste0("Standard of care <br>(n = ", format(Count$Control, big.mark = ","), ")"), 
+                                         paste0("ICD-Pieces <br>intervention <br>(n = ", format(Count$`ICD - Pieces`, big.mark = ","), ")"))
baseline_characteristics <- baseline_characteristics[,c(1, 3, 2)]
DoubleLine <- c(1, 2, 6, 12, 15:21, 25, 34)

bsln_arm <- kable(baseline_characteristics,
  style = 'html', escape = F, align = "c",
  caption = 'Baseline Characteristics: Parkland Health and Hospital System Cohort',
  booktabs = T, row.names = F) %>%
  #add_header_above(c("." = 1, "PHHS" = 2), bold = T, align = "c", line = T, background = "lightskyblue",
                   #extra_css = " border-right: solid; border-left: solid;") %>%
  kable_classic_2(full_width = F,
                  html_font = 'Times New Roman') %>%
  row_spec(0, bold = T, extra_css = "vertical-align: middle; border-right: solid; border-left: solid;") %>%
  row_spec(DoubleLine,
           extra_css = "border-bottom-style: double; background-color: white;") %>%
  column_spec(c(1, 2, 3),  extra_css = "border-right: solid; border-left: solid;") %>% 
  column_spec(1, extra_css = "text-align:left;") %>%
  collapse_rows(1) %>%   
  kable_styling(latex_options = "scale_down", font_size = 12) %>%
  row_spec(0, background = "lightskyblue") #%>% 
  # row_spec(c(3), bold = c(T, F, F),
  #          extra_css = "border-bottom-style: double; background-color: white" )%>% 
bsln_arm
# bsln_arm %>% save_kable('Outputs/PHHS Baseline Characteristics (by arm).pdf')

bsln_overall <- kable(baseline_characteristics %>% select(Characteristics, Category, All),
                style = 'html',
                booktabs = T,
                caption = 'PHHS - Baseline Characteristics') %>%
                kable_styling('striped','bordered') %>%
                kable_classic_2(full_width = F, 
                                html_font = 'Arial') %>%
                row_spec(0, bold = T) %>% 
                column_spec(1:2,'3.8in') %>% 
                collapse_rows(1) %>% 
                kable_styling(latex_options = "scale_down", font_size = 20)
# bsln_overall %>% save_kable('Outputs/PHHS Baseline Characteristics (overall).pdf')

####################### KM file V8 ####################### 

# km_complete_final_obs_subgroups <- read.xlsx("Outputs/Final_version/PHHS_KM_v11.xlsx") %>% 
#   select(-`30-Day.Readmissions`, -n) %>% 
#   mutate(start_dt = openxlsx::convertToDate(start_dt),
#          Censor_date = openxlsx::convertToDate(Censor_date),
#          Death_date = openxlsx::convertToDate(Death_date),
#          Hosp_date = openxlsx::convertToDate(Hosp_date),
#          Dial_date = openxlsx::convertToDate(Dial_date),
#          Admit_Date_Observation = openxlsx::convertToDate(Admit_Date_Observation))

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs, 
                                             y = left_join(x = egfr_bsln, 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn) %>% 
                                               select(StudyID, egfr_val), 
                                             by = "StudyID")

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = dbp_lt90 %>% select(pat_id, DBP_lt90), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn) %>% 
                                               select(StudyID, DBP_lt90), 
                                             by = "StudyID")

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = sbp_lt140 %>% select(pat_id, SBP_lt140), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn) %>% 
                                               select(StudyID, SBP_lt140), 
                                             by = "StudyID")

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = CAD %>% select(pat_id, Code), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn, CAD = Code) %>% 
                                               select(StudyID, CAD), 
                                             by = "StudyID")

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = charlson10 %>% select(pat_id, chf), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn, CHF = chf) %>% 
                                               select(StudyID, CHF), 
                                             by = "StudyID") 

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = charlson10_AgeAdj %>% select(pat_id, ageAdjScore), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn) %>% 
                                               select(StudyID, ageAdjScore), 
                                             by = "StudyID") %>% dplyr::rename(Age_adj_Charlson = ageAdjScore)

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = left_join(x = a1c_bsln %>% select(pat_id, lt_7.5), 
                                                           y = masterdemo %>% select(pat_id, patient_mrn), 
                                                           by = "pat_id") %>% 
                                               dplyr::rename(StudyID = patient_mrn) %>% 
                                               select(StudyID, lt_7.5), 
                                             by = "StudyID") %>% dplyr::rename(HbA1C_lt7.5 = lt_7.5)

km_complete_final_obs_subgroups <- km_complete_final_obs_subgroups %>% 
  mutate(Primary_Outcome = case_when(HospY == "Y" | observation_flag == 1 ~ "Yes", 
                                     TRUE ~ "No"))

merged_mini <- left_join(x = dfhc_obs %>% 
                           dplyr::rename(patient_mrn = Pat_MRN_ID),
                         y = masterdemo %>% select(patient_mrn, enroll_date) %>% mutate(patient_mrn = unfactor(patient_mrn)),
                         by = "patient_mrn") %>% dplyr::rename(StudyID = patient_mrn) %>% 
  mutate(StudyID = as.character(StudyID)) %>% 
  mutate(Admit_Date = as.Date(Admit_Date),
         Discharge_Date = as.Date(Discharge_Date))

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups,
                                             y = merged_mini %>%
                                               filter(Admit_Type == "Medical Emergency" & Submission_Purpose == "O" & Is_observation_room == 0 &
                                                        Admit_Date < (enroll_date +365) & Admit_Date >=enroll_date) %>% 
                                               select(StudyID) %>% 
                                               unique() %>% mutate(ED_Visit = 1) %>% 
                                               mutate(StudyID = as.character(StudyID)),
                                             by = "StudyID") %>% 
                                    mutate(ED_Visit = case_when(ED_Visit == 1 ~ "Yes",
                                                                TRUE ~ "No"))

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = merged_mini %>% 
                                               select(StudyID, Submission_Purpose, Is_observation_room, Admit_Date,
                                                      enroll_date, Discharge_Date) %>% 
                                               filter (Submission_Purpose=='I' & 
                                                         Is_observation_room==0 & 
                                                         Admit_Date < (enroll_date +365) & 
                                                         Admit_Date >=enroll_date) %>%
                                               group_by (StudyID) %>% arrange(Admit_Date) %>%
                                               mutate(fu_admit =Admit_Date - lag(Discharge_Date)) %>% 
                                               filter(fu_admit <=30 & fu_admit > 0) %>% tally(),
                                             by = "StudyID") %>% 
                                    mutate(`30-Day Readmissions` = case_when(n >= 1 ~ "Yes",
                                                                TRUE ~ "No")) %>% 
  select(-n)

km_complete_final_obs_subgroups <- km_complete_final_obs_subgroups %>% mutate(Arm = case_when(Arm == "ICD - Pieces" ~ "ICD-Pieces",
                                                                                              TRUE ~ "Control"))

DiseaseSpecific <- AHRQ %>% select(contains("PQI"), contains("IQI")) %>% unlist() %>% na.omit()

findDiseaseSpecific <- function(x) {
  res = ifelse(length(which(x%in% DiseaseSpecific)) > 0, 1, 0)
  res
}

DiseaseSpecific = apply(re, 1, FUN = findDiseaseSpecific)
DiseaseSpecific <- cbind(re$patient_mrn, DiseaseSpecific) %>% as.data.frame()  %>% 
  dplyr::rename(StudyID = V1) %>% 
  mutate(DiseaseSpecific = unfactor(DiseaseSpecific))
DiseaseSpecific <- DiseaseSpecific %>% group_by(StudyID) %>% dplyr::summarise(n = max(DiseaseSpecific)) 

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = DiseaseSpecific, 
                                             by = "StudyID") 


CV_Procedure <- AHRQ %>% select(CardiacProc) %>% unlist() %>% na.omit()

findCV_Event <- function(x) {
  res = ifelse(length(which(x%in% CV_Procedure)) > 0, 1, 0)
  res
}

CV_Procedure = apply(re, 1, FUN = findCV_Event)
CV_Procedure <- cbind(re$patient_mrn, CV_Procedure) %>% as.data.frame()  %>% 
  dplyr::rename(StudyID = V1) %>% 
  mutate(CV_Procedure = unfactor(CV_Procedure))
CV_Procedure <- CV_Procedure %>% group_by(StudyID) %>% summarise(CV_Procedure = max(CV_Procedure)) 

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = CV_Procedure, 
                                             by = "StudyID") 


CV_Event <- AHRQ %>% select(CV_Event) %>% unlist() %>% na.omit()

findCV_Event <- function(x) {
  res = ifelse(length(which(x%in% CV_Event)) > 0, 1, 0)
  res
}

CV_Event = apply(re, 1, FUN = findCV_Event)
CV_Event <- cbind(re$patient_mrn, CV_Event) %>% as.data.frame()  %>% 
  dplyr::rename(StudyID = V1) %>% 
  mutate(CV_Event = unfactor(CV_Event))
CV_Event <- CV_Event %>% group_by(StudyID) %>% summarise(CV_Event = max(CV_Event)) 

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = CV_Event, 
                                             by = "StudyID") 


TransplantationFun <- function(x) {
  res <- ifelse(length(which(x %in% c("Z94.0", "Z940"))) > 0, 1, 0)
  res
} 

Transplantation = apply(re %>% select(contains("DX")), 1, FUN = TransplantationFun)
Transplantation <- cbind(re$patient_mrn, Transplantation) %>% as.data.frame()  %>% 
  dplyr::rename(StudyID = V1) %>% 
  mutate(Transplantation = unfactor(Transplantation))
Transplantation <- Transplantation %>% group_by(StudyID) %>% summarise(Transplantation = max(Transplantation)) %>% 
  mutate(Transplantation = ifelse(StudyID == 315090, 0, Transplantation))

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = Transplantation, 
                                             by = "StudyID") 


km_complete_final_obs_subgroups <- km_complete_final_obs_subgroups %>% 
  mutate(Age_adj_Charlson = case_when(Age_adj_Charlson == 1 | Age_adj_Charlson == 2 ~ 'Mild',
                                      Age_adj_Charlson == 3 | Age_adj_Charlson == 4 ~ 'Moderate',
                                      TRUE ~ 'Severe'))


macr_phhs <- left_join(x = labs_macr_f[labs_macr_f$pat_id %in% masterdemo$pat_id,],
                       y = masterdemo %>% select(pat_id, enroll_date),
                       by = "pat_id") #%>% filter(val >= 300)

macr_phhs_within1year <- macr_phhs %>% filter((res_d <= enroll_date) & (enroll_date - res_d < 365)) %>% 
  arrange(pat_id, res_d) %>% filter(!duplicated(pat_id)) %>% 
  mutate(macr_gt30 = case_when(val < 30 ~"A1",
                               val >=30 & val <= 300 ~ "A2",
                               val > 300 ~ "A3"))
  

# macr_phhs_within1year <- macr_phhs %>% filter((res_d < enroll_date) & (enroll_date - res_d < 365)) %>% 
#   mutate(within1year = 1)
# 
# macr_phhs <- left_join(x = macr_phhs, y = macr_phhs_within1year %>% select(pat_id, within1year),
#                        by = "pat_id") %>% filter(!is.na(within1year)) %>% unique()
# 
# macr_phhs1 <- left_join(x = macr_phhs, y = macr_phhs %>% group_by(pat_id) %>% summarise(n = n()) %>% filter(n>1), 
#                        by = "pat_id") %>% arrange(pat_id, res_d) %>% filter(!is.na(n)) %>% unique() 
# macr_phhs1[ , diff := res_d - shift(res_d), by = pat_id]   
# macr_phhs2 <- macr_phhs1 %>% group_by(pat_id) %>% summarise(m = sum(diff, na.rm = T)) %>% filter(m > 90) %>% mutate(macr_gt30 = "Yes")
# macr_phhs2 <- left_join(x = macr_phhs2, y = masterdemo %>% select(pat_id, patient_mrn), by = "pat_id")
macr_phhs2 <- left_join(x = macr_phhs_within1year, y = masterdemo %>% select(pat_id, patient_mrn), by = "pat_id")

km_complete_final_obs_subgroups <- left_join(x = km_complete_final_obs_subgroups, 
                                             y = macr_phhs2 %>% select(patient_mrn, macr_gt30) %>% rename(StudyID = patient_mrn),
                                             by = "StudyID") %>% 
  rename(MACR = macr_gt30) %>% 
  mutate(MACR = ifelse(is.na(MACR), "MACR Unknown", MACR)) # %>% 
  # mutate(macr_gt30 = ifelse(macr_gt30 == "Yes", macr_gt30, "No"))

# km_complete_final_obs_subgroups <- km_complete_final_obs_subgroups %>% mutate(macr_gt30 = ifelse(is.na(macr_gt30), "No", macr_gt30))

write.xlsx(km_complete_final_obs_subgroups , file = 'Outputs/Final_version/PHHS_KM_v11.xlsx', asTable = T)

dialysisP <- km_complete_final_obs_subgroups[km_complete_final_obs_subgroups$DialysisYN == "Y",c("StudyID", "Age", "Sex", "start_dt","Dial_date", "Arm")] %>% 
  mutate(diff = Dial_date - start_dt) %>% mutate(diff = ifelse(diff > 365, 365, diff)) %>% 
  select(StudyID, Age, Sex, diff, start_dt, Dial_date, Arm) %>% 
  rename(`Study Intervention Duration` = diff, `Study Intervention Start Date` = start_dt, `SAE Onset Date` = Dial_date)

# write.csv(dialysisP , file = 'Outputs/Dialysis_patients.csv', row.names = F)

deathP <- km_complete_final_obs_subgroups[km_complete_final_obs_subgroups$DeathYN == "Y",c("StudyID", "Age", "Sex", "start_dt","Death_date", "Arm")] %>% 
  mutate(diff = Death_date - start_dt) %>% mutate(diff = ifelse(diff > 365, 365, diff)) %>% 
  select(StudyID, Age, Sex, diff, start_dt, Death_date, Arm) %>% 
  rename(`Study Intervention Duration` = diff, `Study Intervention Start Date` = start_dt, `SAE Onset Date` = Death_date)

# write.csv(deathP , file = 'Outputs/Death_patients.csv', row.names = F)

################ Charlson score vs HbA1c and EGFR ################ 
a1c_bsln_Charlson <- left_join(x = a1c_bsln, y = charlson10, by = c('pat_id', 'arm'))


a1c_mean_charl <- a1c_bsln_Charlson %>% 
  group_by(arm) %>%
  mutate(hba1c_mean = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1))) %>%
  ungroup() %>% 
  mutate(All = paste0(round(mean(val, na.rm = T),1), " +/- ", round(sd(val, na.rm = T),1)),
         Diabetes = 'NA',
         `Diabetes with complications` = 'NA') %>% 
  select(arm, hba1c_mean, All, Diabetes, `Diabetes with complications`) %>% 
  unique() %>% 
  mutate(Characteristics = 'HbA1c',
         Category = 'Mean +/- SD') %>%  
  reshape2::dcast(Characteristics + Category + All + Diabetes + `Diabetes with complications` ~ arm, value.var = 'hba1c_mean') %>%
  mutate_all( ~lapply(.,as.character)) %>%
  select(Characteristics, Category, All, Control, `ICD - Pieces`, Diabetes, `Diabetes with complications`) 

a1c_7.5 <-   left_join(x = a1c_bsln_Charlson %>%
                         group_by(arm, lt_7.5) %>%
                         dplyr::summarise(n = n()) %>%
                         mutate(percent = paste0(n, " (", round((n/sum(n))*100,1), "%)")) %>% 
                         select(-n),
                       y = a1c_bsln_Charlson %>%
                         group_by(lt_7.5) %>%
                         dplyr::summarise(n = n(), Diabetes = sum(diab), `Diabetes with complications` = sum(diabwc)) %>%
                         mutate(All = paste0(n, " (", round((n/sum(n))*100,1), "%)")) %>% 
                         select(-n),
                       by = 'lt_7.5') %>% 
  mutate(Characteristics = 'HbA1c',
         Category = lt_7.5) %>% 
  reshape2::dcast(Characteristics + Category + All + Diabetes + `Diabetes with complications` ~ arm , value.var = 'percent') %>% 
  arrange(desc(Control))%>%
  mutate(Category = case_when(Category == "Yes" ~ "<7.5", Category == "No" ~ ">7.5", TRUE ~ "Unknown")) %>% 
  mutate_all(~lapply(., as.character)) %>%
  select(Characteristics, Category, All, Control, `ICD - Pieces`, Diabetes, `Diabetes with complications`)

a1c_charlson <- kable(bind_rows(a1c_mean_charl, a1c_7.5),
                     style = 'html',
                     booktabs = T,
                     caption = 'PHHS - HbA1c and Diabetes Comparison') %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F, 
                  html_font = 'Arial') %>%
  row_spec(0, bold = T) %>% 
  column_spec(1:2,'3.8in') %>% 
  collapse_rows(1) %>% 
  kable_styling(latex_options = "scale_down", font_size = 20)
  
egfr_bsln_charlson <- left_join(x = egfr_bsln, y = charlson10, by = c('pat_id', 'arm'))

egfr_prop_charlson <-  left_join( x = egfr_bsln_charlson %>% 
                           group_by(arm, egfr_val) %>%
                           dplyr::summarise(n = n()) %>% 
                           mutate(percent = paste0(n, " (", round((n/sum(n))*100,1), "%)")) %>% 
                           select(-n),
                         
                         y = egfr_bsln_charlson %>% 
                           group_by(egfr_val) %>%
                           dplyr::summarise(n = n(), `Renal disease` = sum(rend)) %>% 
                           mutate(All = paste0(n, " (", round((n/sum(n))*100,1), "%)") ) %>% 
                           select(-n),
                         by = 'egfr_val') %>% 
  mutate(Characteristics = 'Estimated GFR',
         Category = egfr_val)%>% 
  reshape2::dcast(Characteristics + Category + All +  `Renal disease` ~ arm , value.var = 'percent') %>% 
  mutate(rank = case_when(Category == '>= 60 ml/min/1.73m2' ~ 1,
                          Category == '45 to < 60 ml/min/1.73m2' ~ 2,
                          Category == '30 to < 45 ml/min/1.73m2' ~ 3,
                          Category == '15 to < 30 ml/min/1.73m2' ~ 4,
                          Category == '0 to < 15 ml/min/1.73m2' ~ 5,
                          Category == 'Unknown' ~ 6)) %>%
  arrange((rank))%>%
  select(-rank) %>% 
  mutate_all( ~lapply(.,as.character)) %>%
  select(Characteristics, Category, All, Control, `ICD - Pieces`, `Renal disease`)

egfr_charlson <- kable(egfr_prop_charlson,
                     style = 'html',
                     booktabs = T,
                     caption = 'PHHS - HbA1c and Diabetes Comparison') %>%
  kable_styling('striped','bordered') %>%
  kable_classic_2(full_width = F, 
                  html_font = 'Montserrat') %>%
  row_spec(0, bold = T) %>% 
  column_spec(1:2,'3.8in') %>% 
  collapse_rows(1) %>% 
  kable_styling(latex_options = "scale_down", font_size = 20)
###############################################################################################################

# Fidelity Markers Graphs
Bp_140_90_pre <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d <= enroll_date  & enroll_date - bp_d <= 365) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Baseline')
          
Bp_140_90_Q5 <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d > enroll_date & bp_d - enroll_date <= 365 ) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'One year post-baseline')  

Bp_140_90_Q4 <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d > enroll_date & bp_d - enroll_date <= 365 & bp_d - enroll_date > 270 ) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q4')

Bp_140_90_Q3 <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d > enroll_date & bp_d - enroll_date <= 270 & bp_d - enroll_date > 180 ) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q3')


Bp_140_90_Q2 <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d > enroll_date & bp_d - enroll_date <= 180 & bp_d - enroll_date > 90 ) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q2')


Bp_140_90_Q1 <- left_join(x = masterdemo, y = bp, by = 'pat_id') %>% 
             filter(bp_d > enroll_date & bp_d - enroll_date <= 90) %>% 
             arrange(pat_id, desc(bp_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, sbp, dbp) %>% 
             mutate(`BP < 140/90` = case_when(sbp < 140 & dbp < 90 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q1')

# BP_tally <- rbind(Bp_140_90_pre, Bp_140_90_Q1, Bp_140_90_Q2, Bp_140_90_Q3, Bp_140_90_Q4,Bp_140_90_Q5)
BP_tally <- rbind(Bp_140_90_pre, Bp_140_90_Q5) %>% mutate(arm = ifelse(arm == "ICD - Pieces", "ICD-Pieces", "Standard of care"))

save(BP_tally, file = "Inputs/BP_tally_PHHS.RData")
#proportion plot/ stacked - by arm
ggplot(data = BP_tally %>%
              group_by(arm, Period,`BP < 140/90` ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = arm, y = n, fill = `BP < 140/90`)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Arm') + ylab('Number of Patients') +
       ggtitle('PHHS - BP < 140/90 mmHg (by arm) ') +
       scale_y_continuous(breaks = seq(0,1500,200))+
       geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       facet_wrap(~ Period, nrow = 1) +
       labs(fill ='BP < 140/90') +
       theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))

#ggsave('Outputs/PHHS BP 140_90 - By arm.pdf', plot = last_plot())

#proportion plot/ stacked - overall
ggplot(data = BP_tally %>%
              group_by(Period,`BP < 140/90` ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = Period, y = n, fill = `BP < 140/90`)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Period') + ylab('Number of Patients') +
       ggtitle('PHHS - BP < 140/90 mmHg (overall) ') +
       scale_y_continuous(breaks = seq(0,3000,200))+
       geom_text(aes(x = Period, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       labs(fill ='BP < 140/90') +
       theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS BP 140_90 - Overall.pdf', plot = last_plot())

#If you want just the table - by arms:
bp_table <- BP_tally %>% group_by(arm, Period, `BP < 140/90` ) %>%
dplyr::summarise(count = n()) %>%
mutate(percent = round((count/sum(count))*100,2)) %>%
mutate(Characteristics = 'Clinical Fidelity Metrics',
       Category = 'BP') %>% as.data.table() %>%
data.table::dcast.data.table(Characteristics + Category  +  Period + `BP < 140/90`  ~ arm , value.var = c('percent', 'count')) %>% as.data.frame() %>%
mutate_all( ~lapply(.,as.character))
          
#box plots - Systolic - by arm
ggplot(BP_tally, aes(x = arm, y = sbp, fill = arm))+
  geom_boxplot()+
  xlab('')+
  ylab('Systolic Blood Pressure (mmHg)')+
  scale_y_continuous(breaks = seq(0,300,20))+
  ggtitle('PHHS - Systolic blood pressure (by arm) ') +
  facet_wrap(~Period, nrow =1, strip.position = 'bottom') +
  scale_fill_manual(values = c('lightblue','cyan'))+
  theme_classic()+ theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank()) +
  labs(fill = 'Arm')+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS BP - Systolic - by arm.pdf', plot = last_plot())

#box plots - Systolic - overall
ggplot(BP_tally, aes(x = Period, y = sbp))+
  geom_boxplot()+
  ylab('Systolic Blood Pressure (mmHg)')+
  ggtitle('PHHS - Systolic blood pressure (overall) ') +
  theme_classic()+
  labs(fill = 'Period')+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS BP - Systolic - overall.pdf', plot = last_plot())

#box plots - Diastolic - by arm
ggplot(BP_tally, aes(x = arm, y = dbp, fill = arm))+
  geom_boxplot()+
  xlab('')+
  ylab('Diastolic Blood Pressure (mmHg)')+
  scale_y_continuous(breaks = seq(0,160,20))+
  ggtitle('PHHS - Diastolic blood pressure (by arm) ') +
  facet_wrap(~Period, nrow =1, strip.position = 'bottom') +
  scale_fill_manual(values = c('lightblue','cyan'))+
  theme_classic()+ theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank()) +
  labs(fill = 'Arm')+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS BP - Diastolic - by arm.pdf', plot = last_plot())

#box plots - Diastolic - overall
ggplot(BP_tally, aes(x = Period, y = dbp))+
  geom_boxplot()+
  ylab('Diastolic Blood Pressure (mmHg)')+
  scale_y_continuous(breaks = seq(0,160,20))+
  ggtitle('PHHS - Diastolic blood pressure (overall) ') +
  theme_classic()+ 
  labs(fill = 'Period')+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS BP - Diastolic - overall.pdf', plot = last_plot())




#Hba1c
Hba1c_pre <- a1c_prior %>% 
             filter(res_d <= enroll_date  & enroll_date - res_d <= 365) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>%  
             select(pat_id, arm, val) %>%  
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Baseline')
          
Hba1c_Q5 <- a1c_post %>% 
             filter(res_d> enroll_date & res_d- enroll_date <= 365 ) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, val) %>% 
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'One year follow up')  

Hba1c_Q4 <- a1c_post %>% 
             filter(res_d> enroll_date & res_d - enroll_date <= 365 & res_d- enroll_date > 270 ) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, val) %>% 
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q4')

Hba1c_Q3 <- a1c_post %>% 
             filter(res_d> enroll_date & res_d- enroll_date <= 270 & res_d- enroll_date > 180 ) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, val) %>% 
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q3')


Hba1c_Q2 <- a1c_post %>%  
             filter(res_d> enroll_date & res_d- enroll_date <= 180 & res_d- enroll_date > 90 ) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, val) %>% 
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q2')


Hba1c_Q1 <- a1c_post %>% 
             filter(res_d> enroll_date & res_d- enroll_date <= 90) %>% 
             arrange(pat_id, desc(res_d)) %>% 
             filter(!duplicated(pat_id)) %>% 
             select(pat_id, arm, val) %>% 
             mutate(`A1c < 7.5` = case_when(val < 7.5 ~ 'Yes', TRUE ~ 'No'),
                    Period = 'Q1')


A1c_tally <- rbind(Hba1c_pre, Hba1c_Q1, Hba1c_Q2, Hba1c_Q3, Hba1c_Q4, Hba1c_Q5)
save(A1c_tally, file= "Inputs/A1c_tally.RData")
#proportion plot - by arm
ggplot(data = A1c_tally %>%
              group_by(arm, Period,`A1c < 7.5` ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = arm, y = n, fill = `A1c < 7.5`)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Arm') + ylab('Number of Patients') +
       ggtitle('PHHS HbA1c < 7.5% (by arm)') +
       scale_y_continuous(breaks = seq(0,1500,200))+
       geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       facet_wrap(~ Period, nrow = 1) +
       labs(fill ='HbA1c < 7.5%') + theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS HbA1c less than 7.5 (by arm).pdf', plot = last_plot())

#proportion plot - overall
ggplot(data = A1c_tally %>%
              group_by(Period,`A1c < 7.5` ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = Period, y = n, fill = `A1c < 7.5`)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Period') + ylab('Number of Patients') +
       ggtitle('PHHS HbA1c < 7.5% (overall) ') +
       scale_y_continuous(breaks = seq(0,3000,200))+
       geom_text(aes(x = Period, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       labs(fill ='HbA1c < 7.5%') + theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS HbA1c less than 7.5 (overall).pdf', plot = last_plot())


#If you want table - by arm:
A1c_table <- A1c_tally %>% group_by(arm, Period, `A1c < 7.5` ) %>%
dplyr::summarise(count = n()) %>%
mutate(percent = round((count/sum(count))*100,2)) %>%
mutate(Characteristics = 'Clinical Fidelity Metrics',
       Category = 'HbA1c') %>% as.data.table() %>%
data.table::dcast.data.table(Characteristics + Category + Period + `A1c < 7.5`  ~ arm , value.var = c('percent', 'count')) %>% as.data.frame() %>%
mutate_all( ~lapply(.,as.character))
   
            
#box plot - by arm
ggplot(A1c_tally, aes(x = arm, y = val, fill = arm))+
  geom_boxplot()+
  xlab('')+
  ylab('HbA1c Level (%)')+
  ggtitle('PHHS HbA1c (by arm) ') +
  scale_y_continuous(breaks = seq(0,20,2))+
  facet_wrap(~Period, nrow =1, strip.position = 'bottom') +
  scale_fill_manual(values = c('lightblue','cyan'))+
  theme_classic()+ theme(axis.text.x = element_blank(),
                         axis.ticks.x = element_blank()) +
  labs(fill = 'Arm')+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS HbA1c (by arm).pdf', plot = last_plot())

#box plot - by overall
ggplot(A1c_tally, aes(x = Period, y = val))+
  geom_boxplot()+
  xlab('')+
  ylab('HbA1c Level (%)')+
  ggtitle('PHHS HbA1c (overall)  ') +
  scale_y_continuous(breaks = seq(0,20,2))+
  theme_classic()+ 
  theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS HbA1c (overall).pdf', plot = last_plot())


#Medication (ACEI/ARB & Statin)
# medication_prior <- left_join(x = masterdemo %>% as.data.frame(),
#                         y = left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
#                                       y = med %>% as.data.frame() %>% select(pat_id, order_d, description),
#                                       by = 'pat_id') %>%
#                             filter(order_d <= fucens & order_d < enroll_date & enroll_date - order_d <= 365) %>% #-- prior
#                           # filter(order_d <= fucens & order_d <= enroll_date & enroll_date - order_d <= 365) %>% #-- prior
#                             unique() %>%
#                             select(pat_id, order_d, description),
#                         by = 'pat_id') %>% 
#               mutate(acetemp =  as.numeric(grepl('TRIBENZOR|OLMESARTAN|ZESTRIL|ALTACE|AZILSARTAN|EDARBI|EDARBYCLOR|MOEXIPRIL|UNIRETIC|UNIVASC|ENALAPRIL|EPANED|AVAPRO|CANDESARTAN|PERINDOPRIL|PRESTALIA|VALSARTAN|ENTRESTO|CAPTOPRIL |LISINOPRIL|RAMIPRIL|TRANDOLAPRIL|CILAZAPRIL|IRBESARTAN|TELMISARTAN|PRINIVIL|ACCUPRIL|PRINZIDE|MAVIK|MICARDIS|TEVETEN|CAPTOPRIL|EPROSARTAN|QUINAPRIL|DIOVAN|BENICAR|TEVETEN HCT|ATACAND|EXFORGE|AZOR|MICARDIS-HCT|ATACAND HCT|DIOVAN HCT|ACCURETIC|AVALIDE|ZESTORETIC|BENICAR HCT|EXFORGE HCT|ACEON|FOSINOPRIL|MONOPRIL|VASOTEC|VASERETIC|TWYNSTA|TARKA|BENAZEPRIL|LOTREL|LOTENSIN HCT|LOTENSIN|LOSARTAN|HYZAAR|COZAAR', description)),
#                      statintemp = as.numeric(grepl('LOVASTATIN|SIMVASTATIN|ATORVASTATIN|FLUVASTATIN|ROSUVASTATIN|PITAVASTATIN|PRAVASTATIN|LESCOL|ZOCOR|VYTORIN|LIPTRUZET|MEVACOR|LIPITOR|CADUET|CRESTOR|LIVALO|ALTOPREV|PRAVACHOL', description))) %>% 
#               select(pat_id, enroll_date, order_d, arm , acetemp, statintemp) %>% 
#               group_by(pat_id) %>% 
#               mutate(ACE = max(acetemp, na.rm = T),
#                      STATIN = max(statintemp, na.rm = T)) %>% 
#               ungroup() %>% 
#               select(pat_id, enroll_date, order_d,arm, ACE, STATIN) %>% 
#               unique()
# 
# medication_post <- left_join(x = masterdemo %>% as.data.frame(),
#                         y = left_join(x = masterdemo %>% as.data.frame() %>% select(pat_id, enroll_date),
#                                       y = med %>% as.data.frame() %>% select(pat_id, order_d, description),
#                                       by = 'pat_id') %>%
#                             filter(order_d <= fucens & order_d >= enroll_date &   order_d - enroll_date <= 365) %>% #-- post
#                             unique() %>%
#                             select(pat_id, order_d, description),
#                         by = 'pat_id') %>% 
#               mutate(acetemp =  as.numeric(grepl('TRIBENZOR|OLMESARTAN|ZESTRIL|ALTACE|AZILSARTAN|EDARBI|EDARBYCLOR|MOEXIPRIL|UNIRETIC|UNIVASC|ENALAPRIL|EPANED|AVAPRO|CANDESARTAN|PERINDOPRIL|PRESTALIA|VALSARTAN|ENTRESTO|CAPTOPRIL |LISINOPRIL|RAMIPRIL|TRANDOLAPRIL|CILAZAPRIL|IRBESARTAN|TELMISARTAN|PRINIVIL|ACCUPRIL|PRINZIDE|MAVIK|MICARDIS|TEVETEN|CAPTOPRIL|EPROSARTAN|QUINAPRIL|DIOVAN|BENICAR|TEVETEN HCT|ATACAND|EXFORGE|AZOR|MICARDIS-HCT|ATACAND HCT|DIOVAN HCT|ACCURETIC|AVALIDE|ZESTORETIC|BENICAR HCT|EXFORGE HCT|ACEON|FOSINOPRIL|MONOPRIL|VASOTEC|VASERETIC|TWYNSTA|TARKA|BENAZEPRIL|LOTREL|LOTENSIN HCT|LOTENSIN|LOSARTAN|HYZAAR|COZAAR', description)),
#                      statintemp = as.numeric(grepl('LOVASTATIN|SIMVASTATIN|ATORVASTATIN|FLUVASTATIN|ROSUVASTATIN|PITAVASTATIN|PRAVASTATIN|LESCOL|ZOCOR|VYTORIN|LIPTRUZET|MEVACOR|LIPITOR|CADUET|CRESTOR|LIVALO|ALTOPREV|PRAVACHOL', description))) %>% 
#               select(pat_id, enroll_date, order_d, arm , acetemp, statintemp) %>% 
#               group_by(pat_id) %>% 
#               mutate(ACE = max(acetemp, na.rm = T),
#                      STATIN = max(statintemp, na.rm = T)) %>% 
#               ungroup() %>% 
#               select(pat_id, enroll_date, order_d,arm, ACE, STATIN) %>% 
#               unique()
# #ACEI
# acei_pre <- medication_prior %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Baseline')
# 
# acei_Q5 <- medication_post %>%  
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='One year post-baseline')
# 
# 
# acei_Q4 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 365 & order_d - enroll_date > 270 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q4')
# 
# acei_Q3 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 270 & order_d - enroll_date > 180 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q3')
# 
# acei_Q2 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 180 & order_d - enroll_date > 90 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q2')
# 
# acei_Q1 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 90 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(ACE)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q1')
# # ace_tally <- rbind(acei_pre, acei_Q1, acei_Q2, acei_Q3, acei_Q4, acei_Q5)
# ace_tally <- rbind(acei_pre, acei_Q5) %>% mutate(arm = ifelse(arm == "Control", "Standard of care", "ICD-Pieces"))
# save(ace_tally, file = "Inputs/ace_tally_PHHS.RData")

#proportion plot - by arm
# ggplot(data = ace_tally %>%
#               group_by(arm, Period, had_meds ) %>%
#               tally %>%
#               mutate(percent = round(n*100/sum(n),1)),
#        aes(x = arm, y = n, fill = had_meds)) +
#        geom_bar(stat = "identity", width = 0.5) +
#        xlab('Arm') + ylab('Number of Patients') +
#        ggtitle('PHHS - ACE Inhibitors active prescription (by arm)') +
#        scale_y_continuous(breaks = seq(0,1500,200))+
#        geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
#        facet_wrap(~ Period, nrow = 1) +
#        labs(fill ='Active Prescription') + theme_classic()+
#        theme(plot.title = element_text(hjust = 0.5))
# #ggsave('Outputs/PHHS ACE active prescription (by arm).pdf',plot = last_plot())
# 
# #proportion plot - overall
# ggplot(data = ace_tally %>%
#               group_by(Period, had_meds ) %>%
#               tally %>%
#               mutate(percent = round(n*100/sum(n),1)),
#        aes(x = Period, y = n, fill = had_meds)) +
#        geom_bar(stat = "identity", width = 0.5) +
#        xlab('Period') + ylab('Number of Patients') +
#        ggtitle('PHHS - ACE Inhibitor active prescription (overall)') +
#        scale_y_continuous(breaks = seq(0,3000,200))+
#        geom_text(aes(x = Period, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
#        labs(fill ='Active Prescription') + theme_classic()+
#        theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS ACE active prescription (overall).pdf',plot = last_plot())

#If you want table:
# ace_statin_table <- ace_tally %>% group_by(arm, Period, had_meds) %>%
# dplyr::summarise(count = n()) %>%
# mutate(percent = round((count/sum(count))*100,2)) %>%
# mutate(Characteristics = 'Clinical Fidelity Metrics',
#        Category = 'ACEI') %>% as.data.table() %>%
# data.table::dcast.data.table(Characteristics + Category + Period + had_meds  ~ arm , value.var = c('percent', 'count')) %>% as.data.frame() %>%
# mutate_all( ~lapply(.,as.character))

#STATIN
# statin_pre <- medication_prior %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Baseline')
# 
# statin_Q5 <- medication_post %>%  
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='One year post-baseline')
# 
# 
# statin_Q4 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 365 & order_d - enroll_date > 270 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q4')
# 
# statin_Q3 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 270 & order_d - enroll_date > 180 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q3')
# 
# statin_Q2 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 180 & order_d - enroll_date > 90 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q2')
# 
# statin_Q1 <- medication_post %>%
#              filter(order_d > enroll_date & order_d - enroll_date <= 90 ) %>% 
#              group_by(pat_id) %>% 
#              mutate(had_meds = sum(STATIN)) %>% 
#              select(pat_id, arm, had_meds) %>%
#              unique() %>% 
#              mutate(had_meds = case_when(had_meds > 0 ~ 'Yes', TRUE ~ 'No'),
#                     Period ='Q1')
# # statin_tally <- rbind(statin_pre, statin_Q1, statin_Q2, statin_Q3, statin_Q4, statin_Q5)
# statin_tally <- rbind(statin_pre, statin_Q5) %>% mutate(arm = ifelse(arm == "Control", "Standard of care", "ICD-Pieces"))
save(statin_tally, file = "Inputs/statin_tally_PHHS.RData")

#proportion plot - by arm
ggplot(data = statin_tally %>%
              group_by(arm, Period, had_meds ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = arm, y = n, fill = had_meds)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Arm') + ylab('Number of Patients') +
       ggtitle('PHHS - STATIN active prescription (by arm)') +
       scale_y_continuous(breaks = seq(0,1500,200))+
       geom_text(aes(x = arm, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       facet_wrap(~ Period, nrow = 1) +
       labs(fill ='Active Prescription') + theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS STATIN active prescription (by arm).pdf',plot = last_plot())

#proportion plot - overall
ggplot(data = statin_tally %>%
              group_by(Period, had_meds ) %>%
              tally %>%
              mutate(percent = round(n*100/sum(n),1)),
       aes(x = Period, y = n, fill = had_meds)) +
       geom_bar(stat = "identity", width = 0.5) +
       xlab('Period') + ylab('Number of Patients') +
       ggtitle('PHHS - STATIN active prescription (overall)') +
       scale_y_continuous(breaks = seq(0,3000,200))+
       geom_text(aes(x = Period, label = paste0(percent,'%')),color = 'black',size = 2.5, position = position_stack(vjust = 0.5))+ 
       labs(fill ='Active Prescription') + theme_classic()+
       theme(plot.title = element_text(hjust = 0.5))
#ggsave('Outputs/PHHS STATIN active prescription (overall).pdf',plot = last_plot())

#If you want table:
acei_table <- statin_tally %>% group_by(arm, Period, had_meds) %>%
dplyr::summarise(count = n()) %>%
mutate(percent = round((count/sum(count))*100,2)) %>%
mutate(Characteristics = 'Clinical Fidelity Metrics',
       Category = 'STATIN') %>% as.data.table() %>%
data.table::dcast.data.table(Characteristics + Category + Period + had_meds  ~ arm , value.var = c('percent', 'count')) %>% as.data.frame() %>%
mutate_all( ~lapply(.,as.character))


qpdf::pdf_combine(c('Outputs/For review/PHHS-THR Medical Chart Review (by arm).pdf',
              'Outputs/For review/PHHS Baseline Characteristics (by arm).pdf',
              'Outputs/For review/PHHS BP 140_90 - By arm.pdf',
              'Outputs/For review/PHHS BP - Systolic - by arm.pdf',
              'Outputs/For review/PHHS BP - Diastolic - by arm.pdf',
              'Outputs/For review/PHHS HbA1c (by arm).pdf',
              'Outputs/For review/PHHS HbA1c less than 7.5 (by arm).pdf',
              'Outputs/For review/PHHS ACE active prescription (by arm).pdf',
              'Outputs/For review/PHHS STATIN active prescription (by arm).pdf'
              ), output = 'Outputs/For review/by arm.pdf')


qpdf::pdf_combine(c( 'Outputs/For review/PHHS-THR Medical Chart Review (overall).pdf',
              'Outputs/For review/PHHS Baseline Characteristics (overall).pdf',
              'Outputs/For review/PHHS BP 140_90 - Overall.pdf',
              'Outputs/For review/PHHS BP - Systolic - overall.pdf',
              'Outputs/For review/PHHS BP - Diastolic - overall.pdf',
              'Outputs/For review/PHHS HbA1c (overall).pdf',
              'Outputs/For review/PHHS HbA1c less than 7.5 (overall).pdf',
              'Outputs/For review/PHHS ACE active prescription (overall).pdf',
              'Outputs/For review/PHHS STATIN active prescription (overall).pdf'
              ), output = 'Outputs/For review/overall.pdf')
              
# Proteinuria

load("Inputs/protein.RData")
protein <- protein %>% mutate(LabComponentEpicId = as.numeric(LabComponentEpicId))
protein %>% filter(LabComponentEpicId== 1551412  ) %>% select(LabResultValue) %>% group_by(LabResultValue) %>% tally()

labs_macr<-protein[protein$LabComponentEpicId==1557742, ] # 1557742 not found 1551412
# (0,30, 300, 1000, 2000, 3000, and 4000 mg/g
  
#PCR
labs_pcr<-lab[lab$component_id==33418450, ] # 33418450 not found
# PCR (0, 0.047, 0.5, 1.6, 3.1, 4.7 and 6.2 mg/g
       
$udip
# Urine Dipstick (0, trace, mild , moderate, severe)
     
$Urine Protein collection
upr
 
# one in 12 months of enrollment( ordered at enrollment)
# (two or more values)
 
 
 
 # T:\NIH Chronic Disease Management\Data\Shelley Handoff\PHHS SHELLEY\For Albert\PHHS
 ## from DataProcessing_adaptedRegistryBackfill.R
setwd("/nfs/smb/tdrive/NIH Chronic Disease Management/Data/Shelley Handoff/PHHS SHELLEY")

 
 #upr
labs_upr1<-protein[protein$LabComponentEpicId%in%c(1534066, 1553957),]
labs_upr1<-labs_upr1[!is.na(labs_upr1$NumericValue), ]
labs_upr1$value<- gsub(">|<","", labs_upr1$ord_value)
labs_upr1$val<-as.numeric(labs_upr1$value) #need warning suppress for cron
labs_upr1<-labs_upr1[!is.na(labs_upr1$val),]


labs_puad<-lab[lab$component_id%in%c(1552111, 1551412),] # 1552111 not found, 1551412 found
puad_val_neg<-read.table("puad_val_neg.txt",header=FALSE, as.is=T, colClasses = "character")[,1]
puad_val_tra<-read.table("puad_val_tra.txt",header=FALSE, as.is=T, colClasses = "character")[,1]
puad_val_30 <-read.table("puad_val_30.txt", header=FALSE, as.is=T, colClasses = "character")[,1]
puad_val_100 <-read.table("puad_val_100.txt", header=FALSE, as.is=T, colClasses = "character")[,1]
puad_val_300 <-read.table("puad_val_300.txt", header=FALSE, as.is=T, colClasses = "character")[,1]
puad_val_301 <-read.table("puad_val_301.txt", header=FALSE, as.is=T, colClasses = "character")[,1]

#correct u for sg for selection
Reduce(intersect, list(puad_val_30, puad_val_100, puad_val_301, puad_val_300, puad_val_neg, puad_val_tra))

labs_puad_p2$val_neg<-as.numeric(labs_puad_p2$ord_value%in%puad_val_neg)
labs_puad_p2$val_tra<-as.numeric(labs_puad_p2$ord_value%in%puad_val_tra)
labs_puad_p2$val_30<-as.numeric(labs_puad_p2$ord_value%in%puad_val_30)
labs_puad_p2$val_100<-as.numeric(labs_puad_p2$ord_value%in%puad_val_100)
labs_puad_p2$val_300<-as.numeric(labs_puad_p2$ord_value%in%puad_val_300)
labs_puad_p2$val_301<-as.numeric(labs_puad_p2$ord_value%in%puad_val_301)
labs_puad_p2$value<-ifelse(labs_puad_p2$val_neg==1, "negative",
                          ifelse(labs_puad_p2$val_tra==1, "trace",
                                 ifelse(labs_puad_p2$val_30==1, "30",
                                        ifelse(labs_puad_p2$val_100==1, "100",
                                               ifelse(labs_puad_p2$val_300==1, "300",
                                                      ifelse(labs_puad_p2$val_301==1, "301", ""))))))
labs_puad_p2<-labs_puad_p2[!(labs_puad_p2$value==""),]

labs_puad_p1$value<-ifelse(labs_puad_p1$val==0, "negative",
                          ifelse(labs_puad_p1$val<30, "trace",
                                 ifelse(labs_puad_p1$val<100, "30",
                                        ifelse(labs_puad_p1$val<300, '100',
                                               ifelse(labs_puad_p1$val==300, '300',
                                                      ifelse(labs_puad_p1$ord_v<10000, '301', ''))))))


labs_macr<-lab[lab$component_id==1557742, ]
labs_macr<-labs_macr[!is.na(labs_macr$ord_num_value), ]
labs_macr$ord_value<-tolower(labs_macr$ord_value)
labs_macr$less_than<-as.numeric(str_detect(labs_macr$ord_value, '<'))
labs_macr$value<-gsub('<', '', labs_macr$ord_value)
labs_macr$val<-as.numeric(labs_macr$value) #suppress warn in cron
labs_macr<-labs_macr[!is.na(labs_macr$val),]
labs_macr$val<-ifelse(labs_macr$less_than==1, labs_macr$val-1, labs_macr$val)
labs_macr_f<-labs_macr[, c('pat_id', 'res_d', 'val')]
rm(labs_macr)

labs_pcr<-lab[lab$component_id==33418450, ]
labs_pcr$value<-gsub('<|>|=', '', labs_pcr$ord_value)
labs_pcr$val<-as.numeric(labs_pcr$value) #suppress warn in cron
labs_pcr<-labs_pcr[!is.na(labs_pcr$val),]
labs_pcr_f<-labs_pcr[, c('pat_id', 'res_d', 'val')]
labs_pcr_f<-labs_pcr_f[!duplicated(labs_pcr_f),]
