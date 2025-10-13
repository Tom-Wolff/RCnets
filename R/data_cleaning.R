#####################################
#    D A T A   R E S H A P I N G    #
#####################################

library(tidyverse)
library(ideanet)

# Read in Relational Coordination Data
load("./ICPSR_37453/DS0002/37453-0002-Data.rda")

# Don't like the dataset name here, renaming to `raw_data`
raw_data <- da37453.0002

# This dataset is basically a bunch of two-mode adjacency matrices stitched
# together in a long dataset. The code below shows how we would extract the first
# of these two-mode adjacency matrices and rename variables and values to
# ensure aggregation into a proper adjacency matrix
data <- raw_data %>%
  rename(Q3_2_1 = Q2_2_1,
         Q9_8_1 = Q8_8_1,
         Q9_12_1 = Q8_12_1) %>%
  mutate(city = (raw_data$CASEID - raw_data$CASEID %% 100)/100,
         WORKGROUP = case_when(stringr::str_detect(WORKGROUP, "(01)") ~ "research",
                               stringr::str_detect(WORKGROUP, "(02)") ~ "city_govt",
                               stringr::str_detect(WORKGROUP, "(03)") ~ "corrections",
                               stringr::str_detect(WORKGROUP, "(04)") ~ "da_office",
                               stringr::str_detect(WORKGROUP, "(05)") ~ "emp_devel",
                               stringr::str_detect(WORKGROUP, "(06)") ~ "faith_based",
                               stringr::str_detect(WORKGROUP, "(07)") ~ "family_social",
                               stringr::str_detect(WORKGROUP, "(08)") ~ "health",
                               stringr::str_detect(WORKGROUP, "(09)") ~ "parole",
                               stringr::str_detect(WORKGROUP, "(10)") ~ "police",
                               stringr::str_detect(WORKGROUP, "(11)") ~ "probation",
                               stringr::str_detect(WORKGROUP, "(12)") ~ "school",
                               stringr::str_detect(WORKGROUP, "(13)") ~ "youth_out",
                               TRUE ~ NA),
         intervention2 = INTERVENTION2 == "(01) Yes",
         intervention3 = INTERVENTION3 == "(1) Yes",
         intervention4 = INTERVENTION3 == "(1) Yes"
  ) %>%
  dplyr::select(city, CASEID, WORKGROUP, intervention2, intervention3, intervention4, dplyr::everything())

names(data)

# The first number, with Q prefix, is the type of question
names(data) <- stringr::str_replace(names(data), "Q3_", "VAR_commFreq_")
names(data) <- stringr::str_replace(names(data), "Q5_", "VAR_commTimely_")
names(data) <- stringr::str_replace(names(data), "Q7_", "VAR_commAccurately_")
names(data) <- stringr::str_replace(names(data), "Q9_", "VAR_blameOthers_")
names(data) <- stringr::str_replace(names(data), "Q10_", "VAR_shareGoals_")
names(data) <- stringr::str_replace(names(data), "Q11_", "VAR_knowYourWork_")
names(data) <- stringr::str_replace(names(data), "Q12_", "VAR_respectYourWork_")

# The second number is the role being asked about
names(data) <- stringr::str_replace(names(data), "_1_", "_cityGovt_")
names(data) <- stringr::str_replace(names(data), "_2_", "_corrections_")
names(data) <- stringr::str_replace(names(data), "_3_", "_daOffice_")
names(data) <- stringr::str_replace(names(data), "_4_", "_empDevel_")
names(data) <- stringr::str_replace(names(data), "_5_", "_faithBased_")
names(data) <- stringr::str_replace(names(data), "_6_", "_familySocial_")
names(data) <- stringr::str_replace(names(data), "_7_", "_health_")
names(data) <- stringr::str_replace(names(data), "_8_", "_parole_")
names(data) <- stringr::str_replace(names(data), "_9_", "_police_")
names(data) <- stringr::str_replace(names(data), "_10_", "_probation_")
names(data) <- stringr::str_replace(names(data), "_11_", "_research_")
names(data) <- stringr::str_replace(names(data), "_12_", "_school_")
names(data) <- stringr::str_replace(names(data), "_13_", "_youthOut_")
names(data)
################################################################################
# Cleaning Heba's Way
data2 <- data
### Recoding `6` and `9` values to missing
recode_vars <- which(stringr::str_detect(names(data), "^VAR"))
for (i in recode_vars) {
  data2[,i] <- ifelse(data2[,i] == 6, NA, data2[,i])
  data2[,i] <- ifelse(data2[,i] == 99, NA, data2[,i])
}

### Frequency code cleaning
commFreq_vars <- which(stringr::str_detect(names(data), "^VAR_commFreq"))
for (i in commFreq_vars) {
  data2[,i] <- ifelse(data2[,i] == 2, 99, data2[,i])
  data2[,i] <- ifelse(data2[,i] == 5, 2, data2[,i])
  data2[,i] <- ifelse(data2[,i] == 3, 5, data2[,i])
  data2[,i] <- ifelse(data2[,i] == 99, 3, data2[,i])
}

########## The rest of this section replicate's Heba's calculation of aggregated
########## RC measures
# label list WORKGROUP / getting the ties scores
data3 <- data2 %>%
  dplyr::mutate(# commFreq Between
                freq_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_commFreq_research_1,
                                                     TRUE ~ NA),
                freq_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_commFreq_cityGovt_1,
                                                     TRUE ~ NA),
                freq_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_commFreq_corrections_1,
                                                      TRUE ~ NA),
                freq_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_commFreq_daOffice_1,
                                                   TRUE ~ NA),
                freq_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_commFreq_empDevel_1,
                                                     TRUE ~ NA),
                freq_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_commFreq_faithBased_1,
                                                    TRUE ~ NA),
                freq_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_commFreq_familySocial_1,
                                                    TRUE ~ NA),
                freq_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_commFreq_health_1,
                                                    TRUE ~ NA),
                freq_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_commFreq_parole_1,
                                                      TRUE ~ NA),
                freq_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_commFreq_police_1,
                                                      TRUE ~ NA),
                freq_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_commFreq_probation_1,
                                                     TRUE ~ NA),
                freq_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_commFreq_school_1,
                                                       TRUE ~ NA),
                freq_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_commFreq_youthOut_1,
                                                      TRUE ~ NA),
                # commTimely Between
                time_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_commTimely_research_1,
                                                     TRUE ~ NA),
                time_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_commTimely_cityGovt_1,
                                                     TRUE ~ NA),
                time_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_commTimely_corrections_1,
                                                      TRUE ~ NA),
                time_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_commTimely_daOffice_1,
                                                   TRUE ~ NA),
                time_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_commTimely_empDevel_1,
                                                     TRUE ~ NA),
                time_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_commTimely_faithBased_1,
                                                    TRUE ~ NA),
                time_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_commTimely_familySocial_1,
                                                    TRUE ~ NA),
                time_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_commTimely_health_1,
                                                    TRUE ~ NA),
                time_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_commTimely_parole_1,
                                                      TRUE ~ NA),
                time_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_commTimely_police_1,
                                                       TRUE ~ NA),
                time_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_commTimely_probation_1,
                                                     TRUE ~ NA),
                time_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_commTimely_school_1,
                                                       TRUE ~ NA),
                time_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_commTimely_youthOut_1,
                                                      TRUE ~ NA),
                # commAccurately Between
                accur_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_commAccurately_research_1,
                                                     TRUE ~ NA),
                accur_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_commAccurately_cityGovt_1,
                                                     TRUE ~ NA),
                accur_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_commAccurately_corrections_1,
                                                      TRUE ~ NA),
                accur_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_commAccurately_daOffice_1,
                                                   TRUE ~ NA),
                accur_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_commAccurately_empDevel_1,
                                                     TRUE ~ NA),
                accur_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_commAccurately_faithBased_1,
                                                    TRUE ~ NA),
                accur_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_commAccurately_familySocial_1,
                                                    TRUE ~ NA),
                accur_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_commAccurately_health_1,
                                                    TRUE ~ NA),
                accur_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_commAccurately_parole_1,
                                                      TRUE ~ NA),
                accur_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_commAccurately_police_1,
                                                       TRUE ~ NA),
                accur_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_commAccurately_probation_1,
                                                     TRUE ~ NA),
                accur_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_commAccurately_school_1,
                                                       TRUE ~ NA),
                accur_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_commAccurately_youthOut_1,
                                                      TRUE ~ NA),
                # Problem Solve / Blame Others Between
                psolv_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_blameOthers_research_1,
                                                     TRUE ~ NA),
                psolv_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_blameOthers_cityGovt_1,
                                                     TRUE ~ NA),
                psolv_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_blameOthers_corrections_1,
                                                      TRUE ~ NA),
                psolv_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_blameOthers_daOffice_1,
                                                   TRUE ~ NA),
                psolv_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_blameOthers_empDevel_1,
                                                     TRUE ~ NA),
                psolv_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_blameOthers_faithBased_1,
                                                    TRUE ~ NA),
                psolv_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_blameOthers_familySocial_1,
                                                    TRUE ~ NA),
                psolv_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_blameOthers_health_1,
                                                    TRUE ~ NA),
                psolv_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_blameOthers_parole_1,
                                                      TRUE ~ NA),
                psolv_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_blameOthers_police_1,
                                                       TRUE ~ NA),
                psolv_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_blameOthers_probation_1,
                                                     TRUE ~ NA),
                psolv_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_blameOthers_school_1,
                                                       TRUE ~ NA),
                psolv_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_blameOthers_youthOut_1,
                                                      TRUE ~ NA),
                # Shared Goals Between
                sgoal_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_shareGoals_research_1,
                                                     TRUE ~ NA),
                sgoal_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_shareGoals_cityGovt_1,
                                                     TRUE ~ NA),
                sgoal_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_shareGoals_corrections_1,
                                                      TRUE ~ NA),
                sgoal_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_shareGoals_daOffice_1,
                                                   TRUE ~ NA),
                sgoal_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_shareGoals_empDevel_1,
                                                     TRUE ~ NA),
                sgoal_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_shareGoals_faithBased_1,
                                                    TRUE ~ NA),
                sgoal_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_shareGoals_familySocial_1,
                                                    TRUE ~ NA),
                sgoal_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_shareGoals_health_1,
                                                    TRUE ~ NA),
                sgoal_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_shareGoals_parole_1,
                                                      TRUE ~ NA),
                sgoal_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_shareGoals_police_1,
                                                       TRUE ~ NA),
                sgoal_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_shareGoals_probation_1,
                                                     TRUE ~ NA),
                sgoal_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_shareGoals_school_1,
                                                       TRUE ~ NA),
                sgoal_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_shareGoals_youthOut_1,
                                                      TRUE ~ NA),
                # Shared Knowledge Between
                sknow_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_knowYourWork_research_1,
                                                     TRUE ~ NA),
                sknow_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_knowYourWork_cityGovt_1,
                                                     TRUE ~ NA),
                sknow_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_knowYourWork_corrections_1,
                                                      TRUE ~ NA),
                sknow_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_knowYourWork_daOffice_1,
                                                   TRUE ~ NA),
                sknow_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_knowYourWork_empDevel_1,
                                                     TRUE ~ NA),
                sknow_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_knowYourWork_faithBased_1,
                                                    TRUE ~ NA),
                sknow_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_knowYourWork_familySocial_1,
                                                    TRUE ~ NA),
                sknow_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_knowYourWork_health_1,
                                                    TRUE ~ NA),
                sknow_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_knowYourWork_parole_1,
                                                      TRUE ~ NA),
                sknow_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_knowYourWork_police_1,
                                                       TRUE ~ NA),
                sknow_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_knowYourWork_probation_1,
                                                     TRUE ~ NA),
                sknow_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_knowYourWork_school_1,
                                                       TRUE ~ NA),
                sknow_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_knowYourWork_youthOut_1,
                                                      TRUE ~ NA),
                # Respect Your Work Between
                mresp_Between_Acad = dplyr::case_when(WORKGROUP != 1 ~ VAR_respectYourWork_research_1,
                                                     TRUE ~ NA),
                mresp_Between_CGov = dplyr::case_when(WORKGROUP != 2 ~ VAR_respectYourWork_cityGovt_1,
                                                     TRUE ~ NA),
                mresp_Between_Corec = dplyr::case_when(WORKGROUP != 3 ~ VAR_respectYourWork_corrections_1,
                                                      TRUE ~ NA),
                mresp_Between_DA = dplyr::case_when(WORKGROUP != 4 ~ VAR_respectYourWork_daOffice_1,
                                                   TRUE ~ NA),
                mresp_Between_EDEV = dplyr::case_when(WORKGROUP != 5 ~ VAR_respectYourWork_empDevel_1,
                                                     TRUE ~ NA),
                mresp_Between_FBS = dplyr::case_when(WORKGROUP != 6 ~ VAR_respectYourWork_faithBased_1,
                                                    TRUE ~ NA),
                mresp_Between_FFS = dplyr::case_when(WORKGROUP != 7 ~ VAR_respectYourWork_familySocial_1,
                                                    TRUE ~ NA),
                mresp_Between_HMH = dplyr::case_when(WORKGROUP != 8 ~ VAR_respectYourWork_health_1,
                                                    TRUE ~ NA),
                mresp_Between_Parol = dplyr::case_when(WORKGROUP != 9 ~ VAR_respectYourWork_parole_1,
                                                      TRUE ~ NA),
                mresp_Between_Police = dplyr::case_when(WORKGROUP != 10 ~ VAR_respectYourWork_police_1,
                                                       TRUE ~ NA),
                mresp_Between_Prob = dplyr::case_when(WORKGROUP != 11 ~ VAR_respectYourWork_probation_1,
                                                     TRUE ~ NA),
                mresp_Between_School = dplyr::case_when(WORKGROUP != 12 ~ VAR_respectYourWork_school_1,
                                                       TRUE ~ NA),
                mresp_Between_Youth = dplyr::case_when(WORKGROUP!= 13 ~ VAR_respectYourWork_youthOut_1,
                                                      TRUE ~ NA),


                # commFreq Within
                freq_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_commFreq_research_1,
                                                    TRUE ~ NA),
                freq_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_commFreq_cityGovt_1,
                                                    TRUE ~ NA),
                freq_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_commFreq_corrections_1,
                                                    TRUE ~ NA),
                freq_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_commFreq_daOffice_1,
                                                    TRUE ~ NA),
                freq_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_commFreq_empDevel_1,
                                                    TRUE ~ NA),
                freq_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_commFreq_faithBased_1,
                                                    TRUE ~ NA),
                freq_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_commFreq_familySocial_1,
                                                    TRUE ~ NA),
                freq_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_commFreq_health_1,
                                                    TRUE ~ NA),
                freq_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_commFreq_parole_1,
                                                    TRUE ~ NA),
                freq_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_commFreq_police_1,
                                                    TRUE ~ NA),
                freq_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_commFreq_probation_1,
                                                    TRUE ~ NA),
                freq_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_commFreq_school_1,
                                                    TRUE ~ NA),
                freq_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_commFreq_youthOut_1,
                                                    TRUE ~ NA),
                # commTimely Within
                time_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_commTimely_research_1,
                                                    TRUE ~ NA),
                time_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_commTimely_cityGovt_1,
                                                    TRUE ~ NA),
                time_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_commTimely_corrections_1,
                                                      TRUE ~ NA),
                time_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_commTimely_daOffice_1,
                                                  TRUE ~ NA),
                time_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_commTimely_empDevel_1,
                                                    TRUE ~ NA),
                time_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_commTimely_faithBased_1,
                                                   TRUE ~ NA),
                time_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_commTimely_familySocial_1,
                                                   TRUE ~ NA),
                time_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_commTimely_health_1,
                                                   TRUE ~ NA),
                time_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_commTimely_parole_1,
                                                     TRUE ~ NA),
                time_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_commTimely_police_1,
                                                      TRUE ~ NA),
                time_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_commTimely_probation_1,
                                                    TRUE ~ NA),
                time_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_commTimely_school_1,
                                                      TRUE ~ NA),
                time_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_commTimely_youthOut_1,
                                                     TRUE ~ NA),
                # commAccurately Within
                accur_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_commAccurately_research_1,
                                                    TRUE ~ NA),
                accur_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_commAccurately_cityGovt_1,
                                                    TRUE ~ NA),
                accur_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_commAccurately_corrections_1,
                                                      TRUE ~ NA),
                accur_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_commAccurately_daOffice_1,
                                                  TRUE ~ NA),
                accur_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_commAccurately_empDevel_1,
                                                    TRUE ~ NA),
                accur_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_commAccurately_faithBased_1,
                                                   TRUE ~ NA),
                accur_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_commAccurately_familySocial_1,
                                                   TRUE ~ NA),
                accur_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_commAccurately_health_1,
                                                   TRUE ~ NA),
                accur_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_commAccurately_parole_1,
                                                     TRUE ~ NA),
                accur_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_commAccurately_police_1,
                                                      TRUE ~ NA),
                accur_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_commAccurately_probation_1,
                                                    TRUE ~ NA),
                accur_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_commAccurately_school_1,
                                                      TRUE ~ NA),
                accur_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_commAccurately_youthOut_1,
                                                     TRUE ~ NA),
                # PSolve Within
                psolv_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_blameOthers_research_1,
                                                    TRUE ~ NA),
                psolv_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_blameOthers_cityGovt_1,
                                                    TRUE ~ NA),
                psolv_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_blameOthers_corrections_1,
                                                      TRUE ~ NA),
                psolv_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_blameOthers_daOffice_1,
                                                  TRUE ~ NA),
                psolv_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_blameOthers_empDevel_1,
                                                    TRUE ~ NA),
                psolv_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_blameOthers_faithBased_1,
                                                   TRUE ~ NA),
                psolv_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_blameOthers_familySocial_1,
                                                   TRUE ~ NA),
                psolv_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_blameOthers_health_1,
                                                   TRUE ~ NA),
                psolv_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_blameOthers_parole_1,
                                                     TRUE ~ NA),
                psolv_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_blameOthers_police_1,
                                                      TRUE ~ NA),
                psolv_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_blameOthers_probation_1,
                                                    TRUE ~ NA),
                psolv_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_blameOthers_school_1,
                                                      TRUE ~ NA),
                psolv_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_blameOthers_youthOut_1,
                                                     TRUE ~ NA),
                # Shared Goals Within
                sgoal_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_shareGoals_research_1,
                                                    TRUE ~ NA),
                sgoal_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_shareGoals_cityGovt_1,
                                                    TRUE ~ NA),
                sgoal_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_shareGoals_corrections_1,
                                                      TRUE ~ NA),
                sgoal_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_shareGoals_daOffice_1,
                                                  TRUE ~ NA),
                sgoal_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_shareGoals_empDevel_1,
                                                    TRUE ~ NA),
                sgoal_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_shareGoals_faithBased_1,
                                                   TRUE ~ NA),
                sgoal_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_shareGoals_familySocial_1,
                                                   TRUE ~ NA),
                sgoal_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_shareGoals_health_1,
                                                   TRUE ~ NA),
                sgoal_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_shareGoals_parole_1,
                                                     TRUE ~ NA),
                sgoal_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_shareGoals_police_1,
                                                      TRUE ~ NA),
                sgoal_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_shareGoals_probation_1,
                                                    TRUE ~ NA),
                sgoal_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_shareGoals_school_1,
                                                      TRUE ~ NA),
                sgoal_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_shareGoals_youthOut_1,
                                                     TRUE ~ NA),
                # Shared Knowledge Within
                sknow_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_knowYourWork_research_1,
                                                    TRUE ~ NA),
                sknow_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_knowYourWork_cityGovt_1,
                                                    TRUE ~ NA),
                sknow_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_knowYourWork_corrections_1,
                                                      TRUE ~ NA),
                sknow_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_knowYourWork_daOffice_1,
                                                  TRUE ~ NA),
                sknow_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_knowYourWork_empDevel_1,
                                                    TRUE ~ NA),
                sknow_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_knowYourWork_faithBased_1,
                                                   TRUE ~ NA),
                sknow_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_knowYourWork_familySocial_1,
                                                   TRUE ~ NA),
                sknow_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_knowYourWork_health_1,
                                                   TRUE ~ NA),
                sknow_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_knowYourWork_parole_1,
                                                     TRUE ~ NA),
                sknow_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_knowYourWork_police_1,
                                                      TRUE ~ NA),
                sknow_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_knowYourWork_probation_1,
                                                    TRUE ~ NA),
                sknow_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_knowYourWork_school_1,
                                                      TRUE ~ NA),
                sknow_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_knowYourWork_youthOut_1,
                                                     TRUE ~ NA),
                # Respect Work Within
                mresp_Within_Acad = dplyr::case_when(WORKGROUP == 1 ~ VAR_respectYourWork_research_1,
                                                    TRUE ~ NA),
                mresp_Within_CGov = dplyr::case_when(WORKGROUP == 2 ~ VAR_respectYourWork_cityGovt_1,
                                                    TRUE ~ NA),
                mresp_Within_Correc = dplyr::case_when(WORKGROUP == 3 ~ VAR_respectYourWork_corrections_1,
                                                      TRUE ~ NA),
                mresp_Within_DA = dplyr::case_when(WORKGROUP == 4 ~ VAR_respectYourWork_daOffice_1,
                                                  TRUE ~ NA),
                mresp_Within_EDEV = dplyr::case_when(WORKGROUP == 5 ~ VAR_respectYourWork_empDevel_1,
                                                    TRUE ~ NA),
                mresp_Within_FBS = dplyr::case_when(WORKGROUP == 6 ~ VAR_respectYourWork_faithBased_1,
                                                   TRUE ~ NA),
                mresp_Within_FFS = dplyr::case_when(WORKGROUP == 7 ~ VAR_respectYourWork_familySocial_1,
                                                   TRUE ~ NA),
                mresp_Within_HMH = dplyr::case_when(WORKGROUP == 8 ~ VAR_respectYourWork_health_1,
                                                   TRUE ~ NA),
                mresp_Within_Parol = dplyr::case_when(WORKGROUP == 9 ~ VAR_respectYourWork_parole_1,
                                                     TRUE ~ NA),
                mresp_Within_Police = dplyr::case_when(WORKGROUP == 10 ~ VAR_respectYourWork_police_1,
                                                      TRUE ~ NA),
                mresp_Within_Prob = dplyr::case_when(WORKGROUP == 11 ~ VAR_respectYourWork_probation_1,
                                                    TRUE ~ NA),
                mresp_Within_School = dplyr::case_when(WORKGROUP == 12 ~ VAR_respectYourWork_school_1,
                                                      TRUE ~ NA),
                mresp_Within_Youth = dplyr::case_when(WORKGROUP == 13 ~ VAR_respectYourWork_youthOut_1,
                                                     TRUE ~ NA)

                ) %>%
  # Create RC scores and dimensions
  dplyr::mutate(freq_Ext = rowMeans(dplyr::select(., dplyr::contains("freq_Between")), na.rm = TRUE),
                time_Ext = rowMeans(dplyr::select(., dplyr::contains("time_Between")), na.rm = TRUE),
                accur_Ext = rowMeans(dplyr::select(., dplyr::contains("accur_Between")), na.rm = TRUE),
                psolv_Ext = rowMeans(dplyr::select(., dplyr::contains("psolv_Between")), na.rm = TRUE),
                sgoal_Ext = rowMeans(dplyr::select(., dplyr::contains("sgoal_Between")), na.rm = TRUE),
                sknow_Ext = rowMeans(dplyr::select(., dplyr::contains("sknow_Between")), na.rm = TRUE),
                mresp_Ext = rowMeans(dplyr::select(., dplyr::contains("mresp_Between")), na.rm = TRUE)) %>%
  dplyr::filter(!is.nan(freq_Ext)) %>%
  dplyr::filter(!is.nan(time_Ext)) %>%
  dplyr::filter(!is.nan(accur_Ext)) %>%
  dplyr::filter(!is.nan(psolv_Ext)) %>%
  dplyr::filter(!is.nan(sgoal_Ext)) %>%
  dplyr::filter(!is.nan(sknow_Ext)) %>%
  dplyr::filter(!is.nan(mresp_Ext)) %>%
  dplyr::mutate(RCindex = rowMeans(dplyr::select(., dplyr::contains("Ext"))))




################################################################################
rcLong <- data2 %>%
  dplyr::select(city, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                dplyr::starts_with("VAR_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("VAR_"),
                      names_to = "fullname",
                      values_to = "weight") %>%
  dplyr::mutate(fullname = stringr::str_remove(fullname, "^VAR_"),
                var = stringr::str_extract(fullname, "^[^_]+"),
                alter = sub("_.*", "", sub("^[^_]+_", "", fullname)),
                round = as.numeric(stringr::str_extract(fullname, "\\d$")),
                weight2 = as.numeric(stringr::str_extract(weight, "\\d")),
                within = WORKGROUP == alter,
                between = WORKGROUP != alter
  ) %>%
  dplyr::select(-fullname) %>%
  dplyr::select(city, round, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                alter, within, between, var, weight = weight2, weight_label = weight) %>%
  dplyr::mutate(alter = dplyr::case_when(alter == "cityGovt" ~ "city_govt",
                                         alter == "daOffice" ~ "da_office",
                                         alter == "empDevel" ~ "emp_devel",
                                         alter == "faithBased" ~ "faith_based",
                                         alter == "familySocial" ~ "family_social",
                                         alter == "youthOut" ~ "youth_out",
                                         TRUE ~ alter)) %>%
  # Recoding `commFreq` measures to better reflect scale
  dplyr::mutate(weight = dplyr::case_when((var == "commFreq" & weight_label == "(1) Not Nearly Enough") ~ 1,
                                          (var == "commFreq" & weight_label == "(2) Not Enough") ~ 3,
                                          (var == "commFreq" & weight_label == "(3) Just About the RIght Amount") ~ 5,
                                          (var == "commFreq" & weight_label == "(3) Just About the Right Amount") ~ 5,
                                          (var == "commFreq" & weight_label == "(4) Too Often") ~ 4,
                                          (var == "commFreq" & weight_label == "(5) Much Too Often") ~ 2,
                                          TRUE ~ weight
                                          )
                                          )

write.csv(rcLong, "./ICPSR_37453/rcLong.csv")
save(rcLong, file = "./data/rcLong.rda")


test <- rcLong %>%
  dplyr::filter(between == TRUE & round == 1) %>%
  dplyr::select(CASEID, round, var, between, weight) %>%
  dplyr::group_by(CASEID, round, var, between) %>%
  dplyr::summarize(weight = mean(weight)) %>%
  tidyr::pivot_wider(names_from = between,
                     values_from = weight,
                     names_prefix = "between") %>%
  dplyr::rename(between = betweenTRUE) %>%
  tidyr::pivot_wider(names_from = var, values_from = between)

test2 <- test %>% dplyr::select(CASEID, dplyr::starts_with("between"))
test3 <- test[complete.cases(test),]

###############################################################################
#    C R E A T I N G   A V E R A G E D ,   ' S C A L E D '   W E I G H T S    #
###############################################################################

# Set threshold for number of missing values allowed before dropping
# dyad-level RC score
drop_threshold <- 1

rcScaled <- rcLong %>%
  # dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(city, round, CASEID, WORKGROUP, alter) %>%
  dplyr::mutate(num_missing = sum(is.na(weight))) %>%
  dplyr::filter(num_missing < drop_threshold) %>%
  dplyr::summarize(weight = mean(weight, na.rm = TRUE),
                   num_missing = mean(num_missing)) %>%
  dplyr::mutate(num_response = 7-num_missing,
                var = "RC_scale") %>%
  dplyr::ungroup()

write.csv(rcScaled, "./ICPSR_37453/rcScaled.csv")
save(rcScaled, file = "./data/rcScaled.rda")


#############################################
#    F U L L Y   M I S S I N G   T I E S    #
#############################################

# Jody suggests that non-loop dyads missing all RC measures indicate ties
# that are probably missing by design, perhaps as a result of policy decisions
# determining that members of two role groups don't need to be in communication.
# The following code is meant to identify such dyads

rcMissing <- rcLong %>%
  dplyr::group_by(city, round, CASEID, WORKGROUP, alter) %>%
  dplyr::mutate(num_missing = sum(is.na(weight))) %>%
  dplyr::filter(num_missing == 7) %>%
  dplyr::filter(between == TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(CASEID) %>%
  dplyr::mutate(var = "fully_missing") %>%
  dplyr::select(-num_missing) %>%
  dplyr::mutate(weight = .5)

write.csv(rcMissing, "./ICPSR_37453/rcMissing.csv")
save(rcMissing, file = "./data/rcMissing.rda")

#############################################################
#    I N D I V I D U A L - L E V E L   R C   S C O R E S    #
#############################################################

rcScores <- rcScaled %>%
  dplyr::group_by(city, round, CASEID) %>%
  dplyr::summarize(rc_score = mean(weight),
                   num_relationships = dplyr::n())

write.csv(rcScores, "./ICPSR_37453/rcScores.csv")
save(rcScores, file = "./data/rcScores.rda")
