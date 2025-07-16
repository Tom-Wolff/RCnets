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

rcLong <- data %>%
  dplyr::select(city, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                dplyr::starts_with("VAR_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("VAR_"),
                      names_to = "fullname",
                      values_to = "weight") %>%
  dplyr::mutate(fullname = stringr::str_remove(fullname, "^VAR_"),
                var = stringr::str_extract(fullname, "^[^_]+"),
                alter = sub("_.*", "", sub("^[^_]+_", "", fullname)),
                round = as.numeric(stringr::str_extract(fullname, "\\d$")),
                weight2 = as.numeric(stringr::str_extract(weight, "\\d"))
  ) %>%
  dplyr::select(-fullname) %>%
  dplyr::select(city, round, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                alter, var, weight = weight2, weight_label = weight) %>%
  dplyr::mutate(alter = dplyr::case_when(alter == "cityGovt" ~ "city_govt",
                                         alter == "daOffice" ~ "da_office",
                                         alter == "empDevel" ~ "emp_devel",
                                         alter == "faithBased" ~ "faith_based",
                                         alter == "familySocial" ~ "family_social",
                                         alter == "youthOut" ~ "youth_out",
                                         TRUE ~ alter))

write.csv(data2, "~/Desktop/ICPSR_37453/rcLong.csv")


###############################################################################
#    C R E A T I N G   A V E R A G E D ,   ' S C A L E D '   W E I G H T S    #
###############################################################################

rcScaled <- rcLong %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(city, round, CASEID, WORKGROUP, alter) %>%
  dplyr::summarize(weight = mean(weight, na.rm = TRUE),
                   num_responses = dplyr::n()) %>%
  dplyr::ungroup()

write.csv(rcScaled, "~/Desktop/ICPSR_37453/rcScaled.csv")


