library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(readxl)
library(stringr)
library(fuzzyjoin)



# Rename columns
colnames(HOSP10_2022_NMRC) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_VAL_NUM")
colnames(HOSP10_2022_ALPHA) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_ALPHNMRC_ITM_TXT")
colnames(HOSP10_2022_RPT) <- c("RPT_REC_NUM", "PRVDR_CTRL_TYPE_CD", "PROVIDER_NUMBER", "NPI", "RPT_STUS_CD", 
                               "FY_BGN_DT", "FY_END_DT", "PROC_DT", "INITL_RPT_SW", "LAST_RPT_SW", 
                               "TRNSMTL_NUM", "FI_NUM", "ADR_VNDR_CD", "FI_CREAT_DT", "UTIL_CD", 
                               "NPR_DT", "SPEC_IND", "FI_RCPT_DT")

# Filter for Georgia providers only
GA_providers <- HOSPITAL10_PROVIDER_ID_INFO %>% filter(State == "GA")

# Get report numbers
GA_RPT_REC_NUM <- HOSP10_2022_RPT %>%
  filter(PROVIDER_NUMBER %in% GA_providers$PROVIDER_NUMBER)

# Merge and clean
GA_Hospitals <- GA_providers %>%
  left_join(GA_RPT_REC_NUM %>% select(PROVIDER_NUMBER, RPT_REC_NUM), by = "PROVIDER_NUMBER") %>%
  select(-FYB, -FYE, -STATUS, -CTRL_TYPE, -PO_Box) %>%
  select(RPT_REC_NUM, PROVIDER_NUMBER, everything())

# Add Filled Slots
filled_slots <- HOSP10_2022_NMRC %>%
  filter(WKSHT_CD == "E40A180", LINE_NUM == "00600") %>%
  select(RPT_REC_NUM, ITM_VAL_NUM) %>%
  rename(`Filled Slots` = ITM_VAL_NUM)

GA_Hospitals <- GA_Hospitals %>%
  left_join(filled_slots, by = "RPT_REC_NUM") %>%
  filter(!is.na(`Filled Slots`) & `Filled Slots` > 0)

# Helper: Pull numeric values (NMRC)
pull_value <- function(wksht_cd, line_num, clmn_num, new_col_name) {
  HOSP10_2022_NMRC %>%
    filter(
      WKSHT_CD == wksht_cd,
      LINE_NUM == str_pad(line_num, 5, pad = "0"),
      CLMN_NUM == str_pad(clmn_num, 5, pad = "0")
    ) %>%
    select(RPT_REC_NUM, ITM_VAL_NUM) %>%
    rename(!!new_col_name := ITM_VAL_NUM)
}

# Helper: Pull alphanumeric values (ALPHA)
pull_alpha_value <- function(wksht_cd, line_num, clmn_num, new_col_name) {
  HOSP10_2022_ALPHA %>%
    filter(
      WKSHT_CD == wksht_cd,
      LINE_NUM == str_pad(line_num, 5, pad = "0"),
      CLMN_NUM == str_pad(clmn_num, 5, pad = "0")
    ) %>%
    select(RPT_REC_NUM, ITM_ALPHNMRC_ITM_TXT) %>%
    rename(!!new_col_name := ITM_ALPHNMRC_ITM_TXT)
}

# Pull Medicare GME payment components
resident_salary <- pull_value("B000001", "02100", "02100", "Resident Salary")
faculty_cost <- pull_value("B000001", "02200", "02200", "Faculty Cost")


# Pull Provider Type from ALPHA
system_affiliation <- pull_alpha_value("S200001", "14100", "00100", "System Affiliation")
type_of_control <- pull_alpha_value("S200001", "02100", "00100", "Type of Control")


# Join everything
GA_Hospitals <- GA_Hospitals %>%
  left_join(pull_value("S200001", "02700", "00100", "Urban/Rural Status"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("S300001", "01400", "00200", "Number of Beds"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("E40A180", "00100", "00100", "Federal GME Cap"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("E40A180", "00600", "00100", "FTE Residents"), by = "RPT_REC_NUM") %>%
  left_join(system_affiliation, by = "RPT_REC_NUM") %>%
  left_join(type_of_control, by = "RPT_REC_NUM") %>%
  left_join(pull_value("S300001", "01400", "00600", "Medicare Days"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("S300001", "01400", "00700", "Medicaid Days"), by = "RPT_REC_NUM") %>%
  left_join(pull_value("G300000", "00100", "00100", "Hospital Revenue"), by = "RPT_REC_NUM") %>%
  left_join(resident_salary, by = "RPT_REC_NUM") %>%
  left_join(faculty_cost, by = "RPT_REC_NUM") %>%
  mutate(
    Medicare_GME_Payment = rowMeans(cbind(`Resident Salary`, `Faculty Cost`), na.rm = TRUE),
    Medicare_GME_Per_Resident = ifelse(`FTE Residents` > 0, Medicare_GME_Payment / `FTE Residents`, NA),

    Medicare_Pct = ifelse((`Medicare Days` + `Medicaid Days`) > 0,
                          `Medicare Days` / (`Medicare Days` + `Medicaid Days`), NA),
    Medicaid_Pct = ifelse((`Medicare Days` + `Medicaid Days`) > 0,
                          `Medicaid Days` / (`Medicare Days` + `Medicaid Days`), NA),
    
    Academic_Medical_Center = case_when(
      `FTE Residents` > 0 ~ "Yes",
      `FTE Residents` == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    
    Ownership_Type = case_when(
      `Type of Control` %in% c(1, 2) ~ "Non-Profit",
      `Type of Control` %in% c(3, 4, 5) ~ "For-Profit",
      `Type of Control` %in% c(6, 7, 8) ~ "Government",
      TRUE ~ "Other"
    ),
    
    Hospital_System_Affiliation = case_when(
      !is.na(`System Affiliation`) ~ "System Member",
      is.na(`System Affiliation`) ~ "Stand-Alone"
    ),
    
    Ownership_Type = case_when(
      `Type of Control` %in% c("1", "2", "13") ~ "Non-Profit",
      `Type of Control` %in% c("3", "4", "5", "12") ~ "For-Profit",
      `Type of Control` %in% c("6", "7", "8", "10", "11") ~ "Government",
      `Type of Control` == "9" ~ "Other",
      is.na(`Type of Control`) ~ "Unknown",
      TRUE ~ "Other"
    ),
  )






# Export
write.csv(GA_Hospitals, 
          "C:/Users/Jacob Thielemier/OneDrive - Hull Property Group/Desktop/Capstone-Project/GA_Hospitals.csv", 
          row.names = FALSE)




