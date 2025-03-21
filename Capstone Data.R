library(ggplot2)
library(dplyr)

#rename all the columns in each df
colnames(HOSP10_2022_NMRC) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_VAL_NUM")
colnames(HOSP10_2022_ALPHA) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_ALPHNMRC_ITM_TXT")
colnames(HOSP10_2022_RPT) <- c("RPT_REC_NUM", "PRVDR_CTRL_TYPE_CD", "PROVIDER_NUMBER", "NPI", "RPT_STUS_CD", "FY_BGN_DT", "FY_END_DT", "PROC_DT", "INITL_RPT_SW", "LAST_RPT_SW", "TRNSMTL_NUM", "FI_NUM", "ADR_VNDR_CD", "FI_CREAT_DT", "UTIL_CD", "NPR_DT", "SPEC_IND", "FI_RCPT_DT")

# Count total number of NAs in the entire data frame
sum(is.na(HOSP10_2022_NMRC))

#create a df with just GA providers
GA_providers <- subset(HOSPITAL10_PROVIDER_ID_INFO, State == "GA")

#create a df of RPT_REC_NUM filtered by GA providers
GA_RPT_REC_NUM <- HOSP10_2022_RPT[HOSP10_2022_RPT$PROVIDER_NUMBER %in% GA_providers$PROVIDER_NUMBER, ]

GA_Hospitals <- GA_providers %>%
  left_join(
    GA_RPT_REC_NUM %>% select(PROVIDER_NUMBER, RPT_REC_NUM),
    by = "PROVIDER_NUMBER"
  )

GA_Hospitals <- GA_Hospitals %>%
  select(-FYB, -FYE, -STATUS, -CTRL_TYPE, -PO_Box)

GA_Hospitals <- GA_Hospitals %>%
  select(RPT_REC_NUM, PROVIDER_NUMBER, everything())



# Filter the df by filled slots
# Step 1: Filter the target data from HOSP10_2022_NMRC
E40A180_data <- HOSP10_2022_NMRC %>%
  filter(WKSHT_CD == "E40A180", LINE_NUM == "00600") %>%
  select(RPT_REC_NUM, ITM_VAL_NUM) %>%
  rename(E40A180_00600_Value = ITM_VAL_NUM)

# Step 2: Join with GA_Hospitals
GA_Hospitals_updated <- GA_Hospitals %>%
  left_join(E40A180_data, by = "RPT_REC_NUM")

# Step 3: Remove rows where E40A180_00600_Value is NA or 0
GA_Hospitals_filtered <- GA_Hospitals_updated %>%
  filter(!is.na(E40A180_00600_Value) & E40A180_00600_Value != 0)

# Step 4:Rename the column
GA_Hospitals_filtered <- GA_Hospitals_filtered %>%
  rename(`Filled Slots` = E40A180_00600_Value)


# Add new columns for the characteristics
# Function to pull a specific value by worksheet and line number
pull_value <- function(wksht_cd, line_num, new_col_name) {
  HOSP10_2022_NMRC %>%
    filter(WKSHT_CD == wksht_cd, LINE_NUM == line_num) %>%
    select(RPT_REC_NUM, ITM_VAL_NUM) %>%
    rename(!!new_col_name := ITM_VAL_NUM)
}

# Pull multiple values based on different WKSHT_CD and LINE_NUM combos
filled_slots <- pull_value("E40A180", "00600", "Filled_Slots")
cap_slots    <- pull_value("E40A180", "01000", "Cap_Slots")
other_value  <- pull_value("E40A180", "01400", "Other_Value")

# Join all values to the main hospitals table (no filtering)
GA_Hospitals_updated <- GA_Hospitals_filtered %>%
  left_join(filled_slots, by = "RPT_REC_NUM") %>%
  left_join(cap_slots, by = "RPT_REC_NUM") %>%
  left_join(other_value, by = "RPT_REC_NUM")