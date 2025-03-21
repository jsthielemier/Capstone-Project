library(ggplot2)
library(dplyr)

#rename all the columns in each df
colnames(HOSP10_2022_NMRC) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_VAL_NUM")
colnames(HOSP10_2022_ALPHA) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_ALPHNMRC_ITM_TXT")
colnames(HOSP10_2022_RPT) <- c("RPT_REC_NUM", "PRVDR_CTRL_TYPE_CD", "PROVIDER_NUMBER", "NPI", "RPT_STUS_CD", "FY_BGN_DT", "FY_END_DT", "PROC_DT", "INITL_RPT_SW", "LAST_RPT_SW", "TRNSMTL_NUM", "FI_NUM", "ADR_VNDR_CD", "FI_CREAT_DT", "UTIL_CD", "NPR_DT", "SPEC_IND", "FI_RCPT_DT")


# 1. Pull all providers (no state filter)
US_providers <- HOSPITAL10_PROVIDER_ID_INFO

# 2. Get report numbers for all providers
US_RPT_REC_NUM <- HOSP10_2022_RPT %>%
  filter(PROVIDER_NUMBER %in% US_providers$PROVIDER_NUMBER)

# 3. Merge providers and report numbers
US_Hospitals <- US_providers %>%
  left_join(
    US_RPT_REC_NUM %>% select(PROVIDER_NUMBER, RPT_REC_NUM),
    by = "PROVIDER_NUMBER"
  )

# 4. Clean columns (drop extras if desired)
US_Hospitals <- US_Hospitals %>%
  select(-FYB, -FYE, -STATUS, -CTRL_TYPE, -PO_Box)

# 5. Reorder columns
US_Hospitals <- US_Hospitals %>%
  select(RPT_REC_NUM, PROVIDER_NUMBER, everything())

# 6. Add Filled Slots column
filled_slots <- HOSP10_2022_NMRC %>%
  filter(WKSHT_CD == "E40A180", LINE_NUM == "00600") %>%
  select(RPT_REC_NUM, ITM_VAL_NUM) %>%
  rename(`Filled Slots` = ITM_VAL_NUM)

# 7. Join Filled Slots to hospitals
US_Hospitals_updated <- US_Hospitals %>%
  left_join(filled_slots, by = "RPT_REC_NUM")

# 8. Filter hospitals: keep only where Filled Slots > 0
US_Hospitals_filtered <- US_Hospitals_updated %>%
  filter(!is.na(`Filled Slots`) & `Filled Slots` > 0)

# 9. Function to pull additional columns (no filters on these)
pull_value <- function(wksht_cd, line_num, new_col_name) {
  HOSP10_2022_NMRC %>%
    filter(WKSHT_CD == wksht_cd, LINE_NUM == line_num) %>%
    select(RPT_REC_NUM, ITM_VAL_NUM) %>%
    rename(!!new_col_name := ITM_VAL_NUM)
}