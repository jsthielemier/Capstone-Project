library(ggplot2)

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

#new dfs of specific line numbers
GME_Cap_Line_5 <- subset(HOSP10_2022_NMRC, 
                         WKSHT_CD == "E40A180" &
                           LINE_NUM == "00500" &
                           CLMN_NUM == "00100")

GME_Total_Line_21 <- subset(HOSP10_2022_NMRC, 
                            WKSHT_CD == "E40A180" &
                              LINE_NUM == "00600" &
                              CLMN_NUM == "00100")

Average_Resident_Cost_Line_18 <- subset(HOSP10_2022_NMRC, 
                                        WKSHT_CD == "E40A180" &
                                          LINE_NUM == "01800" &
                                          CLMN_NUM == "00100")

# Subset for the first part
GME_Part1 <- subset(HOSP10_2022_NMRC, 
                    WKSHT_CD == "B000001" &
                      LINE_NUM == "02100" &
                      CLMN_NUM == "02100")

# Calculate the average for the first part
Avg_Part1 <- mean(GME_Part1$ITM_VAL_NUM, na.rm = TRUE)

# Subset for the second part
GME_Part2 <- subset(HOSP10_2022_NMRC, 
                    WKSHT_CD == "B000001" &
                      LINE_NUM == "02200" &
                      CLMN_NUM == "02200")

# Calculate the average for the second part
Avg_Part2 <- mean(GME_Part2$ITM_VAL_NUM, na.rm = TRUE)

# Subset for the third part (subtract this one)
GME_Part3 <- subset(HOSP10_2022_NMRC, 
                    WKSHT_CD == "E40A180" &
                      LINE_NUM == "02500" &
                      CLMN_NUM == "00100")

# Calculate the average for the third part
Avg_Part3 <- mean(GME_Part3$ITM_VAL_NUM, na.rm = TRUE)

# Calculate the final GME_Delta value
GME_Delta <- Avg_Part1 + Avg_Part2 - Avg_Part3

# Print the result
GME_Delta



#average GTE Cap
Average_GME_Cap <- mean(GME_Cap_Line_5$ITM_VAL_NUM)
print(Average_GME_Cap)

#average over cap
Average_GME_Total <- mean(GME_Total_Line_21$ITM_VAL_NUM)
print(Average_GME_Total)

#average per resident cost
Total_Average_Resident_Cost_Line_18 <- mean(Average_Resident_Cost_Line_18$ITM_VAL_NUM)
print(Total_Average_Resident_Cost_Line_18)



#plotting
hist(GME_Cap_Line_5$ITM_VAL_NUM, 
     breaks = 40,               
     col = "lightblue",         
     main = "Histogram of GME Cap", 
     xlab = "FTE Cap", 
     ylab = "Frequency", 
     border = "black")

#plotting
hist(GME_Total_Line_21$ITM_VAL_NUM, 
     breaks = 40,               
     col = "lightblue",         
     main = "Histogram of GME Total", 
     xlab = "FTE Over Cap", 
     ylab = "Frequency", 
     border = "black")

#plotting
hist(Average_Resident_Cost_Line_18$ITM_VAL_NUM, 
     breaks = 40,               
     col = "lightblue",          
     main = "Histogram of Average Resident Cost", 
     xlab = "Per Resident Cost", 
     ylab = "Frequency", 
     border = "black")

Hospital_Type <- subset(HOSP10_2022_NMRC, 
                        WKSHT_CD == "S200001")
print(Hospital_Type)
