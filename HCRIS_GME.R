library(ggplot2)

#rename all the columns in each df
colnames(HOSP10_2023_NMRC) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_VAL_NUM")
colnames(HOSP10_2023_ALPHA) <- c("RPT_REC_NUM", "WKSHT_CD", "LINE_NUM", "CLMN_NUM", "ITM_ALPHNMRC_ITM_TXT")
colnames(HOSP10_2023_RPT) <- c("RPT_REC_NUM", "PRVDR_CTRL_TYPE_CD", "PRVDR_NUM", "NPI", "RPT_STUS_CD", "FY_BGN_DT", "FY_END_DT", "PROC_DT", "INITL_RPT_SW", "LAST_RPT_SW", "TRNSMTL_NUM", "FI_NUM", "ADR_VNDR_CD", "FI_CREAT_DT", "UTIL_CD", "NPR_DT", "SPEC_IND", "FI_RCPT_DT")

#create a df with just GA providers
GA_providers <- subset(HOSPITAL10_PROVIDER_ID_INFO, State == "GA")

#create a df of RPT_REC_NUM filtered by GA providers
GA_RPT_REC_NUM <- HOSP10_2023_RPT[HOSP10_2023_RPT$PRVDR_NUM %in% GA_providers$PROVIDER_NUMBER, ]

#need to use B pt 1 lines 21 and 22 for actual refunds
#use row 19 for actual DGME cost

#new dfs of specific line numbers
Medicare_FTE_Cap_Line_5 <- subset(HOSP10_2023_NMRC, 
                                  WKSHT_CD == "E40A180" &
                                  LINE_NUM == "00500" &
                                  CLMN_NUM == "00100")

Medicare_FTE_Total_Over_Cap_Line_21 <- subset(HOSP10_2023_NMRC, 
                                       WKSHT_CD == "E40A180" &
                                         LINE_NUM == "02100" &
                                         CLMN_NUM == "00100")

Per_Resident_Cost_Line_18 <- subset(HOSP10_2023_NMRC, 
                                    WKSHT_CD == "E40A180" &
                                      LINE_NUM == "01800" &
                                      CLMN_NUM == "00100")

Total_DGME_Amount_Line_25 <- subset(HOSP10_2023_NMRC, 
                                    WKSHT_CD == "E40A180" &
                                      LINE_NUM == "02500" &
                                      CLMN_NUM == "00100")


#average FTE cap
Average_FTE_cap <- mean(Medicare_FTE_Cap_Line_5$ITM_VAL_NUM)
print(Average_FTE_cap)

#average over cap
Average_over_cap <- mean(Medicare_FTE_Total_Over_Cap_Line_21$ITM_VAL_NUM)
print(Average_over_cap)

#average per resident cost
Average_resident_cost <- mean(Per_Resident_Cost_Line_18$ITM_VAL_NUM)
print(Average_resident_cost)

#average DGME amount
Average_DGME_amount <- mean(Total_DGME_Amount_Line_25$ITM_VAL_NUM)
print(Average_DGME_amount)

#top 5 hospitals in each state. Over 1 billion by state


#plotting
hist(Medicare_FTE_Cap_Line_5$ITM_VAL_NUM, 
     breaks = 25,               # Number of bins
     col = "lightblue",          # Color of the bars
     main = "Histogram of FTE Cap", 
     xlab = "FTE Cap", 
     ylab = "Frequency", 
     border = "black")

#plotting
hist(Medicare_FTE_Total_Over_Cap_Line_21$ITM_VAL_NUM, 
     breaks = 25,               # Number of bins
     col = "lightblue",          # Color of the bars
     main = "Histogram of FTE Over Cap", 
     xlab = "FTE Over Cap", 
     ylab = "Frequency", 
     border = "black")

#plotting
hist(Per_Resident_Cost_Line_18$ITM_VAL_NUM, 
     breaks = 25,               # Number of bins
     col = "lightblue",          # Color of the bars
     main = "Histogram of Per Resident Cost", 
     xlab = "Per Resident Cost", 
     ylab = "Frequency", 
     border = "black")

#plotting
hist(Total_DGME_Amount_Line_25$ITM_VAL_NUM, 
     breaks = 10,               # Number of bins
     col = "lightblue",          # Color of the bars
     main = "Histogram of DGME Amount", 
     xlab = "DGME Amount", 
     ylab = "Frequency", 
     border = "black")  
