
# Introduction ------------------------------------------------------------

# This code reproduces analyses of the situation of the global
# banking system during the Covid-19 pandemic, with a focus on
# developments in credit risk and in banks' liquidity and capital
# position. There is no liability towards this code and no guarantee
# is offered. In particular, please bear in mind that the results might
# change depending on the availability of bank earnings disclosures,
# since the code dynamically gets data from Bloomberg.
#
# Note: to run this code, the user needs an open Bloomberg session.
#
# Author: Douglas Araujo, Basel Committee Secretariat, BIS (Basel, Switzerland)

# Packages ----------------------------------------------------------------
library(Rblpapi)
library(data.table)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(alfred)
library(stargazer)

con <- blpConnect()

# Load the bank tickers ---------------------------------------------------

GSIBs <- c("JPM US Equity",
           "C US Equity",
           "HSBA LN Equity",
           "BAC US Equity",
           "3988 hk Equity",
           "BARC LN Equity",
           "BNP FP Equity",
           "DBK EU Equity",
           "GS US Equity",
           "601398 CH Equity",
           "WFC US Equity",
           "8306 JP Equity",
           "1288 HK Equity",
           "SAN SM Equity",
           "BK US Equity",
           "939 HK Equity",
           "ACA FP Equity",
           "CSGN SW Equity",
           "3692594Z FP Equity",
           "INGA NA Equity",
           "8411 JP Equity",
           "MS US Equity",
           "RY CN Equity",
           "GLE FP Equity",
           "STAN LN Equity",
           "STT US Equity",
           "8316 JP Equity",
           "TD CN Equity",
           "UBSG SW Equity",
           "UCG IM Equity")


sample_banks <- c("JPM US Equity",
                  "HSBA LN Equity",
                  "C US Equity",
                  "601398 CH Equity",
                  "3988 HK Equity",
                  "BNP FP Equity",
                  "8306 JP Equity",
                  "BAC US Equity",
                  "WFC US Equity",
                  "BARC LN Equity",
                  "DBK EU Equity",
                  "GS US Equity",
                  "939 HK Equity",
                  "1288 HK Equity",
                  "ACA FP Equity",
                  "8316 JP Equity",
                  "8411 JP Equity",
                  "SAN SM Equity",
                  "GLE FP Equity",
                  "RY CN Equity",
                  "TD CN Equity",
                  "INGA NA Equity",
                  "UCG IM Equity",
                  "ms us equity",
                  "csgn sw equity",
                  "STAN LN Equity",
                  "bk us equity",
                  "stt us equity",
                  "7182 JP Equity",
                  "1658 HK Equity",
                  "3328 HK Equity",
                  "600036 CH Equity",
                  "LLOY LN Equity",
                  "601166 CH Equity",
                  "600000 CH Equity",
                  "RBS LN Equity",
                  "998 HK Equity",
                  "600016 CH Equity",
                  "3692594Z FP Equity",
                  "ISP IM Equity",
                  "BNS CN Equity",
                  "BBVA SM Equity",
                  "601818 CH Equity",
                  "NDA SS Equity",
                  "BMO CN Equity",
                  "DANSKE DC Equity",
                  "000001 CH Equity",
                  "CBK GR Equity",
                  "UBSG SW Equity",
                  "SBIN IN Equity",
                  "8308 JP Equity",
                  "TFC US Equity",
                  "CM CN Equity",
                  "055550 KS Equity",
                  "CABK SM Equity",
                  "DBS SP Equity",
                  "105560 KS Equity",
                  "PNC US Equity",
                  "ABN NA Equity",
                  "600015 CH Equity",
                  "SBER RM Equity",
                  "8421 JP Equity",
                  "601169 CH Equity",
                  "086790 KS Equity",
                  "SHBA SS Equity",
                  "OCBC SP Equity",
                  "ITUB4 BZ Equity",
                  "KBC BB Equity",
                  "SEBA SS Equity",
                  "601229 CH Equity",
                  "316140 KS Equity",
                  "600919 CH Equity",
                  "DNB NO Equity",
                  "BBAS3 BZ Equity",
                  "UOB SP Equity",
                  "EBS AV Equity",
                  "BBDC4 BZ Equity",
                  "024110 KS Equity",
                  "SWEDA SS Equity",
                  "QNBK QD Equity",
                  "2016 HK Equity",
                  "SAB SM Equity",
                  "BKIA SM Equity",
                  "FAB UH Equity",
                  "NA CN Equity",
                  "VTBR RM Equity",
                  "2891 TT Equity",
                  "601009 CH Equity",
                  "002142 CH Equity",
                  "MAY MK Equity",
                  "EMIRATES UH Equity",
                  "BAMI IM Equity",
                  "FITB US Equity",
                  "CFG US Equity",
                  "RBI AV Equity",
                  "7186 JP Equity",
                  "7167 JP Equity",
                  "KEY US Equity",
                  "LUMI IT Equity",
                  "3618 HK Equity",
                  "BMPS IM Equity",
                  "600926 CH Equity",
                  "NCB AB Equity",
                  "UBI IM Equity",
                  "POLI IT Equity",
                  "8331 JP Equity",
                  "CIMB MK Equity",
                  "RF US Equity",
                  "5880 TT Equity",
                  "MTB US Equity",
                  "FRC US Equity",
                  "2886 TT Equity",
                  "8377 JP Equity",
                  "HBAN US Equity",
                  "ADCB UH Equity",
                  "8355 JP Equity")


# Variables of interest ---------------------------------------------------

bank_variables <- c("IS_PROV_FOR_LOAN_LOSS",
                    "BS_TOT_LOAN",
                    "BS_LOAN_MTG",
                    "BS_RSRV_LOAN_LOSS",
                    "IS_OPER_INC",
                    "EARN_FOR_COMMON",
                    "BS_CUSTOMER_DEPOSITS",
                    "BS_LEV_RATIO_TO_TANG_CAP",
                    "BS_TIER1_COM_EQUITY_RATIO",
                    "BS_TIER1_CAP_RATIO",
                    "BS_TOT_CAP_TO_RISK_BASE_CAP",
                    "BS_RISK_WEIGHTED_ASSETS")

# Other parameters --------------------------------------------------------

currency <- "USD"


# Queries -----------------------------------------------------------------
bank_info = list()
bank_info[["G-SIBs"]] <- bdp(securities = GSIBs,
                              fields = c("SHORT_NAME",
                                         "CNTRY_OF_RISK",
                                         "ACCOUNTING_STANDARD"))
bank_info[["sample_banks"]] <- bdp(securities = sample_banks,
                                    fields = c("SHORT_NAME",
                                               "CNTRY_OF_RISK",
                                               "ACCOUNTING_STANDARD"))
bank_info <- lapply(bank_info, function(x) cbind(x, Bank = rownames(x)))

# Get bank data --------------------------------------------------------------
opt <- c("currency" = "USD")
BankData <- bdh(securities = sample_banks,
                fields = bank_variables,
                start.date = as.Date("2017-12-01"),
                options = opt)

# Custom function to normalise the definition of quarter ------------------

# This function is necessary to ensure that bank quarters are comparable, since some banks have fiscal years ending in odd-months (notably Canadadian banks)

DateQuarter <- function(date_to_convert) {
  calendar_year <- ifelse(month(date_to_convert) != 1, year(date_to_convert), year(date_to_convert) - 1)
  calendar_quarter <- ifelse(month(date_to_convert) %in% c(3, 4), "Q1",
                             ifelse(month(date_to_convert) %in% c(6, 7), "Q2",
                                    ifelse(month(date_to_convert) %in% c(9, 10), "Q3", "Q4")))
  datequarter <- paste(calendar_year, calendar_quarter, sep = "")
  return(datequarter)
}

# Adds the quarter denomination to each bank's data and melts it-----------------------

BankData <- lapply(BankData, function(x) cbind(x, date_quarter = DateQuarter(x$date)))
for (i in names(BankData)) BankData[[i]] <- cbind(BankData[[i]], Bank = i, GSIB = toupper(i) %in% toupper(GSIBs))
BankData <- lapply(BankData, melt)
BankData <- Reduce(rbind, BankData)

BankData <- BankData %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  select(-date) %>% 
  mutate(qLLP_to_total_loans = IS_PROV_FOR_LOAN_LOSS / BS_TOT_LOAN) %>% 
  mutate(Reserves_to_total_loans = BS_RSRV_LOAN_LOSS / BS_TOT_LOAN)
BankData <- left_join(BankData, bank_info[["sample_banks"]], by = "Bank")

# BankData$LLP_to_total_loans <- BankData$IS_PROV_FOR_LOAN_LOSS / BankData$BS_TOT_LOAN
# BankData$Reserves_to_total_loans <- BankData$BS_RSRV_LOAN_LOSS / BankData$BS_TOT_LOAN


# Simplifies some variables -----------------------------------------------


BankData <- BankData %>% 
  mutate(Bank = toupper(Bank)) %>% 
  mutate(CNTRY_OF_RISK = ifelse(CNTRY_OF_RISK %in% c("SE",
                                                     "ES",
                                                     "DE",
                                                     "NL",
                                                     "IT",
                                                     "FR",
                                                     "FI",
                                                     "BE",
                                                     "AT",
                                                     "DK"),
                                "EU", 
                                CNTRY_OF_RISK))


BankData <- BankData %>% mutate(ACCOUNTING_STANDARD = ifelse(ACCOUNTING_STANDARD %in% c("IAS/IFRS", "US GAAP"),
                                                             ACCOUNTING_STANDARD, "National GAAP"))



# slide 3: total loan loss reserves as % of total loans, by jurisd --------

slide_3 <- BankData %>% 
  filter(date_quarter %in% c("2019Q4",
                            "2020Q1",
                            "2020Q2") &
           GSIB == TRUE) %>% 
  select(CNTRY_OF_RISK, SHORT_NAME, date_quarter, Reserves_to_total_loans, qLLP_to_total_loans) %>% 
  mutate(Reserves_to_total_loans = 100 * Reserves_to_total_loans) %>% 
  arrange(CNTRY_OF_RISK, Bank, SHORT_NAME, date_quarter)

write.csv(slide_3, "slide 03.csv")

slide_3_jurisdiction_averages <- rbind(
  cbind(date_quarter = "2019Q4", slide_3 %>% filter(date_quarter == "2019Q4") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))),
  cbind(date_quarter = "2020Q1", slide_3 %>% filter(date_quarter == "2020Q1") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))),
  cbind(date_quarter = "2020Q2", slide_3 %>% filter(date_quarter == "2020Q2") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2)))) %>% 
  spread(key = CNTRY_OF_RISK, value = mean_qLLP_loans)

write.csv(slide_3_jurisdiction_averages, "slide 03 jur avg.csv")

KWtest_X_2019 <- slide_3 %>% filter(CNTRY_OF_RISK != "JP" & substr(date_quarter, 1, 4) == "2019") %>% select(CNTRY_OF_RISK, qLLP_to_total_loans) %>% group_split(CNTRY_OF_RISK)
KWtest_X_2020 <- slide_3 %>% filter(CNTRY_OF_RISK != "JP" & substr(date_quarter, 1, 4) == "2020") %>% select(CNTRY_OF_RISK, qLLP_to_total_loans) %>% group_split(CNTRY_OF_RISK)
kruskal_preparation <- function(X) {
  names_X <- NULL
  for(i in 1:length(X)) names_X[i] <- X[[i]]$CNTRY_OF_RISK[1]
  names(X) <- names_X; rm(names_X)
  X <- lapply(X, function(x) x <- x$qLLP_to_total_loans)
  X
}

kw2019 <- kruskal.test(kruskal_preparation(KWtest_X_2019))
kw2020 <- kruskal.test(kruskal_preparation(KWtest_X_2020))

ifelse(kw2019$p.value < 0.05, "Quarterly provision for G-SIBS in 2019 was statistically different between jurisdictions.", "Quarterly provision for G-SIBS in 2019 was NOT statistically different between jurisdictions.")
ifelse(kw2020$p.value < 0.05, "Quarterly provision for G-SIBS in 2020 was statistically different between jurisdictions.", "Quarterly provision for G-SIBS in 2020 was NOT statistically different between jurisdictions.")

# the code below reproduces the histogram on slide 3, but augmented to include all banks in the sample
slide_3_aug <- BankData %>% 
  filter(date_quarter %in% c("2019Q4",
                            "2020Q1",
                            "2020Q2")) %>% 
  select(date_quarter, Bank, SHORT_NAME, GSIB, CNTRY_OF_RISK, Reserves_to_total_loans, qLLP_to_total_loans) %>% 
  mutate(Reserves_to_total_loans = 100 * Reserves_to_total_loans) %>% 
  arrange(CNTRY_OF_RISK, Bank, SHORT_NAME, date_quarter)

slide_3_aug %>% filter(date_quarter == "2019Q4") %>% group_by(CNTRY_OF_RISK) %>% summarise(round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))
slide_3_aug %>% filter(date_quarter == "2020Q1") %>% group_by(CNTRY_OF_RISK) %>% summarise(round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))
slide_3_aug %>% filter(date_quarter == "2020Q2") %>% group_by(CNTRY_OF_RISK) %>% summarise(round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))

slide_3_aug_jurisdiction_averages <- rbind(
  cbind(date_quarter = "2019Q4", slide_3_aug %>% filter(date_quarter == "2019Q4") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))),
  cbind(date_quarter = "2020Q1", slide_3_aug %>% filter(date_quarter == "2020Q1") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2))),
  cbind(date_quarter = "2020Q2", slide_3_aug %>% filter(date_quarter == "2020Q2") %>% group_by(CNTRY_OF_RISK) %>% summarise(mean_qLLP_loans = round(mean(100 * qLLP_to_total_loans, na.rm = TRUE), 2)))) %>% 
  spread(key = CNTRY_OF_RISK, value = mean_qLLP_loans)

# slide 4: dispersion in provisioning practices ---------------------------

slide_4_plot <- BankData %>%
  filter(substr(date_quarter, 1, 4) %in% c("2019", "2020")) %>% 
  ggplot(aes(x = 100 * qLLP_to_total_loans, color = substr(date_quarter, 1, 4), group = date_quarter)) +
  geom_density(size = 1.5, alpha = 0.8, position = "identity", aes(linetype = substr(date_quarter, 5, 6)))+
  theme(panel.background = element_rect(fill = "#d3d6d4"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#f2f2f2"),
        strip.text.y = element_blank()) +
  scale_color_manual(values = c("#C28191", "#6CADE1", "#FFEC72")) +
  scale_linetype_manual(values = c("solid", "longdash", "dashed", "dotted")) +
  labs(y = "Density",
       x = "Loan loss provisions as % of total loans",
       color = "Year",
       linetype = "Quarter")
ggsave("slide 04.png", slide_4_plot)

# slide 5: LLPs and earnings ----------------------------------------------

slide_5 <- BankData %>% 
            filter(date_quarter %in% c("2019Q4", "2020Q1", "2020Q2")) %>% 
            select(date_quarter, Bank, GSIB, ACCOUNTING_STANDARD, CNTRY_OF_RISK, IS_PROV_FOR_LOAN_LOSS, IS_OPER_INC) 

slide_5_plot <- slide_5 %>% 
  ggplot(aes(x = log(IS_OPER_INC),
             y = log(IS_PROV_FOR_LOAN_LOSS),
             group = date_quarter,
             color = date_quarter,
             shape = GSIB)) +
  geom_point() +
  theme(panel.background = element_rect(fill = "#d3d6d4"),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#f2f2f2"),
        strip.text.y = element_blank()) +
  scale_color_manual(values = c("#C28191", "#6CADE1", "#FFEC72")) +
  facet_grid("date_quarter" ~ ACCOUNTING_STANDARD) +
  geom_smooth(method = "lm", fill = NA) +
  labs(x = "Operating income",
       y = "Loan loss provisions",
       caption = "All values in log.",
       color = "Quarter",
       shape = "G-SIB?")

ggsave("slide 05.png", slide_5_plot)

write.csv(slide_5, "slide 05.csv")

# slide 6: market expectations for loan loss provisions -------------------
opt <- c("EQY_FUND_CRNCY" = currency)
fiscal_quarter_override <- c("BEST_FPERIOD_OVERRIDE" = "1FQ")
LLP_forecast_2020Q3 <- bdp(securities = sample_banks,
                            fields = "IEST_FINL_PROV_FOR_LOAN_LOSS",
                            overrides = c(fiscal_quarter_override, opt))
fiscal_quarter_override <- c("BEST_FPERIOD_OVERRIDE" = "2FQ")
LLP_forecast_2020Q4 <- bdp(securities = sample_banks,
                           fields = "IEST_FINL_PROV_FOR_LOAN_LOSS",
                           overrides = c(fiscal_quarter_override, opt))
LLP_forecast_2020Q3 <- cbind(Bank = toupper(rownames(LLP_forecast_2020Q3)), LLP_forecast_2020Q3)
LLP_forecast_2020Q4 <- cbind(Bank = toupper(rownames(LLP_forecast_2020Q4)), LLP_forecast_2020Q4)

banks_with_forecast <- merge(LLP_forecast_2020Q3, 
                             LLP_forecast_2020Q4,
                             by = "Bank") %>% filter(complete.cases(.))
past_data <- BankData %>%
              filter(Bank %in% banks_with_forecast$Bank) %>%
              filter(complete.cases(.))
banks_with_past_data <- unique(past_data$Bank)
banks_with_forecast <- banks_with_forecast %>% filter(Bank %in% banks_with_past_data)
  
agg_provision <- past_data %>% 
                  group_by(date_quarter) %>% 
                  summarise(quarterly_provision = sum(IS_PROV_FOR_LOAN_LOSS))

agg_provision <- rbind(agg_provision,
                       tibble(date_quarter = c("2020Q3", "2020Q4"),
                              quarterly_provision = c(sum(banks_with_forecast$IEST_FINL_PROV_FOR_LOAN_LOSS.x),
                                                      sum(banks_with_forecast$IEST_FINL_PROV_FOR_LOAN_LOSS.y))))

paste("Balanced sample of", length(unique(past_data$Bank)), "banks, of which", sum(unique(past_data$Bank) %in% toupper(GSIBs)),
      "are G-SIBs.")

write.csv(agg_provision, "slide 06.csv")

# slide 9: household expenditure by income ------------------------------------------

expenditure <- data.table(read.csv("https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/Affinity%20-%20National%20-%20Daily.csv",
                                   stringsAsFactors = FALSE, na.strings = "."))
expenditure[, date_day := as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")]
expenditure <- expenditure %>% filter(complete.cases(.)) %>% select(date_day, spend_all_inchigh, spend_all_incmiddle, spend_all_inclow)

expenditure_by_income <- expenditure %>% 
                          gather("variable", "value", 2:4)

# In St Louis Fed's FRED, series "CCLACBW027SBOG" represents 
# "Consumer Loans: Credit Cards and Other Revolving Plans, All Commercial Bank"
# NOTE: measuerd in USD billions, seasonally adjusted

consumer_credit_card_loans <- get_fred_series("CCLACBW027SBOG", "credit_card_consumer_loans",
                                              observation_start = "2020-01-01")
consumer_credit_card_loans<- consumer_credit_card_loans %>%
                                filter(date >= "2020-01-08") %>% 
                                slice(rep(1:n(), each = 7))
consumer_credit_card_loans$date <- seq(min(consumer_credit_card_loans$date - 6), by = "day", length.out = nrow(consumer_credit_card_loans))
consumer_credit_card_loans <- data.frame(date_day = as.Date(consumer_credit_card_loans$date),
                                         variable = "credit_card_consumer_loans",
                                         value = consumer_credit_card_loans$credit_card_consumer_loans)
expenditure_consumer_loans <- expenditure_consumer_loans %>% 
                                spread(key = variable, value = value) %>% 
                                filter(date_day > "2020-01-13")
expenditure_consumer_loans <- rbind(expenditure_by_income, consumer_credit_card_loans)

write.csv(expenditure_consumer_loans, "slide 09.csv")

# slide 10: loan growth and liquidity -------------------------------------

BankData <- BankData %>% 
              group_by(Bank) %>% 
              mutate(previous_quarter_loan = lag(BS_LOAN_MTG, order_by = date_quarter)) %>% 
              mutate(previous_quarter_deposits = lag(BS_CUSTOMER_DEPOSITS, order_by = date_quarter)) 

BankData <- BankData %>% 
              mutate(loan_growth = log(BS_LOAN_MTG) - log(previous_quarter_loan)) %>% 
              mutate(deposit_growth = log(BS_CUSTOMER_DEPOSITS) - log(previous_quarter_deposits))

reg_dates = c("2019Q1", "2019Q2", "2019Q3", "2019Q4", "2020Q1", "2020Q2")

q_specific_regressions_GSIBs <- list()
for (qtr in reg_dates) {
  reg_data <- BankData %>% filter(date_quarter == qtr & GSIB == TRUE) %>% select(Bank, loan_growth, deposit_growth)
  q_specific_regressions_GSIBs[[qtr]] = lm(deposit_growth ~ loan_growth, data = reg_data)
}

q_specific_regressions_GSIBs_aug <- list()
for (qtr in reg_dates) {
  reg_data <- BankData %>% filter(date_quarter == qtr & GSIB == TRUE) %>% select(Bank, loan_growth, deposit_growth, GSIB, CNTRY_OF_RISK, qLLP_to_total_loans, Reserves_to_total_loans)
  q_specific_regressions_GSIBs_aug[[qtr]] = lm(deposit_growth ~ loan_growth + GSIB + CNTRY_OF_RISK + qLLP_to_total_loans + Reserves_to_total_loans, data = reg_data)
}


q_specific_regressions <- list()
for (qtr in reg_dates) {
  reg_data <- BankData %>% filter(date_quarter == qtr) %>% select(Bank, loan_growth, deposit_growth)
  q_specific_regressions[[qtr]] = lm(deposit_growth ~ loan_growth, data = reg_data)
}

q_specific_regressions_aug <- list()
for (qtr in reg_dates) {
  reg_data <- BankData %>% filter(date_quarter == qtr) %>% select(Bank, loan_growth, deposit_growth, GSIB, CNTRY_OF_RISK, qLLP_to_total_loans, Reserves_to_total_loans)
  q_specific_regressions_aug[[qtr]] = lm(deposit_growth ~ loan_growth + GSIB + CNTRY_OF_RISK + qLLP_to_total_loans + Reserves_to_total_loans, data = reg_data)
}


stargazer(q_specific_regressions_GSIBs, type = "text", column.labels = reg_dates, keep = c("loan_growth", "Constant"), 
          notes.label = "G-SIBs only", dep.var.labels = "Deposit growth", covariate.labels = "Loan growth",
          add.lines = list(c("Jurisdiction", rep("No", 6)),
                           c("Quarterly LLP", rep("No", 6)),
                           c("Reserves-to-loan ratio", rep("No", 6))))
stargazer(q_specific_regressions_GSIBs_aug, type = "text", column.labels = reg_dates, keep = c("loan_growth", "Constant"),
          notes.label = "G-SIBs only", dep.var.labels = "Deposit growth", covariate.labels = "Loan growth",
          add.lines = list(c("Jurisdiction", rep("Yes", 6)),
                           c("Quarterly LLP", rep("Yes", 6)),
                           c("Reserves-to-loan ratio", rep("Yes", 6))))
stargazer(q_specific_regressions, type = "text", column.labels = reg_dates, keep = c("loan_growth", "Constant"), 
          notes.label = "All sampled banks", dep.var.labels = "Deposit growth", covariate.labels = "Loan growth",
          add.lines = list(c("G-SIB dummy", rep("No", 6)),
                           c("Jurisdiction", rep("No", 6)),
                           c("Quarterly LLP", rep("No", 6)),
                           c("Reserves-to-loan ratio", rep("No", 6))))
stargazer(q_specific_regressions_aug, type = "text", column.labels = reg_dates, keep = c("loan_growth", "Constant"), 
          notes.label = "All sampled banks", dep.var.labels = "Deposit growth", covariate.labels = "Loan growth",
          add.lines = list(c("G-SIB dummy", rep("Yes", 6)),
                           c("Jurisdiction", rep("Yes", 6)),
                           c("Quarterly LLP", rep("Yes", 6)),
                           c("Reserves-to-loan ratio", rep("Yes", 6))))


# slide 11: trends in loan growth ---------------------------------------------

slide_11 <- BankData %>% 
  filter(date_quarter %in% c("2019Q4",
                             "2020Q1",
                             "2020Q2") &
           GSIB == TRUE) %>% 
  select(CNTRY_OF_RISK, SHORT_NAME, date_quarter,   
         BS_LOAN_MTG, loan_growth) %>% 
  arrange(CNTRY_OF_RISK, Bank, SHORT_NAME, date_quarter)

write.csv(slide_11, "slide 11.csv")

# slide 12: trends in capital ---------------------------------------------

slide_12 <- BankData %>% 
              filter(date_quarter %in% c("2019Q4",
                                         "2020Q1",
                                         "2020Q2") &
                       GSIB == TRUE) %>% 
              select(CNTRY_OF_RISK, SHORT_NAME, date_quarter, 
                     BS_LEV_RATIO_TO_TANG_CAP, BS_TIER1_COM_EQUITY_RATIO, BS_TIER1_CAP_RATIO, BS_TOT_CAP_TO_RISK_BASE_CAP) %>% 
              arrange(CNTRY_OF_RISK, Bank, SHORT_NAME, date_quarter)

write.csv(slide_12, "slide 12.csv")
