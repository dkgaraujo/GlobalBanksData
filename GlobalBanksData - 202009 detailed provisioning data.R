
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
library(purrr)
library(dplyr)
library(tidyr)
library(writexl)
con <- blpConnect()

# Load the bank tickers ---------------------------------------------------

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




# Gets the detailed provision data by bank --------------------------------

detailed_provision_data <- tibble(banks = sample_banks) %>% 
  mutate(LoanLossProvision_byProduct = banks %>% map(~ bds(security = .x,
                                                           field = "PG_PROV_LOAN_LOSS",
                                                           overrides = c(EQY_FUND_CRNCY = "USD",
                                                                         PRODUCT_GEO_OVERRIDE = "P"))),
         LoanLossProvision_byGeography = banks %>% map(~ bds(security = .x,
                                                           field = "PG_PROV_LOAN_LOSS",
                                                           overrides = c(EQY_FUND_CRNCY = "USD",
                                                                         PRODUCT_GEO_OVERRIDE = "G"))))


detailed_provision_by_product <- detailed_provision_data %>% 
  unnest(LoanLossProvision_byProduct)

detailed_provision_by_geography <- detailed_provision_data %>% 
  unnest(LoanLossProvision_byGeography)


# Prints out the results in Excel files -----------------------------------

write_xlsx(detailed_provision_by_product, "detailed_provision_by_product.xlsx")
write_xlsx(detailed_provision_by_geography, "detailed_provision_by_geography.xlsx")
