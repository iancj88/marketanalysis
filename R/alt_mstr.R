pacman::p_load(readxl,
               stringr)

#############################################################################
# Constants
#
path_to_files <- "C:/Users/d66x816/Documents/marketanalysis/Src/"

low_ratio_threshold <- .6
hgh_ratio_threshold <- 1.5
#############################################################################


# Set the path to the Source Files here
path_to_files <- "C:/Users/d66x816/Documents/marketanalysis/Src/"

# This file contains the ~40 updated JCAT, SOC and CUPA codes
# after the HR-BP analysis on 9/15/17
hrbp_updated_file <- read_xlsx(str_c(path_to_files,
                                     "Copy of HR BP Data working version-4.xlsx"))

# The common PIDM + POSN key to link the updated file to the
# original file containing the master dataset
hrbp_updated_file$KEY <- str_c(hrbp_updated_file$PIDM,
                               hrbp_updated_file$POSN)

# The master dataset
mstr_data <- read_xlsx(str_c(path_to_files,
                             "HR BP Data v4 20170918.xlsx"))

# The mstr dataset with the most up-to-date JCAT, SOC, and CUPA
# in addition to all the info necessary to pull new market comp
# data for analysis
mstr_data$KEY <- str_c(mstr_data$ID, mstr_data$POSN)
mstr_data_updt <- select(mstr_data,
                         PIDM,
                         ID,
                         POSN,
                         KEY,
                         CATEGORY,
                         `FY FTE SALARY w/LONGEVITY`,
                         `SEARCH REGION GENERAL`,
                         `SEARCH REGION SPECIFIC`,
                         JCAT_UPDT,
                         SOC_UPDT,
                         CUPA_UPDT)


# The BLS and CUPA market comparator data
soc_data_list <- LoadSOCMrkt()
soc_nat <- soc_data_list[[1]]
soc_ste <- soc_data_list[[2]]
soc_msa <- soc_data_list[[3]]

cupa_data <- LoadCUPAMrkt()

# Rename the columns to explicitly specify the scope of the data. Keep the first
# column name common to allow the datasets to be joined as necessary. OCC_CODE
# is another name for SOC Code.
names(soc_nat) <- str_c(names(soc_nat), "_BLS_NATIONAL")
names(soc_nat)[1] <- "OCC_CODE"
names(soc_ste) <- str_c(names(soc_ste), "_BLS_STATE")
names(soc_ste)[1] <- "OCC_CODE"
names(soc_msa) <- str_c(names(soc_msa), "_BLS_REGIONAL")
names(soc_msa)[1] <- "OCC_CODE"
names(cupa_data) <- str_c(names(cupa_data), "_CUPA")
names(cupa_data)[1] <- "CUPA_CODE"

# Pull the specific data needed for analysis. In this case, the Annual Median
# values are pulled. the *_join dataframes here are used for joins the the
# master update file. Regional median data is only supplied in hourly figures,
# so the median annual is computed by multiplying the hourly rate by 2080.
soc_nat_join <- select(soc_nat,
                       OCC_CODE,
                       A_MEDIAN_BLS_NATIONAL)
soc_ste_join <- select(soc_ste,
                       OCC_CODE,
                       A_MEDIAN_BLS_STATE)
soc_msa_join <- select(soc_msa,
                       OCC_CODE,
                       `Median Hourly_BLS_REGIONAL`)

soc_msa_join$A_MEDIAN_BLS_REGIONAL <-
  soc_msa_join$`Median Hourly_BLS_REGIONAL` * 2080
soc_msa_join <- select(soc_msa_join,
                       -`Median Hourly_BLS_REGIONAL`)
soc_msa_join$A_MEDIAN_BLS_REGIONAL[soc_msa_join$A_MEDIAN_BLS_REGIONAL < 0] <- NA

cupa_join <- select(cupa_data, CUPA_CODE, Group.Median_CUPA)

mstr_data_updt <- left_join(x = mstr_data_updt,
                            y = soc_nat_join,
                            by = c("SOC_UPDT" = "OCC_CODE")) %>%
  left_join(y = soc_ste_join,
            by = c("SOC_UPDT" = "OCC_CODE")) %>%
  left_join(y = soc_msa_join,
            by = c("SOC_UPDT" = "OCC_CODE")) %>%
  left_join(y = cupa_join,
            by = c("CUPA_UPDT" = "CUPA_CODE"))
rm(soc_nat_join, soc_ste_join, soc_msa_join, cupa_join)
# This section applies the 'search region' or specified market comparator to
# each row. For example, if a job is specified as 'Local', then the local BLS -
# MSA salary data is used for the 'market comp' column for that row. The markets
# specified for this analysis include, 'Local', 'State', 'National' (all BLS
# sourced data), and 'National University' (CUPA sourced). If data does not
# exist for a code for a specified market, the next more general market is used.
# I.e. 'State' is used if 'Local' is missing, 'National' is used if 'State' is
# missing. `SEARCH REGION GENERAL` specifies the more general market data used.
region1 <- mstr_data_updt[,"SEARCH REGION SPECIFIC"]

mstr_data_updt$market_comp[region1 == "National"] <-
  mstr_data_updt$A_MEDIAN_BLS_NATIONAL[region1 == "National"]

mstr_data_updt$market_comp[region1 == "State"] <-
  mstr_data_updt$A_MEDIAN_BLS_STATE[region1 == "State"]

mstr_data_updt$market_comp[region1 == "Local"] <-
  mstr_data_updt$A_MEDIAN_BLS_REGIONAL[region1 == "Local"]

mstr_data_updt$market_comp[region1 == "National University"] <-
  mstr_data_updt$Group.Median_CUPA[region1 == "National University"]

reg1_missing <- filter(mstr_data_updt, is.na(market_comp))
reg1_not_missing <- filter(mstr_data_updt, !is.na(market_comp))

region2 <- reg1_missing[,"SEARCH REGION GENERAL"]

reg1_missing$market_comp[region2 == "National"] <-
  reg1_missing$A_MEDIAN_BLS_NATIONAL[region2 == "National"]
reg1_missing$market_comp[region2 == "State"] <-
  reg1_missing$A_MEDIAN_BLS_STATE[region2 == "State"]

still_missing <- is.na(reg1_missing$market_comp)
reg1_missing$`SEARCH REGION GENERAL`[still_missing] <- "National"
reg1_missing$market_comp[still_missing] <-
  reg1_missing$A_MEDIAN_BLS_NATIONAL[still_missing]

mstr_data_updt <- bind_rows(reg1_missing, reg1_not_missing)
rm(region1, region2, reg1_missing, reg1_not_missing, still_missing)

# Clean the dataset for the '#' character used by BLS to specify that a salary
# is greater than the recorded range.
mstr_data_updt$market_comp[mstr_data_updt$market_comp == "#"] <-
  mstr_data_updt$Group.Median_CUPA[mstr_data_updt$market_comp == "#"]

mstr_data_updt$market_comp <- (as.numeric(mstr_data_updt$market_comp))

mstr_data_updt$comp_ratio <-
  mstr_data_updt$`FY FTE SALARY w/LONGEVITY` / mstr_data_updt$market_comp

mstr_emp_info <- select(mstr_data,   # Employee specific info
                        KEY,
                        LAST_NAME,
                        FIRST_NAME,
                        NAME,
                        PIDM,
                        ID,
                        GENDER,
                        BIRTH_DATE,
                        FIRST_HIRE_DATE,
                        CURRENT_HIRE_DATE,
                        LONG_DATE,
                        LONG_EFF_DATE,
                        LONG_YRS,
                        HOME_DEPT,
                        DEPT_NAME,
                        EVAL_2015,
                        EVAL_2016,
                        EVAL_2017)

mstr_job_info <- select(mstr_data, # Job specific info
                        KEY,
                        HOME_DEPT,
                        DEPT_NAME,
                        COLLEGE,
                        DIVISION,
                        JOB_START_DATE,
                        JOB_END_DATE,sun
                        CATEGORY,
                        JOB_STATUS,
                        POSN,
                        SUFF,
                        JOB_TITLE,
                        POSITION_TITLE,
                        FTE,
                        MONTHS,
                        MONTHLY_HRS,
                        MUS_CONTRACT,
                        HOURLY_RATE,
                        MONTHLY_RATE,
                        SALARY,
                        `FTE SALARY`,
                        `FY FTE SALARY`,
                        `FY FTE SALARY w/LONGEVITY`,
                        ENTRY_HIGH,
                        ENTRY_LOW)

mstr_fnd_info <- select(mstr_data, # Job Fund specific info
                             KEY,
                             TOTAL,
                             INST,
                             RES,
                             AES,
                             ES,
                             AUX,
                             SUPP,
                             OTHER,
                             CURRENT_UNRESTRICTED,
                             RESTRICTED,
                             DESIGNATED,
                             ES_FSTS,
                             MAES,
                             AUX_B,
                             TOTAL_B,
                             `PROPORTION of BASE AVAILABLE FOR ADJUSTMENT`)

# compare the updated file to the old file for a data-integrity check
old_fail_bool <-
  mstr_data$`LONG SALARY/MARKET` >= hgh_ratio_threshold |
  mstr_data$`LONG SALARY/MARKET` <= low_ratio_threshold
old_fail <- mstr_data[old_fail_bool, ]

new_fail_bool <-
  mstr_data_updt$comp_ratio >= hgh_ratio_threshold |
  mstr_data_updt$comp_ratio <= low_ratio_threshold
new_fail <- mstr_data_updt[new_fail_bool, ]

mstr_data_analysis <- group_by(mstr_data_updt, POSITION_TITLE)
View(mstr_data_updt)

