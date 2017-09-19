
LoadMstrFile <- function(fname, fpath) {
  require(stringr)
  require(readxl)

  len_fpath <- str_length(fpath)
  fpath_final_char <- str_sub(fpath, len_fpath - 1, len_fpath)
  if (!fpath_final_char == "/") {
    fpath <- str_c(fpath, "/")
  }

  full_file_path <- str_c(fpath, fname)

  mstr_data <- read_xlsx(full_file_path)

  return(mstr_data)
}

JoinEMRToDataset <- function(df, dept_number_col_name) {
  require(readxl,
          dplyr)
  emr_lookup_df <- read_xlsx("./Src/TableEMRDept.xlsx",
                             skip = 1) %>%
    select(`Dept Number`, EMROrg)
  names(emr_lookup_df) <- c(dept_number_col_name,
                            "EMROrg")
  df <- left_join(df, emr_lookup_df)

  return(df)
}

AdjustSalaries <- function(df,
                           sal_col_name         = "FY FTE SALARY w/LONGEVITY",
                           med_ratio_col_name   = "LONG SALARY/MARKET",
                           med_market_col_name  = "MARKET COMP",
                           ignore.aux.res.funds = TRUE,
                           ratio_threshold      = .75,
                           total_expend         = 500000) {

  n_ee_below_thresh <- sum(df[, med_ratio_col_name] < ratio_threshold)
  ee_rows_below_thresh <- df[,sal_col_name] < ratio_threshold

  # equal 'flat' distribution of funds to all those below
  # the threshold
  flat_adjust_amnt <- total_expend / n_ee_below_thresh

  # set the adjust_amnt depending on the raise distribution scheme
  raise_amnt <- flat_adjust_amnt

  # add the raise and recompute ratio
  df$raise_amnt[!ee_rows_below_thresh] <- 0
  df$raise_amnt[ee_rows_below_thresh] <- raise_amnt

  df$new_salary <- df$raise_amnt + df[, sal_col_name]
  df$new_ratio <- df$new_salary / df[, med_market_col_name]

  return(df)
}


BuildHRBPDataset <- function(df) {
  require(dplyr)
  df_orig <- df
  df <- select(df,
               `LONG SALARY/MARKET`,
               EVAL_2017,
               PIDM,
               LAST_NAME,
               FIRST_NAME,
               JOB_TITLE,
               POSITION_TITLE,
               POSN,
               SUFF,
               MUS_CONTRACT,
               `PEABARG Union`,
               `Status Desc`,
               CURRENT_HIRE_DATE,
               JOB_START_DATE,
               DEPT_NAME,
               COLLEGE,
               DIVISION,
               JCAT,
               JCAT_DESC,
               JCAT_SOC,
               NAT_JCAT_SOC_OCC_TITLE,
               JCAT_CUPA,
               JCAT_CUPA_TITLE)

  return(df)
}





