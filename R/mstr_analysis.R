
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

