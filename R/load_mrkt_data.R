LoadSOCMrkt <- function(path_to_files  = NULL) {
  require(readxl)
  require(stringr)

  if (is.null(path_to_files)) {
    path_to_files <- "C:/Users/d66x816/Documents/marketanalysis/Src/mrkt_data/"
  }

  fpath_bls_nat <- str_c(path_to_files,
                         "BLS_national_M2016_dl.xlsx")
  fpath_bls_ste <- str_c(path_to_files,
                         "BLS_state_M2016_dl.xlsx")
  fpath_bls_msa <- str_c(path_to_files,
                         "BLS_MSA_M2016_dl_USE.xlsx")

  bls_nat <- read_xlsx(path = fpath_bls_nat,
                      sheet = 1,
                      stringsas)
  bls_ste <- read_xlsx(path = fpath_bls_ste,
                       sheet = 1)
  bls_msa <- read_xlsx(path = fpath_bls_msa,
                       sheet = 1)

  bls_ste <- filter(bls_ste, STATE == "Montana") %>%
    select(-AREA, -ST, -STATE)
  bls_ste$SCOPE <- "State"

  bls_nat$SCOPE <- "National"

  bls_msa$SCOPE <- "Regional"

  bls_all <- list(bls_nat, bls_ste, bls_msa)

  return(bls_all)
}

LoadCUPAMrkt <- function(path_to_files  = NULL) {
  if (is.null(path_to_files)) {
    path_to_files <- "C:/Users/d66x816/Documents/marketanalysis/Src/mrkt_data/"
  }

  require(stringr)
  require(dplyr)

  fpath_admin <- str_c(path_to_files, "AdminCUPA.csv")
  fpath_profe <- str_c(path_to_files, "ProfessionalCUPA.csv")
  fpath_staff <- str_c(path_to_files, "StaffCUPA.csv")


  cupa_admn <- read.csv(file = fpath_admin)
  cupa_stff <- read.csv(file = fpath_staff)
  cupa_prof <- read.csv(file = fpath_profe)
  cupa_mrkt <- bind_rows(cupa_admn, cupa_prof, cupa_stff)

  return(cupa_mrkt)
}
