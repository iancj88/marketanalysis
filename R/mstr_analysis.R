
LoadMstrFile <- function(filename, filepath) {
  require(stringr)
  len_fpath <-str_length(filepath)
  fpath_final_char <- str_sub(filepath, len_fpath - 1, len_fpath)
  if (!fpath_final_char == "/") {
    filepath <- str_c(filepath, "/")
  }

  full_file_path <- str_c(filepath, filename)

  mstr_data <- read.xlsx(full_file_path)

  return(mstr_data)
}

