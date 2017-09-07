

CompilePerformanceEvals <- function(path_to_file, file_name) {
  require(readxl,
          stringr)

  if(!str_sub(path_to_file,
             str_length(path_to_file) - 1,
             str_length(path_to_file)) == "/" ) {
    path_to_file <- str_c(path_to_file, "/")
  }

  full_path <- str_c(path_to_file, file_name)

  eval_data <- read_xlsx(path = full_path,
                         sheet = 1)

  return(eval_data)
}

CleanEvalData <- function(eval_data) {

  # remove rows with no performance evals

}


AddEvalScore <- function(eval_data) {
  eval_data_1 <- !is.na(eval_data$eval1)
  eval_data_2 <- !is.na(eval_data$eval2)
  eval_data_3 <- !is.na(eval_data$eval3)
  eval_data_4 <- !is.na(eval_data$eval4)
  eval_data_5 <- !is.na(eval_data$eval5)

  eval_data$eval_score[eval_data_1] <- 1
  eval_data$eval_score[eval_data_2] <- 2
  eval_data$eval_score[eval_data_3] <- 3
  eval_data$eval_score[eval_data_4] <- 4
  eval_data$eval_score[eval_data_5] <- 5

  return(eval_data)
}


eval_data <- read_xlsx(path = "./Src/Performance_eval_mstr.xlsx",
                       sheet = 2)

eval_data_sprd <- select(eval_data, `Key - GIDPosnSuffix`, eval_score, FY) %>%
  spread(key = FY, value = eval_score)
