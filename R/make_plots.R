BuildMAPlots <- function(df) {
  # First create a horizontal box plot containing comp ratios for each position
  # title with at least 10 people in it.
  #
  unique_position_cnts <- group_by(df, Position_Title_Cln) %>%
    summarize(n = n(), med = median(comp_ratio)) %>%
    arrange(med)

  positions_n_10 <- filter(unique_position_cnts, n >= 10)
  positions_list_10 <- positions_n_10$Position_Title_Cln

  df$Position_Title <- factor(df$PositionTitle,
                              levels = unique_position_cnts$Position_Title_Cln)

  boxp_data <- filter(df,
                      Position_Title %in% positions_list_10)

  position_boxp <- ggplot(boxp_data,
                          aes(x = Position_Title_Cln, y = comp_ratio)) +
    geom_boxplot() +
    coord_flip()
  return(position_boxp)
}
