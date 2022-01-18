# master_mind

score_row <- function(v_try, v_tru) {

  # green
  green <- which(v_try == v_tru)

  # yellow
  yellow <- which(v_try %in% v_tru)

  # re-asses yellow
  yellow <- dplyr::setdiff(yellow, green)

  # gray
  gray <- dplyr::setdiff(c(1:length(v_try)),
                         dplyr::union(green, yellow))

  score <- rep('', length(v_try))
  score[green]  <- 'green'
  score[yellow] <- '#fada5e'
  score[gray]   <- 'gray'

  #names(score) <- v_try

  return(score)
}


score_df <- function(dt, true_word) {

  #browser()
  # Split true word into vector
  v_tru <- unlist(strsplit(true_word, split = '')) %>% toupper()

  # Determine dimensions
  n_cols <- length(dt)
  n_rows <- NROW(dt)

  # Initialize scores
  df_res <- matrix(rep('gray', n_cols*n_rows), nrow = n_rows) %>%
    tibble::as_tibble(.name_repair = 'unique')


  # Score one row at a time and update df_res
  for (i in c(1:n_rows)) {

    # Current row
    v_try <- unlist(dt[i,])

    # score row
    r_score <- score_row(v_try, v_tru)

    # update v_res
    df_res[i, ] <- as.list(r_score)

  }

  return(df_res)
}
