# master_mind.R

#' score_row
#'
#' Calculates score for a given guess word
#'
#' @param v_try Vectorized version of the guess word
#' @param v_tru Vectorized version of the correct word
#' @return A character vector of scores with the colors
score_row <- function(v_try, v_tru) {

  # Scoring code from
  # https://github.com/coolbutuseless/wordle/blob/main/R/wordle-game.R
  exact   <- 2L * (v_try == v_tru)
  inexact <- which(v_try != v_tru)

  for (i in inexact) {
    for (j in inexact) {
      wrong_spot = v_try[i] == v_tru[j]
      if (wrong_spot) {
        exact[i] <- 1L
        inexact <- inexact[inexact != j]
        break
      }
    }
  }

  score <- rep('', length(v_try))
  score[exact == 2]  <- '#689111' # green
  score[exact == 1]  <- '#E3AE1E' # yellow
  score[exact == 0]  <- '#615F5B' # gray

  return(score)
}

#' score_df
#'
#' Calculates scores for the attempts data frame
#'
#' @param dt The attempts data frame
#' @param true_word The word to be guessed
#' @return A data frame of characters with scores (colors)
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
