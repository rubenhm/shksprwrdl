# master_mind

calculate_colors <- function(try_word, true_word) {

  # Verify inputs
  # browser()
  # number of characters
  if (nchar(try_word) != nchar(true_word)) {
    stop('Input word is not the right number of characters')
  }

  # Split characters into vector
  v_try <- unlist(strsplit(try_word, split = ''))
  v_tru <- unlist(strsplit(true_word, split = ''))

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
  score[yellow] <- 'yellow'
  score[gray]   <- 'gray'

  names(score) <- v_try

  return(score)
}
