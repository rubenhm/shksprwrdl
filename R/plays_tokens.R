#' Tokens from Shakespeare's plays
#'
#' A data frame containing individual words
#' from Shakespeare' plays along with their
#' frequency and character count.
#' We only include words with frequency n such that
#' n >= 75th percentile, n <= 99.95th percentile.
#' Number of observations: 3,180 tokens.
#' @format A data frame with the following variables:
#' \describe{
#' \item{word}{Individual token, may include apostrophes and proper names.}
#' \item{n}{This is the word frequency across all 37 plays.}
#' \item{nchar}{Number of characters in token.}
#' }
#' @source \url{http://www.ibiblio.org/xml/examples/shakespeare/}
"plays_tokens"
