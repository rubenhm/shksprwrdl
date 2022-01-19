#' Complete text of Shakespeare's 37 plays in tidy format
#'
#' A data frame containing complete text of
#' Shakespeare' plays. Text parsed from xml files from
#' http://www.ibiblio.org/xml/examples/shakespeare/
#' Number of observations: 106,950 lines.
#' @format A data frame with the following variables:
#' \describe{
#' \item{play}{The name of the play}
#' \item{act}{Act title}
#' \item{scene}{Scene title}
#' \item{speaker}{Name of speaker}
#' \item{line}{Text of spoken line}
#' }
#' @source \url{http://www.ibiblio.org/xml/examples/shakespeare/}
"plays_text"
