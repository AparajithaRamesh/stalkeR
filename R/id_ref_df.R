#' id_ref frame obtained from the RFID / PIT tag readers
#'
#' A dataset containing the putput with required formatted columns
#' "id", "time", "date", "antenna".
#'
#' @format A data frame with xxx rows and yy variables:
#' \describe{
#'   \item{id}{RFID /PIT tag id of individuals}
#'   \item{Date}{date of read}
#'   \item{SL}{time of read}
#'   \item{Weight}{antenna number}
#'   \item{Plates}{Plates}
#'   \item{Tagger}{Jakob}
#'   \item{Remarks}{Remarks while tagging}
#'   \item{Pond}{pond_id}
#'   \item{Treatment_seq}{treatment sequence}
#'   \item{Replicate}{replicate number}
#' }
#'
"id_ref_df"
