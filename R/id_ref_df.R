#' id_ref frame obtained from the RFID / PIT tag readers
#'
#' A data set containing the output with required formatted columns
#' "id", "time", "date", "antenna".
#'
#' @format A data frame with 200 rows (individual id and attributes) and 10 columns (variables):
#' \describe{
#'   \item{id}{RFID /PIT tag id of individuals}
#'   \item{Date}{Date of reads}
#'   \item{SL}{Time of reads}
#'   \item{Weight}{Antenna number where individual was detected}
#'   \item{Plates}{The platedness of sticklebacks - classified into totally (T), semi (S), low (L)}
#'   \item{Tagger}{The person who tagged (Jakob)}
#'   \item{Remarks}{Remarks}
#'   \item{Pond}{Pond number (experimental block) of the experiment}
#'   \item{Treatment_seq}{Treatment sequence}
#'   \item{Replicate}{Replicate number}
#' }
#'
"id_ref_df"
