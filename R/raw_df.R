#' Raw data frame obtained from the RFID / PIT tag readers
#'
#' A dataset containing the putput with required formatted columns
#' "id", "time", "date", "antenna".
#'
#' @format A data frame with 808 rows and yy variables:
#' \describe{
#'   \item{id}{RFID /PIT tag id of individuals}
#'   \item{date}{date of read}
#'   \item{time}{time of read}
#'   \item{antenna}{antenna number}
#'   \item{Antenna.number}{time of read}
#'   \item{Event}{time of read}
#'   \item{GPS.coordinates}{time of read}
#'   \item{Identifier}{time of read}
#'   \item{Input.status}{time of read}
#'   \item{Output.status}{time of read}
#'   \item{Transponder.type}{time of read}
#'   \item{Weight.x}{time of read}
#'   \item{Date}{Date from the reader directly}
#'   \item{SL}{Standard length of the fish}
#'   \item{Weight.y}{Weight of the fish}
#'   \item{Plates}{Plate category: Completely plated (T), Semi-plated (S) and Low plaetd (L)}
#'   \item{Pond}{Pond number in which fish are placed}
#'   \item{Treatment_seq}{Sequence of treatements the fish were subjected to}
#'   \item{Replicate}{pond replicate}
#'   \item{Remarks}{Remarks made by the tagger}
#'   \item{Tagger}{Person who tagged}
#'
#'   ...
#' }
#'
"raw_df"
