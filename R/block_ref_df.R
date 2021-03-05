#' block_ref_df frame obtained from the RFID / PIT tag readers
#'
#'  This dataframe is the working unit of this package.
#'  It uses the block_ref_df and raw_df objects as inputs, and outputs a cleaner version of raw_df named block_df. The function does the following actions:
#'  It keeps the columns of interest (i.e. id, antenna and time).
#'  The time format is changed to Posixt format.
#'  It keeps one read per individual at a certain antenna per second (i.e. remove repeats).
#'  It removes ghost reads test reads, or any read that do not correspond to individuals present in the block_ref_df object.
#'  It orders the data frame based on time.
#'  The output block_df, is the working unit of this package.A dataset containing the putput with required formatted columns
#' "id", "time", "date", "antenna".
#'
#' @format A data frame with xxx rows and yy variables:
#' \describe{
#'   \item{id}{Unique RFID / PIT tag id per individuals, usually an alphanumeric}
#'   \item{SL}{Standard length, a measure of size in fish}
#'   \item{Weight}{Weight of the fish}
#'   \item{Plates}{Lateral plates}
#'   \item{Tagger}{Jakob}
#'   \item{Remarks}{Remarks while tagging}
#'   \item{Pond}{Pond_id}
#'   \item{Treatment_seq}{treatment}
#'   \item{Replicate}{replicate}
#'
#' }
#'
"block_ref_df"
