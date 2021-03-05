#' Raw data frame obtained from the RFID / PIT tag readers
#'
#' Raw data frame containing reads from one experimental block, as obtained
#' from the RFID / PIT transponder. Check vignette for explanation of experimental
#' block.
#'
#' @format A data frame with 808 rows and yy variables:
#' \describe{
#'   \item{Identifier}{Identifier records the serial number of each read from the RFID / PIT transponder}
#'   \item{id}{Unique RFID / PIT tag id per individuals, usually an alphanumeric}
#'   \item{date}{Date of recorded read in dd-mm-yy format}
#'   \item{time}{Time of recorded read hh:mm:ss format}
#'   \item{antenna}{Unique antenna number / code where the RFID / PIT tag was read}
#'   \item{GPS.coordinates}{Optional columns: GPS coordinates of the antenna}
#'
#'   ...
#' }
#'
"raw_df"
