% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pr_latency_reach.R
\name{pr_latency_reach}
\alias{pr_latency_reach}
\title{Computes the latency to reach one or several antennas}
\usage{
pr_latency_reach(
  block_df,
  block_ref_df,
  antenna_nb,
  start_time,
  end_time,
  keep_NA = FALSE,
  unit = "m"
)
}
\arguments{
\item{block_df}{A data frame containing the reads from an experimental block}

\item{block_ref_df}{A data frame containing a reference list with all individuals present in the experimental block.}

\item{antenna_nb}{If there is one antenna of interest, then antenna_nb corresponds to its number/name (\code{antenna_nb_1} in examples).
However, if there are several antennas of interest, the argument should be a vector of antenna names/numbers (\code{antenna_nb_2} in examples).}

\item{start_time}{The time from which the experimental block starts (\emph{i.e. POSIXct} format, see examples).}

\item{end_time}{The time until which the experimental block lasts (\emph{i.e. POSIXct} format, see examples).}

\item{keep_NA}{Logical argument. If the individuals are never read by the antenna(s), the latency might be set as the maximal possible value,
\emph{i.e.} \code{end_time} - \code{start_time} (default option), or be kept as NA.}

\item{unit}{Unit of the latency duration: 'm' as default.}
}
\value{
A data frame containing the latency, for every individual, to enter the antenna(s) of interest.
}
\description{
This function computes, for every individual, the latency to reach (\emph{i.e.} be read at) certain antennas.
This can for example be used in the context of an exploration test, in which individuals enter an exploration box,
in which they are read by certain antennas.

The function computes the difference between the initial time (time at which the antennas were introduced/started to record)
and the first read at one or several \emph{antennas of interest}. Individuals that were not read at these antennas (notably obtained
from \code{block_ref_df}) receive a score corresponding to the maximal possible duration by default.
}
\examples{

data(block_df)
data(block_ref_df)
antenna_nb_1 <- 44
antenna_nb_2 <- c(41, 42, 43, 44)
start_time <-
  as.POSIXct(strptime(c("2020-11-05 12:30:00"), "\%Y-\%m-\%d \%H:\%M:\%OS"), "UTC")
end_time <-
  as.POSIXct(strptime(c("2020-11-05 15:00:00"), "\%Y-\%m-\%d \%H:\%M:\%OS"), "UTC")



#pr_latency_reach(
#block_df,
#block_ref_df,
#antenna_nb_2,
#start_time,
#end_time,
#keep_NA = T,
#unit = 's')

#pr_latency_reach(
#block_df,
#block_ref_df,
#antenna_nb_1,
#start_time,
#end_time,
#keep_NA = T,
#unit = 'm')
}
