% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eumohp_write.R
\name{eumohp_write}
\alias{eumohp_write}
\title{Export of the clipped EU-MOHP data}
\usage{
eumohp_write(eumohp_starsproxy, directory_output, parallel = FALSE)
}
\arguments{
\item{eumohp_starsproxy}{A list of stars proxy objects as derived
from the function eumohp_clip().}

\item{directory_output}{A character vector of length one. A directory
where all the clipped EU-MOHP .tif files should be written to.}

\item{parallel}{A logical vector of length one.
If parallel = TRUE, furrr::future_imap() is used to write files in parallel.
Default is FALSE.}
}
\value{
...
}
\description{
Writes the clipped EU-MOHP data as raster data to GEOTIFF (.tif)
files to a user specified directory.
Depending on the area of interest and selections, this function call
might run very long (up to days on slow computers in sequential mode).
It is recommended to run it in parallel mode.
}
\examples{
\dontrun{
# Write files sequentially:
eumohp_clip(
   directory_input = "directory/to/EU-MOHPfiles/",
   region_name_spatcov = c("italy2"),
   hydrologic_order = 1:4,
   abbreviation_measure = c("dsd", "lp"),
   eumohp_version = "v013.1.1"
) |>
eumohp_write(directory_output = "directory/to/write/EU-MOHPfiles/")


# Write files parallelly:

future::plan(future::multisession, workers = 10)
eumohp_clip(
   directory_input = "directory/to/EU-MOHPfiles/",
   region_name_spatcov = c("italy2"),
   hydrologic_order = 1:4,
   abbreviation_measure = c("dsd", "lp"),
   eumohp_version = "v013.1.1"
) |>
eumohp_write(directory_output = "directory/to/write/EU-MOHPfiles/",
   parallel = TRUE)
}
}
