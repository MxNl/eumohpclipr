#' Export of the clipped EUMOHP data
#'
#' Writes the clipped EUMOHP data as raster data to GEOTIFF (.tif)
#' files to a user specified directory.
#' Depending on the area of interest and selections, this function call
#' might run very long (up to days on slow computers in sequential mode).
#' It is recommended to run it in parallel mode.
#'
#' @param eumohp_starsproxy A list of stars proxy objects as derived
#' from the function eumohp_clip().
#' @param directory_output A character vector of length one. A directory
#' where all the clipped EUMOHP .tif files should be written to.
#' @param parallel A logical vector of length one.
#' If parallel = TRUE, furrr::future_imap() is used to write files in parallel.
#' Default is FALSE.
#' @return ...
#' @examples
#' \dontrun{
#' # Write files sequentially:
#' eumohp_clip(
#'    directory_input = "directory/to/EUMOHPfiles/",
#'    region_name_spatcov = c("italy2"),
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' ) |>
#' eumohp_write(directory_output = "directory/to/write/EUMOHPfiles/")
#'
#'
#' # Write files parallelly:
#'
#'future::plan(future::multisession, workers = 10)
#' eumohp_clip(
#'    directory_input = "directory/to/EUMOHPfiles/",
#'    region_name_spatcov = c("italy2"),
#'    hydrologic_order = 1:4,
#'    abbreviation_measure = c("dsd", "lp"),
#'    eumohp_version = "v013.1.1"
#' ) |>
#' eumohp_write(directory_output = "directory/to/write/EUMOHPfiles/",
#'    parallel = TRUE)
#' }
#' @export
eumohp_write <- function(eumohp_starsproxy,
                         directory_output,
                         parallel = FALSE) {
  mapper <- ifelse(parallel, furrr::future_imap, purrr::imap)
  mapper(
    eumohp_starsproxy,
    ~ stars::write_stars(
      obj = .x,
      dsn = str_glue("{directory_output}/mohp_custom-{.y}.tif")
    )
  )
}
