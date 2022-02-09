starsproxylist_as_mosaic <- function(starsproxylist) {
  mosaic <-
    stars::st_mosaic(starsproxylist[[1]])

  if (length(starsproxylist) > 1) {
    for (i in 2:length(starsproxylist)) {
      mosaic <-
        stars::st_mosaic(mosaic, starsproxylist[[i]])
    }
  }
  mosaic
}
