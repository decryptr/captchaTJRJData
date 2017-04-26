#' Download one captcha image from TJRJ website
#'
#' Download just one captcha image from TJRJ website
#'
#' @param dir character string naming a directory path for writing the image
#'
#' @return response
#'
#' @export
captcha_download_one <- function(dir) {
  url_img <- "http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha"
  datetime <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  arq <- tempfile(pattern = datetime, tmpdir = dir, fileext = '.png')
  wd_img <- httr::write_disk(arq, overwrite = TRUE)
  httr::GET(url_img, wd_img)
}

#' Download n captcha images from TJRJ website at once
#'
#' Download n captcha images from TJRJ website at once
#'
#' @param n number of captchas to download
#' @param dir character string naming a directory path for writing the image
#'
#' @return list of responses
#'
#' @export
captcha_download <- function(n, dir = 'data-raw/captcha') {
  p <- progress::progress_bar$new(total = n)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- purrr::possibly(captcha_download_one, 'erro')
  purrr::walk(seq_len(n), ~{
    f(dir)
    p$tick()
  })
  invisible(TRUE)
}
