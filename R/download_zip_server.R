#' RM2C2: Scoring, Summarizing

#' @name download_zip_server
#' @export
#' @param url class: string
#' @param params class: list
#' @param save_filename class: string
#' @param overwrite_zip class: boolean
#' @param unzip class: boolean
#' @param remove_zip class: boolean
#' @import tidyverse
#' @import httr
#' @examples
#' download_zip_server(url, params, save_filename, overwrite_zip=T, unzip=T, remove_zip=F)
download_zip_server <- function(url, params, save_filename, overwrite_zip=F, unzip=T, remove_zip=F) {
  print(paste0("Downloading data from remote server: ", url))
  
  # save latest file and parse it
  res <- try(httr::POST(url, body = params, encode = "form", httr::write_disk(save_filename, overwrite = overwrite_zip)))
  
  # unzip if requested
  if(unzip) {
    # unzip the file
    try(unzip(save_filename))
  }
  
  # remove zip if requested
  if(remove_zip) {
    file.remove(save_filename)
  }
  
  return(list(zip_location = paste0(save_filename)))
}