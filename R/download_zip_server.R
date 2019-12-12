#' RM2C2

#' @name download_zip_server
#' @param url class: string
#' @param params class: list
#' @param save_filename class: string
#' @param save_path class: string
#' @param overwrite_zip class: boolean
#' @param unzip class: boolean
#' @param unzip_folder class: boolean
#' @param remove_zip class: boolean
#' @import tidyverse
#' @import httr
#' @examples
#' download_zip_server(url, params, save_filename, save_path, unzip=T, remove_zip=F)
#' @export
download_zip_server <- function(url, params, save_filename, save_path, overwrite_zip=F, unzip=T, unzip_folder=NA, remove_zip=F) {
  print(paste0("Downloading data from remote server: ", url))
  
  # save latest file and parse it
  res <- try(httr::POST(url, body = params, encode = "form", httr::write_disk(save_filename, overwrite = overwrite_zip)))
  
  # unzip if requested
  if(unzip) {
    # unzip the file
    try(unzip(save_filename))
    
    if(!is.na(unzip_folder)){
      # get list of files in unzipped folderlatest output folder
      files_in_folder <- list.files(pattern=unzip_folder, recursive=T, full.names=T)
    } else {
      files_in_folder <- "ERROR: unzipped folder name unknown"
    }
  }
  
  # remove zip if requested
  if(remove_zip) {
    file.remove(save_filename)
    # unlink(latest_out, recursive = TRUE)
  }
  
  return(list(zip_location = paste0(getwd(), "/", save_filename), filenames = files_in_folder))
}