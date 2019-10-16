#' RM2C2: Scoring, Summarizing

#' @name read_parsed_dir
#' @param filepath class: string
#' @keywords m2c2, cognition
#' @import tidyverse
#' @examples
#' read_parsed_dir(dir, filetype='txt', spattern="gamedata_", recursive=T, na=".")

#' @export
read_parsed_dir <- function(dir, filetype='txt', spattern="gamedata_", recursive=T, na=".") {
  # list files in dir
  fl <- list.files(dir,
                   pattern=spattern,
                   full.names = T, 
                   recursive = recursive)
  
  # create blank metadata df
  meta.df <- data.frame()
  
  # start timer
  stime <- Sys.time()
  
  result <- do.call(dplyr::bind_rows, lapply(fl, function(path) {
    # read depending on csv
    if(tolower(filetype) == 'txt') {
      #df <- readr::read_csv(path, comment = comment)
      df.o <- RM2C2::read_ambcog(path, na=na)
      df.o$user_id <- as.character(df.o$user_id)
      df.o$participant_id <- as.character(df.o$participant_id)
    }
    print(spec(df.o))
    # need this or else no DF returned
    df.o
    
  }))
  
  # get time difference in seconds
  print(difftime(Sys.time(),stime,units='secs'))
  
  # return DF
  return(list(result=result))
}