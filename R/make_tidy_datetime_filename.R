#' RM2C2: Scoring, Summarizing

#' @name make_tidy_datetime_filename
#' @param datetime class: POSIXct
#' @param timezone class: string
#' @param prefix class: string
#' @param suffix class: string
#' @import tidyverse
#' @examples
#' make_tidy_datetime(datetime=NA, timezone="UTC")
#' @export
make_tidy_datetime_filename <- function(datetime=Sys.time(), timezone="UTC", prefix="", suffix="") {
  dt <- as.POSIXlt(datetime, timezone, "%Y_%m_%dT%H_%M_%S")
  dt <- paste0(prefix, dt, suffix)
  
  n <- gsub("%+", "_pct_", dt)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  n <- gsub("(^_+|_+$)", "", n)
  n <- gsub("_+", "_", n)
  
  return(n)
}