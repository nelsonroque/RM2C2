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
make_tidy_datetime_filename <- function(datetime=Sys.time(), timezone="UTC") {
  dt <- format(datetime, "%Y_%m_%dT%H_%M_%S")
  return(dt)
}