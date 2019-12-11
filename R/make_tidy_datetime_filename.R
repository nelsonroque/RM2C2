#' RM2C2: Scoring, Summarizing

#' @name make_tidy_datetime_filename
#' @param datetime class: POSIXct
#' @param timezone class: string
#' @import tidyverse
#' @examples
#' make_tidy_datetime_filename(datetime=NA, timezone="UTC")
#' @export
make_tidy_datetime_filename <- function() {
  cur_dts <- Sys.time()
  dt <- format(cur_dts, "%Y_%m_%dT%H_%M_%S")
  return(dt)
}