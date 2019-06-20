#' RM2C2: Scoring, Summarizing

#' @name summary_go_nogo
#' @export
summary_go_nogo <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "GO_NOGO"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.all_trials = median(response_time,na.rm=T),
              median.RT.HIT_trials = median(response_time[HIT == 1],na.rm=T),
              median.RT.FA_trials = median(response_time[FA == 1],na.rm=T),
              median.RT.MISS_trials = median(response_time[MISS == 1],na.rm=T),
              median.RT.CR_trials = median(response_time[CR == 1],na.rm=T),
              n.HIT = sum(HIT),
              n.FA = sum(FA),
              n.CR = sum(CR),
              n.MISS = sum(MISS),
              n.nogo.frames = sum(is.nogo.frame),
              n.frames = n()) %>%
    mutate(n.go.frames = n.frames - n.nogo.frames) %>%
    mutate(HIT.rate = n.HIT/n.go.frames,
           FA.rate = n.FA/n.nogo.frames) %>%
    mutate(MISS.rate = 1 - HIT.rate,
           CR.rate = 1 - FA.rate) %>%
    SDT_adj(.) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
    # add task name to column names
    len_group_var = length(group_var)
    names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
    return(summary.df)
}