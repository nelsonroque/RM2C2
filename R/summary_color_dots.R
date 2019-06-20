#' RM2C2: Scoring, Summarizing

#' @name summary_color_dots
#' @export
summary_color_dots <- function(df, group_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  TASK_NAME <- "COLOR_DOTS"
  summary.df <- df %>%
    group_by_(.dots = group_var) %>%
    summarise(median.RT.color = median(ColorRT,na.rm=T),
              sd.RT.color = sd(ColorRT,na.rm=T),
              
              median.RT.location = median(LocRT,na.rm=T),
              sd.RT.location = sd(LocRT,na.rm=T),
              
              stage2.median.precision.swap = median(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
              stage2.sd.precision.swap = sd(stage2.distance.from.color.probe[stage2.classification == "SWAP"], na.rm=T),
              
              stage2.median.precision.correct = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
              stage2.sd.precision.correct = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT"], na.rm=T),
              
              stage2.median.precision.random = median(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
              stage2.sd.precision.random = sd(stage2.distance.from.color.probe[stage2.classification == "RANDOM"], na.rm=T),
              
              stage2.median.precision.correct.or.swap = median(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
              stage2.sd.precision.correct.or.swap = sd(stage2.distance.from.color.probe[stage2.classification == "CORRECT" | stage2.classification == "SWAP"], na.rm=T),
              
              stage1.swap.count = sum(stage1.classification == "SWAP"),
              stage2.swap.count = sum(stage2.classification == "SWAP"),
              
              stage1.random.count = sum(stage1.classification == "RANDOM"),
              stage2.random.count = sum(stage2.classification == "RANDOM"),
              
              stage1.correct.count = sum(stage1.classification == "CORRECT"),
              stage2.correct.count = sum(stage2.classification == "CORRECT"),
              
              n = n()) %>%
    mutate(stage1.swap.prop = stage1.swap.count / n,
           stage2.swap.prop = stage2.swap.count / n,
           
           stage1.random.prop = stage1.random.count / n,
           stage2.random.prop = stage2.random.count / n,
           
           stage1.correct.prop = stage1.correct.count / n,
           stage2.correct.prop = stage2.correct.count / n) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  
  # add task name to column names
  len_group_var = length(group_var)
  names(summary.df)[(len_group_var+1):ncol(summary.df)] <- paste0(TASK_NAME,".",names(summary.df)[(len_group_var+1):ncol(summary.df)])
  return(summary.df)
}