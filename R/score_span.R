#' RM2C2: Scoring, Summarizing
#' 
#' @name score_span
#' @export
score_span <- function(df, id_var, trial_var) {
  # calculate max set size from column names in parsed data
  max_set_size <- get_max_setsize_span(df)
  set_seq <- seq(1,max_set_size,1)
  
  for(i in 1:max_set_size){
    # score distractor responses
    varname1 <- paste0('distractor.correct.', i)
    new_method1 <- paste0("dist",i,".RESP","==","dist",i,".CRESP")
    
    # score recall responses
    varname2 <- paste0('recall.correct.', i)
    new_method2 <- paste0("Choice",i,"==","mem",i)

    df <- df %>% mutate_(.dots = setNames(new_method1, varname1))
    df <- df %>% mutate_(.dots = setNames(new_method2, varname2))
  }
  # ///////////////////////////////////////////////////////
  # CONSTRUCT VARNAMES AND METHODS
  # ///////////////////////////////////////////////////////
  
  # sum correct distractor responses
  varname3 <- "sum.distractor.correct.anyorder"
  new_method3 <- paste0("sum(",paste0("distractor.correct.",set_seq,collapse=","), ",na.rm=T)")
  
  # sum correct recall responses
  varname4 <- "sum.recall.correct.anyorder"
  new_method4 <- paste0("sum(",paste0("recall.correct.",set_seq,collapse=","), ",na.rm=T)")
  
  # sum correct distractor responses
  varname5 <- "perfect.distractor.trial"
  new_method5 <- paste0("sum.distractor.correct.anyorder", "==", "set_size")
  
  # sum correct recall responses
  varname6 <- "perfect.recall.trial"
  new_method6 <- paste0("sum.recall.correct.anyorder", "==", "set_size")
  
  # sum distractor RT
  varname7 <- "sum.dist.RT"
  new_method7 <- paste0("sum(",paste0("dist",set_seq,".RT",collapse=","), ",na.rm=T)")
  
  # mean distractor RT
  varname8 <- "mean.dist.RT"
  new_method8 <- paste0("mean(",paste0("dist",set_seq,".RT",collapse=","), ",na.rm=T)")
  
  # median distractor RT
  varname9 <- "median.dist.RT"
  new_method9 <- paste0("median(",paste0("dist",set_seq,".RT",collapse=","), ",na.rm=T)")
  
  # sd distractor RT
  varname10 <- "sd.dist.RT"
  new_method10 <- paste0("sd(c(",paste0("dist",set_seq,".RT",collapse=","), "),na.rm=T)")
  
  # ///////////////////////////////////////////////////////
  
  # run sequential calls to mutate based on constructed methods
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method3a, varname3a))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method3, varname3))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method4, varname4))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method5, varname5))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method6, varname6))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method7, varname7))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method8, varname8))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method9, varname9))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method10, varname10))
  
  # run other commands that dont require NSE
  df <- df %>% rowwise() %>% mutate(PSP.score = ifelse(sum.recall.correct.anyorder == set_size, set_size, sum.recall.correct.anyorder))
  df <- df %>% rowwise() %>% mutate(TSP.score = ifelse(sum.recall.correct.anyorder == set_size, set_size, 0))
  
  df <- df %>% rowwise() %>% mutate(prop.correct.anyorder.distractor.items = sum.distractor.correct.anyorder / set_size)
  df <- df %>% rowwise() %>% mutate(prop.correct.anyorder.recall.items = sum.recall.correct.anyorder / set_size)
  
  return(df)
}