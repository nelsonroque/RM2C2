#' RM2C2: Scoring, Summarizing
#' 
#' @name score_span
#' @export
score_span <- function(df, id_var, trial_var) {
  PACKAGE.VERSION <- packageVersion("RM2C2")
  
  # calculate max set size from column names in parsed data -----
  # ............................................................
  max_set_size <- get_max_setsize_span(df)
  set_seq <- seq(1,max_set_size,1)
  
  for(i in 1:max_set_size){
    
    # perfect =right order, right items
    # full = any order, right items
    # prtial = any item, any order, gaps # most variance 
    
    # score distractor responses
    varname1 <- paste0('distractor.correct.', i)
    new_method1 <- paste0("dist",i,".RESP","==","dist",i,".CRESP")
    
    # score recall responses
    varname2 <- paste0('recall.correct.insequence.', i)
    new_method2 <- paste0("Choice",i,"==","mem",i)

    df <- df %>% mutate_(.dots = setNames(new_method1, varname1))
    df <- df %>% mutate_(.dots = setNames(new_method2, varname2))
  }
  
  # CONSTRUCT VARNAMES AND METHODS -----
  # ............................................................
  
  # sum correct distractor responses
  varname3 <- "sum.distractor.correct"
  new_method3 <- paste0("sum(",paste0("distractor.correct.",set_seq,collapse=","), ",na.rm=T)")
  
  # sum correct recall responses
  varname4 <- "sum.recall.correct.insequence"
  new_method4 <- paste0("sum(",paste0("recall.correct.insequence.",set_seq,collapse=","), ",na.rm=T)")
  
  # sum correct distractor responses
  varname5 <- "perfect.distractor.trial"
  new_method5 <- paste0("sum.distractor.correct", "==", "set_size")
  
  # sum correct recall responses
  varname6 <- "perfect.recall.trial"
  new_method6 <- paste0("sum.recall.correct.insequence", "==", "set_size")
  
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
  
  varname11 <- "sum.recall.correct.thatexist"
  new_method11 <- paste0("sum(", "c(", paste0("Choice",set_seq,collapse=","), ") %in%", " c(", paste0("mem",set_seq,collapse=","), "), na.rm=T)", "-", " sum(is.na(c(",paste0("mem",set_seq,collapse=","),")))")
  
  # ............................................................
  
  # run sequential calls to mutate based on constructed methods -----
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method3, varname3))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method4, varname4))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method5, varname5))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method6, varname6))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method7, varname7))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method8, varname8))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method9, varname9))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method10, varname10))
  df <- df %>% rowwise() %>% mutate_(.dots = set_names(new_method11, varname11))
  
  # run other commands that dont require NSE -----
  # ............................................................
  
  # calculate scoring for trials using various methods
  
  # full scoring method: if all not in order and correct, then score is 0
  df <- df %>% rowwise() %>% mutate(FSM.score = ifelse(sum.recall.correct.insequence == set_size, set_size, 0))
  
  # partial scoring method, ordered
  df <- df %>% rowwise() %>% mutate(PSM.ordered.score = sum.recall.correct.insequence)
  df <- df %>% rowwise() %>% mutate(PSM.unordered.score = sum.recall.correct.thatexist)
  
  df <- df %>% rowwise() %>% mutate(prop.correct.distractor.items = sum.distractor.correct / set_size)
  df <- df %>% rowwise() %>% mutate(prop.correct.recall.inorder = sum.recall.correct.insequence/set_size)
  df <- df %>% rowwise() %>% mutate(prop.correct.recall.anyorder = sum.recall.correct.thatexist/set_size)
  
  # number of selection trail seperators
  df <- df %>% rowwise() %>% mutate(n.selections = stringr::str_count(selection_trail, ">") + 1)
  df <- df %>% rowwise() %>% mutate(n.selections.equals.set = ifelse(n.selections == set_size, 1, 0))
  
  scored <- df  %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)
  
  return(scored)
}