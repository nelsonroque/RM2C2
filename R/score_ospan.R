#' RM2C2: Scoring, Summarizing

#' @name score_ospan
#' @export
score_ospan <- function(df, threshold=0.6){
  scored <- df %>%
    mutate(dist1.RESP = ifelse(dist1.RESP == ".", NA, dist1.RESP),
           dist2.RESP = ifelse(dist2.RESP == ".", NA, dist2.RESP),
           dist3.RESP = ifelse(dist3.RESP == ".", NA, dist3.RESP),
           dist4.RESP = ifelse(dist4.RESP == ".", NA, dist4.RESP),
           dist1.CRESP = ifelse(dist1.CRESP == ".", NA, dist1.CRESP),
           dist2.CRESP = ifelse(dist2.CRESP == ".", NA, dist2.CRESP),
           dist3.CRESP = ifelse(dist3.CRESP == ".", NA, dist3.CRESP),
           dist4.CRESP = ifelse(dist4.CRESP == ".", NA, dist4.CRESP),
           mem1 = ifelse(mem1 == ".", NA, mem1),
           mem2 = ifelse(mem2 == ".", NA, mem2),
           mem3 = ifelse(mem3 == ".", NA, mem3),
           mem4 = ifelse(mem4 == ".", NA, mem4)) %>%
    mutate(dist1.is.correct = ifelse(dist1.RESP == dist1.CRESP, 1, 0),
           dist2.is.correct = ifelse(dist2.RESP == dist2.CRESP, 1, 0),
           dist3.is.correct = ifelse(dist3.RESP == dist3.CRESP, 1, 0),
           dist4.is.correct = ifelse(dist4.RESP == dist4.CRESP, 1, 0),
           mem1.is.correct = ifelse(mem1 == Choice1, 1, 0),
           mem2.is.correct = ifelse(mem2 == Choice2, 1, 0),
           mem3.is.correct = ifelse(mem3 == Choice3, 1, 0),
           mem4.is.correct = ifelse(mem4 == Choice4, 1, 0)) %>%
    rowwise() %>%
    mutate(sum.dist.correct = sum(c(dist1.is.correct,dist2.is.correct,dist3.is.correct,dist4.is.correct),na.rm=T),
           sum.mem.correct = sum(c(mem1.is.correct, mem2.is.correct, mem3.is.correct, mem4.is.correct),na.rm=T),
           sum.dist.RT = sum(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           mean.dist.RT = mean(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           median.dist.RT = median(c(as.numeric(dist1.RT), as.numeric(dist2.RT), as.numeric(dist3.RT), as.numeric(dist4.RT)), na.rm=T),
           sd.dist.RT = sd(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T),
           IQR.dist.RT = IQR(c(dist1.RT, dist2.RT, dist3.RT, dist4.RT), na.rm=T)) %>%
    mutate(perfect.distractor = ifelse(sum.dist.correct == set_size, 1, 0),
           perfect.recall = ifelse(sum.mem.correct == set_size, 1, 0)) %>%
    mutate(prop.dist.accuracy = sum.dist.correct / set_size,
           prop.mem.accuracy = sum.mem.correct / set_size) %>%
    mutate(flag.dist.low.accuracy = ifelse(prop.dist.accuracy < threshold, 1, 0),
           flag.mem.low.accuracy = ifelse(prop.mem.accuracy < threshold, 1, 0)) %>%
    span_order(.)
  
  return(scored)
}