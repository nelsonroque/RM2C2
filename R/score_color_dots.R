#' RM2C2: Scoring, Summarizing

#' @name score_color_dots
#' @export
score_color_dots <- function(df, threshold=75){
  PACKAGE.VERSION <- packageVersion("RM2C2")
  scored <- df %>%
    separate(Loc1, into=c("Loc1_x", "Loc1_y"), " ", convert=T) %>%
    separate(Loc2, into=c("Loc2_x", "Loc2_y"), " ", convert=T) %>%
    separate(Loc3, into=c("Loc3_x", "Loc3_y"), " ", convert=T) %>%
    separate(ProbedLocation, into=c("probe_x", "probe_y"), " ", convert=T) %>%
    separate(FinalLocation, into=c("final_x", "final_y"), " ", convert=T) %>%
    mutate(stage1.loc1_distance = distance(Loc1_x, probe_x, Loc1_y, probe_y),
           stage1.loc2_distance = distance(Loc2_x, probe_x, Loc2_y, probe_y),
           stage1.loc3_distance = distance(Loc3_x, probe_x, Loc3_y, probe_y)) %>%
    rowwise() %>%
    mutate(stage1.which.location.probed = which.min(c(stage1.loc1_distance, stage1.loc2_distance, stage1.loc3_distance))) %>%
    mutate(stage1.color.at.probed.location = ifelse(stage1.which.location.probed == 1,Col1,
                                      ifelse(stage1.which.location.probed == 2,Col2,
                                      ifelse(stage1.which.location.probed == 3, Col3, NA))),
           
           stage1.probe.location_x = ifelse(stage1.which.location.probed == 1, Loc1_x,
                                     ifelse(stage1.which.location.probed == 2, Loc2_x,
                                     ifelse(stage1.which.location.probed == 3, Loc3_x, NA))),
           
           stage1.probe.location_y = ifelse(stage1.which.location.probed == 1, Loc1_y,
                                     ifelse(stage1.which.location.probed == 2, Loc2_y,
                                     ifelse(stage1.which.location.probed == 3, Loc3_y, NA)))) %>%
    mutate(stage1.is.color.correct = ifelse(stage1.color.at.probed.location == ColorChoice, 1, 0)) %>%
    rowwise() %>%
    mutate(stage1.is.color.chosen.shown = ifelse(ColorChoice %in% c(Col1,Col2,Col3), 1, 0)) %>%
    mutate(stage1.classification = ifelse(stage1.is.color.chosen.shown == 1 & stage1.is.color.correct == 1, "CORRECT",
                                   ifelse(stage1.is.color.chosen.shown == 0 & stage1.is.color.correct == 0, "RANDOM",
                                   ifelse(stage1.is.color.chosen.shown == 1 & stage1.is.color.correct == 0, "SWAP")))) %>%
    mutate(stage2.col1_match = ifelse(ProbedColor == Col1, 1, 0),
           stage2.col2_match = ifelse(ProbedColor == Col2, 1, 0),
           stage2.col3_match = ifelse(ProbedColor == Col3, 1, 0)) %>%
    mutate(stage2.which.location.is.probed.color = which(1 == c(stage2.col1_match, stage2.col2_match, stage2.col3_match))) %>%
    mutate(stage2.location.at.probed.color.x = ifelse(stage2.which.location.is.probed.color == 1, Loc1_x,
                                               ifelse(stage2.which.location.is.probed.color == 2, Loc2_x,
                                               ifelse(stage2.which.location.is.probed.color == 3, Loc3_x, NA)))) %>%
    mutate(stage2.location.at.probed.color.y = ifelse(stage2.which.location.is.probed.color == 1, Loc1_y,
                                               ifelse(stage2.which.location.is.probed.color == 2, Loc2_y,
                                               ifelse(stage2.which.location.is.probed.color == 3, Loc3_y, NA)))) %>%
    mutate(stage2.loc1_distance = distance(Loc1_x, stage2.location.at.probed.color.x, 
                                           Loc1_y, stage2.location.at.probed.color.y),
           stage2.loc2_distance = distance(Loc2_x, stage2.location.at.probed.color.x, 
                                           Loc2_y, stage2.location.at.probed.color.y),
           stage2.loc3_distance = distance(Loc3_x, stage2.location.at.probed.color.x, 
                                           Loc3_y, stage2.location.at.probed.color.y),
           stage2.distance.from.color.probe = distance(final_x, stage2.location.at.probed.color.x, 
                                                            final_y, stage2.location.at.probed.color.y),
           stage2.distance.from.location.probe = distance(final_x, stage1.probe.location_x,
                                                       final_y, stage1.probe.location_y)) %>%
    mutate(which.location.unprobed = setdiff(c(1,2,3),c(stage1.which.location.probed, stage2.which.location.is.probed.color))) %>%     
    mutate(location.unprobed.x = ifelse(which.location.unprobed == 1, Loc1_x,
                                 ifelse(which.location.unprobed == 2, Loc2_x,
                                 ifelse(which.location.unprobed == 3, Loc3_x, NA))),
           location.unprobed.y = ifelse(which.location.unprobed == 1, Loc1_y,
                                 ifelse(which.location.unprobed == 2, Loc2_y,
                                 ifelse(which.location.unprobed == 3, Loc3_y, NA)))) %>%
    mutate(stage2.distance.from.location.unprobed = distance(final_x, location.unprobed.x,
                                                             final_y, location.unprobed.y)) %>%
    mutate(stage2.is.response.distance.near.color.probe = ifelse(stage2.distance.from.color.probe <= threshold, 1, 0),
           stage2.is.response.distance.near.location.probe = ifelse(stage2.distance.from.location.probe <= threshold, 1, 0),
           stage2.is.response.distance.near.location.unprobed = ifelse(stage2.distance.from.location.unprobed <= threshold, 1, 0)) %>%
    mutate(stage2.classification = ifelse(stage2.is.response.distance.near.color.probe == 1 & stage2.is.response.distance.near.location.probe == 0, "CORRECT",
                                   ifelse(stage2.is.response.distance.near.color.probe == 0 & stage2.is.response.distance.near.location.probe == 1, "SWAP",
                                   ifelse(stage2.is.response.distance.near.color.probe == 0 & stage2.is.response.distance.near.location.probe == 0, "RANDOM", NA)))) %>%
    mutate(PACKAGE.VERSION = PACKAGE.VERSION)

    
    return(scored)
}