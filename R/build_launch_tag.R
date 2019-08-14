#' RM2C2: Scoring, Summarizing

#' @name build_launch_tag
#' @keywords m2c2, cognition
#' @examples
#' build_launch_tag(df, root_tag)
#' @export
build_launch_tag <- function(dolphin, launch, sensors, settings, root_tag, launch_tag, sensor_tag, settings_tag) {
  
  # build `Dolphin` tag
  # ______________________________________________________
  root <- build_root_tag(dolphin, root_tag)
  
  # build `Launch` tag
  # ______________________________________________________
  launch_node = newXMLNode(launch_tag, parent=root)
  
  for(i in 1:nrow(launch)) {
    nd <- launch[i,] %>% select_if(function(x) any(!is.na(x)))
    next_node = newXMLNode(nd$tag[1], parent=launch_node)
    xmlAttrs(next_node) = nd[,2:ncol(nd)]
  }
  
  # build `Sensors` tag
  # ______________________________________________________
  sensors_node = newXMLNode(sensor_tag, parent=launch_node)
  
  for(i in 1:nrow(sensors)) {
    nd <- sensors[i,] %>% select_if(function(x) any(!is.na(x)))
    next_node = newXMLNode(nd$tag[1], parent=sensors_node)
    xmlAttrs(next_node) = nd[,2:ncol(nd)]
  }
  
  # build `Settings` tag
  # ______________________________________________________
  settings_node = newXMLNode(settings_tag, parent=root)
  
  for(i in 1:nrow(settings)) {
    nd <- settings[i,] %>% select_if(function(x) any(!is.na(x)))
    next_node = newXMLNode(nd$tag[1], parent=settings_node)
    xmlAttrs(next_node) = nd[,2:ncol(nd)]
  }
  
  return(root)
}