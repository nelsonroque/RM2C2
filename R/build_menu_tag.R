#' RM2C2: Scoring, Summarizing

#' @name build_menu_tag
#' @keywords m2c2, cognition
#' @examples
#' build_menu_tag(df, root_tag)
#' @export
build_menu_tag <- function(root, menu, menu_tag, title_tag, element_tag, action_tag) {
  # build `Menu` tag
  menu_node = newXMLNode(menu_tag, parent=root)
  xmlAttrs(menu_node) = menu %>% 
    filter(row_number() == 1) %>% 
    select(menu_id) %>% 
    mutate(id = menu_id) %>% 
    select(-menu_id)
  
  # add menu title
  menu_title = newXMLNode(title_tag, parent=menu_node)
  
  xmlAttrs(menu_title) = menu %>% 
    select(title_text) %>% 
    mutate(text = title_text) %>% 
    select(-title_text) %>%
    filter(row_number() == 1)
  
  # for each unique title, 
  # segment data for each title
  # build the associated menu
  # iterate
  
  for(i in 1:nrow(menu)) {
    nd <- menu[i,] %>% select_if(function(x) any(!is.na(x)))
    next_node = newXMLNode(element_tag, parent=menu_node)
    xmlAttrs(next_node) = nd %>% select(type, text)
    
    inner_node = newXMLNode(action_tag, parent=next_node)
    xmlAttrs(inner_node) = nd %>% select(action, target_id)
  }
  return(root)
}