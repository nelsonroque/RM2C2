#' RM2C2: Scoring, Summarizing

#' @name build_pack_tag
#' @keywords m2c2, cognition
#' @examples
#' build_pack_tag(df, root_tag)
#' @export
build_pack_tag <- function(root, pack, pack_tag, game_tag, screen_tag, branch_tag, nextscreen_tag, element_tag, option_tag) {
  
  # pack list
  packing_list = split(pack, pack$pack_id)
  
  for(i in 1:length(packing_list)) {
    # create pack node in root node
    pack_node = newXMLNode(pack_tag, parent=root)
    
    # getting all data for this pack
    cur_pack = packing_list[[i]]
    
    cur_pack_meta = cur_pack %>% select(pack_id, pack_startscreen) %>% 
      mutate(id = pack_id, startScreen = pack_startscreen) %>%
      select(-pack_id, -pack_startscreen) %>%
      filter(row_number() == 1)
    
    #print(cur_pack_meta)
    
    # add pack tag attributes
    xmlAttrs(pack_node) = cur_pack_meta
    
    # list of all screens in pack
    screen_list = split(cur_pack, cur_pack$screen_id)
    
    # for each screen
    for(j in 1:length(screen_list)){
      # create screen node in pack node
      screen_node = newXMLNode(screen_tag, parent=pack_node)
      
      # does screen have a marker
      # does screen have a game
      # does screen have a branch
      
      # get all screen for this one pack
      cur_screen = screen_list[[j]]
      
      # add screen id to screen node
      xmlAttrs(screen_node) = cur_screen %>% 
        select(screen_id) %>% 
        mutate(id = screen_id) %>% 
        select(-screen_id)
      
      # list of all elements in screen
      element_list = split(cur_screen, cur_screen$element_id)
      for(k in 1:length(element_list)){
        # get all screen for this one pack
        cur_element = element_list[[k]]
        
        # select element type info
        cur_element_meta = cur_element %>% 
          select(element_id, element_type, element_text) %>% 
          mutate(type = element_type, 
                 text = element_text, 
                 id = element_id) %>%
                # id = gsub("[[:punct:]]","_",gsub(" ","_",tolower(element_text)))) %>% 
          select(-element_type, -element_text, -element_id)
        
        if(cur_element_meta$type == "game") {
          
          # get game info
          cur_game_meta1 = cur_element %>% 
            select(game_name,game_blocks, game_trials) %>% 
            mutate(name = game_name,
                   blocks = game_blocks,
                   trials = game_trials)
          
          cur_game_meta2 = cur_element %>% 
            select(game_blocks, game_trials) %>% 
            mutate(blocks = game_blocks,
                   trials = game_trials)
          
          # create element node in screen node
          game_node = newXMLNode(game_tag, parent=screen_node)
          
          # check game name and add params
          if(tolower(cur_game_meta1$name) == "multiobjecttracking"){
            cur_game_meta_f <- cur_game_meta1 %>%
              mutate(CueScreenInstructions="Follow these {targetStimCount} dots.",
                     ResponseScreenInstructions="Where were the colored dots?",
                     MaxResponseInfoText="You cannot select more than {targetStimCount} responses!",
                     FrameHeight = "600",
                     TargetStimColor="0,176,80",
                     StandardStimColor="127,127,127",
                     SelectedColor="28,116,169",
                     IncorrectColor="255,0,0",
                     FeedbackOutlineColor="0_0_0",
                     Stimradius="36",
                     StimRepulseFactor="1.7",
                     Stimspeed="1.5",
                     StimtouchFactor="2" ,
                     ResponseTimeout="20000",
                     FeedbackTime="1000",
                     MaxResponseCount="-1",
                     MinResponseCount="-1",
                     TargetStimCount="3" ,
                     DistractorStimCount="6",
                     RandomizeBlocks="false",
                     RandomizeTrialSets="false",
                     movementTime="10000",
                     CueBeforeTime="500", 
                     CueBlinkTime="800", 
                     CueAfterTime="500", 
                     CueShowTime="500") %>%
              select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
            
            # add attributes
            xmlAttrs(game_node) = cur_game_meta_f
            
            # # add blank blocks and trial sets
            block_node = newXMLNode("Block", parent=game_node)
            
            trial_node = newXMLNode("TrialSet", parent=block_node)
            
            xmlAttrs(trial_node) = cur_game_meta1 %>% mutate(TrialNum = trials) %>% select(TrialNum)
            
          } else {
            if(tolower(cur_game_meta1$name) == "dotmemory"){
              cur_game_meta_f <- cur_game_meta1 %>%
                mutate(InterferenceRatio="5" ,
                       ReadyTime="100" ,
                       InterferenceTime="8000" ,
                       DotPresentTime="3000" ,
                       ShowDelayTime="100" ,
                       InterferenceZip="dm_easy.zip", 
                       InterferenceText="Touch the Stars!",
                       InterferenceSize="50",
                       TrialNum = trials) %>%
                select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
              
              # add attributes
              xmlAttrs(game_node) = cur_game_meta_f
              
            } else {
              if(tolower(cur_game_meta1$name) == "colordots"){
                cur_game_meta_f <- cur_game_meta1 %>%
                  mutate(FixationTime="1000",
                         StudyTime="3000",
                         DelayTime="750",
                         DotNum="3",
                         Test1Mode="1",
                         Test1DisplayHold="1000",
                         TrialNum = trials) %>%
                  select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
                
                # add attributes
                xmlAttrs(game_node) = cur_game_meta_f
                
              } else {
                if(tolower(cur_game_meta1$name) == "symbolsearch" | tolower(cur_game_meta1$name) == "symbolmatch"){
                  cur_game_meta_f <- cur_game_meta1 %>%
                    mutate(InstructionScreen="true",
                           Feedback="0",
                           LurePerc="25",
                           LeftCorrectPerc="50",
                           FeedbackDelay="500",
                           StartDelay="1000",
                           MatchPairsTop="2",
                           TrialNum = trials) %>%
                    select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
                  
                  # add attributes
                  xmlAttrs(game_node) = cur_game_meta_f
                  
                } else {
                  if(tolower(cur_game_meta1$name) == "shoppinglist"){
                    cur_game_meta_f <- cur_game_meta1 %>%
                      mutate(JudgmentTime="3000",
                             JudgmentDelayTime="1000",
                             ResponseDelayTime="1000",
                             useTransitions="false",
                             DebugInfo="false",
                             TrialNum = trials) %>%
                      select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
                    
                    # add attributes
                    xmlAttrs(game_node) = cur_game_meta_f
                    
                  } else {
                    if(tolower(cur_game_meta1$name) == "changedetection"){
                      cur_game_meta_f <- cur_game_meta1 %>%
                        mutate(ReadyTime="1000",
                               FixationTime="900",
                               StudyTime="100",
                               TestTime="-1",
                               PostTrialFixationTime="1000",
                               SameTrials="5", 
                               SquareNum="3",
                               SquareDispNum="1",
                               TrialNum = trials) %>%
                        select(-blocks, -trials, -game_name, -game_blocks, -game_trials)
                      
                      # add attributes
                      xmlAttrs(game_node) = cur_game_meta_f
                    } 
                  }
                }
              }
            }
          }
          
        }
        
        
        else {
          # create element node in screen node
          element_node = newXMLNode(element_tag, parent=screen_node)
          
          # add attributes
          xmlAttrs(element_node) = cur_element_meta
          
          # get id for current element
          cur_element_id = cur_element_meta %>% select(id)
          
          if(cur_element_meta$type == "dropdown" | cur_element_meta$type == "radiobutton"| cur_element_meta$type == "checkbox"){
            element_options = cur_element %>% select(element_options)
            element_options_str = as.character(element_options)#tolower(element_options)
            element_options_list = strsplit(element_options_str, "\\_")
            
            for(m in 1:length(element_options_list[[1]])){
              cur_option = element_options_list[[1]][m]
              option_node = newXMLNode("option", parent=element_node)
              xmlAttrs(option_node) = tibble(id=cur_option, text=cur_option)
            }
          } else {
            if(cur_element_meta$type == "slider"){
              element_options = cur_element %>% 
                select(element_lefttext, element_righttext) %>%
                mutate(leftText = element_lefttext,
                       rightText = element_righttext) %>%
                select(leftText, rightText)
              xmlAttrs(element_node) = element_options
            }
          }
        }
      }
      
      cur_element_next = cur_element %>% 
        select(nextscreen_action, nextscreen_id, branch_target, branch_cond) %>%
        mutate(action = nextscreen_action,
               id = nextscreen_id,
               target_id = branch_target,
               cond = branch_cond, 
               cond_f = paste0(cur_element_id,"=",branch_cond))
      
      nextscreen_node = newXMLNode(nextscreen_tag, parent=screen_node)
      
      if(is.na(cur_element_next$nextscreen_action)) {
        # write nextscreen id
        xmlAttrs(nextscreen_node) = cur_element_next %>% select(id)
        
        if(is.na(cur_element_next$cond)){
          # if no branching
        } else {
          # get number of branching elements
          
          branch_conds = cur_element %>% select(branch_cond)
          branch_conds_str = as.character(branch_conds)#tolower(branch_conds)
          branch_conds_list = strsplit(branch_conds_str, "\\_")
          
          # fore ach, make branch
          
          #if yes branching
          branch_node = newXMLNode(branch_tag, parent=nextscreen_node)
          
          
          xmlAttrs(branch_node) = cur_element_next %>% select(cond_f, target_id) %>% mutate(cond = cond_f) %>% select(-cond_f)
        }
        
      } else {
        # write nextscreen action here
        xmlAttrs(nextscreen_node) = cur_element_next %>% select(action)
      }
      
    }
  }
  return(root)
}