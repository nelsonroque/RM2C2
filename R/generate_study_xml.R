#' RM2C2: Scoring, Summarizing

#' @name generate_study_xml
#' @keywords m2c2, cognition
#' @examples
#' generate_study_xml(input_file, project_name, verbose=T)
#' @export
generate_study_xml <- function(input_file, project_name, verbose=T) {
  
  # place all in dataframe / tibble
  root_tag <- "<Dolphin></Dolphin>"
  launch_tag <- "Launch"
  sensor_tag <- "sensors"
  settings_tag <- "Settings"
  menu_tag <- "Menu"
  title_tag <- "title"
  element_tag <- "element"
  action_tag <- "action"
  pack_tag <- "Pack"
  game_tag <- "game"
  screen_tag <- "Screen"
  nextscreen_tag <- "nextScreen"
  branch_tag <- "branch"
  option_tag <- "option"
  
  # create output file
  SESSION_STAMP = format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(project_name, "_", SESSION_STAMP, ".xml")
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # LOAD DATA (read all sheets from Excel workbook)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # load files / worksheets
  dolphin <- read_excel(input_file, sheet = "Dolphin")
  launch <- read_excel(input_file, sheet = "Launch")
  sensors <- read_excel(input_file, sheet = "Sensors")
  settings <- read_excel(input_file, sheet = "Settings")
  menu <- read_excel(input_file, sheet = "Menu")
  pack <- read_excel(input_file, sheet = "Pack")
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # DATA VALIDATION
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  ARE_ALL_PACK_ELEMENTS_UNIQUE = length(unique(pack$element_id)) == nrow(pack)
  print(paste0("TEST: Are all pack elements unique? | RESULT: ", ARE_ALL_PACK_ELEMENTS_UNIQUE))
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # RUN STEPS TO BUILD XML
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # step 1) build launch tag
  doc_root <- build_launch_tag(dolphin, launch, sensors, settings, root_tag, launch_tag, sensor_tag, settings_tag)
  
  # step 2) build menu tag
  doc_root_menu <- build_menu_tag(doc_root, menu, menu_tag, title_tag, element_tag, action_tag)
  
  # step 3) build pack
  doc_root_menu_pack <- build_pack_tag(doc_root, pack, pack_tag, game_tag, screen_tag, branch_tag, nextscreen_tag, element_tag, option_tag)
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # VIEW FINAL XML
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  if(verbose){
    print(doc_root_menu_pack)
  }
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # SAVE XML TO FILE
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  saveXML(doc_root_menu_pack, file=output_file)
}