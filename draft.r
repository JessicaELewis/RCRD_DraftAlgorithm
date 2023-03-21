# Run these first
#install.packages("matchingR")
#library(matchingR)
#install.packages("googlesheets4")
#library(googlesheets4)


gsheet_connect <- function() {
  read_sheet("https://docs.google.com/spreadsheets/d/1M39feSDV1giAlA9kAhXx1RtWpmyKGUJZkNq6jAFXdkI/edit?usp=sharing", range = "DRAFT_SK!C1:E27")
}

import_player_prefs <- function(pp_sheet, pp_range) {
  # URL of skater preferences spreadsheet (from the sharing screen) and the range of cells (only the cells with numbers, ignore timestamps and names)
  playerpref <- read_sheet(pp_sheet, range = pp_range)
  
  return(playerpref)
}

import_team_prefs <- function(tp_sheet, tp_range) {
  # URL of team preferences spreadsheet (from the sharing screen) and the range of cells (only the cells with numbers, ignore timestamps and names)
  teampref <- read_sheet(tp_sheet, range =tp_range)
  
  return(teampref)
}

set_vars <- function() {
  # Open slots in the same order as teams submitted their preferences in the team preferences spreadsheet
  slots <- c(11,12,7)
  players <- t(playerpref)
  teams <- t(teampref)
}

get_draft_results <- function(teamPref, skaterPref, slots) {
  results = galeShapley.collegeAdmissions(studentPref = skaterPref,
                                          collegePref = teamPref,
                                          slots = slots,
                                          studentOptimal = TRUE)
  return(results)
}

clean_up_results <- function(results, spreadsheet, player_names_range){
  colnames(results) <- c("team")
  res = transform(results, team = factor(team, levels = c(1,2,3), labels = c("DLF", "GD", "TR")))
  
  # URL same as skater preferences, but range includes ONLY the names
  skaters = read_sheet(spreadsheet, range = player_names_range)
  
  teams = res[,"team"]
  final <- data.frame('skater' = skaters, 'team' = teams)
  
  dlf = final[final$team == 'DLF',]
  gd = final[final$team == 'GD',]
  tr = final[final$team == 'TR',]
  
  return(final)
}

get_results <- function(spreadsheet, player_range, team_range, capacity, player_names_range) {
  playerpref = import_player_prefs(spreadsheet, player_range)
  teampref = import_team_prefs(spreadsheet, team_range)
  
  players <- t(playerpref)
  teams <- t(teampref)
  
  results = get_draft_results(teams, players, capacity)
  results = results[[4]]
  
  cleaned <- clean_up_results(results, spreadsheet, player_names_range)
  
  cleaned
}

gsheet_connect()
get_results(
  "https://docs.google.com/spreadsheets/d/1M39feSDV1giAlA9kAhXx1RtWpmyKGUJZkNq6jAFXdkI/edit?usp=sharing", 
  "DRAFT_SK!C1:E27", 
  "DRAFT_HT!C1:AB4", 
  c(11,12,7),
  "DRAFT_SK!B1:B27")

