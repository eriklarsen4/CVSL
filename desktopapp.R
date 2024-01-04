

# load libraries ----
# install.packages(c("shiny", "ggplot2", "DT", "baseballr", "tidyverse", "here", "rsconnect"))
# library(shiny)
# library(ggplot2)
# library(ggExtra)
library(DT)
library(baseballr)
library(tidyverse)

library(here)

library(rsconnect)

load("CompleteUniversesAndLeaderboards.RData") # for deployment

# load(
#   here("GitHub", "CVSL", "CompleteUniversesAndLeaderboards.RData")
# )
# 
# load(
#   here("CompleteUniversesAndLeaderboards.RData")
# ) # for development

# Add continuous age, universe status for Player dfs ----

Player_Batting <- Player_Batting %>%
  dplyr::mutate(key_bbref = bbref_id) %>%
  left_join(CHAD_LU_2, by = "key_bbref") %>%
  dplyr::rename(Name = Name.x) %>%
  dplyr::mutate(Age_continuous = case_when(!is.na(bday) ~ as.numeric(difftime(as.Date(paste(Year, "-07-01", sep = "")), as.Date(bday), units = "days"))/ 365.25,
                                           TRUE ~ NA_real_)) %>%
  dplyr::select(-bday, -contains(".y")) %>%
  dplyr::mutate(across(c(11,12,14:48), as.numeric)) %>%
  dplyr::select(-49)

Player_Pitching <- Player_Pitching  %>%
  dplyr::mutate(key_bbref = bbref_id) %>%
  left_join(CHAD_LU_2, by = "key_bbref") %>%
  dplyr::rename(Name = Name.x) %>%
  dplyr::mutate(Age_continuous = case_when(!is.na(bday) ~ as.numeric(difftime(as.Date(paste(Year, "-07-01", sep = "")), as.Date(bday), units = "days"))/ 365.25,
                                           TRUE ~ NA_real_)) %>%
  dplyr::select(-bday, -contains(".y")) %>%
  dplyr::mutate(across(c(11:54,59), as.numeric)) %>%
  dplyr::select(-RA9extras, -Acquired, -Salary, -key_bbref) %>%
  dplyr::relocate(bWAR, .after = Traded) %>%
  dplyr::relocate(RAR, .after = bWAR) %>%
  dplyr::select(-49)

CVSL_universe = c("SFG", "LAD", "ARI", "COL", "SDP",
                  "LAA", "OAK", "TEX",
                  "STL",
                  "MIN", "CLE",
                  "NYM", "ATL", "PHI", "MIA", "WSN",
                  "NYY", "TBR", "TOR", "BOS")

Player_Pitching <- Player_Pitching %>%
  dplyr::mutate(in_universe = case_when(Tm %in% CVSL_universe == T ~ 1,
                                        TRUE ~ 0),
                Role = case_when(as.numeric(GS) == as.numeric(G) &
                                   as.numeric(GS) >= 8 ~ 1,
                                 as.numeric(GS) != as.numeric(G) &
                                   as.numeric(GS) < 8 ~ 2,
                                 as.numeric(GS) == 0 ~ 2,
                                 TRUE ~ 3),
                Role = case_when(as.character(Role) == 1 ~ "SP",
                                 as.character(Role) == 2 ~ "RP",
                                 as.character(Role) == 3 ~ "SP RP"))

Player_Batting <- Player_Batting %>%
  dplyr::mutate(in_universe = case_when(Tm %in% CVSL_universe == T ~ 1,
                                        TRUE ~ 0),
                CFs = case_when(grepl(Positions, pattern = "(CF)") ~ "1",
                                TRUE ~ "0"),
                Position_Group = case_when(CFs == "1" &
                                             Positions == "CF" ~ "CF",
                                           CFs == "1" &
                                             (grepl(Positions, pattern = "L|R") &
                                                !grepl(Positions, pattern = "B")) ~ "OF",
                                           CFs == "1" &
                                             (grepl(Positions, pattern = "B") |
                                                grepl(Positions, pattern = "SS")) ~ "UTIL",
                                           CFs == "0" &
                                             grepl(Positions, pattern = "C") ~ "C",
                                           CFs == "0" &
                                             Positions == "SS" ~ "SS",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "SS") |
                                                grepl(Positions, pattern = "2B") &
                                                !grepl(Positions, pattern = "F") &
                                                !grepl(Positions, pattern = "1B")) ~ "MIF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "3B") |
                                                grepl(Positions, pattern = "1B") &
                                                !grepl(Positions, pattern = "F")) ~ "CIF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "F") &
                                                !grepl(Positions, pattern = "B")) ~ "COF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "B") &
                                                grepl(Positions, pattern = "F")) ~ "UTIL",
                                           CFs == "0" &
                                             grepl(Positions, pattern = "C") ~ "C",
                                           TRUE ~ Positions)) %>%
  dplyr::select(-CFs)

DRAFTS_hitters <- DRAFTS_hitters %>%
  dplyr::mutate(CFs = case_when(grepl(Positions, pattern = "(CF)") ~ "1",
                                TRUE ~ "0"),
                Position_Group = case_when(CFs == "1" &
                                             Positions == "CF" ~ "CF",
                                           CFs == "1" &
                                             (grepl(Positions, pattern = "L|R") &
                                                !grepl(Positions, pattern = "B")) ~ "OF",
                                           CFs == "1" &
                                             (grepl(Positions, pattern = "B") |
                                                grepl(Positions, pattern = "SS")) ~ "UTIL",
                                           CFs == "0" &
                                             grepl(Positions, pattern = "C") ~ "C",
                                           CFs == "0" &
                                             Positions == "SS" ~ "SS",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "SS") |
                                                grepl(Positions, pattern = "2B") &
                                                !grepl(Positions, pattern = "F") &
                                                !grepl(Positions, pattern = "1B")) ~ "MIF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "3B") |
                                                grepl(Positions, pattern = "1B") &
                                                !grepl(Positions, pattern = "F")) ~ "CIF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "F") &
                                                !grepl(Positions, pattern = "B")) ~ "COF",
                                           CFs == "0" &
                                             (grepl(Positions, pattern = "B") &
                                                grepl(Positions, pattern = "F")) ~ "UTIL",
                                           CFs == "0" &
                                             grepl(Positions, pattern = "C") ~ "C",
                                           TRUE ~ Positions)) %>%
  dplyr::select(-CFs)

DRAFTS_pitchers <- DRAFTS_pitchers %>%
  dplyr::mutate(Role = case_when(as.numeric(GS) == as.numeric(G) &
                                   as.numeric(GS) >= 8 ~ 1,
                                 as.numeric(GS) != as.numeric(G) &
                                   as.numeric(GS) < 8 ~ 2,
                                 as.numeric(GS) == 0 ~ 2,
                                 TRUE ~ 3),
                Role = case_when(as.character(Role) == 1 ~ "SP",
                                 as.character(Role) == 2 ~ "RP",
                                 as.character(Role) == 3 ~ "SP RP"))

# ui object ----

ui <- navbarPage(
  
  title = "CVSL Draft Tool",
  theme = bslib::bs_theme(bootswatch = "spacelab"),#, bg = "black", fg = "skyblue", primary = "skyblue", secondary = "gray",
  # ),
  inverse = TRUE,
  
  
  
  navbarMenu("Hitter Season Totals",
             tabPanel("Table", sidebarLayout(sidebarPanel = sidebarPanel(checkboxInput(inputId = "hitCVSLThresholdReqTab",
                                                                                       label = span(strong("CVSL min. AB Filter"), "(100 for C; 175 for non-C)"),
                                                                                       value = F),
                                                                         checkboxInput(inputId = "hitUniverseFilterTab",
                                                                                       label = strong("CVSL-Eligible MLB Team Filter"), value = F),
                                                                         checkboxGroupInput(inputId = "hitcolumns",
                                                                                            label = strong("Choose Additional Columns:"),
                                                                                            choices = c("Counting Stats", "Traditional Metrics", "Advanced Metrics"),
                                                                                            selected = NULL),
                                                                         downloadButton(outputId = "DownloadHitterSeasonTable",
                                                                                        label = strong("Download Table as .csv")
                                                                                        ),
                                                                         width = 3),
                                             mainPanel = dataTableOutput("HittersAgg"),
                                             position = "left")
                      ),
             tabPanel("Scatter Plot", sidebarLayout(
                                                     sidebarPanel = sidebarPanel(em("(click and drag",br(em("on plot for data)"))),
                                                                                 varSelectInput("MLBhitxvar",
                                                                                                p(strong("X variable"), "(choose one)", strong(":")),
                                                                                                Player_Batting,
                                                                                                selected = "Age_continuous"),
                                                                                 varSelectInput("MLBhityvar",
                                                                                                p(strong("Y variable"), "(choose one)", strong(":")),
                                                                                                Player_Batting,
                                                                                                selected = "bWAR"),
                                                                                 p(checkboxInput(inputId = "hitUniverseFilterPlot", label = strong("CVSL-Eligible MLB Team Filter"), value = F)),
                                                                                 p(checkboxInput(inputId = "hitEligible", label = strong("Min. AB req. Filter"), value = F)),
                                                                                 sliderInput(inputId = "MLBAB", label = strong("AB"), min = 0, max = 800, value = c(100,800), step = 1),
                                                                                 checkboxGroupInput(inputId = "MLBhitby_Year",
                                                                                                    label = p(strong("Filter by Year"), "(choose any)", strong(":")),
                                                                                                    choices = c(sort(unique(as.character(Player_Batting$Year)))),
                                                                                                    selected = c(sort(unique(as.character(Player_Batting$Year))))),
                                                                                 checkboxGroupInput(inputId = "MLBhitcolor",
                                                                                                    label = p(strong("Color Variable"), "(choose one)", strong(":")),
                                                                                                    choices = c(colnames(Player_Batting)[-c(2,3,4,5,8,11:50)]),
                                                                                                    selected = NULL),
                                                                                 checkboxGroupInput(inputId = "MLBhitgroupby",
                                                                                                    label = p(strong("Group By"),
                                                                                                              "(choose 2 or fewer)",
                                                                                                              strong(":"),
                                                                                                              br(em("(Grouping Variable overrides Color)"))),
                                                                                                    choices = c("Year", "Bats", "Position_Group"),
                                                                                                    selected = NULL),
                                                                                 width = 3
                                                                                 ),
                                                     mainPanel = mainPanel(
                                                                            plotOutput("MLBHit_scatter",
                                                                                       brush = brushOpts("hitter_mlb_season_brush",resetOnNew = T),
                                                                                       width = 6),
                                                                            verbatimTextOutput("hitter_mlb_season_brush_info")
                                                     ),
                                                     position = "left")
                      )
             ),
  
  navbarMenu("Pitcher Season Totals",
             tabPanel("Table", sidebarLayout(sidebarPanel = sidebarPanel(checkboxInput(inputId = "pitCVSLThresholdReqTab",
                                                                                       label = span(strong("CVSL min. IP Filter"), "(30 IP, 8 GS)"),
                                                                                       value = F),
                                                                         checkboxInput(inputId = "pitUniverseFilterTab",
                                                                                       label = strong("CVSL-Eligible MLB Team Filter"), value = F),
                                                                         checkboxGroupInput(inputId = "pitcolumns",
                                                                                            label = strong("Choose Additional Columns:"),
                                                                                            choices = c("Counting Stats", "Traditional Metrics", "Advanced Metrics"),
                                                                                            selected = NULL),
                                                                         downloadButton(outputId = "DownloadPitcherSeasonTable",
                                                                                        label = strong("Download Table as .csv")
                                                                                        ),
                                                                         width = 3),
                                             mainPanel = dataTableOutput("PitchersAgg"),
                                             position = "left")
             ),
             tabPanel("Scatter Plot", sidebarLayout(
                                                     sidebarPanel = sidebarPanel(em("(click and drag",br(em("on plot for data)"))),
                                                                                 varSelectInput("MLBpitxvar",
                                                                                                p(strong("X variable"), "(choose one)", strong(":")),
                                                                                                Player_Batting,
                                                                                                selected = "Age_continuous"),
                                                                                 varSelectInput("MLBpityvar",
                                                                                                p(strong("Y variable"), "(choose one)", strong(":")),
                                                                                                Player_Batting,
                                                                                                selected = "bWAR"),
                                                                                 p(checkboxInput(inputId = "pitUniverseFilterPlot", label = strong("CVSL-Eligible MLB Team Filter"), value = F)),
                                                                                 p(checkboxInput(inputId = "pitEligible", label = strong("Min. IP req. Filter"), value = F)),
                                                                                 sliderInput(inputId = "MLBIP", label = strong("IP"), min = 0, max = 240, value = c(30,240), step = 0.1),
                                                                                 checkboxGroupInput(inputId = "MLBpitby_Year",
                                                                                                    label = p(strong("Filter by Year"), "(choose any)", strong(":")),
                                                                                                    choices = c(sort(unique(as.character(Player_Pitching$Year)))),
                                                                                                    selected = c(sort(unique(as.character(Player_Pitching$Year))))),
                                                                                 checkboxGroupInput(inputId = "MLBpitcolor",
                                                                                                    label = p(strong("Color Variable"), "(choose one)", strong(":")),
                                                                                                    choices = c(colnames(Player_Pitching)[c(1,6,7,8,10,56)]),
                                                                                                    selected = NULL),
                                                                                 checkboxGroupInput(inputId = "MLBpitgroupby",
                                                                                                    label = p(strong("Group By"),
                                                                                                              "(choose 2 or fewer)",
                                                                                                              strong(":"),
                                                                                                              br(em("(Grouping Variable overrides Color)"))),
                                                                                                    choices = c("Year", "Throws", "Role"),
                                                                                                    selected = NULL),
                                                                                 
                                                                                 
                                                                                 width = 3
                                                                               ),
                                                     mainPanel = mainPanel(
                                                                           plotOutput("MLBPit_scatter",
                                                                                      brush = brushOpts("pitcher_mlb_season_brush",resetOnNew = T),
                                                                                      width = 6),
                                                                           verbatimTextOutput("pitcher_mlb_season_brush_info")
                                                     ),
                                                     position = "left")
                                                   )
  ),
  
  navbarMenu("2023 Season Splits",
             
             tabPanel("Hitters vLHP", sidebarLayout(sidebarPanel = sidebarPanel(downloadButton(outputId = "DownloadHvLHP",
                                                                                               label = strong("Download Table as .csv")
                                                                                               ),
                                                                                width = 3),
                                                    mainPanel = dataTableOutput("HitterSplitsvLHP", width = 6),
                                                    position = "left") 
                      
                      ),
             tabPanel("Hitters vRHP", sidebarLayout(sidebarPanel = sidebarPanel(downloadButton(outputId = "DownloadHvRHP",
                                                                                               label = strong("Download Table as .csv")
                                                                                               ),
                                                                                width = 3),
                                                    mainPanel = dataTableOutput("HitterSplitsvRHP", width = 6),
                                                    position = "left")
                      ),
             tabPanel("Pitcher vLHB", sidebarLayout(sidebarPanel = sidebarPanel(downloadButton(outputId = "DownloadPvLHB",
                                                                                               label = strong("Download Table as .csv")
                                                                                               ),
                                                                                width = 3),
                                                    mainPanel = dataTableOutput("PitcherSplitsvLHB", width = 6),
                                                    position = "left")
                      ),
             tabPanel("Pitcher vRHB", sidebarLayout(sidebarPanel = sidebarPanel(downloadButton(outputId = "DownloadPvRHB",
                                                                                               label = strong("Download Table as .csv")
                                                                                               ),
                                                                                width = 3),
                                                    mainPanel = dataTableOutput("PitcherSplitsvRHB", width = 6),
                                                    position = "left")
                      )
             ),

  navbarMenu("CVSL Draft History", 
             
             tabPanel("Draft History, Hitters", sidebarLayout(

                                                               sidebarPanel = sidebarPanel(em("(click and drag",br(em("on plot for data)"))),
                                                                                           varSelectInput("hitxvar",
                                                                                                          p(strong("X variable"), "(choose one)", strong(":")),
                                                                                                          DRAFTS_hitters,
                                                                                                          selected = "bWAR"),
                                                                                           varSelectInput("hityvar",
                                                                                                          p(strong("Y variable"), "(choose one)", strong(":")),
                                                                                                          DRAFTS_hitters,
                                                                                                          selected = "Salary"),
                                                                                           checkboxGroupInput(inputId = "hitteam",
                                                                                                              label = p(strong("Filter by CVSL Team"), "(choose at least 1)", strong(":")),
                                                                                                              choices = c(sort(unique(DRAFTS_hitters$CVSLTeam))),
                                                                                                              selected = c(sort(unique(DRAFTS_hitters$CVSLTeam)))),
                                                                                           checkboxGroupInput(inputId = "hitby_Year",
                                                                                                              label = p(strong("Filter by Draft Year"), "(choose at least 1)", strong(":")),
                                                                                                              choices = c(as.character(unique(DRAFTS_hitters$`Draft Year`))),
                                                                                                              selected = c(as.character(unique(DRAFTS_hitters$`Draft Year`)))),
                                                                                           checkboxGroupInput(inputId = "hitGroup", label = p(strong("Group By"), "(choose 1 when not grouped by team):"),
                                                                                                              choices = c(colnames(DRAFTS_hitters)[c(9,13,54)]),
                                                                                                              selected = NULL),
                                                                                           checkboxInput("hitgroupbyteam",
                                                                                                         label = p(strong("Group By CVSL Team"), br(em("(select for separate panels)"))),
                                                                                                         value = F),
                                                                                           checkboxInput("hitshowfit",
                                                                                                         label = p(strong("Show Trend Line"), br(em("(select to add when"),br(em("not grouped by team)")))),
                                                                                                         value = F),
                                                                                           width = 3),

                                                               mainPanel = mainPanel(
                                                                                     plotOutput("draft_h",
                                                                                                brush = brushOpts("draft_hitter_brush",resetOnNew = T),
                                                                                                width = 6),
                                                                                     verbatimTextOutput("draft_hitter_brush_info")
                                                                                   ),
                                                               position = "left" 
                                                
                                                             )
             
             ),
             tabPanel("Draft History, Pitchers", sidebarLayout(
               
                                                               sidebarPanel = sidebarPanel(em("(click and drag",br(em("on plot for data)"))),
                                                                                           varSelectInput("xvar",
                                                                                                          p(strong("X variable"), "(choose one)", strong(":")),
                                                                                                          DRAFTS_pitchers,
                                                                                                          selected = "bWAR"),
                                                                                           varSelectInput("yvar",
                                                                                                          p(strong("Y variable"), "(choose one)", strong(":")),
                                                                                                          DRAFTS_pitchers,
                                                                                                          selected = "Salary"),
                                                                                           checkboxGroupInput(inputId = "team",
                                                                                                              label = p(strong("Filter by CVSL Team"), "(choose at least 1)", strong(":")),
                                                                                                              choices = c(sort(unique(DRAFTS_pitchers$CVSLTeam))),
                                                                                                              selected = c(sort(unique(DRAFTS_pitchers$CVSLTeam)))),
                                                                                           checkboxGroupInput(inputId = "by_Year",
                                                                                                              label = p(strong("Filter by Draft Year"), "(choose at least 1)", strong(":")),
                                                                                                              choices = c(as.character(unique(DRAFTS_pitchers$`Draft Year`))),
                                                                                                              selected = c(as.character(unique(DRAFTS_pitchers$`Draft Year`)))),
                                                                                           checkboxGroupInput(inputId = "pitGroup", label = p(strong("Group By"), "(choose 1 when not grouped by team):"),
                                                                                                               choices = c(colnames(DRAFTS_pitchers)[c(10,9,57)]),
                                                                                                               selected = NULL),
                                                                                           checkboxInput("groupbyteam",
                                                                                                         label = p(strong("Group By CVSL Team"), br(em("(select for separate panels)"))),
                                                                                                         value = F),
                                                                                           checkboxInput("showfit",
                                                                                                         label = p(strong("Show Trend Line"), br(em("(select to add when"),br(em("not grouped by team)")))),
                                                                                                         value = F),
                                                                                           width = 3),
                                                               
                                                               mainPanel = mainPanel(
                                                                                       plotOutput("draft_p",
                                                                                                  brush = brushOpts("draft_pitcher_brush",resetOnNew = T),
                                                                                                  width = 6),
                                                                                       verbatimTextOutput("draft_pitcher_brush_info")
                                                                                     ),
                                                               
                                                               position = "left" 
                                                               
                                                             )
             
             )
  ),
  
  navbarMenu("Projections", 
             tabPanel("Hitters", h1("Coming Soon")),
             tabPanel("Pitchers", h1("Coming Soon"))),
  
  navbarMenu("Glossary",
             
             tabPanel("Hitter Metric Term Definitions", h1("Coming Soon")),
             tabPanel("Pitcher Metric Term Definitions", h1("Coming Soon")))
  
  
)

# server logic ----

server <- function(input, output, session) {

  storeWarn = getOption("warn")
  options(warn = -1)
  
  output$HittersAgg = renderDataTable(hitters_table_df(), filter = "top")
  output$PitchersAgg = renderDataTable(pitchers_table_df(), filter = "top")
  
  
  output$PitcherSplitsvLHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "LHB")) %>% dplyr::filter(Year == 2023), filter = "top")
  output$PitcherSplitsvRHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "RHB")) %>% dplyr::filter(Year == 2023), filter = "top")
  output$HitterSplitsvLHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "LHP")) %>% dplyr::filter(Year == 2023), filter = "top")
  output$HitterSplitsvRHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "RHP")) %>% dplyr::filter(Year == 2023), filter = "top")
  
  
  
  # DRAFTS_hitters = DRAFTS_hitters %>%
  #   dplyr::mutate(teamcolors = case_when(CVSLTeam == "Arsenal" ~ "darkgoldenrod3",
  #                                        CVSLTeam == "Bison" ~ "chocolate",
  #                                        CVSLTeam == "Cattlemen" ~ "black",
  #                                        CVSLTeam == "Ducks" ~ "forestgreen",
  #                                        CVSLTeam == "Matadors" ~ "firebrick",
  #                                        CVSLTeam == "Nuggets" ~ "orange",
  #                                        CVSLTeam == "Phillies" ~ "gray48",
  #                                        CVSLTeam == "Renegades" ~ "red",
  #                                        CVSLTeam == "Twins" ~ "navy",
  #                                        CVSLTeam == "Wombats" ~ "skyblue"
  #   ))
  # 
  # DRAFTS_pitchers = DRAFTS_pitchers %>%
  #   dplyr::mutate(teamcolors = case_when(CVSLTeam == "Arsenal" ~ "darkgoldenrod3",
  #                                        CVSLTeam == "Bison" ~ "chocolate",
  #                                        CVSLTeam == "Cattlemen" ~ "black",
  #                                        CVSLTeam == "Ducks" ~ "forestgreen",
  #                                        CVSLTeam == "Matadors" ~ "firebrick",
  #                                        CVSLTeam == "Nuggets" ~ "orange",
  #                                        CVSLTeam == "Phillies" ~ "gray48",
  #                                        CVSLTeam == "Renegades" ~ "red",
  #                                        CVSLTeam == "Twins" ~ "navy",
  #                                        CVSLTeam == "Wombats" ~ "skyblue"
  #   ))
  
  

  draft_hitters_teams_df <- reactive({


      df <- DRAFTS_hitters %>%
        dplyr::filter(CVSLTeam %in% sort(unlist(input$hitteam)[c(1:10)]))


      df2 <- df %>%
        dplyr::filter(as.character(`Draft Year`) %in% input$hitby_Year)


      df2

  })

  draft_pitchers_teams_df <- reactive({
    
    
    df3 <- DRAFTS_pitchers %>%
      dplyr::filter(CVSLTeam %in% sort(unlist(input$hitteam)[c(1:10)]))
    
    
    df4 <- df3 %>%
      dplyr::filter(as.character(`Draft Year`) %in% input$by_Year)
    
    
    df4
    
  })
  
  hitters_plot_df <- reactive({
    
    if (input$hitUniverseFilterPlot == F) {
      
      if (input$hitEligible == F) {
        
        df <- Player_Batting %>%
          dplyr::filter(as.character(Year) %in% input$MLBhitby_Year) %>%
          dplyr::filter(between(PA, input$MLBAB[1], input$MLBAB[2])) %>%
          dplyr::select(-in_universe)
        
      } else if (input$hitEligible == T) {
        
        df <- Player_Batting %>%
          dplyr::filter(as.character(Year) %in% input$MLBhitby_Year) %>%
          dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                TRUE ~ "1"),
                        c_ab_filter = case_when(c_ab_filter == "0" &
                                                  grepl(Positions, pattern = "C") ~ "C",
                                                TRUE ~ c_ab_filter),
                        c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                TRUE ~ 1),
                        other_ab_filter = case_when(c_ab_filter == 1 &
                                                      as.numeric(AB) >= 100 ~ 1,
                                                    c_ab_filter == 1 &
                                                      as.numeric(AB) < 100 ~ 0,
                                                    c_ab_filter == 0 &
                                                      as.numeric(AB) >= 175 ~ 1,
                                                    c_ab_filter == 0 &
                                                      as.numeric(AB) < 175 ~ 0)) %>%
          dplyr::filter(between(PA, input$MLBAB[1], input$MLBAB[2])) %>%
          dplyr::filter(other_ab_filter == 1) %>%
          dplyr::select(-c_ab_filter, -other_ab_filter, -in_universe)
        
      }
      
    } else if (input$hitUniverseFilterPlot == T) {
      
      if (input$hitEligible == F) {
        
        df <- Player_Batting %>%
          dplyr::filter(as.character(Year) %in% input$MLBhitby_Year) %>%
          dplyr::filter(between(PA, input$MLBAB[1], input$MLBAB[2])) %>%
          dplyr::filter(in_universe == 1) %>%
          dplyr::select(-in_universe)
        
      } else if (input$hitEligible == T) {
        
        df <- Player_Batting %>%
          dplyr::filter(as.character(Year) %in% input$MLBhitby_Year) %>%
          dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                TRUE ~ "1"),
                        c_ab_filter = case_when(c_ab_filter == "0" &
                                                  grepl(Positions, pattern = "C") ~ "C",
                                                TRUE ~ c_ab_filter),
                        c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                TRUE ~ 1),
                        other_ab_filter = case_when(c_ab_filter == 1 &
                                                      as.numeric(AB) >= 100 ~ 1,
                                                    c_ab_filter == 1 &
                                                      as.numeric(AB) < 100 ~ 0,
                                                    c_ab_filter == 0 &
                                                      as.numeric(AB) >= 175 ~ 1,
                                                    c_ab_filter == 0 &
                                                      as.numeric(AB) < 175 ~ 0)) %>%
          dplyr::filter(between(PA, input$MLBAB[1], input$MLBAB[2])) %>%
          dplyr::filter(other_ab_filter == 1) %>%
          dplyr::filter(in_universe == 1) %>%
          dplyr::select(-c_ab_filter, -other_ab_filter, -in_universe)
        
      }
      
    }
    
    df
    
  })
  
  pitchers_plot_df <- reactive({
    
    
    if ( input$pitUniverseFilterPlot == F) {
      
      if ( input$pitEligible == F) {
        
        df <- Player_Pitching %>%
          dplyr::filter(as.character(Year) %in% input$MLBpitby_Year) %>%
          dplyr::filter(between(IP, input$MLBIP[1], input$MLBIP[2])) %>%
          dplyr::select(-in_universe)
        
      } else if (input$pitEligible == T){
        
        df <- Player_Pitching %>%
          dplyr::filter(as.character(Year) %in% input$MLBpitby_Year) %>%
          dplyr::filter(between(IP, input$MLBIP[1], input$MLBIP[2])) %>%
          dplyr::mutate(filly = case_when(Role == "SP" &
                                            IP >= 30 & GS >= 8 ~ 1,
                                          Role == "RP" &
                                            IP >= 30 ~ 1,
                                          Role == "SP RP" &
                                            IP >= 30 ~ 1,
                                          TRUE ~ 0)) %>%
          dplyr::filter(filly == 1) %>%
          dplyr::select(-in_universe, -filly)
      }
      } 
    
    else if (input$pitUniverseFilterPlot == T) {
      
      if (input$pitEligible == F) {
        
        df <- Player_Pitching %>%
          dplyr::filter(as.character(Year) %in% input$MLBpitby_Year) %>%
          dplyr::filter(between(IP, input$MLBIP[1], input$MLBIP[2])) %>%
          dplyr::filter(in_universe == 1) %>%
          dplyr::select(-in_universe)
        
      } else if (input$pitEligible == T) {
        
        df <- Player_Pitching %>%
          dplyr::filter(as.character(Year) %in% input$MLBpitby_Year) %>%
          dplyr::filter(between(IP, input$MLBIP[1], input$MLBIP[2])) %>%
          dplyr::filter(in_universe == 1) %>%
          dplyr::mutate(filly = case_when(Role == "SP" &
                                            IP >= 30 & GS >= 8 ~ 1,
                                          Role == "RP" &
                                            IP >= 30 ~ 1,
                                          Role == "SP RP" &
                                            IP >= 30 ~ 1,
                                          TRUE ~ 0)) %>%
          dplyr::filter(filly == 1) %>%
          dplyr::select(-in_universe, -filly)
        
      }
      
    }
      
    df
    
  })
  
  hitters_table_df <- reactive({

    
    hitcols = input$hitcolumns
    
    
    if (is.null(input$hitcolumns)) {
      
      if (input$hitCVSLThresholdReqTab == F) {
        
        if (input$hitUniverseFilterTab == F) {
          
          df <- Player_Batting %>%
            dplyr::select(c(1:13))
          
        } else if (input$hitUniverseFilterTab == T) {
          
          df <- Player_Batting %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:13))
          
        }        
        
        } 
      
      else if (input$hitCVSLThresholdReqTab == T) {
          
          if (input$hitUniverseFilterTab == F) {
            
            df<- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                       grepl(Positions, pattern = "C") ~ "C",
                                                     TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                     TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(c(1:13))
            
          } else if (input$hitUniverseFilterTab == T) {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(c(1:13))
          
        }
        }
      }
    
    else if (length(hitcols) == 1){
      
      if (input$hitCVSLThresholdReqTab == F) {
        
        if (input$hitUniverseFilterTab == F) {
          
          if (hitcols == "Counting Stats") {
            
            df <- Player_Batting %>%
              dplyr::select(c(1:13), c(14:26), c(32:37))
            
          } else if (hitcols == "Traditional Metrics") {
            
            df <- Player_Batting %>%
              dplyr::select(c(1:13), c(27:31))
            
          } else if (hitcols == "Advanced Metrics") {
            
            df <- Player_Batting %>%
              dplyr::select(c(1:13), c(38:48))
            
          }
          
        } else if (input$hitUniverseFilterTab == T) {
          
          if (hitcols == "Counting Stats") {
            
            df <- Player_Batting %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(14:26), c(32:37))
            
          } else if (hitcols == "Traditional Metrics") {
            
            df <- Player_Batting %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(27:31))
            
          } else if (hitcols == "Advanced Metrics") {
            
            df <- Player_Batting %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(38:48))
            
          }
          
        }
        
        } else if (input$hitCVSLThresholdReqTab == T) {
          
          if (input$hitUniverseFilterTab == F) {
            
            if (hitcols == "Counting Stats") {
            
              df <- Player_Batting %>%
                dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                      TRUE ~ "1"),
                              c_ab_filter = case_when(c_ab_filter == "0" &
                                                        grepl(Positions, pattern = "C") ~ "C",
                                                      TRUE ~ c_ab_filter),
                              c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                      TRUE ~ 1),
                              other_ab_filter = case_when(c_ab_filter == 1 &
                                                            as.numeric(AB) >= 100 ~ 1,
                                                          c_ab_filter == 1 &
                                                            as.numeric(AB) < 100 ~ 0,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) >= 175 ~ 1,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) < 175 ~ 0)) %>%
                dplyr::filter(other_ab_filter == 1) %>%
                dplyr::select(-c_ab_filter, -other_ab_filter) %>%
                dplyr::select(c(1:13), c(14:26), c(32:37))
            
          } else if (hitcols == "Traditional Metrics") {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::select(c(1:13), c(27:31))
            
          } else if (hitcols == "Advanced Metrics") {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::select(c(1:13), c(38:48))
            
          }
          
        } else if (input$hitUniverseFilterTab == T) {
          
          if (hitcols == "Counting Stats") {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(14:26), c(32:37))
            
          } else if (hitcols == "Traditional Metrics") {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(27:31))
            
          } else if (hitcols == "Advanced Metrics") {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(38:48))
            
          }
          
        }
      
      }

    } else if (length(hitcols) == 2) {
      
      if (input$hitCVSLThresholdReqTab == F) {
        
        if (input$hitUniverseFilterTab == F) {
          
          if ("Counting Stats" %in% hitcols) {
            
            if ("Traditional Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                
                dplyr::select(c(1:13), c(14:37))
              
            } else if ("Advanced Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::select(c(1:13), c(14:26), c(32:37), c(38:48))
              
            }
            
          } else if ("Traditional Metrics" %in% hitcols) {
            
            df <- Player_Batting %>%
              dplyr::select(c(1:13), c(27:31), c(38:48))
            
          } 
          
        } else if (input$hitUniverseFilterTab == T) {
          
          if ("Counting Stats" %in% hitcols) {
            
            if ("Traditional Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:13), c(14:37))
              
            } else if ("Advanced Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:13), c(14:26), c(32:37), c(38:48))
              
            }
            
          } else if ("Traditional Metrics" %in% hitcols) {
            
            df <- Player_Batting %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(27:31), c(38:48))
            
          } 
          
        }
        
        
        
      } else if (input$hitCVSLThresholdReqTab == T) {
        
        if (input$hitUniverseFilterTab == F) {
          
          if ("Counting Stats" %in% hitcols) {
            
            if ("Traditional Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                      TRUE ~ "1"),
                              c_ab_filter = case_when(c_ab_filter == "0" &
                                                        grepl(Positions, pattern = "C") ~ "C",
                                                      TRUE ~ c_ab_filter),
                              c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                      TRUE ~ 1),
                              other_ab_filter = case_when(c_ab_filter == 1 &
                                                            as.numeric(AB) >= 100 ~ 1,
                                                          c_ab_filter == 1 &
                                                            as.numeric(AB) < 100 ~ 0,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) >= 175 ~ 1,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) < 175 ~ 0)) %>%
                dplyr::filter(other_ab_filter == 1) %>%
                dplyr::select(-c_ab_filter, -other_ab_filter) %>%
                dplyr::select(c(1:13), c(14:37))
              
            } else if ("Advanced Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                      TRUE ~ "1"),
                              c_ab_filter = case_when(c_ab_filter == "0" &
                                                        grepl(Positions, pattern = "C") ~ "C",
                                                      TRUE ~ c_ab_filter),
                              c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                      TRUE ~ 1),
                              other_ab_filter = case_when(c_ab_filter == 1 &
                                                            as.numeric(AB) >= 100 ~ 1,
                                                          c_ab_filter == 1 &
                                                            as.numeric(AB) < 100 ~ 0,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) >= 175 ~ 1,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) < 175 ~ 0)) %>%
                dplyr::filter(other_ab_filter == 1) %>%
                dplyr::select(-c_ab_filter, -other_ab_filter) %>%
                dplyr::select(c(1:13), c(14:26), c(32:37), c(38:48))
              
            }
            
          } else if ("Traditional Metrics" %in% hitcols) {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::select(c(1:13), c(27:31), c(38:48))
            
          } 
          
        } else if (input$hitUniverseFilterTab == T) {
          
          if ("Counting Stats" %in% hitcols) {
            
            if ("Traditional Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                      TRUE ~ "1"),
                              c_ab_filter = case_when(c_ab_filter == "0" &
                                                        grepl(Positions, pattern = "C") ~ "C",
                                                      TRUE ~ c_ab_filter),
                              c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                      TRUE ~ 1),
                              other_ab_filter = case_when(c_ab_filter == 1 &
                                                            as.numeric(AB) >= 100 ~ 1,
                                                          c_ab_filter == 1 &
                                                            as.numeric(AB) < 100 ~ 0,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) >= 175 ~ 1,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) < 175 ~ 0)) %>%
                dplyr::filter(other_ab_filter == 1) %>%
                dplyr::select(-c_ab_filter, -other_ab_filter) %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:13), c(14:37))
              
            } else if ("Advanced Metrics" %in% hitcols) {
              
              df <- Player_Batting %>%
                dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                      TRUE ~ "1"),
                              c_ab_filter = case_when(c_ab_filter == "0" &
                                                        grepl(Positions, pattern = "C") ~ "C",
                                                      TRUE ~ c_ab_filter),
                              c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                      TRUE ~ 1),
                              other_ab_filter = case_when(c_ab_filter == 1 &
                                                            as.numeric(AB) >= 100 ~ 1,
                                                          c_ab_filter == 1 &
                                                            as.numeric(AB) < 100 ~ 0,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) >= 175 ~ 1,
                                                          c_ab_filter == 0 &
                                                            as.numeric(AB) < 175 ~ 0)) %>%
                dplyr::filter(other_ab_filter == 1) %>%
                dplyr::select(-c_ab_filter, -other_ab_filter) %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:13), c(14:26), c(32:37), c(38:48))
              
            }
            
          } else if ("Traditional Metrics" %in% hitcols) {
            
            df <- Player_Batting %>%
              dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                    TRUE ~ "1"),
                            c_ab_filter = case_when(c_ab_filter == "0" &
                                                      grepl(Positions, pattern = "C") ~ "C",
                                                    TRUE ~ c_ab_filter),
                            c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                    TRUE ~ 1),
                            other_ab_filter = case_when(c_ab_filter == 1 &
                                                          as.numeric(AB) >= 100 ~ 1,
                                                        c_ab_filter == 1 &
                                                          as.numeric(AB) < 100 ~ 0,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) >= 175 ~ 1,
                                                        c_ab_filter == 0 &
                                                          as.numeric(AB) < 175 ~ 0)) %>%
              dplyr::filter(other_ab_filter == 1) %>%
              dplyr::select(-c_ab_filter, -other_ab_filter) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:13), c(27:31), c(38:48))
            
          }
          
        }
        
      }
      
    } else if (length(hitcols) == 3) {
      
      if (input$hitCVSLThresholdReqTab == F) {
        
        if (input$hitUniverseFilterTab == F) {
          
          df <- Player_Batting %>%
            dplyr::select(c(1:48))
          
        } else if (input$hitUniverseFilterTab == T) {
          
          df <- Player_Batting %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:48))
          
        }
        
      } else if (input$hitCVSLThresholdReqTab == T) {
        
        if (input$hitUniverseFilterTab == F) {
          
          df <- Player_Batting %>%
            dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                  TRUE ~ "1"),
                          c_ab_filter = case_when(c_ab_filter == "0" &
                                                    grepl(Positions, pattern = "C") ~ "C",
                                                  TRUE ~ c_ab_filter),
                          c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                  TRUE ~ 1),
                          other_ab_filter = case_when(c_ab_filter == 1 &
                                                        as.numeric(AB) >= 100 ~ 1,
                                                      c_ab_filter == 1 &
                                                        as.numeric(AB) < 100 ~ 0,
                                                      c_ab_filter == 0 &
                                                        as.numeric(AB) >= 175 ~ 1,
                                                      c_ab_filter == 0 &
                                                        as.numeric(AB) < 175 ~ 0)) %>%
            dplyr::filter(other_ab_filter == 1) %>%
            dplyr::select(-c_ab_filter, -other_ab_filter) %>%
            dplyr::select(c(1:48))
          
        } else if (input$hitUniverseFilterTab == T){
          
          df <- Player_Batting %>%
            dplyr::mutate(c_ab_filter = case_when(grepl(Positions, pattern = "(CF)") ~ "0",
                                                  TRUE ~ "1"),
                          c_ab_filter = case_when(c_ab_filter == "0" &
                                                    grepl(Positions, pattern = "C") ~ "C",
                                                  TRUE ~ c_ab_filter),
                          c_ab_filter = case_when(c_ab_filter == "C" ~ 0,
                                                  TRUE ~ 1),
                          other_ab_filter = case_when(c_ab_filter == 1 &
                                                        as.numeric(AB) >= 100 ~ 1,
                                                      c_ab_filter == 1 &
                                                        as.numeric(AB) < 100 ~ 0,
                                                      c_ab_filter == 0 &
                                                        as.numeric(AB) >= 175 ~ 1,
                                                      c_ab_filter == 0 &
                                                        as.numeric(AB) < 175 ~ 0)) %>%
            dplyr::filter(other_ab_filter == 1) %>%
            dplyr::select(-c_ab_filter, -other_ab_filter) %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:48))
          
        }
        
      }
      
    }

    df

  })
  
  pitchers_table_df <- reactive({
    
    
    pitcols = input$pitcolumns
    
    
    if (is.null(input$pitcolumns)) {
      
      if (input$pitCVSLThresholdReqTab == F){
        
        if (input$pitUniverseFilterTab == F){
          
          df <- Player_Pitching %>%
            dplyr::select(c(1:12))
          
        } else if (input$pitUniverseFilterTab == T) {
          
          df <- Player_Pitching %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:12))
          
        }
        
      } else if (input$pitCVSLThresholdReqTab == T) {
        
        if (input$pitUniverseFilterTab == F) {
          
          df <- Player_Pitching %>%
            dplyr::filter(IP >= 30) %>%
            dplyr::select(c(1:12))
          
        } else if (input$pitUniverseFilterTab == T) {
          
          df <- Player_Pitching %>%
            dplyr::filter(IP >= 30) %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:12))
          
        }
        
      }
      
    } else if (length(pitcols) == 1){
      
      if (input$pitCVSLThresholdReqTab == F) {
        
        if (input$pitUniverseFilterTab == F) {
          
          if (pitcols == "Counting Stats") {
            
            df <- Player_Pitching %>%
              dplyr::select(c(1:12), c(13,14), c(17:34))
            
          } else if (pitcols == "Traditional Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::select(c(1:12), c(15,16))
            
          } else if (pitcols == "Advanced Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::select(c(1:12), c(35:54))
            
          }
          
        } else if (input$pitUniverseFilterTab == T) {
          
          if (pitcols == "Counting Stats") {
            
            df <- Player_Pitching %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(13,14), c(17:34))
            
          } else if (pitcols == "Traditional Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(15,16))
            
          } else if (pitcols == "Advanced Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(35:54))
            
          }
          
        }
        
      } else if (input$pitCVSLThresholdReqTab == T) {
        
        if (input$pitUniverseFilterTab == F) {
          
          if (pitcols == "Counting Stats") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::select(c(1:12), c(13,14), c(17:34))
            
          } else if (pitcols == "Traditional Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::select(c(1:12), c(15,16))
            
          } else if (pitcols == "Advanced Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::select(c(1:12), c(35:54))
            
          }
          
        } else if (input$pitUniverseFilterTab == T) {
          
          if (pitcols == "Counting Stats") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(13,14), c(17:34))
            
          } else if (pitcols == "Traditional Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(15,16))
            
          } else if (pitcols == "Advanced Metrics") {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(35:54))
            
          }
          
        }
        
      }
      
    } else if (length(pitcols) == 2) {
      
      if (input$pitCVSLThresholdReqTab == F) {
        
        if (input$pitUniverseFilterTab == F) {
          
          if ("Counting Stats" %in% pitcols) {
            
            if ("Traditional Metrics" %in% pitcols) {
              
              df <- Player_Batting %>%
                dplyr::select(c(1:34))
              
            } else if ("Advanced Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::select(c(1:12), c(13,14), c(17:54))
              
            }
            
          } else if ("Traditional Metrics" %in% pitcols) {
            
            df <- Player_Pitching %>%
              dplyr::select(c(1:12), c(15,16), c(35:54))
            
          }
          
        } else if (input$pitUniverseFilterTab == T) {
          
          if ("Counting Stats" %in% pitcols) {
            
            if ("Traditional Metrics" %in% pitcols) {
              
              df <- Player_Batting %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:34))
              
            } else if ("Advanced Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:12), c(13,14), c(17:54))
              
            }
            
          } else if ("Traditional Metrics" %in% pitcols) {
            
            df <- Player_Pitching %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(15,16), c(35:54))
            
          }
          
        }
        
      } else if (input$pitCVSLThresholdReqTab == T) {
        
        if (input$pitUniverseFilterTab == F) {
          
          if ("Counting Stats" %in% pitcols) {
            
            if ("Traditional Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::filter(IP >= 30) %>%
                dplyr::select(c(1:34))
              
            } else if ("Advanced Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::filter(IP >= 30) %>%
                dplyr::select(c(1:12), c(13,14), c(17:54))
              
            }
            
          } else if ("Traditional Metrics" %in% pitcols) {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::select(c(1:12), c(15,16), c(35:54))
            
          }
          
        } else if (input$pitUniverseFilterTab == T) {
          
          if ("Counting Stats" %in% pitcols) {
            
            if ("Traditional Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::filter(IP >= 30) %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:34))
              
            } else if ("Advanced Metrics" %in% pitcols) {
              
              df <- Player_Pitching %>%
                dplyr::filter(IP >= 30) %>%
                dplyr::filter(in_universe == 1) %>%
                dplyr::select(c(1:12), c(13,14), c(17:54))
              
            }
            
          } else if ("Traditional Metrics" %in% pitcols) {
            
            df <- Player_Pitching %>%
              dplyr::filter(IP >= 30) %>%
              dplyr::filter(in_universe == 1) %>%
              dplyr::select(c(1:12), c(15,16), c(35:54))
            
          }
          
        }
        
      }
      
    } else if (length(pitcols) == 3) {
      
      if (input$pitCVSLThresholdReqTab == F) {
        
        if (input$pitUniverseFilterTab == F) {
          
          df <- Player_Pitching %>%
            dplyr::select(c(1:54))
          
        } else if (input$pitUniverseFilterTab == T) {
          
          df <- Player_Pitching %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:54))
          
        }
        
      } else if (input$pitCVSLThresholdReqTab == T) {
        
        if (input$pitUniverseFilterTab == F) {
          
          df <- Player_Pitching %>%
            dplyr::filter(IP >= 30) %>%
            dplyr::select(c(1:54))
          
        } else if (input$pitUniverseFilterTab == T) {
          
          df <- Player_Pitching %>%
            dplyr::filter(IP >= 30) %>%
            dplyr::filter(in_universe == 1) %>%
            dplyr::select(c(1:54))
          
        }
        
      }
      
    }
    
    df
    
  })
  
  output$DownloadHitterSeasonTable <- downloadHandler(filename = "HitterSeasonData.csv",
                                                      content = function(file) {
                                                        write.csv(hitters_table_df(), file)
                                                      })
  
  output$DownloadPitcherSeasonTable <- downloadHandler(filename = "PitcherSeasonData.csv",
                                                       content = function(file) {
                                                         write.csv(pitchers_table_df(), file)
                                                       })
  
  output$DownloadHvLHP <- downloadHandler(filename = "HitterSplitsVLHP.csv",
                                          content = function(file) {
                                            write.csv(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "LHP")) %>% dplyr::filter(Year == 2023), file)
                                          })
  
  output$DownloadHvRHP <- downloadHandler(filename = "HitterSplitsVRHP.csv",
                                          content = function(file) {
                                            write.csv(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "RHP")) %>% dplyr::filter(Year == 2023), file)
                                          })
  
  output$DownloadPvLHB <- downloadHandler(filename = "PitcherSplitsVLHB.csv",
                                          content = function(file) {
                                            write.csv(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "LHH")) %>% dplyr::filter(Year == 2023), file)
                                          })
  
  output$DownloadPvRHB <- downloadHandler(filename = "PitcherSplitsVRHB.csv",
                                          content = function(file) {
                                            write.csv(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "RHH")) %>% dplyr::filter(Year == 2023), file)
                                          })

  output$MLBHit_scatter = renderPlot({
    
    req( length(input$MLBhitby_Year) >= 1)
    req( length(input$MLBhitcolor) <= 1)

    
    groupies = input$MLBhitgroupby
    
    collies = input$MLBhitcolor
    
    if ( is.null(groupies) ) {
      
      if ( is.null(collies) ) {
        
          
          h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Hitters, 2016-2023") +
            geom_point(aes(color = as.character(Year))) +
            scale_color_discrete(name = "Year") +
            # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
            #                               "navy", "violet", "peach", "dodgerblue")) +
            guides(alpha = "none")
          
      } else if ( length(collies) == 1){
        
        h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Hitters, 2016-2023") +
          geom_point(aes(color = as.character(hitters_plot_df()[ , input$MLBhitcolor]))) +
          scale_color_discrete(name = input$MLBhitcolor) +
          # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
          #                               "navy", "violet", "peach", "dodgerblue")) +
          guides(alpha = "none")
        
      }
      
    } else if ( length(groupies) == 1) {
      
      if (groupies == "Year" ) {
        
        if ( length(collies) == 1) {
          
          h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Hitters, 2016-2023") +
            geom_point(aes(color = as.character(hitters_plot_df()[ ,input$MLBhitcolor]))) +
            scale_color_discrete(name = input$MLBhitcolor) +
            # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
            #                                              "navy", "violet", "peach", "dodgerblue"), aesthetics = "color") +
            facet_wrap(~Year) +
            guides(alpha = "none")
          
        } else if ( is.null(input$MLBhitcolor) ){
          
          h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Hitters, 2016-2023") +
            geom_point(aes(color = as.character(Year))) +
            scale_color_discrete(name = "Year") +
            # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
            #                                              "navy", "violet", "peach", "dodgerblue"), aesthetics = "color") +
            facet_wrap(~Year) +
            guides(alpha = "none")
          
        }
      
      } else if ( groupies == "Bats") {
        
        if ( length(collies) == 1) {
          
          h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Hitters, 2016-2023") +
            geom_point(aes(color = as.character(hitters_plot_df()[ , input$MLBhitcolor]))) +
            scale_color_discrete(name = input$MLBhitcolor) +
            # scale_color_manual(values = c("darkgoldenrod3", "navy", "skyblue")) +
            facet_wrap(~Bats) +
            guides(alpha = "none")
          
        } else if ( is.null(input$MLBhitcolor) ) {
          
          h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Hitters, 2016-2023") +
            geom_point(aes(color = Bats)) +
            scale_color_manual(values = c("darkgoldenrod3", "navy", "skyblue")) +
            facet_wrap(~Bats) +
            guides(alpha = "none")
          
        }
      
    } else if ( groupies == "Position_Group") {
      
      if ( length(collies) == 1 ) {
        
        h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Hitters, 2016-2023") +
          geom_point(aes(color = as.character(hitters_plot_df()[ , input$MLBhitcolor]))) +
          scale_color_discrete(name = input$MLBhitcolor) +
          # scale_color_manual(values = c("darkgoldenrod3", "navy", "skyblue")) +
          facet_wrap(~Position_Group) +
          guides(alpha = "none")
        
      } else if ( is.null(input$MLBhitcolor) ) {
        
        h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Hitters, 2016-2023") +
          geom_point(aes(color = Position_Group)) +
          scale_color_discrete(name = "Position_Group") +
          facet_wrap(~Position_Group) +
          guides(alpha = "none")
        
      }
    }
  } else if ( length( groupies ) > 1 ) {
            
            if ( length(collies) == 1) {
              
              h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(title = "MLB Hitters, 2016-2023") +
                geom_point(aes(color = as.character(hitters_plot_df()[ ,input$MLBhitcolor]))) +
                scale_color_discrete(name = input$MLBhitcolor) +
                facet_grid(rows = vars(Bats), cols = vars(Year)) +
                guides(alpha = "none")
              
            } else if ( is.null(input$MLBhitcolor)) {
            
              h_mlb <- ggplot(hitters_plot_df(), aes(!!input$MLBhitxvar, !!input$MLBhityvar, alpha = 0.3)) +
                theme(plot.title = element_text(hjust = 0.5)) +
                labs(title = "MLB Hitters, 2016-2023") +
                geom_point(aes(color = Bats)) +
                scale_color_manual(values = c("darkgoldenrod3", "navy", "skyblue")) +
                facet_grid(rows = vars(Bats), cols = vars(Year)) +
                guides(alpha = "none")
            
          }
        } else {
          
          h_mlb <- NULL
          
        }
    
    h_mlb
    
  }, height = 400, width = 700, res = 100)
  
  output$hitter_mlb_season_brush_info = renderPrint({
    
    brushedPoints(hitters_plot_df(), brush = input$hitter_mlb_season_brush, xvar = as.character(input$MLBhitxvar), yvar = as.character(input$MLBhityvar))
    
  })
  
  
  output$MLBPit_scatter = renderPlot({
    
    req( length(input$MLBpitby_Year) >= 1)
    req( length(input$MLBpitcolor) <= 1)
    
    groupies = input$MLBpitgroupby
    
    collies = input$MLBpitcolor
    
    
    if ( is.null(groupies) ) {
      
      if ( is.null(collies) ) {
        
        p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Pitchers, 2016-2023") +
          geom_point(aes(color = as.character(Year))) +
          scale_color_discrete(name = "Year") +
          # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
          #                               "navy", "violet", "peach", "dodgerblue")) +
          guides(alpha = "none")
        
      } else if ( length(collies) == 1){
        
        p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Pitchers, 2016-2023") +
          geom_point(aes(color = as.character(pitchers_plot_df()[ , input$MLBpitcolor]))) +
          scale_color_discrete(name = input$MLBpitcolor) +
          # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
          #                               "navy", "violet", "peach", "dodgerblue")) +
          guides(alpha = "none")
        
      }
      
      
      
    }
    
    else if ( length(groupies) == 1 ) {
      
      if (groupies == "Year") {
        
        if ( is.null(collies) ) {
        
          p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Pitchers, 2016-2023") +
            geom_point(aes(color = as.character(Year))) +
            scale_color_discrete(name = "Year") +
            # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
            #                                              "navy", "violet", "peach", "dodgerblue"), aesthetics = "color") +
            facet_wrap(~Year) +
            guides(alpha = "none")
          
          } else if ( length(collies) == 1) {
        
            p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
              theme(plot.title = element_text(hjust = 0.5)) +
              labs(title = "MLB Pitchers, 2016-2023") +
              geom_point(aes(color = as.character(pitchers_plot_df()[ , input$MLBpitcolor]))) +
              scale_color_discrete(name = input$MLBpitcolor) +
              # scale_color_manual(values = c("darkgoldenrod3", "firebrick", "forestgreen", "chocolate",
              #                                              "navy", "violet", "peach", "dodgerblue"), aesthetics = "color") +
              facet_wrap(~Year) +
              guides(alpha = "none")
        
      }
    }
    
    else if ( groupies == "Throws") {
        
        if ( is.null(collies) ) {
          
          p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Pitchers, 2016-2023") +
            geom_point(aes(color = Throws)) +
            scale_color_manual(values = c("darkgoldenrod3", "navy")) +
            facet_wrap(~Throws) +
            guides(alpha = "none")
          
        } else if ( length(collies) == 1) {
          
          p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Pitchers, 2016-2023") +
            geom_point(aes(color = as.character(pitchers_plot_df()[ , input$MLBpitcolor]))) +
            scale_color_discrete(name = input$MLBpitcolor) +
            facet_wrap(~Throws) +
            guides(alpha = "none")
          
        }
    } 
      else if ( groupies == "Role") {
        
        if ( is.null(collies) ) {
          
          p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Pitchers, 2016-2023") +
            geom_point(aes(color = Role)) +
            scale_color_manual(values = c("darkgoldenrod3", "navy", "skyblue")) +
            facet_wrap(~Role) +
            guides(alpha = "none")
          
        } else if ( length(collies) == 1) {
          
          p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(title = "MLB Pitchers, 2016-2023") +
            geom_point(aes(color = as.character(pitchers_plot_df()[ , input$MLBpitcolor]))) +
            scale_color_discrete(name = input$MLBpitcolor) +
            facet_wrap(~Role) +
            guides(alpha = "none")
          
        }
        
      }
    }
    
    else if ( length( groupies ) > 1 ) {
      
      if ( is.null(collies) ) {
        
        p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Pitchers, 2016-2023") +
          geom_point(aes(color = Throws)) +
          scale_color_manual(values = c("darkgoldenrod3", "navy")) +
          facet_grid(rows = vars(Throws), cols = vars(Year)) +
          guides(alpha = "none")
        
      } else if ( length(collies) == 1 ) {
        
        p_mlb <- ggplot(pitchers_plot_df(), aes(!!input$MLBpitxvar, !!input$MLBpityvar, alpha = 0.3)) +
          theme(plot.title = element_text(hjust = 0.5)) +
          labs(title = "MLB Pitchers, 2016-2023") +
          geom_point(aes(color = as.character(pitchers_plot_df()[ , input$MLBpitcolor]))) +
          scale_color_discrete(name = input$MLBpitcolor) +
          facet_grid(rows = vars(Throws), cols = vars(Year)) +
          guides(alpha = "none")
        
      }
    }
    
    p_mlb
    
  }, height = 400, width = 700, res = 100)
  
  output$pitcher_mlb_season_brush_info = renderPrint({
    
    brushedPoints(pitchers_plot_df(), brush = input$pitcher_mlb_season_brush, xvar = as.character(input$MLBpitxvar), yvar = as.character(input$MLBpityvar))
    
  })
  

  tryCatch({output$draft_h = renderPlot({
    
    req( length( sort(unlist(input$hitteam)[c(1:10)]) ) >= 1)
    req( length(input$hitGroup) <= 1)
    
    if (input$hitgroupbyteam == T & #input$hitshowfit == T & 
        length(input$hitby_Year) >= 1 & 
        is.null(input$hitGroup))
      
      h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
      theme(plot.title = element_text(hjust = 0.5))+
      labs(title = "CVSL Auctions, Hitters (2019-2023)")+
      geom_point(aes(color = CVSLTeam)) +
      scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                    "forestgreen", "firebrick", "orange", "gray48",
                                    "red", "navy", "skyblue")) +
      # geom_smooth(aes(x = !!input$hitxvar, y = !!input$hityvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
      facet_wrap(~ CVSLTeam) +
      guides(alpha = "none")
    
    # else if (input$hitgroupbyteam == T & input$hitshowfit == F & length(input$hitby_Year) >= 1)
    #   h <- ggplot(hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
    #     theme(plot.title = element_text(hjust = 0.5))+
    #     labs(title = "CVSL Auctions, Hitters (2019-2023)")+
    #     geom_point(aes(color = CVSLTeam)) +
    #     scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
    #                                   "forestgreen", "firebrick", "orange", "gray48",
    #                                   "red", "navy", "skyblue")) +
    #     facet_wrap(~ CVSLTeam) +
    #     guides(alpha = "none")
    
    else if (input$hitgroupbyteam == F &
             is.null(input$hitGroup) &
             input$hitshowfit == T & 
             length(input$hitby_Year) >= 1)
      
      h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        geom_smooth(aes(x = !!input$hitxvar, y = !!input$hityvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        guides(alpha = "none")
    
    else if (length(input$hitby_Year) >= 1 &
             is.null(input$hitGroup))
      h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        guides(alpha = "none")
    
    else if (input$hitGroup == "Age" &
             input$hitgroupbyteam == F)
        
        h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
          theme(plot.title = element_text(hjust = 0.5))+
          labs(title = "CVSL Auctions, Hitters (2019-2023)")+
          geom_point(aes(color = CVSLTeam)) +
          scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                        "forestgreen", "firebrick", "orange", "gray48",
                                        "red", "navy", "skyblue"))+
          facet_wrap(~Age)+
          guides(alpha = "none")
    
    else if (input$hitGroup == "Bats" &
             input$hitgroupbyteam == F)
      
      h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue"))+
        facet_wrap(~Bats)+
        guides(alpha = "none")
      
    else if (input$hitGroup == "Position_Group" &
             input$hitgroupbyteam == F)
      
      h <- ggplot(draft_hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue"))+
        facet_wrap(~Position_Group)+
        guides(alpha = "none")
    
    else h = NULL
    
    h
    
  }, height = 400, width = 700, res = 100)
  })
  
  
  
  tryCatch({output$draft_p = renderPlot({
    
    req( length( sort(unlist(input$team)[c(1:10)]) ) >= 1)
    req( length( input$pitGroup) <= 1)
    
    if (input$groupbyteam == T &
        length(input$by_Year) >= 1 &
        is.null(input$pitGroup))
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        # geom_smooth(aes(x = !!input$xvar, y = !!input$yvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        facet_wrap(~ CVSLTeam) +
        guides(alpha = "none")
    
    
    else if (input$groupbyteam == F &
             is.null(input$pitGroup) &
             input$showfit == T &
             length(input$by_Year) >= 1)
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        geom_smooth(aes(x = !!input$xvar, y = !!input$yvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        guides(alpha = "none")
    
    else if (is.null(input$pitGroup) &
             length(input$by_Year >= 1))
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        guides(alpha = "none")
      
      
    
    else if (input$pitGroup == "Age" &
             input$groupbyteam == F &
             input$showfit == F &
             length(input$by_Year >=1))
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        facet_wrap(~Age)+
        guides(alpha = "none")
    
    else if (input$pitGroup == "Throws" &
             input$groupbyteam == F &
             input$showfit == F & 
             length(input$by_Year >=1))
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        facet_wrap(~Throws)+
        guides(alpha = "none")
    
    else if (input$pitGroup == "Role" &
             input$groupbyteam == F &
             input$showfit == F &
             length(input$by_Year >=1))
      
      p <- ggplot(draft_pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        facet_wrap(~Role)+
        guides(alpha = "none")
    
    else p = NULL
  
    p
    
  }, height = 400, width = 700, res = 100)
  })
  
  output$draft_hitter_brush_info = renderPrint({
    
    brushedPoints(draft_hitters_teams_df(), brush = input$draft_hitter_brush, xvar = as.character(input$hitxvar), yvar = as.character(input$hityvar))
    
  })
  
  output$draft_pitcher_brush_info = renderPrint({
    
    brushedPoints(draft_pitchers_teams_df(), brush = input$draft_pitcher_brush, xvar = as.character(input$xvar), yvar = as.character(input$yvar))
    
  })
  
}



# run the app ----

shinyApp(ui = ui, server = server)
