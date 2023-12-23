

# load libraries ----
library(shiny)
library(ggplot2)
library(ggExtra)
library(DT)
library(baseballr)
library(tidyverse)

library(here)

# rsconnect::setAccountInfo(name='genuinechuckles', token='36B974DE17CDC1C90A155DA4FCC44339', secret='h3eBnnKZp9HNe6CllCcEXFUZUNexNnGMqk2WRj9l')

library(rsconnect)


# load(url("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/CompleteUniversesAndLeaderboardsEnv.RData"))
# load(".data/CompleteUniversesAndLeaderboards.RData")
# load("~/GitHub/Baseball/CVSL/2022/CompleteUniversesAndLeaderboardsEnv.RData")
# save.image("~/GitHub/Baseball/CVSL/CVSLDraftPrep/data/CompleteUniversesAndLeaderboards.RData")
# setwd("./data")

load(
  here("CVSL", "CVSLDraftPrep", "data" , "CompleteUniversesAndLeaderboards.RData")
)



# commented out commands ----
# CHAD_LU_2 = chadwick_player_lu()
# 
# CHAD_LU_2 = CHAD_LU_2 %>%
#   dplyr::filter(mlb_played_last >= 2016) %>%
#   dplyr::mutate(bday = as.Date(paste(birth_year, "-", birth_month, "-", birth_day, sep = "")),
#                 Name = paste(name_first, name_last, sep = " ")) %>%
#   dplyr::select(Name, key_bbref, bday)
# 
# CHAD_LU = CHAD_LU %>%
#   inner_join(CHAD_LU_2)
# 
# 
# DRAFTS %>%
#   dplyr::mutate(`Draft Year` = as.numeric(Year),
#                 Year = Year - 1) %>%
#   left_join(Player_Batting, by = c("bbref_id", "Year")) %>%
#   dplyr::filter(!is.na(Name.y)) %>%
#   dplyr::rename(Name = Name.x) %>%
#   dplyr::select(-Name.y, -contains("key")) %>%
#   dplyr::mutate(key_bbref = bbref_id) %>%
#   dplyr::left_join(CHAD_LU_2, by = "key_bbref") %>%
#   dplyr::mutate(Age_continuous = as.numeric(difftime(as.Date(paste(Year, "-07-01", sep = "")), as.Date(bday), units = "days"))/ 365.25) %>%
#   dplyr::select(-contains(".y"), -contains("key")) %>%
#   dplyr::rename(Name = Name.x) %>%
#   dplyr::mutate(CVSLTeam = case_when(CVSLTeam == "Bears" ~ "Renegades",
#                                      CVSLTeam == "Copperheads" ~ "Arsenal",
#                                      CVSLTeam == "Golden Bears" ~ "Renegades",
#                                      CVSLTeam == "Tamales" ~ "Wombats",
#                                      CVSLTeam == "Dodgers" ~ "Twins",
#                                      TRUE ~ CVSLTeam))
# 
# DRAFTS_hitters = DRAFTS %>%
#   dplyr::mutate(CVSLTeam = case_when(CVSLTeam == "Bears" ~ "Renegades",
#                                      CVSLTeam == "Copperheads" ~ "Arsenal",
#                                      CVSLTeam == "Golden Bears" ~ "Renegades",
#                                      CVSLTeam == "Tamales" ~ "Wombats",
#                                      CVSLTeam == "Dodgers" ~ "Twins",
#                                      TRUE ~ CVSLTeam)) %>%
#   dplyr::mutate(`Draft Year` = as.numeric(Year),
#                 Year = Year - 1) %>%
#   left_join(Player_Batting, by = c("bbref_id", "Year")) %>%
#   dplyr::filter(CVSLTeam == "Arsenal" & `Draft Year` == 2022)
#   
#   dplyr::filter(!is.na(Name.y)) %>%
#   dplyr::rename(Name = Name.x) %>%
#   dplyr::select(-Name.y, -contains("key")) %>%
#   dplyr::mutate(key_bbref = bbref_id) %>%
#   left_join(CHAD_LU_2, by = "key_bbref") %>%
#   dplyr::mutate(Age_continuous = as.numeric(difftime(as.Date(paste(Year, "-07-01", sep = "")), as.Date(bday), units = "days"))/ 365.25) %>%
#   dplyr::select(-contains(".y"), -contains("key")) %>%
#   dplyr::rename(Name = Name.x) %>%
#   
#   dplyr::mutate(across(c(14:15,17:51), as.double))
# 
# DRAFTS_pitchers = DRAFTS %>%
#   dplyr::mutate(`Draft Year` = as.numeric(Year),
#                 Year = Year - 1) %>%
#   left_join(Player_Pitching, by = c("bbref_id", "Year")) %>%
#   dplyr::filter(!is.na(Name.y)) %>%
#   dplyr::rename(Name = Name.x,
#                 Salary = Salary.x) %>%
#   dplyr::select(-contains("key"), -contains(".y"), -Acquired, -RA9extras, -contains("%"), -PPFp) %>%
#   dplyr::mutate(key_bbref = bbref_id) %>%
#   left_join(CHAD_LU_2, by = "key_bbref") %>%
#   dplyr::mutate(Age_continuous = as.numeric(difftime(as.Date(paste(Year, "-07-01", sep = "")), as.Date(bday), units = "days")) / 365.25,
#                 playerid = as.character(playerid)) %>%
#   dplyr::rename(Name = Name.x) %>%
#   dplyr::select(-Name.y) %>%
#   dplyr::mutate(CVSLTeam = case_when(CVSLTeam == "Bears" ~ "Renegades",
#                                      CVSLTeam == "Copperheads" ~ "Arsenal",
#                                      CVSLTeam == "Golden Bears" ~ "Renegades",
#                                      CVSLTeam == "Tamales" ~ "Wombats",
#                                      CVSLTeam == "Dodgers" ~ "Twins",
#                                      TRUE ~ CVSLTeam)) %>%
#   dplyr::mutate(across(c(14:53), as.double))
# 
# DRAFTS_full = DRAFTS_pitchers %>%
#   full_join(DRAFTS_hitters) %>% dplyr::distinct()

# ui object ----

ui <- navbarPage(
  
  title = "CVSL Draft Prep",
  theme = bslib::bs_theme(bootswatch = "spacelab"),#, bg = "black", fg = "skyblue", primary = "skyblue", secondary = "gray",
  # ),
  inverse = TRUE,
  
  
  
  navbarMenu("Aggregated Hitter Season Totals",
             tabPanel("2016", dataTableOutput('HittersAgg2016')),
             tabPanel("2017", dataTableOutput('HittersAgg2017')),
             tabPanel("2018", dataTableOutput('HittersAgg2018')),
             tabPanel("2019", dataTableOutput('HittersAgg2019')),
             tabPanel("2020", dataTableOutput('HittersAgg2020')),
             tabPanel("2021", dataTableOutput('HittersAgg2021')),
             tabPanel("2022", dataTableOutput('HittersAgg2022')),
             tabPanel("2023", dataTableOutput('HittersAgg2023'))),
  
  navbarMenu("Aggregated Pitcher Season Totals",
             tabPanel("2016", dataTableOutput("PitchersAgg2016")),
             tabPanel("2017", dataTableOutput("PitchersAgg2017")),
             tabPanel("2018", dataTableOutput("PitchersAgg2018")),
             tabPanel("2019", dataTableOutput("PitchersAgg2019")),
             tabPanel("2020", dataTableOutput("PitchersAgg2020")),
             tabPanel("2021", dataTableOutput("PitchersAgg2021")),
             tabPanel("2022", dataTableOutput("PitchersAgg2022")),
             tabPanel("2023", dataTableOutput("PitchersAgg2023"))),
  
  navbarMenu("2023 Season Splits",
             
             tabPanel("Hitters vLHP", dataTableOutput("HitterSplitsvLHP")),
             tabPanel("Hitters vRHP", dataTableOutput("HitterSplitsvRHP")),
             tabPanel("Pitcher vLHB", dataTableOutput("PitcherSplitsvLHB")),
             tabPanel("Pitcher vRHB", dataTableOutput("PitcherSplitsvRHB"))),

  navbarMenu("CVSL Draft History", 
             
             tabPanel("Draft History, Hitters", sidebarLayout(

                                                               sidebarPanel = sidebarPanel(varSelectInput("xvar", "X variable", DRAFTS_hitters, selected = "bWAR"),
                                                                                           varSelectInput("yvar", "Y variable", DRAFTS_hitters, selected = "Salary"),
                                                                                           checkboxGroupInput("team", "Filter by CVSL Team",
                                                                                                              choices = c(unique(DRAFTS_hitters$CVSLTeam)),
                                                                                                              selected = c(unique(DRAFTS_hitters$CVSLTeam))),
                                                                                           checkboxGroupInput("by_Year", "Filter by Draft Year",
                                                                                                              choices = c(as.character(unique(DRAFTS_hitters$`Draft Year`))),
                                                                                                              selected = c(as.character(unique(DRAFTS_hitters$`Draft Year`))))),

                                                               mainPanel = plotOutput("draft_h"),
                                                
                                                               position = "left"
                                                
                                                             )
             
             ),
             tabPanel("Draft History, Pitchers", sidebarLayout(
               
                                                               sidebarPanel = sidebarPanel(varSelectInput("xvar", "X variable", DRAFTS_pitchers, selected = "bWAR"),
                                                                                           varSelectInput("yvar", "Y variable", DRAFTS_pitchers, selected = "Salary"),
                                                                                           checkboxInput("by_CVSLTeam", "Show Team", TRUE),
                                                                                           checkboxGroupInput("team", "Filter by CVSL Team",
                                                                                                              choices = unique(DRAFTS_pitchers$CVSLTeam),
                                                                                                              selected = unique(DRAFTS_pitchers$CVSLTeam))),
                                                               
                                                               mainPanel = plotOutput("draft_p"),
                                                               
                                                               position = "left"
                                                               
                                                               )
             
             )
  )
  
  
)






# server logic ----

server <- function(input, output) {

  output$HittersAgg2016 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2016) )
  output$HittersAgg2017 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2017) )
  output$HittersAgg2018 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2018) )
  output$HittersAgg2019 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2019) )
  output$HittersAgg2020 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2020) )
  output$HittersAgg2021 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2021) )
  output$HittersAgg2022 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2022) )
  output$HittersAgg2023 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2023) )

  output$PitchersAgg2016 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2016) )
  output$PitchersAgg2017 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2017) )
  output$PitchersAgg2018 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2018) )
  output$PitchersAgg2019 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2019) )
  output$PitchersAgg2020 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2020) )
  output$PitchersAgg2021 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2021) )
  output$PitchersAgg2022 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2022) )
  output$PitchersAgg2023 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2023) )
  
  output$PitcherSplitsvLHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "LHB")))
  output$PitcherSplitsvRHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "RHB")))
  output$HitterSplitsvLHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "LHP")))
  output$HitterSplitsvRHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "RHP")))
  
  
  hitters_years_df <- reactive({

    
    if ( length(input$by_Year) >= 1
         
         ) {
        
        data = DRAFTS_hitters %>% #dplyr::filter(CVSLTeam %in% c(input$team)) %>%
          dplyr::filter(as.character(`Draft Year`) %in% c(input$by_Year))
      
    } else {
      
      data = DRAFTS_hitters
      
    } 
    
    data

  })
  
  hitters_teams_df <- reactive({
    
    if ( length(input$team) >= 1
         
    ) {
      
      data = hitters_years_df()[DRAFTS_hitters$CVSLTeam %in% c(input$team), ]
      
    } else {
      
      data = hitters_years_df()
      
    }
    
    data
    
  })

  output$draft_h = renderPlot({
    
    p <- ggplot(hitters_teams_df() ,
               
               aes(!!input$xvar, !!input$yvar, alpha = 0.5, color = CVSLTeam)) +
      # geom_smooth(aes(!!input$xvar, !!input$yvar), method = "gam") +
      
      # scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
      #                                              "forestgreen", "firebrick", "orange", "gray48",
      #                                              "red", "navy", "skyblue")) #+
      
      list(
            geom_point(),
            # scale_color_manual(values = c(team_colors)),
            guides(alpha = "none")
            

      )
      
    p
    
  }, height = 600, width = 900, res = 100)
  
  
  subset_pitchers_df <- reactive({
    
    req(input$by_CVSLTeam)
    DRAFTS_pitchers %>% dplyr::filter(CVSLTeam %in% input$by_CVSLTeam)
    
    
  })
  
  output$draft_p = renderPlot({
    
    p = ggplot(subset_pitchers_df(),
               
               aes(!!input$xvar, !!input$yvar, color = CVSLTeam)) +
      
      geom_point() #+ 
      # scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
      #                                              "forestgreen", "firebrick", "orange", "gray48",
      #                                              "red", "navy", "skyblue")) #+ 
    
    # list(
    #   
    #       if (input$by_CVSLTeam >= 1) facet_wrap(~CVSLTeam)
    #   
    # )
    
    
    # list(
    #   
    #   
    #   # if (input$xvar == "bWAR") theme(panel.grid.major.x = c(-1, 11))
    #   
    #   geom_point() +
    #     scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
    #                                   "forestgreen", "firebrick", "orange", "gray48",
    #                                   "red", "navy", "skyblue"))
    #            
    #   )
    
    p
    
  }, height = 600, width = 900, res = 100)
  
}

# run the app ----

shinyApp(ui = ui, server = server)

# deployApp("~/GitHub/Baseball/CVSL")
# ----
# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
