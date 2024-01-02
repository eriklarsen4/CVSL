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

load("CompleteUniversesAndLeaderboards.RData")



# ui object ----

ui <- f7Page(
  
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
             tabPanel("2016", dataTableOutput("PitchersAgg2016", width = "20%")),
             tabPanel("2017", dataTableOutput("PitchersAgg2017", width = "20%")),
             tabPanel("2018", dataTableOutput("PitchersAgg2018", width = "20%")),
             tabPanel("2019", dataTableOutput("PitchersAgg2019", width = "20%")),
             tabPanel("2020", dataTableOutput("PitchersAgg2020", width = "20%")),
             tabPanel("2021", dataTableOutput("PitchersAgg2021", width = "20%")),
             tabPanel("2022", dataTableOutput("PitchersAgg2022", width = "20%")),
             tabPanel("2023", dataTableOutput("PitchersAgg2023", width = "20%"))),
  
  navbarMenu("2023 Season Splits",
             
             tabPanel("Hitters vLHP", dataTableOutput("HitterSplitsvLHP", width = "20%")),
             tabPanel("Hitters vRHP", dataTableOutput("HitterSplitsvRHP", width = "20%")),
             tabPanel("Pitcher vLHB", dataTableOutput("PitcherSplitsvLHB", width = "20%")),
             tabPanel("Pitcher vRHB", dataTableOutput("PitcherSplitsvRHB", width = "20%"))),
  
  navbarMenu("CVSL Draft History", 
             
             tabPanel("Draft History, Hitters", sidebarLayout(
               
               sidebarPanel = sidebarPanel(
                 varSelectInput("hitxvar", strong("X variable"), DRAFTS_hitters, selected = "bWAR"),
                 varSelectInput("hityvar", strong("Y variable"), DRAFTS_hitters, selected = "Salary"),
                 checkboxGroupInput(inputId = "hitteam", label = strong("Filter by CVSL Team"),
                                    choices = c(sort(unique(DRAFTS_hitters$CVSLTeam))),
                                    selected = c(sort(unique(DRAFTS_hitters$CVSLTeam)))),
                 checkboxGroupInput(inputId = "hitby_Year", label = strong("Filter by Draft Year"),
                                    choices = c(as.character(unique(DRAFTS_hitters$`Draft Year`))),
                                    selected = c(as.character(unique(DRAFTS_hitters$`Draft Year`)))),
                 checkboxInput("hitgroupbyteam",label = strong("Group By Team"), value = F),
                 checkboxInput("hitshowfit", label = strong("Show Trend Line"), value = F), width = 3),
               
               mainPanel = plotOutput("draft_h", width = 6),
               
               position = "left" 
               
             )
             
             ),
             tabPanel("Draft History, Pitchers", sidebarLayout(
               
               sidebarPanel = sidebarPanel(
                 varSelectInput("xvar", strong("X variable"), DRAFTS_pitchers, selected = "bWAR"),
                 varSelectInput("yvar", strong("Y variable"), DRAFTS_pitchers, selected = "Salary"),
                 checkboxGroupInput(inputId = "team", label = strong("Filter by CVSL Team"),
                                    choices = c(sort(unique(DRAFTS_pitchers$CVSLTeam))),
                                    selected = c(sort(unique(DRAFTS_pitchers$CVSLTeam)))),
                 checkboxGroupInput(inputId = "by_Year", label = strong("Filter by Draft Year"),
                                    choices = c(as.character(unique(DRAFTS_pitchers$`Draft Year`))),
                                    selected = c(as.character(unique(DRAFTS_pitchers$`Draft Year`)))),
                 checkboxInput("groupbyteam",label = strong("Group By Team"), value = F),
                 checkboxInput("showfit", label = strong("Show Trend Line"), value = F), width = 3),
               
               mainPanel = plotOutput("draft_p", width = 6),
               
               position = "left" 
               
             )
             
             )
  )
  
  
)

# server logic ----

server <- function(input, output, session) {
  
  storeWarn = getOption("warn")
  options(warn = -1)
  
  output$HittersAgg2016 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2016), filter = "top")
  output$HittersAgg2017 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2017), filter = "top" )
  output$HittersAgg2018 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2018), filter = "top" )
  output$HittersAgg2019 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2019), filter = "top" )
  output$HittersAgg2020 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2020), filter = "top" )
  output$HittersAgg2021 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2021), filter = "top" )
  output$HittersAgg2022 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2022), filter = "top" )
  output$HittersAgg2023 = renderDataTable(Player_Batting %>% dplyr::select(-c(38:42,44)) %>% dplyr::filter(Year == 2023) , filter = "top")
  
  output$PitchersAgg2016 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2016), filter = "top" )
  output$PitchersAgg2017 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2017) , filter = "top")
  output$PitchersAgg2018 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2018), filter = "top" )
  output$PitchersAgg2019 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2019) , filter = "top")
  output$PitchersAgg2020 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2020), filter = "top" )
  output$PitchersAgg2021 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2021), filter = "top" )
  output$PitchersAgg2022 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2022), filter = "top" )
  output$PitchersAgg2023 = renderDataTable(Player_Pitching %>% dplyr::select(-c(42:46,48,49,53:57)) %>% dplyr::filter(Year == 2023) , filter = "top")
  
  output$PitcherSplitsvLHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "LHB")), filter = "top")
  output$PitcherSplitsvRHB = renderDataTable(Pitcher_splits %>% dplyr::filter(grepl(Split, pattern = "RHB")), filter = "top")
  output$HitterSplitsvLHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "LHP")), filter = "top")
  output$HitterSplitsvRHP = renderDataTable(Hitter_splits %>% dplyr::filter(grepl(Split, pattern = "RHP")), filter = "top")
  
  DRAFTS_hitters = DRAFTS_hitters %>%
    dplyr::mutate(teamcolors = case_when(CVSLTeam == "Arsenal" ~ "darkgoldenrod3",
                                         CVSLTeam == "Bison" ~ "chocolate",
                                         CVSLTeam == "Cattlemen" ~ "black",
                                         CVSLTeam == "Ducks" ~ "forestgreen",
                                         CVSLTeam == "Matadors" ~ "firebrick",
                                         CVSLTeam == "Nuggets" ~ "orange",
                                         CVSLTeam == "Phillies" ~ "gray48",
                                         CVSLTeam == "Renegades" ~ "red",
                                         CVSLTeam == "Twins" ~ "navy",
                                         CVSLTeam == "Wombats" ~ "skyblue"
    ))
  
  DRAFTS_pitchers = DRAFTS_pitchers %>%
    dplyr::mutate(teamcolors = case_when(CVSLTeam == "Arsenal" ~ "darkgoldenrod3",
                                         CVSLTeam == "Bison" ~ "chocolate",
                                         CVSLTeam == "Cattlemen" ~ "black",
                                         CVSLTeam == "Ducks" ~ "forestgreen",
                                         CVSLTeam == "Matadors" ~ "firebrick",
                                         CVSLTeam == "Nuggets" ~ "orange",
                                         CVSLTeam == "Phillies" ~ "gray48",
                                         CVSLTeam == "Renegades" ~ "red",
                                         CVSLTeam == "Twins" ~ "navy",
                                         CVSLTeam == "Wombats" ~ "skyblue"
    ))
  
  
  hitters_teams_df <- reactive({
    
    
    df <- DRAFTS_hitters %>%
      dplyr::filter(CVSLTeam %in% sort(unlist(input$hitteam)[c(1:10)]))
    
    
    df2 <- df %>%
      dplyr::filter(as.character(`Draft Year`) %in% input$hitby_Year)
    
    
    df2
    
  })
  
  pitchers_teams_df <- reactive({
    
    
    df3 <- DRAFTS_pitchers %>%
      dplyr::filter(CVSLTeam %in% input$team)
    
    
    df4 <- df3 %>%
      dplyr::filter(as.character(`Draft Year`) %in% input$by_Year)
    
    
    df4
    
  })
  
  
  tryCatch({output$draft_h = renderPlot({
    
    req( length( sort(unlist(input$hitteam)[c(1:10)]) ) >= 1)
    
    if (input$hitgroupbyteam == T & #input$hitshowfit == T & 
        length(input$hitby_Year) >= 1 )
      
      h <- ggplot(hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
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
    
    else if (input$hitgroupbyteam == F & input$hitshowfit == T & 
             length(input$hitby_Year) >= 1)
      h <- ggplot(hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        geom_smooth(aes(x = !!input$hitxvar, y = !!input$hityvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        guides(alpha = "none")
    
    else if (length(input$hitby_Year) >= 1)
      h <- ggplot(hitters_teams_df(), aes(!!input$hitxvar, !!input$hityvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Hitters (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        guides(alpha = "none")
    
    else h = NULL
    
    h
    
  }, height = 500, width = 800, res = 100)
  })
  
  
  
  tryCatch({output$draft_p = renderPlot({
    
    req( length( sort(unlist(input$team)[c(1:10)]) ) >= 1)
    if (input$groupbyteam == T & length(input$by_Year) >= 1)
      
      p <- ggplot(pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        # geom_smooth(aes(x = !!input$xvar, y = !!input$yvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        facet_wrap(~ CVSLTeam) +
        guides(alpha = "none")
    
    # else if (input$groupbyteam == T & input$showfit == F & length(input$by_Year) >= 1)
    #   p <- ggplot(pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
    #     theme(plot.title = element_text(hjust = 0.5))+
    #     labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
    #     geom_point(aes(color = CVSLTeam)) +
    #     scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
    #                                   "forestgreen", "firebrick", "orange", "gray48",
    #                                   "red", "navy", "skyblue")) +
    #     facet_wrap(~ CVSLTeam) +
    #     guides(alpha = "none")
    
    else if (input$groupbyteam == F & input$showfit == T & length(input$by_Year) >= 1)
      p <- ggplot(pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        geom_smooth(aes(x = !!input$xvar, y = !!input$yvar), method = "gam", se = F, color = "violet", linetype = "dashed", na.rm = T)+
        guides(alpha = "none")
    
    else if (length(input$by_Year) >= 1)
      p <- ggplot(pitchers_teams_df(), aes(!!input$xvar, !!input$yvar, alpha = 0.5)) +
        theme(plot.title = element_text(hjust = 0.5))+
        labs(title = "CVSL Auctions, Pitchers (2019-2023)")+
        geom_point(aes(color = CVSLTeam)) +
        scale_color_manual(values = c("darkgoldenrod", "chocolate", "black",
                                      "forestgreen", "firebrick", "orange", "gray48",
                                      "red", "navy", "skyblue")) +
        guides(alpha = "none")
    
    else p = NULL
    
    p
    
  }, height = 500, width = 800, res = 100)
  })
  
}

# run the app ----

shinyApp(ui = ui, server = server)