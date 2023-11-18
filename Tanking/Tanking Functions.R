
# not in function ----

"%notin%" = Negate("%in%")

# Baseball Almanac Draft data import function ----
BBA_IMPORT_fn = function(Year, selector){
  ## Read in the URL with desired year and selector
  df = rvest::read_html(paste("https://www.baseball-almanac.com/draft/baseball-draft.php?yr=20", Year, sep = "")
  ) %>%
    html_element(css = almanac_selector) %>%
    html_table() %>%
    as.data.frame() %>% 
    dplyr::slice(c(2:n())) %>%
    dplyr::rename("Rd" = "X1",
                  "#" = "X2",
                  "Phase" = "X3",
                  "Player Name" = "X4",
                  "Drafted By" = "X5",
                  "POS" = "X6",
                  "Drafted From" = "X7") %>%
    dplyr::slice(
      c( 2:(n()-1)
      )
    )
  
  ## Co-erce the Name strings into UTF-8 encoding
  Encoding(df$`Player Name`) = "UTF-8"
  df$`Player Name` = iconv(df$`Player Name`, "UTF-8", "UTF-8", sub = " ")
  
  ## Coerce Rd and pick (`#`) numbers to be numeric (integers)
  ## Add a "year" column for eventual df merging
  df = df %>%
    dplyr::mutate(Rd = as.numeric(Rd),
                  `#` = as.numeric(`#`),
                  Year = as.numeric(paste("20", Year, sep = "")))
  
  ## Save the dataframe as a variable to the global environment
  assign(paste("BBA20", Year, sep = ""), df, envir = .GlobalEnv)
}

# Baseball Reference Standings data import function ----

BR_STAND_IMPORT_fn = function(Year){
  
  df = read_html(
    paste("https://www.baseball-reference.com/leagues/majors/20",
          Year,
          "-standings.shtml#expanded_standings_overall",
          sep = "")
  ) %>%
    html_table()
  ## Combine the divisions into one standings dataframe
  df = purrr::reduce(df[][], full_join, by = c("Tm", "W", "L", "W-L%", "GB"))
  ## Convert the table into a dataframe
  df = as.data.frame(df)
  ## Add rankings within divisions
  df$DivRank = rep(c(1:5), 6)
  ## Re-order by most wins
  df = df[order(df$W, decreasing = TRUE),]
  ## Add a column to track the year for future df merging
  df$Year = as.numeric(paste("20", Year, sep = ""))
  ## Save the dataframe as a variable to the global environment
  assign(paste("Standings_20", Year, sep = ""), df, envir = .GlobalEnv)
  
}

# Baseball Reference player value data import function ----

BR_PLAYER_VAL_IMPORT_fn = function(Year, Player_data_type){
  
  if (Player_data_type == "batting") {
    
    df = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-value-batting.shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_value_batting') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::rename_at(24, ~ "Pos.Summary") %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::filter(!grepl(Pos.Summary, pattern = "1")) %>%
      dplyr::select(-Rk, contains("R[a-z]")) %>%
      dplyr::mutate(Year = as.numeric(paste(20, Year, sep = "")))
    
      assign(paste("Player_Value_",Player_data_type,"_20", Year, sep = ""), df, envir = .GlobalEnv)
    
  } else if (Player_data_type == "pitching") {
    
    df = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-value-pitching.shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_value_pitching') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::select(-Rk, -contains("RA9")) %>%
      dplyr::mutate(Year = as.numeric(paste(20, Year, sep = "")))
    
      assign(paste("Player_Value_",Player_data_type,"_20", Year, sep = ""), df, envir = .GlobalEnv)
    
  }
  
  
}
# Baseball Reference player value data cleaning functions ----


BR_PLAYER_VAL_CLEAN_fn = function(df, Player_data_type){
  
  if (Player_data_type == "batting") {
    
    df = df %>%
      dplyr::mutate(Name = gsub(Name, pattern = "\\*|\\#", replacement = ""))
    
    # Encoding(df$Name) = "UTF-8"
    # df$Name = iconv(df$Name, "UTF-8", "UTF-8", sub = " ")
    
    ## Remove pitchers from the hitters df
    df = df %>%
      dplyr::mutate(idx = case_when(grepl(Pos.Summary, pattern = "1|\\/1|([0-9]-Jan)") ~ 1,
                                    TRUE ~ 0)
      ) %>%
      dplyr::filter(idx == 0) %>%
    
    # df = df %>%
    #   dplyr::mutate(Name = case_when(grepl(Name, pattern = "<a0>") ~ gsub(Name, pattern = "<a0>", replacement = " "),
    #                                  grepl(Name, pattern = "<ed>") ~ gsub(Name, pattern = "<ed>", replacement = "i"),
    #                                  grepl(Name, pattern = "<e1>") ~ gsub(Name, pattern = "<e1>", replacement = "a"),
    #                                  grepl(Name, pattern = "<fa>") ~ gsub(Name, pattern = "<fa>", replacement = "u"),
    #                                  grepl(Name, pattern = "<e9>") ~ gsub(Name, pattern = "<e9>", replacement = "e"),
    #                                  TRUE ~ Name)) %>%
      dplyr::select(-idx)
    
    df$Salary = as.numeric(gsub(df$Salary, pattern = "\\$|\\,", replacement = ""))/1000000
    rownames(df) = NULL
    return(df)
    
  } else if (Player_data_type == "pitching") {
    
    df = df %>%
      dplyr::mutate(Name = gsub(Name, pattern = "\\*|\\#", replacement = ""))
    ## Convert to UTF-8 and remove special characters
    # Encoding(df$Name) = "UTF-8"
    # df$Name = iconv(df$Name, "UTF-8", "UTF-8", sub = " ")
    # 
    # df = df %>%
    #   # dplyr::mutate(Throws = case_when(grepl(Name, pattern = "<a0>") ~ "L",
    #   #                                  TRUE ~ "R"), .after = 1) %>%
    #   dplyr::mutate(Name = case_when(grepl(Name, pattern = "<a0>") ~ gsub(Name, pattern = "<a0>", replacement = " "),
    #                                  grepl(Name, pattern = "<ed>") ~ gsub(Name, pattern = "<ed>", replacement = "i"),
    #                                  grepl(Name, pattern = "<e1>") ~ gsub(Name, pattern = "<e1>", replacement = "a"),
    #                                  grepl(Name, pattern = "<fa>") ~ gsub(Name, pattern = "<fa>", replacement = "u"),
    #                                  grepl(Name, pattern = "<e9>") ~ gsub(Name, pattern = "<e9>", replacement = "e"),
    #                                  TRUE ~ Name))
    ## Convert the "salary" column
    df$Salary = as.numeric(gsub(df$Salary, pattern = "\\$|\\,", replacement = ""))/1000000
    rownames(df) = NULL
    return(df)
    
  }
}

# ID Adding function ----
ID_ADD_fn = function(df, player_type, site_abbrev) {
  
  hitter_playerids = vector()
  hitter_last_init = vector()
  
  pitcher_playerids = vector()
  pitcher_last_init = vector()
  
  if (site_abbrev == "bbref"){
    
    ## hitters
    if (player_type == "hitters") {
      
      for (i in 1:length(hitternames)) {
        
        if ( length(str_split_1(hitternames[i], pattern = "\\s+")) > 2 ) {
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "II", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "III", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "Jr|Sr") == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "II|III|Jr|Sr") == T ) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
        }
        
        if (length(str_split_1(hitternames[i], pattern = "\\s+")) == 2) {
          
          if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
          ]) > 0 ) {
            
            hitter_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                            CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]
            
          } else {
            
            hitter_playerids[i] = NA_real_
            
          }
        } else {
          
          hitter_playerids[i] = NA_real_
          
        }
      }
      
      for (i in 1:length(hitternames)) {
        
        if (!is.na(hitter_playerids[i]) ){
          
          hitter_last_init[i] = str_extract(hitter_playerids[i], pattern = "[a-z]{1}")
          
        } else {
          
          hitter_last_init[i] = NA_real_
          
        }
      }
      
      df = df %>%
        dplyr::mutate(bbref_id = hitter_playerids, .after = 1) %>%
        dplyr::mutate(lastinit = hitter_last_init, .after = 1)
      
      hitter_playerids <<- hitter_playerids
      hitter_last_init <<- hitter_last_init
    }
    
    ## pitchers
    if (player_type == "pitchers") {
     
      for (i in 1:length(pitchernames)) {
        
        if ( length(str_split_1(pitchernames[i], pattern = "\\s+")) > 2 ) {
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "II", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "III", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "Jr|Sr") == T) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "II|III|Jr|Sr") == T ) ) {
            
            if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
        }
        
        if (length(str_split_1(pitchernames[i], pattern = "\\s+")) == 2) {
          
          if ( length(CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                              CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
          ]) > 0 ) {
            
            pitcher_playerids[i] = CHAD_LU$key_bbref[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                             CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]
            
          } else {
            
            pitcher_playerids[i] = NA_real_
            
          }
        } else {
          
          pitcher_playerids[i] = NA_real_
          
        }
      }
      
      for (i in 1:length(pitchernames)) {
        
        if (!is.na(pitcher_playerids[i]) ){
          
          pitcher_last_init[i] = str_extract(pitcher_playerids[i], pattern = "[a-z]{1}")
          
        } else {
          
          pitcher_last_init[i] = NA_real_
          
        }
      }
      
      df = df %>%
        dplyr::mutate(bbref_id = pitcher_playerids, .after = 1) %>%
        dplyr::mutate(lastinit = pitcher_last_init, .after = 1)
      
      pitcher_playerids <<- pitcher_playerids
      pitcher_last_init <<- pitcher_last_init
    }
    
    return(df)
    
  }
}


# Baseball Reference Team Salary/Payroll import function ----

BR_TEAM_VALUE_IMPORT_fn = function(Year, Player_data_type) {
  ## Read in the URL with desired year and player type
  df = read_html(
    paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-value-", Player_data_type, ".shtml", sep = "")
  ) %>%
    html_table()
  ## Convert the table into a dataframe
  df = as.data.frame(df)
  ## Remove the duplicate row on the bottom
  df = df[ -32, ]
  ## Remove the "$" signs and ","s and divide the salaries by 1M
  df$Salary = as.numeric(
    gsub(df$Salary, pattern = "\\$|\\,", replacement = ""))/1000000
  ## Add a column to track the year for future df merging
  df$Year = as.numeric(paste("20", Year, sep = ""))
  ## Save the dataframe as a variable to the global environment
  assign(paste("Team_", Player_data_type, "value_20", Year, sep = ""), df, envir = .GlobalEnv)
}

# Baseball Reference Player Team Change import function ----

BR_PLAYER_CHANGER_IMPORT_fn = function(year, lastinitial, bbref_id, Player_data_type){
  
  df = read_html(
    paste("https://www.baseball-reference.com/players/",lastinitial,"/",bbref_id,".shtml", sep = "")
  ) %>%
    html_element(css = paste('#',Player_data_type,'_standard', sep = "")) %>%
    html_table() %>%
    as.data.frame() %>%
    dplyr::select(Year, Tm) %>%
    dplyr::filter(Year == year) %>%
    dplyr::filter(Tm != "TOT") %>%
    dplyr::filter(grepl(Tm, pattern = "[A-Z]{3}$")) %>%
    dplyr::mutate(lastinit = lastinitial, .before = 1) %>%
    dplyr::mutate(bbref_id = bbref_id, .after = 1) %>%
    dplyr::mutate(Year = as.numeric(Year))
  assign(paste("Changer_",bbref_id,"_", year, sep = ""), df, envir = .GlobalEnv)

}

# Baseball Almanac draft data analysis function ----

DRAFT_DF_DATA_ANALYSIS_fn = function(df, Team){
  
  ## Loop through and determine how many of which position a given team drafted in a given draft; how many of all were college picks
  for(i in levels(as.factor(df$POS[which(df$Tm == Team)]))){
    print(
      paste(
        sum(str_count(df$POS[which(df$Tm == Team)], i)),
        i,
        ";",
        length(which(grepl(df$`Drafted From`[ which(df$Tm == Team)],
                           pattern = "University|College|UCLA|Cal Poly|Academy of Art|Institute of Technology|UNLV|State|Virginia Tech") == TRUE)),
        "/",
        length(which(df$Tm == Team) == TRUE),
        "College Picks")
    )
  }
}

# ESPN Free Agent Data import function ----

ESPN_FA_import_fn = function(Year, selector){
  
  df = read_html(
    paste("https://www.espn.com/mlb/freeagents/_/year/20", Year, "/type/signed", sep = "")
  ) %>%
    html_element(css = selector) %>%
    html_table()
  df = as.data.frame(df)
  df = df %>%
    dplyr::slice(c(2:nrow(df))) %>%
    dplyr::filter(X1 != "PLAYER") %>%
    dplyr::filter(X1 != "Total:") %>%
    dplyr::select(-4, -5, -8) %>%
    dplyr::mutate(TotalValue = X9,
                  TotalValue = gsub(TotalValue, pattern = "--", replacement = "0"),
                  TotalValue = gsub(TotalValue, pattern = "\\$", replacement = ""),
                  TotalValue = gsub(TotalValue, pattern = "\\,", replacement = ""),
                  TotalValue = gsub(TotalValue, pattern = "Minor Lg", replacement = "0"),
                  TotalValue = as.numeric(TotalValue) / 1000000,
                  Type = X9,
                  Type = case_when(Type == "--" ~ "Undisclosed",
                                   Type == "Minor Lg" ~ "MiLB",
                                   grepl(Type, pattern = "\\,") ~ "MLB",
                                   TRUE ~ "")
    ) %>%
    dplyr::select(-X9) %>%
    dplyr::rename("Name" = "X1",
                  "Position" = "X2",
                  "Age" = "X3",
                  "Team" = "X6",
                  "Yrs" = "X7") %>%
    dplyr::mutate(AAV = as.numeric(TotalValue) / as.numeric(Yrs)) %>%
    dplyr::select(Name, Position, Age, Team, AAV, TotalValue, Yrs, Type) %>%
    dplyr::mutate(Age = as.numeric(Age) - (2021 - as.numeric(paste("20",Year, sep = ""))),
                  Year = paste("20", Year, sep = ""))
  
  
  
  # df = read_html(
  #   paste("https://www.espn.com/mlb/freeagents/_/year/20", Year, "/type/signed", sep = "")
  # ) %>%
  #   html_element(css = selector) %>%
  #   html_table()
  # df = as.data.frame(df)
  # df = df[ c(1,nrow(df)), -c(4,5,8)]
  # df = df[ -which(df$X1 == "PLAYER"), ]
  # ## Duplicate the total value column
  # df$TotalValue = df$X9
  # ## Add a column of character values, describing whether the contract was a minors contract
  # df$Type = ""
  # ## Fill values of Minor Lg contracts with "MiLB" string
  # 
  # df = df %>%
  #   dplyr::mutate(Type = case_when(length(which(grepl(X9, pattern = "Minor Lg") == T)) > 0 ~ "MiLB",
  #                                  length(which(grepl(X9, pattern = "--") == T)) > 0 ~ "Undisclosed",
  #                                  grepl(X9, pattern = "\\,") ~ "MLB",
  #                                  TRUE ~ ""),
  #                 X9 = case_when(grepl(X9, pattern = "--") ~ "",
  #                                grepl(X9, pattern = "Minor Lg") ~ "",
  #                                TRUE ~ X9),
  #                 X9 = gsub(X9, pattern = "\\$", replacement = ""),
  #                 X9 = gsub(X9, pattern = "\\,", replacement = ""),
  #                 TotalValue = gsub(TotalValue, pattern = "\\$", replacement = ""),
  #                 TotalValue = gsub(TotalValue, pattern = "\\,", replacement = ""),
  #                 TotalValue = as.numeric(TotalValue) / 1000000, # data is in M of USD
  #                 X9 = as.numeric(X9) / as.numeric(X7) / 1000000) %>%
  #   dplyr::mutate(year = "", .before = 2) %>%
  #   dplyr::mutate(year = as.numeric(paste("20", Year, sep = ""))) %>%
  #   dplyr::mutate(Year = year, .before = 2) %>%
  #   dplyr::select(-year) %>%
  #   dplyr::mutate(Age = as.numeric(Age) - (2021 - as.numeric(Year)))
  # 
  # colnames(df)[c(1,3,7)] = c("Name", "Position", "Age", "Team", "Years", "AAV")
  
  
  # if ( length(
  #   which(grepl(df$X9, pattern = "Minor Lg") == TRUE)
  # ) > 0){
  #   df$Type[ which(grepl(df$X9, pattern = "Minor Lg") == TRUE) ] = "MiLB"
  # } 
  # if ( length(
  #   which(grepl(df$X9, pattern = "--") == TRUE) 
  # ) > 0 ){
  #   df$Type[ which(grepl(df$X9, pattern = "--") == TRUE) ] = "Undisclosed"
  # }
  # df$Type[ which(grepl(df$X9, pattern = "\\,") == TRUE) ] = "MLB"
  # 
  # ## Change undisclosed contract values to 0
  # df$X9[ which(grepl(df$X9, pattern = "--") == TRUE) ] = 0
  # ## Change minor league deal values to 0
  # df$X9[ which(grepl(df$X9, pattern = "Minor Lg") == TRUE) ] = 0
  # 
  # ## Change undisclosed contract values to 0
  # df$TotalValue[ which(grepl(df$TotalValue, pattern = "--") == TRUE) ] = 0
  # ## Change minor league deal values to 0
  # df$TotalValue[ which(grepl(df$TotalValue, pattern = "Minor Lg") == TRUE) ] = 0
  # ## Remove dollar signs and commas from value; replace ,s and divide by 1M to display values in terms of millions of dollars
  # df$X9 = gsub(df$X9, pattern = "\\$", replacement = "")
  # df$X9 = gsub(df$X9, pattern = "\\,", replacement = "")
  # df$TotalValue = gsub(df$TotalValue, pattern = "\\$", replacement = "")
  # df$TotalValue = gsub(df$TotalValue, pattern = "\\,", replacement = "")
  # df$TotalValue = as.numeric(df$TotalValue) / 1000000 ## Data is in Millions of USD
  # df$X9 = (as.numeric(df$X9) / as.numeric(df$X7)) / 1000000
  # df = add_column(df, Year = "", .before = 2)
  # df[,2] = as.numeric(paste("20", Year, sep = ""))
  # colnames(df)[c(1,3:7)] = c("Name", "Position", "Age", "Team", "Years", "AAV")
  # 
  # df$Age = as.numeric(df$Age) - (2021 - as.numeric(df$Year) )
  
  # for (i in 11:21){
  #   if ( df$Year == as.numeric(
  #     paste("20", i, sep = "") )
  #   ) {
  #     
  #   }
  # }
  
  assign(paste("ESPN_FA_20", Year, sep = ""), df, envir = .GlobalEnv)
  return(df)
  
}

# Spotrac Intl Signing Data import function ----

spotrac_Intl_selector = "#main > div > div.teams > table"

## Create a function that will scrape International Signing Data from Spotrac and clean the dataframe
Spotrac_Intl_import_fn = function(Year, spotrac_selector){
  df = read_html(
    paste("https://www.spotrac.com/mlb/international/20", Year, "/", sep = "")
  ) %>% 
    html_element(css = spotrac_selector) %>%
    html_table()
  df = as.data.frame(df)
  colnames(df) = c("Player", "Pos", "Country", "Team", "Bonus")
  df$Bonus = as.numeric(gsub(df$Bonus, pattern = "\\$|\\,", replacement = ""))/1000000
  df$Year = as.numeric(paste("20", Year, sep = ""))
  assign(paste("Intl_Signings_20", Year, sep = ""), df, envir = .GlobalEnv)
}