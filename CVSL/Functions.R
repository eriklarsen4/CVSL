
# not in function ----

"%notin%" = Negate("%in%")


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
      dplyr::mutate(Bats = case_when(grepl(Name, pattern = "\\*") ~ "L",
                                     grepl(Name, pattern = "\\#") ~ "S",
                                     TRUE ~ "R"), .after = 1) %>%
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
      dplyr::mutate(Throws = case_when(grepl(Name, pattern = "\\*") ~ "L",
                                       TRUE ~ "R"), .after = 1) %>%
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


# Baseball Reference player stats import function ----

BR_PLAYER_STATS_IMPORT_fn = function(Year, Player_data_type){
  
  if (Player_data_type == "batting") {
    
    df = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20",Year,"-standard-", Player_data_type,".shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_standard_batting') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::rename_at(30, ~ "Pos.Summary") %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::mutate(Bats = case_when(grepl(Name, pattern = "\\*") ~ "L",
                                     grepl(Name, pattern = "\\#") ~ "S",
                                     TRUE ~ "R"), .after = 2) %>%
      dplyr::mutate(rm_idx = case_when(Pos.Summary == "1" ~ 1,
                                       grepl(Pos.Summary, pattern = "1") ~ 1,
                                       TRUE ~ 0),
                    Year = as.numeric(paste(20,Year, sep = "")),
                    Name = gsub(Name, pattern = "\\*|\\#", replacement = "")) %>%
      dplyr::filter(rm_idx == 0) %>%
      dplyr::select(-Rk, contains("R[a-z]"), -rm_idx)
    
    df2 = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-value-", Player_data_type, ".shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_value_batting') %>%
      html_table() %>%
      as.data.frame()
    
    df2 = df2 %>%
      dplyr::rename_at(24, ~ "Pos.Summary") %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::mutate(Bats = case_when(grepl(Name, pattern = "\\*") ~ "L",
                                     grepl(Name, pattern = "\\#") ~ "S",
                                     TRUE ~ "R"), .after = 2) %>%
      dplyr::mutate(rm_idx = case_when(Pos.Summary == "1" ~ 1,
                                       grepl(Pos.Summary, pattern = "1") ~ 1,
                                       TRUE ~ 0),
                    Year = as.numeric(paste(20,Year, sep = "")),
                    Name = gsub(Name, pattern = "\\*|\\#", replacement = "")) %>%
      dplyr::filter(rm_idx == 0) %>%
      dplyr::select(-Rk, -rm_idx, -Pos.Summary, -Salary, -Acquired, -`waaWL%`, -`162WL%`) %>%
      dplyr::select(Name, contains("R"), contains("W"), Year)
    
    df3 = df %>%
      inner_join(df2, by = c("Name", "Year"))
    
    assign(paste("Player_",Player_data_type,"_20", Year, sep = ""), df3, envir = .GlobalEnv)
    
    
  } else if (Player_data_type == "pitching") {
    
    df = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-standard-",Player_data_type,".shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_standard_pitching') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::mutate(Throws = case_when(grepl(Name, pattern = "\\*") ~ "L",
                                       TRUE ~ "R"), .after = 2) %>%
      dplyr::select(-Rk, -contains("RA9")) %>%
      dplyr::mutate(Year = as.numeric(paste(20, Year, sep = "")),
                    BF = as.numeric(BF),
                    Name = gsub(Name, pattern = "\\*", replacement = "")) %>%
      dplyr::filter(`W-L%` != "") %>%
      dplyr::filter(Name %notin% hitters)
    
    df2 = read_html(
      paste("https://www.baseball-reference.com/leagues/majors/20",Year,"-value-",Player_data_type,".shtml", sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#players_value_pitching') %>%
      html_table() %>%
      as.data.frame()
    
    df2 = df2 %>%
      dplyr::filter(Name != "Name") %>%
      dplyr::mutate(Year = as.numeric(paste(20,Year, sep = "")),
                    Name = gsub(Name, pattern = "\\*", replacement = "")) %>%
      dplyr::select(-Rk, Name, Year, contains("RA9"), WAR, RAA, RAR, -Age, -Tm, -IP, -G, -GS, -R)
    
    df3 = df %>%
      inner_join(df2, by = c("Name", "Year"))
    
    assign(paste("Player_",Player_data_type,"_20", Year, sep = ""), df3, envir = .GlobalEnv)
    
  }
  
}

# Baseball Reference player splits scrape function ----

BR_PLAYER_SPLITS_IMPORT_fn = function(Year, Player_data_type, playerid) {
  
  if (Player_data_type == "batting") {
    
    split_type = "b"
    
    df = read_html(
      paste("https://www.baseball-reference.com/players/split.fcgi?id=",playerid,"&year=20",Year,"&t=",split_type, sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#plato') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::mutate(bbref_id = playerid, .before = 1) %>%
      dplyr::mutate(Year = as.numeric(paste(20,Year,sep = ""))) %>%
      dplyr::filter(Split == "vs LHP" | Split == "vs RHP")
    
    
  } else if (Player_data_type == "pitching") {
    
    split_type = "p"
    
    df = read_html(
      paste("https://www.baseball-reference.com/players/split.fcgi?id=",playerid,"&year=20",Year,"&t=",split_type, sep = "")
    ) %>%
      html_nodes(xpath = '//comment()') %>%
      html_text() %>%
      paste(collapse = '') %>%
      read_html() %>%
      html_node('#plato') %>%
      html_table() %>%
      as.data.frame()
    
    df = df %>%
      dplyr::mutate(bbref_id = playerid, .before = 1) %>%
      dplyr::mutate(Year = as.numeric(paste(20,Year,sep = ""))) %>%
      dplyr::filter(Split == "vs LHB" | Split == "vs RHB")
    
  }
  
  assign(paste(Player_data_type, "_splits_", playerid, "_20", Year, sep = ""), df, envir = .GlobalEnv)
  
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

# ID Adding function ----

    ## site_abbrev == bbref|fg
    
    ## player_type == hitters|pitchers
    
    ## df can be whichever bbref leaderboard

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
    
  } else if (site_abbrev == "fg"){
    
    ## hitters
    if (player_type == "hitters") {
      
      for (i in 1:length(hitternames)) {
        
        if ( length(str_split_1(hitternames[i], pattern = "\\s+")) > 2 ) {
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "II", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "III", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "Jr|Sr") == T) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(hitternames[i], pattern = "\\s+"), pattern = "II|III|Jr|Sr") == T ) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              hitter_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              hitter_playerids[i] = NA_real_
              
            }
          }
        }
        
        if (length(str_split_1(hitternames[i], pattern = "\\s+")) == 2) {
          
          if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
                                              CHAD_LU$name_first == str_split_1(hitternames[i], pattern = "\\s+")[1] )
          ]) > 0 ) {
            
            hitter_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(hitternames[i], pattern = "\\s+")[2] &
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
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "III", perl = T) == T) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "Jr|Sr") == T) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
          
          if ( any(grepl(str_split_1(pitchernames[i], pattern = "\\s+"), pattern = "II|III|Jr|Sr") == T ) ) {
            
            if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
            ]) > 0 ) {
              
              pitcher_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                                               CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
              ]
              
            } else {
              
              pitcher_playerids[i] = NA_real_
              
            }
          }
        }
        
        if (length(str_split_1(pitchernames[i], pattern = "\\s+")) == 2) {
          
          if ( length(CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
                                              CHAD_LU$name_first == str_split_1(pitchernames[i], pattern = "\\s+")[1] )
          ]) > 0 ) {
            
            pitcher_playerids[i] = CHAD_LU$key_fangraphs[which(CHAD_LU$name_last == str_split_1(pitchernames[i], pattern = "\\s+")[2] &
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
  
  }
  return(df)
}
