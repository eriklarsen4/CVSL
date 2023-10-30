
# Baseball Almanac Draft data import function ----
BBA_IMPORT_fn = function(Year, selector){
  ## Read in the URL with desired year and selector
  df = rvest::read_html(
    paste("https://www.baseball-almanac.com/draft/baseball-draft.php?yr=20",
          Year,
          sep = "")
  ) %>%
    html_element(css = selector) %>%
    html_table()
  ## Remove the first row
  df = df[ -1, ]
  ## Re-name the columns
  colnames(df) = df[ 1, ]
  ## Remove the first row and last two rows
  df = df[ -1, ]
  df = df[ -nrow(df), ]
  df = df[ -nrow(df), ]
  
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
  
  df = read_html(
    paste("https://www.baseball-reference.com/leagues/majors/20", Year, "-value-", Player_data_type,".shtml", sep = "")
  ) %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%
    html_node(paste('#players_value_',Player_data_type, sep = "")) %>%
    html_table() %>%
    as.data.frame() %>%
    assign(paste("Player_Value_",Player_data_type,"_20", Year, sep = ""), df, envir = .GlobalEnv)
  
}
# Baseball Reference player value data cleaning functions ----


BR_PLAYER_VAL_CLEAN_fn = function(){
  
  
  read_html(
    paste("https://www.baseball-reference.com/leagues/majors/", year, "-value-batting.shtml", sep = "")
  ) %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%
    html_node(paste('#players_value_',Player_data_type, sep = "")) %>%
    html_table() %>% as.data.frame()
  
  
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
  df = df[ -c(1,nrow(df)), -c(4,5,8)]
  df = df[ -which(df$X1 == "PLAYER"), ]
  ## Duplicate the total value column
  df$TotalValue = df$X9
  ## Add a column of character values, describing whether the contract was a minors contract
  df$Type = ""
  ## Fill values of Minor Lg contracts with "MiLB" string
  if ( length(
    which(grepl(df$X9, pattern = "Minor Lg") == TRUE)
  ) > 0){
    df$Type[ which(grepl(df$X9, pattern = "Minor Lg") == TRUE) ] = "MiLB"
  } 
  if ( length(
    which(grepl(df$X9, pattern = "--") == TRUE) 
  ) > 0 ){
    df$Type[ which(grepl(df$X9, pattern = "--") == TRUE) ] = "Undisclosed"
  }
  df$Type[ which(grepl(df$X9, pattern = "\\,") == TRUE) ] = "MLB"
  
  ## Change undisclosed contract values to 0
  df$X9[ which(grepl(df$X9, pattern = "--") == TRUE) ] = 0
  ## Change minor league deal values to 0
  df$X9[ which(grepl(df$X9, pattern = "Minor Lg") == TRUE) ] = 0
  
  ## Change undisclosed contract values to 0
  df$TotalValue[ which(grepl(df$TotalValue, pattern = "--") == TRUE) ] = 0
  ## Change minor league deal values to 0
  df$TotalValue[ which(grepl(df$TotalValue, pattern = "Minor Lg") == TRUE) ] = 0
  ## Remove dollar signs and commas from value; replace ,s and divide by 1M to display values in terms of millions of dollars
  df$X9 = gsub(df$X9, pattern = "\\$", replacement = "")
  df$X9 = gsub(df$X9, pattern = "\\,", replacement = "")
  df$TotalValue = gsub(df$TotalValue, pattern = "\\$", replacement = "")
  df$TotalValue = gsub(df$TotalValue, pattern = "\\,", replacement = "")
  df$TotalValue = as.numeric(df$TotalValue) / 1000000 ## Data is in Millions of USD
  df$X9 = (as.numeric(df$X9) / as.numeric(df$X7)) / 1000000
  df = add_column(df, Year = "", .before = 2)
  df[,2] = as.numeric(paste("20", Year, sep = ""))
  colnames(df)[c(1,3:7)] = c("Name", "Position", "Age", "Team", "Years", "AAV")
  
  df$Age = as.numeric(df$Age) - (2021 - as.numeric(df$Year) )
  
  for (i in 11:21){
    if ( df$Year == as.numeric(
      paste("20", i, sep = "") )
    ) {
      
    }
  }
  
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