Tanking Data Wrangling
================
Erik Larsen
4/23/2022

# Overview

This snippet was used in [my
article](https://www.theleftycatcher.com/post/diving-deep-into-tanking)
on [TheLeftyCatcher](https://www.theleftycatcher.com), where I compared
and contrasted how `Jeff Luhnow` and `Theo Epstein` tanked to rebuild
the `Chicago Cubs` and `Houston Astros` into World Series winners.

I scraped:

-   `MLB Free Agent` data (ESPN)
-   `MLB Amateur Draft` data (Baseball Almanac)
-   `International Signing` data (Spotrac)
-   `MLB Standings` data (Baseball Reference)
-   `MLB Payroll` data (Baseball Referencee)
-   “`MLB Value`” data (Baseball Reference)

It required some processing to compile and analyze with or without
graphs

The functions used are sourced from
[this](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Functions.R)
file

I created a [separate markdown file for the
graphs](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking-Graphs.md)

# Environment Prep

Load packages and functions

## Load packages

``` r
  ## Data wrangling; df re-arrangement, string manipulations
library(readr)
# library(reshape2)
library(tidyverse)
library(stringr)
# library(stringi)

  ## Web scraping
library(rvest)
library(XML)

  ## Plotting
library(ggplot2)
library(ggrepel)
# library(GGally)
library(ggpubr)
library(plotly)

  ## Modeling
library(mgcv)
library(splines)

## showing tables
library(DT)

source("~/GitHub/Baseball/Tanking/Tanking Functions.R")
```

# Baseball Reference Standings Data

Import `Baseball Reference`’s `Standings` data over relevant years
(`2011`-`2021`)

## Scrape and Clean Baseball Reference Standings Data

``` r
  ## Loop through and import the standings from 2011-2021
for ( i in 11:21 ) {
  if (i %% 17 == 0 ) {
    
    Sys.sleep(60)
    BR_STAND_IMPORT_fn(Year = i)
    
  } else {
    
    BR_STAND_IMPORT_fn(Year = i)
    
  }
}
  ## merge the dfs together
StandALL = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "Standings") == TRUE)]), full_join)
  ## correct team name changes
StandALL = StandALL %>%
  dplyr::mutate(Tm = case_when(grepl(Tm, pattern = "Angels") ~ "Los Angeles Angels",
                               grepl(Tm, pattern = "Florida") ~ "Miami Marlins",
                               grepl(Tm, pattern = "Indians") ~ "Cleveland Guardians",
                               TRUE ~ Tm))
  ## remove the individual dfs
rm(list = ls()[which(grepl(ls(), pattern = "Standings") == T)])
```

# Baseball Reference Player Value Data

Scrape `BBRef`’s `player value` data

## Scrape “player value” Data from `Baseball Reference`

``` r
# rm(list = ls()[which(grepl(ls(), pattern = "Player_Value") == T)])

for ( i in 11:21 ){
  if (i %% 17 == 0 ) {
    
      Sys.sleep(60)
    
    BR_PLAYER_VAL_IMPORT_fn(Year = i, Player_data_type = "batting")
    
    } else {
      
      BR_PLAYER_VAL_IMPORT_fn(Year = i, Player_data_type = "batting")
  
    }
}

for ( i in 11:21 ){
  if (i %% 17 == 0 ) {
    
      Sys.sleep(60)
    
    BR_PLAYER_VAL_IMPORT_fn(Year = i, Player_data_type = "pitching")
    
    } else {
      
      BR_PLAYER_VAL_IMPORT_fn(Year = i, Player_data_type = "pitching")
  
    }
}
```

## Clean the BR Player Value Data

Clean the data:

-   names in these tables scraped from `Baseball Reference` are encoded
    differently from the `CHADWICK DB` and cannot be coerced for joining

-   scraped data in these tables do not contain `Baseball Reference`
    `playerid`s

    -   add `playerid`s from the `CHADWICK DB` in order to scrape
        individual player pages to account for players that changed
        teams within a given season

``` r
  ## Combine the each data "type" into one df
Player_Value_Batting_ALL = purrr::reduce(mget(ls()[which(grepl(ls(),
                                                               pattern = "Value_batting") == T)]),
                                         full_join)

# Player_Value_pitching_2021 = Player_Value_pitching_2021 %>%
#   dplyr::select(-RA9extras)
Player_Value_Pitching_ALL = purrr::reduce(mget(ls()[which(grepl(ls(),
                                                                pattern = "Value_pitching") == T)]),
                                          full_join)
  ## remove individual (yearly) dfs
rm(list = ls()[which(grepl(ls(), pattern = "batting_2|pitching_2") == T)])

Player_Value_Pitching_ALL = BR_PLAYER_VAL_CLEAN_fn(df = Player_Value_Pitching_ALL, Player_data_type = "pitching")
Player_Value_Batting_ALL = BR_PLAYER_VAL_CLEAN_fn(df = Player_Value_Batting_ALL, Player_data_type = "batting")

pitchernames = Player_Value_Pitching_ALL %>%
  dplyr::select(Name) %>%
  as.vector() %>%
  unlist() %>%
  as.character()
hitternames = Player_Value_Batting_ALL %>%
  dplyr::select(Name) %>%
  as.vector() %>%
  unlist() %>%
  as.character()


## use the Chadwick DB as a cross-ref for playerids
CHAD_LU = baseballr::chadwick_player_lu()

  ## concatenate the separated strings for players with initials as first names;
  ## remove players without bbref ids and remove excess columns
CHAD_LU = CHAD_LU %>%
  dplyr::mutate(name_first = case_when(grepl(name_first, pattern = "[A-Z]\\.") ~
                                         gsub(name_first, pattern = "\\s", replacement = ""),
                                       TRUE ~ name_first)) %>%
  dplyr::filter(key_bbref != "") %>%
  dplyr::mutate(Name = paste(name_first, name_last, sep = " ")) %>%
  dplyr::select(contains("name"),
                contains("Name"),
                key_mlbam,
                key_bbref,
                key_fangraphs,
                key_retro,
                contains("mlb_played"))

## because of encoding discrepancies
  ## will need to loop through and assign bbref ids and last initials for each player in the dataset
  
  ## function created for this
    ## used to enable scraping individual bbref pages (for when players changed teams in this project)
Player_Value_Batting_ALL = ID_ADD_fn(df = Player_Value_Batting_ALL,
                                     player_type = "hitters",
                                     site_abbrev = "bbref")

Player_Value_Pitching_ALL = ID_ADD_fn(df = Player_Value_Pitching_ALL,
                                      player_type = "pitchers",
                                      site_abbrev = "bbref")
```

Correct hitter name spellings and IDs (not shown)

Repeat for pitchers and remove hitters from the data frame (not shown)

## Determine Career-Level bWAR and Value

-   Aggregate the player value data to obtain `Career bWAR`,
    `Career Salary`, and `Service time`

-   Aggregate the player value data to obtain `Player-Year-level` `bWAR`
    and `Salaries`

``` r
CAREER_hitter_WAR_df = Player_Value_Batting_ALL %>%
  dplyr::group_by(Name, bbref_id) %>%
  dplyr::summarize(Career_bWAR = as.numeric(round(sum(as.numeric(WAR), na.rm = T), 1)),
                   Career_Sal = sum(Salary, na.rm = T),
                   Service_time = max(as.numeric(Age)) - min(as.numeric(Age)))

CAREER_pitcher_WAR_df = Player_Value_Pitching_ALL %>%
  dplyr::group_by(Name, bbref_id) %>%
  dplyr::summarize(Career_bWAR = as.numeric(round(sum(as.numeric(WAR), na.rm = T), 1)),
                   Career_Sal = sum(Salary, na.rm = T),
                   Service_time = max(as.numeric(Age)) - min(as.numeric(Age)))

CAREER_WAR_df = CAREER_hitter_WAR_df %>%
  dplyr::full_join(CAREER_pitcher_WAR_df, by = c("Name", "bbref_id", "Career_bWAR", "Career_Sal", "Service_time")) %>%
  dplyr::arrange(desc(Career_bWAR))



Hitter_year_WAR_df = Player_Value_Batting_ALL %>%
  dplyr::group_by(Name, bbref_id, Year) %>%
  dplyr::summarize(WAR = as.numeric(round(sum(as.numeric(WAR), na.rm = T), 1)),
                   Age = as.numeric(round(max(as.numeric(Age), na.rm = T))),
                   Salary = as.numeric(mean(Salary, na.rm = T))) %>%
  dplyr::ungroup() %>%
  dplyr::select(Name, bbref_id, Year, WAR, Salary, Age)

Pitcher_year_WAR_df = Player_Value_Pitching_ALL %>%
  dplyr::group_by(Name, bbref_id, Year) %>%
  dplyr::summarize(WAR = as.numeric(round(sum(as.numeric(WAR), na.rm = T), 1)),
                   Age = as.numeric(round(max(as.numeric(Age), na.rm = T))),
                   Salary = as.numeric(mean(Salary, na.rm = T)),
                   Year = max(Year, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Name, bbref_id, Year, WAR, Salary, Age)

PLAYER_YEAR_WAR_df = Hitter_year_WAR_df %>%
  dplyr::full_join(Pitcher_year_WAR_df, by = c("Name", "bbref_id", "Year", "WAR", "Salary", "Age")) %>%
  dplyr::arrange(desc(as.numeric(Salary)))

rm(list = ls()[which(grepl(ls(), pattern = "CAREER_hitter|CAREER_pitcher|Hitter_year|Pitcher_year"))])
```

# Baseball Reference Team Salary/Payroll data

Acquire team salary data

## Scrape and Clean Team Salary/Payroll data

``` r
  ## Loop through the relevant years and import the tables as dataframes
for ( i in 11:21 ){
  if (i %% 17 == 0 ) {
    
      Sys.sleep(60)
      BR_TEAM_VALUE_IMPORT_fn(Year = i, Player_data_type = "batting")
    
    } else {
      
      BR_TEAM_VALUE_IMPORT_fn(Year = i, Player_data_type = "batting")
    }
}
  ## Repeat the loop for pitching stats
for ( i in 11:21 ){
  if( i %% 17 == 0 ) {
    
    Sys.sleep(60)
    BR_TEAM_VALUE_IMPORT_fn(Year = i, Player_data_type = "pitching")
    
  } else {
    
    BR_TEAM_VALUE_IMPORT_fn(Year = i, Player_data_type = "pitching")
  }
}
  ## Concatenate into a aggregate dataframes by player type
TeamBatALL = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "Team_batting") == T)]), full_join)

  ## Remove the Xinn pitching column
Team_pitchingvalue_2021 = Team_pitchingvalue_2021 %>%
  dplyr::select(-RA9extras)

TeamPitALL = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "Team_pitching") == T)]), full_join)

TeamPitALL = TeamPitALL %>%
  dplyr::filter(Tm != "0" &
                  Tm != "") %>%
  dplyr::mutate(Tm = case_when(grepl(Tm, pattern = "Angels") ~ "Los Angeles Angels",
                               grepl(Tm, pattern = "Marlins") ~ "Miami Marlins",
                               grepl(Tm, pattern = "Cleveland") ~ "Cleveland Guardians",
                               TRUE ~ Tm))

## Remove league summ rows and correct team names
TeamBatALL = TeamBatALL %>%
  dplyr::filter(Tm != "0" &
                  Tm != "") %>%
  dplyr::mutate(Tm = case_when(grepl(Tm, pattern = "Angels") ~ "Los Angeles Angels",
                               grepl(Tm, pattern = "Marlins") ~ "Miami Marlins",
                               grepl(Tm, pattern = "Cleveland") ~ "Cleveland Guardians",
                               TRUE ~ Tm))
rm(list = ls()[which(grepl(ls(), pattern = "Team_batting|Team_pitching") == T)])
```

## Clean Individual Player Salary/Payroll Data

Add player acquisition data to the cleaned `Baseball Reference` player
value data

-   first, identify players who changed teams

``` r
Team_Changers_hitters = Player_Value_Batting_ALL %>%
  dplyr::filter(grepl(Tm, pattern = "TM")) %>%
  dplyr::mutate(Tms = Tm) %>%
  dplyr::select(Name, lastinit, bbref_id, Year, Tms, Salary, Acquired) %>%
  dplyr::distinct()


Team_Changers_pitchers = Player_Value_Pitching_ALL %>%
  dplyr::filter(grepl(Tm, pattern = "TM")) %>%
  dplyr::mutate(Tms = Tm) %>%
  dplyr::select(Name, lastinit, bbref_id, Year, Tms, Salary, Acquired) %>%
  dplyr::distinct()
```

-   next, scrape the player’s `Baseball Reference` player page to
    identify which teams for whom these players played

    -   for hitters first

    -   aggregate the data

    -   repeat for pitchers

-   aggregate all the changers’ data into a `Team_Transactions` df

-   a **negative salary change = to dumping payroll**

-   **positive salary change = adding payroll**

-   Join the `Team_Transactions_Sum` df to the `TeamBatALL`,
    `TeamPitALL` dfs and summarize

``` r
  ## Join with Team Value data and remove extra data
TeamBatALL = TeamBatALL %>%
  left_join(Team_Transactions_Sum) %>%
  # dplyr::filter(Year != 2020) %>%
  dplyr::select(Team, Tm, Type, Year, WAR, Salary,
                Sum_Sal_Change, Sum_Num_Trades,
                Sum_Num_FAs, Sum_Num_Claims, Sum_Acqs) %>%
  dplyr::arrange(Sum_Sal_Change)

TeamPitALL = TeamPitALL %>%
  left_join(Team_Transactions_Sum) %>%
  # dplyr::filter(Year != 2020) %>%
  dplyr::select(Team, Tm, Type, Year, WAR, Salary,
                Sum_Sal_Change, Sum_Num_Trades,
                Sum_Num_FAs, Sum_Num_Claims, Sum_Acqs) %>%
  dplyr::arrange(Sum_Sal_Change)

TeamValALL = TeamBatALL %>%
  full_join(TeamPitALL) %>%
  dplyr::mutate(WAR = as.double(WAR)) %>%
  dplyr::group_by(Team, Tm, Year) %>%
  dplyr::summarize(WAR = sum(WAR),
                   Salary = sum(Salary),
                   Sum_Sal_Change = mean(Sum_Sal_Change),
                   Sum_Num_Trades = mean(Sum_Num_Trades),
                   Sum_Num_FAs = mean(Sum_Num_FAs),
                   Sum_Num_Claims = mean(Sum_Num_Claims),
                   Sum_Acqs = mean(Sum_Acqs)) %>%
  dplyr::mutate(Net_Salary = Salary + Sum_Sal_Change) %>%
  dplyr::select(Team, Tm, Year, WAR,
                Net_Salary, Salary, Sum_Sal_Change,
                Sum_Num_Trades, Sum_Num_FAs, Sum_Num_Claims, Sum_Acqs)
```

-   Join the `TeamValALL` df to the `StandALL` df

``` r
TeamValALL = StandALL %>%
  dplyr::mutate(Team = Tm, .before = 1) %>%
  dplyr::select(-Tm) %>%
  inner_join(TeamValALL) %>%
  dplyr::arrange(`W-L%`)
```

# Scrape MLB Free-Agent Data

## MLers from ESPN

``` r
  ## Create a variable for a selector
ESPN_selector = "#my-players-table"
  ## Loop through and import the data for the relevant time-frame
for ( i in 11:21 ) {
  ESPN_FA_import_fn(Year = i, selector = ESPN_selector)
}
  ## Combine the dfs into one df
ESPN = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "ESPN_FA_2") == TRUE)]), full_join)

  ## Remove FA deals to Japanese baseball league and "undisclosed" deals
ESPN = ESPN %>%
  dplyr::filter(Team != "Japan") %>%
  dplyr::filter(Type != "Undisclosed")

rm(list = ls()[which(grepl(ls(), pattern = "ESPN_FA_2") == T)])
```

## Scrape and Clean International Signing Data from Spotrac

``` r
  ## Loop through the scraping function by the relevant years
for ( i in 11:21 ) {
  Spotrac_Intl_import_fn(Year = i, spotrac_selector = spotrac_Intl_selector)
}
  ## Put all the dfs into one df and remove the separate dfs
IntlALL = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "Intl_Signings_2") == T)]), full_join)

rm(list = ls()[which(grepl(ls(), pattern = "Intl_Signings_2") == T)])
```

# Amateur Draft Data

## Scrape and Clean Amateur Draft Data

Scrape amateur draft data from `Baseball Almanac`

Data is rife with errors, so clean it up

``` r
  ## set the selector
almanac_selector = "#wrapper > div.container > div.ba-table > table"
  ## Loop through and scrape the data starting from Epstein and Luhnow hires with Cubs/Astros (2011) to present
for ( i in 11:21 ){
  BBA_IMPORT_fn(Year = i, selector = almanac_selector)
}
  ## Merge the dataframes together
BBA_DRAFT_ALL = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "BBA20") == TRUE)]), full_join)
rm(list = ls()[which(grepl(ls(), pattern = "BBA([0-9])") == T)])

Encoding(BBA_DRAFT_ALL$`Player Name`) = "UTF-8"
BBA_DRAFT_ALL$`Player Name` = iconv(BBA_DRAFT_ALL$`Player Name`, "UTF-8", "UTF-8", sub = " ")


  ## Fix team name mistakes
    ## Change " `Drafted By` " to "Tm", and " `Player Name` " to "Name"
BBA_DRAFT_ALL = BBA_DRAFT_ALL %>%
  dplyr::mutate(`Drafted By` = case_when(`Drafted By` == "Chicago" ~ "Chicago Cubs",
                                         `Drafted By` == "Florida Marlins" ~ "Miami Marlins",
                                         grepl(`Drafted By`, pattern = "Miami") ~ "Miami Marlins",
                                         grepl(`Drafted By`, pattern = "Phillies") ~ "Philadelphia Phillies",
                                         grepl(`Drafted By`, pattern = "Pittsburg") ~ "Pittsburgh Pirates",
                                         grepl(`Drafted By`, pattern = "Baltimore") ~ "Baltimore Orioles",
                                         grepl(`Drafted By`, pattern = "Athletics") ~ "Oakland Athletics",
                                         grepl(`Drafted By`, pattern = "Anaheim") ~ "Los Angeles Angels",
                                         grepl(`Drafted By`, pattern = "Senators") ~ "Oakland Athletics",
                                         TRUE ~ `Drafted By`),
                Tm = `Drafted By`,
                Name = `Player Name`) %>%
  dplyr::select(-`Drafted By`, -`Player Name`)

  ## Remove "Carlos Pena" from Chicago Cubs draft records
    ## Remove duplicates
      ## Correct Lance McCullers' name
BBA_DRAFT_ALL = BBA_DRAFT_ALL %>%
  dplyr::filter(Name != "Carlos Pena") %>%
  dplyr::distinct_all() %>%
  dplyr::mutate(Name = case_when(grepl(Name, pattern = "McCullers") ~ "Lance McCullers Jr.",
                                 TRUE ~ Name))

  ## Subset relevant draft data
ASTROS_CUBS_DRAFT_SUB = BBA_DRAFT_ALL %>%
  dplyr::filter(grepl(Tm, pattern = "Houston Astros|Chicago Cubs"))

LEAGUE_DRAFT_SUB = BBA_DRAFT_ALL %>%
  dplyr::filter(!grepl(Tm, pattern = "Houston Astros|Chicago Cubs"))
```

## Add playerids to Draft Data

Add `Baseball Reference` `playerid`s to the data so it can be joined to
player career WAR data

-   same process as with the `Baseball Reference` player value data and
    the `CHADWICK DB` above

# Data Summaries

## Create and Populate Draft Approach DFs

Create dfs

``` r
  ## Join draft subset dfs with career WAR data -> How good drafting was
ASTROS_CUBS_DRAFT_SUB = ASTROS_CUBS_DRAFT_SUB %>%
  dplyr::select(-key_bbref) %>%
  left_join(CAREER_WAR_df, by = "bbref_id") %>%
  dplyr::select(-name_last, -name_first, -name_first,
                -name_given, -name_suffix, -name_matrilineal, -name_nick,
                -contains("key"), -contains("played")) %>%
  dplyr::mutate(Name = Name.x, .after = 7) %>%
  dplyr::select(-Name.x, -Name.y)

ASTROS_CUBS_DRAFT_SUB %>%
  dplyr::filter(Year != "2011") %>%
  dplyr::group_by(Tm) %>%
  dplyr::summarize(Career_bWAR = sum(Career_bWAR, na.rm = T)) %>%
  dplyr::arrange(desc(Career_bWAR))
```

    ## # A tibble: 2 × 2
    ##   Tm             Career_bWAR
    ##   <chr>                <dbl>
    ## 1 Houston Astros       104. 
    ## 2 Chicago Cubs          58.3

``` r
LEAGUE_DRAFT_SUB = LEAGUE_DRAFT_SUB %>%
  dplyr::select(-key_bbref) %>%
  left_join(CAREER_WAR_df, by = "bbref_id") %>%
  dplyr::select(-name_last, -name_first, -name_first,
                -name_given, -name_suffix, -name_matrilineal, -name_nick,
                -contains("key"), -contains("played")) %>%
  dplyr::mutate(Name = Name.x, .after = 7) %>%
  dplyr::select(-Name.x, -Name.y)

options(scipen = 999)

Draft_Results = LEAGUE_DRAFT_SUB %>%
  dplyr::filter(Year > 2011) %>%
  dplyr::group_by(Tm, Year) %>%
  summarize(Career_bWAR = round(sum(Career_bWAR, na.rm = T),1)) %>%
  dplyr::arrange(desc(Career_bWAR))

  ## Create subset dfs of standings, value, international signing and free-agent data for the rest of the league
LEAGUE_STAND_SUB = StandALL[ which(StandALL$Tm != "Houston Astros" & StandALL$Tm != "Chicago Cubs"), ]
LEAGUE_VAL_SUB = TeamValALL[ which(TeamValALL$Tm != "Houston Astros" & TeamValALL$Tm != "Chicago Cubs"), ]
LEAGUE_INTL_SUB = IntlALL[ which(IntlALL$Team != "HOU" & IntlALL$Team != "CHC"), ]
LEAGUE_ESPN_SUB = ESPN[ which(ESPN$Team != "Astros" & ESPN$Team != "Cubs"), ]
```

Prepare `Astros`, `Cubs`, and `League` `Tanking` dfs

``` r
  ## Create separate dfs for the Cubs and Astros
ASTROS_TANK_DF = matrix(ncol = 18, nrow = length(seq(2011,2021)))
CUBS_TANK_DF = matrix(ncol = 18, nrow = length(seq(2011,2021)))

ASTROS_TANK_DF = as.data.frame(ASTROS_TANK_DF)
CUBS_TANK_DF = as.data.frame(CUBS_TANK_DF)

  ## Create column names
colnames(ASTROS_TANK_DF) = c("Year", "Win%", "# Top 5 picks", "# SS Drafted", "# P Drafted",
                             "# College Draftees", "# Draft Picks", "SS %", "P %", "College %",
                             "# Int'l Signings", "Int'l Signing Money Spent", "# FA Contracts",
                             "Mean FA AAV", "Mean FA Length", "Mean FA Total Value", "Team bWAR",
                             "Team payroll")

colnames(CUBS_TANK_DF) = colnames(ASTROS_TANK_DF)

  ## Create a df for lg avg stats to merge
LG_TANK_DF = matrix(nrow = nrow(ASTROS_TANK_DF), ncol = ncol(ASTROS_TANK_DF))
LG_TANK_DF = as.data.frame(LG_TANK_DF)
colnames(LG_TANK_DF) = colnames(ASTROS_TANK_DF)

  ## Add years
for(i in 1:nrow(ASTROS_TANK_DF)){
  ASTROS_TANK_DF[i,1] = seq(2011,2021)[i]
  CUBS_TANK_DF[i,1] = seq(2011,2021)[i]
  LG_TANK_DF[i,1] = seq(2011,2021)[i]
}

  ## Fill Win %
ASTROS_TANK_DF[ ,2] = StandALL$`W-L%`[ which(StandALL$Tm == "Houston Astros") ]
CUBS_TANK_DF[ ,2] = StandALL$`W-L%`[ which(StandALL$Tm == "Chicago Cubs") ]
LG_TANK_DF$`Win%` = 0.500
```

-   Fill `# Top 5 picks` column (shown)

-   fill remaining columns (not shown)

``` r
for (i in 11:21) {
  
  ASTROS_TANK_DF[i-10,3] = nrow(ASTROS_CUBS_DRAFT_SUB %>%
                               dplyr::filter(Tm == "Houston Astros" &
                                               Year == paste(20,i,sep = "") &
                                               Rd <= 5))
  CUBS_TANK_DF[i-10,3] = nrow(ASTROS_CUBS_DRAFT_SUB %>%
                                dplyr::filter(Tm == "Chicago Cubs" &
                                                Year == paste(20,i,sep = "") &
                                                Rd <= 5))
  
  LG_TANK_DF[i-10,3] = round(nrow(LEAGUE_DRAFT_SUB %>%
                                    dplyr::filter(Year == paste(20,i,sep = "") &
                                                    Rd <= 5)) / 28, digits = 2)
  
}
```

-   Add `Career bWAR` of draftees to the dfs

-   Add the `WARPM` metric

-   join the dfs together

-   show the table

``` r
## Add Career WAR of draftees
ASTROS_TANK_DF = ASTROS_CUBS_DRAFT_SUB %>%
  group_by(Year) %>%
  filter(Tm == "Houston Astros") %>%
  summarize(Draft_Class_Career_WAR = sum(Career_bWAR, na.rm = T)) %>%
  select(Draft_Class_Career_WAR) %>%
  bind_cols(ASTROS_TANK_DF)

ASTROS_TANK_DF = ASTROS_TANK_DF %>%
  relocate(Draft_Class_Career_WAR, .after = 19)

CUBS_TANK_DF = ASTROS_CUBS_DRAFT_SUB %>%
  group_by(Year) %>%
  filter(Tm == "Chicago Cubs") %>%
  summarize(Draft_Class_Career_WAR = sum(Career_bWAR, na.rm = T)) %>%
  select(Draft_Class_Career_WAR) %>%
  bind_cols(CUBS_TANK_DF)

CUBS_TANK_DF = CUBS_TANK_DF %>%
    relocate(Draft_Class_Career_WAR, .after = 19)

LG_TANK_DF = LEAGUE_DRAFT_SUB %>%
  group_by(Year) %>%
  summarize(Draft_Class_Career_WAR = sum(Career_bWAR, na.rm = T)/28) %>%
  select(Draft_Class_Career_WAR) %>%
  bind_cols(LG_TANK_DF)

LG_TANK_DF = LG_TANK_DF %>%
  relocate(Draft_Class_Career_WAR, .after = 19)
LG_TANK_DF[ , 19] = round(LG_TANK_DF[ , 19], digits = 1)
  
## Add "WARPM" (WAR Per Million $) col
ASTROS_TANK_DF$WARPM = round(ASTROS_TANK_DF$`Team bWAR` / ASTROS_TANK_DF$`Team payroll`, digits = 2)
CUBS_TANK_DF$WARPM = round(CUBS_TANK_DF$`Team bWAR` / CUBS_TANK_DF$`Team payroll`, digits = 2)
LG_TANK_DF$WARPM = round(LG_TANK_DF$`Team bWAR` / LG_TANK_DF$`Team payroll`, digits = 2)

## Add Team col
ASTROS_TANK_DF$Tm = "Houston Astros"
CUBS_TANK_DF$Tm = "Chicago Cubs"
LG_TANK_DF$Tm = "LeagueAVG"
TANK_DF = rbind.data.frame(ASTROS_TANK_DF, CUBS_TANK_DF, LG_TANK_DF)

TANK_DF = TANK_DF %>%
  dplyr::select(Tm, Year, `Win%`, `# Top 5 picks`, `# SS Drafted`,
                `# P Drafted`, `# College Draftees`, `# Draft Picks`,
                `SS %`, `P %`, `College %`, `# Int'l Signings`,
                `Int'l Signing Money Spent`, `# FA Contracts`, `Mean FA AAV`,
                `Mean FA Length`, `Mean FA Total Value`, `Team bWAR`,
                `Team payroll`, `Draft_Class_Career_WAR`, `WARPM`)
```

    ## # A tibble: 33 × 21
    ##    Tm              Year `Win%` `# Top 5 picks` `# SS Drafted` `# P Drafted`
    ##    <chr>          <int>  <dbl>           <dbl>          <dbl>         <dbl>
    ##  1 Houston Astros  2011  0.346               5              2            24
    ##  2 Houston Astros  2012  0.34                6              6            19
    ##  3 Houston Astros  2013  0.315               5              2            21
    ##  4 Houston Astros  2014  0.432               6              2            15
    ##  5 Houston Astros  2015  0.531               7              3            15
    ##  6 Houston Astros  2016  0.519               5              1            14
    ##  7 Houston Astros  2017  0.623               7              2             0
    ##  8 Houston Astros  2018  0.636               5              5            21
    ##  9 Houston Astros  2019  0.66                5              3            21
    ## 10 Houston Astros  2020  0.483               4              1             2
    ## # ℹ 23 more rows
    ## # ℹ 15 more variables: `# College Draftees` <dbl>, `# Draft Picks` <dbl>,
    ## #   `SS %` <dbl>, `P %` <dbl>, `College %` <dbl>, `# Int'l Signings` <dbl>,
    ## #   `Int'l Signing Money Spent` <dbl>, `# FA Contracts` <dbl>,
    ## #   `Mean FA AAV` <dbl>, `Mean FA Length` <dbl>, `Mean FA Total Value` <dbl>,
    ## #   `Team bWAR` <dbl>, `Team payroll` <dbl>, Draft_Class_Career_WAR <dbl>,
    ## #   WARPM <dbl>

## Create and Clean the Team Performance and Salary DFs

-   Aggregate the `Team Performance/Value df`

    -   filter as described in the article

    -   show the table, sorted by the largest in-season payroll cuts

``` r
  ## Remove the Red Sox, Indians/Guardians, Brewers, Mets, Giants, Tigers, Rays, Jays, Phillies

REBUILDERS_df = TeamValALL %>%
  dplyr::filter(L >= 92) %>%
  dplyr::mutate(GB = as.double(GB)) %>%
  dplyr::arrange(Year) %>%
  dplyr::slice(-c(9,10,19,22,26,29,36,40,47,53,54))

## Re-arrange for display
Printable_REBUILD = REBUILDERS_df %>%
  dplyr::rename(`MLB Trades Made` = Sum_Num_Trades,
                `FA Signings` = Sum_Num_FAs,
                `Waiver Claims` = Sum_Num_Claims,
                `Total Acquisitions` = Sum_Acqs,
                `Payroll (in M $US)` = Salary,
                `Payroll Change (in M $US)` = Sum_Sal_Change,
                `Net Payroll (in M $US)` = Net_Salary,
                bWAR = WAR) %>%
  # dplyr::slice(-c(4,5,7:11,13:16,20:27,31:38,41,42,45,46)) %>%
  dplyr::select(Team, Year, W, L, `W-L%`,
                GB, DivRank, `MLB Trades Made`,
                `FA Signings`, `Waiver Claims`,
                `Total Acquisitions`,
                bWAR, `Payroll (in M $US)`, `Payroll Change (in M $US)`, `Net Payroll (in M $US)`) %>%
  dplyr::arrange(`Payroll Change (in M $US)`)

  ## For plotting with plotly
Rebuild_T = plot_ly(
  type = 'table',
  columnwidth = c(rep(20,12)),
  columnorder = c(seq(1,12,1)),
  header = list(
    values = c(colnames(Printable_REBUILD)),
    align = c("center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("grey", "grey")),
    font = list(family = "Times", size = 14, color = "black")
  ),
  cells = list(
    values = rbind(Printable_REBUILD$Team,
                   Printable_REBUILD$Year,
                   Printable_REBUILD$W,
                   Printable_REBUILD$L,
                   Printable_REBUILD$`W-L%`,
                   Printable_REBUILD$GB,
                   Printable_REBUILD$DivRank,
                   Printable_REBUILD$`MLB Trades Made`,
                   Printable_REBUILD$`FA Signings`,
                   Printable_REBUILD$`Waiver Claims`,
                   Printable_REBUILD$bWAR,
                   Printable_REBUILD$`Net Payroll (in M $US)`),
    align = c("center", "center"),
    line = list(color = "black", width = 1),
    font = list(family = "Times", size = 12, color = c("black"))
  ))


Printable_REBUILD %>%
  dplyr::slice(c(1:5))
```

    ##                    Team Year  W  L  W-L%   GB DivRank MLB Trades Made
    ## 1  Washington Nationals 2021 65 97 0.401 23.5       5              12
    ## 2      San Diego Padres 2016 68 94 0.420 23.0       5              10
    ## 3        Detroit Tigers 2017 64 98 0.395 38.0       5               4
    ## 4     Chicago White Sox 2017 67 95 0.414 35.0       4               5
    ## 5 Philadelphia Phillies 2017 66 96 0.407 31.0       5               3
    ##   FA Signings Waiver Claims Total Acquisitions bWAR Payroll (in M $US)
    ## 1           3             2                 17 25.7           292.0903
    ## 2           5             3                 18 19.2            65.8014
    ## 3           2             0                  6 23.1           167.9392
    ## 4           6             1                 12 25.3           147.1510
    ## 5           4             3                 10 26.5           141.9905
    ##   Payroll Change (in M $US) Net Payroll (in M $US)
    ## 1                 -78.00348               214.0868
    ## 2                 -67.70570                -1.9043
    ## 3                 -66.57500               101.3642
    ## 4                 -59.44350                87.7075
    ## 5                 -45.91250                96.0780
