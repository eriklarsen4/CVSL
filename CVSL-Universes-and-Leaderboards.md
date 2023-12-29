CVSL Universes and Leaderboards
================
Erik Larsen
2023-11-20

# Overview

This snippet is a re-vamp of the data cleaning for our fantasy baseball
`Strat-O-Matic` dynasty league, [The Central Valley Strat-O-Matic
Baseball League
(CVSL)](https://sites.google.com/view/cvslbaseball/home).

-   I scrape `FanGraphs (FG)` and `Baseball-Reference (BR)` player
    Leaderboards for `catchers`, `pitchers`, and `non-catchers`

-   Originally, I downloaded all the data due to scraping difficulty

    -   I’ve revised this since; lots of Encoding and joining issues,
        spelling issues, etc. that required extensive time to fix

Required packages:

[readr](https://cran.r-project.org/package=readr)

[tidyverse](https://cran.r-project.org/package=tidyverse)

[ggplot2](https://cran.r-project.org/package=ggplot2)

[rvest](https://cran.r-project.org/package=rvest)

[XML](https://cran.r-project.org/package=XML)

[stringr](https://cran.r-project.org/package=stringr) and
[stringi](https://cran.r-project.org/package=stringi) to manipulate
strings– this is useful because when you combine tables of different
string encodings (i.e. `Baseball Reference` and `FanGraphs`), the tables
won’t combine!

[baseballr](https://cran.r-project.org/package=baseballr)

# Environment Prep

## Load Packages and Functions

Load packages and [data acquisition & processing
functions](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/Functions.R)

``` r
library(readr)
library(tidyverse)
library(rvest)
library(XML)
library(stringr)
library(stringi)
library(baseballr)
library(ggplot2)
library(ggrepel)

source("~/GitHub/Baseball/CVSL/Functions.R")

load("~/GitHub/Baseball/CVSL/2022/CompleteUniversesAndLeaderboardsEnv.RData")
```

## Scrape and Clean Baseball Reference Hitters Data

Scrape hitting data from `Baseball Reference` for relevant years using
the scraping function I developed ([code
here](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/Functions.R))

``` r
for (i in 16:23){
  if (i %% 17 == 0){
    
    Sys.sleep(60)
    BR_PLAYER_STATS_IMPORT_fn(Year = i,
                              Player_data_type = "batting")
    
  } else {
    
    BR_PLAYER_STATS_IMPORT_fn(Year = i,
                              Player_data_type = "batting")
    
  }
}

Player_Batting_ALL = purrr::reduce(mget(ls()[which(grepl(ls(),
                                                         pattern = "Player_batting") == T)]),
                                         full_join)

rm(list = ls()[which(grepl(ls(), pattern = "Player_b") == T)])
```

Add their `player id`s ([function
here](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/Functions.R))

``` r
hitternames = Player_Batting_ALL %>%
  dplyr::select(Name) %>%
  as.vector() %>%
  unlist() %>%
  as.character()

hitters = hitternames

Player_Batting_ALL = ID_ADD_fn(df = Player_Batting_ALL,
                               player_type = "hitters",
                               site_abbrev = "bbref")
```

## Scrape and Clean Baseball Reference Pitching Data

Scrape pitching data from `Baseball Reference` for relevant years; same
function as for hitters, but tailored to pitchers

``` r
for (i in 16:23){
  if (i %% 17 == 0){
    
    Sys.sleep(60)
    BR_PLAYER_STATS_IMPORT_fn(Year = i,
                              Player_data_type = "pitching")
    
  } else {
    
    BR_PLAYER_STATS_IMPORT_fn(Year = i,
                              Player_data_type = "pitching")
    
  }
}

Player_Pitching_ALL = purrr::reduce(mget(ls()[which(grepl(ls(),
                                                         pattern = "Player_pitching") == T)]),
                                         full_join)

rm(list = ls()[which(grepl(ls(), pattern = "Player_p") == T)])
```

Add their `player id`s

``` r
pitchernames = Player_Pitching_ALL %>%
  dplyr::select(Name) %>%
  as.vector() %>%
  unlist() %>%
  as.character()

Player_Pitching_ALL = ID_ADD_fn(df = Player_Pitching_ALL,
                                player_type = "pitchers",
                                site_abbrev = "bbref")
```

## Account for Players Who Changed Teams

-   identify players who changed teams in-season

``` r
Team_Changers_hitters = Player_Batting_ALL %>%
  dplyr::filter(grepl(Tm, pattern = "TOT")) %>%
  dplyr::select(Name, lastinit, bbref_id, Year) %>%
  dplyr::distinct()


Team_Changers_pitchers = Player_Pitching_ALL %>%
  dplyr::filter(grepl(Tm, pattern = "TOT")) %>%
  dplyr::select(Name, lastinit, bbref_id, Year) %>%
  dplyr::distinct()
```

-   scrape the players’ `Baseball Reference` player pages to identify
    which teams for whom these players played

    -   for hitters first

    -   aggregate the data

    -   repeat for pitchers

``` r
for (i in 1:nrow(Team_Changers_pitchers)){
  if (i %% 17 == 0) {
    
    Sys.sleep(60)
    
    BR_PLAYER_CHANGER_IMPORT_fn(year = Team_Changers_pitchers$Year[i],
                                lastinitial = Team_Changers_pitchers$lastinit[i],
                                bbref_id = Team_Changers_pitchers$bbref_id[i],
                                Player_data_type = "pitching")
    
  } else {
    
    BR_PLAYER_CHANGER_IMPORT_fn(year = Team_Changers_pitchers$Year[i],
                                lastinitial = Team_Changers_pitchers$lastinit[i],
                                bbref_id = Team_Changers_pitchers$bbref_id[i],
                                Player_data_type = "pitching")
    
  }
}

  ## join all the individual dfs
Changing_pitchers = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "Changer_") == T)]), full_join)
  ## remove all the individual dfs
rm(list = ls()[which(grepl(ls(), pattern = "Changer_"))])

  ## aggregate by player-team-year to account for the salaries to team payrolls from mid-season
    ## player acquisitions
Team_Changers_pitchers = Team_Changers_pitchers %>%
  left_join(Changing_pitchers, by = c("lastinit", "bbref_id", "Year")) %>%
  dplyr::mutate(From = case_when(bbref_id == lead(bbref_id) ~ Tm,
                                 TRUE ~ ""),
                From = case_when(From == ""  ~ lag(From),
                                 TRUE ~ From),
                To = case_when(bbref_id == lag(bbref_id) ~ Tm,
                               TRUE ~ ""),
                To = case_when(To == "" ~ lead(To),
                               TRUE ~ To)) %>%
  dplyr::distinct() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(bbref_id, Year, Tm) %>%
  dplyr::select(-From, -To) %>%
  dplyr::distinct()
```

-   replace data

``` r
  ## join all hitters and pitchers who changed teams
# Team_Transactions_ALL = Team_Changers_hitters %>%
  # full_join(Team_Changers_pitchers)

# Team_Changers_ALL %>% # check to see all players are accounted for
#   dplyr::filter(is.na(Tm))

  ## Extract the data of the players who changed teams for last team they played for in a season
x = Team_Changers_hitters %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Name, lastinit, bbref_id, Year) %>%
  dplyr::slice(n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(Player_Batting_ALL, by = c("Name", "lastinit", "bbref_id", "Year")) %>%
  dplyr::filter(Tm.y == "TOT") %>%
  dplyr::group_by(bbref_id, Year, Tm.x) %>%
  dplyr::mutate(idx = case_when(bbref_id == lag(bbref_id) &
                                  Year == lag(Year) &
                                  Tm.x == lag(Tm.x) ~ 1,
                                TRUE ~ 0), .after = 3) %>%
  dplyr::ungroup() %>%
  dplyr::filter(idx == 0) %>%
  dplyr::select(-idx) %>%
  dplyr::mutate(Tm = case_when(Tm.y == "TOT" ~ Tm.x,
                               TRUE ~ Tm.y), .after = 3) %>%
  dplyr::select(-Tm.y, -Tm.x)

  ## replace the "TOT" with the team they ended the season with
y = Player_Batting_ALL %>%
  dplyr::filter(Tm == "TOT") %>%
  dplyr::rename(Team = Tm) %>%
  dplyr::distinct() %>%
  left_join(x) %>%
  dplyr::select(-Team) %>%
  dplyr::relocate(Tm, .after = "Age") %>%
  dplyr::filter(!is.na(Tm))# %>%
  # dplyr::select(-Tm.x)

  ## Re-define the data by IDing the players who were traded, replacing all player seasons with their totals
    ## where, if they were traded, only their total and for the team they ended with are shown
Player_Batting_ALL2 = Player_Batting_ALL %>%
  full_join(y, by = c("Name", "lastinit", "bbref_id", "Bats", "Age", "Year",
                      "WAR", "RAR", "Positions", "G", "PA", "AB", "R", "H", "2B", "3B", "HR",
                      "RBI", "SB", "CS", "BB", "SO", "BA", "OBP", "SLG", "OPS",
                      "OPS+", "TB", "GDP", "HBP", "SH", "SF", "IBB", "Pos.Summary",
                      "Rbat", "Rbaser", "Rdp", "Rfield", "Rpos", "RAA", "Rrep",
                      "oWAR", "dWAR", "oRAR", "WAA")) %>%
  dplyr::mutate(Tm = case_when(Tm.x == "TOT" ~ Tm.y,
                               TRUE ~ Tm.x), .after = 5) %>%
  dplyr::mutate(Traded = case_when(Tm.x == "TOT" ~ 1,
                                   TRUE ~ 0), .after = 6) %>%
  dplyr::mutate(Lg = Lg.x, .after = 6) %>%
  dplyr::mutate(rm_idx = case_when(bbref_id == lag(bbref_id) ~ 1,
                                   TRUE ~ 0), .after = 6) %>%
  dplyr::filter( 
    # (Traded == 0 & rm_idx == 0) |
    #                (Traded == 1) 
                 rm_idx == 0 ) %>%
  dplyr::select(Name, lastinit, bbref_id, Age, Year, Tm, Lg,
                Traded, Bats, WAR, RAR, Positions, G, PA, AB, R, H, `2B`,
                `3B`, HR, RBI, SB, CS, BB, SO, BA, OBP, SLG, OPS,
                `OPS+`, TB, GDP, HBP, SH, SF, IBB, Rbat, Rbaser,
                Rdp, Rfield, Rpos, RAA, Rrep, oWAR, dWAR, oRAR, WAA,
                -Lg.x, -Lg.y, -rm_idx, -Tm.x, -Tm.y, -Pos.Summary) #%>%
  # dplyr::filter( (Traded == 1 & Lg == "MLB") | (Traded == 0))

rm(x)
rm(y)
```

``` r
  ## join all hitters and pitchers who changed teams
# Team_Transactions_ALL = Team_Changers_hitters %>%
  # full_join(Team_Changers_pitchers)

# Team_Changers_ALL %>% # check to see all players are accounted for
#   dplyr::filter(is.na(Tm))

  ## Extract the data of the players who changed teams for last team they played for in a season
x = Team_Changers_pitchers %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Name, lastinit, bbref_id, Year) %>%
  dplyr::slice(n()) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(Player_Pitching_ALL, by = c("Name", "lastinit", "bbref_id", "Year")) %>%
  dplyr::filter(Tm.y == "TOT") %>%
  dplyr::group_by(bbref_id, Year, Tm.x) %>%
  dplyr::mutate(idx = case_when(bbref_id == lag(bbref_id) &
                                  Year == lag(Year) &
                                  Tm.x == lag(Tm.x) ~ 1,
                                TRUE ~ 0), .after = 3) %>%
  dplyr::ungroup() %>%
  dplyr::filter(idx == 0) %>%
  dplyr::select(-idx) %>%
  dplyr::mutate(Tm = case_when(Tm.y == "TOT" ~ Tm.x,
                               TRUE ~ Tm.y), .after = 3) %>%
  dplyr::select(-Tm.y, -Tm.x)

  ## replace the "TOT" with the team they ended the season with
y = Player_Pitching_ALL %>%
  dplyr::filter(Tm == "TOT") %>%
  dplyr::rename(Team = Tm) %>%
  dplyr::distinct() %>%
  left_join(x) %>%
  dplyr::select(-Team) %>%
  dplyr::relocate(Tm, .after = "Age") %>%
  dplyr::filter(!is.na(Tm))

  ## Re-define the data by IDing the players who were traded, replacing all player seasons with their totals
    ## where, if they were traded, only their total and for the team they ended with are shown
Player_Pitching_ALL2 = Player_Pitching_ALL %>%
  full_join(y, by = c("Name", "lastinit", "bbref_id", "Throws", "Age", "W", "L",
                      "W-L%", "ERA", "G", "GS", "GF", "CG", "SHO", "SV", "IP",
                      "H", "R", "ER", "HR", "BB", "IBB", "SO", "HBP", "BK", "WP",
                      "BF", "ERA+", "FIP", "WHIP", "H9", "HR9", "BB9", "SO9",
                      "SO/W", "Year", "RA9", "RA9opp", "RA9def", "RA9role", "PPFp",
                      "RA9avg", "RAA", "WAA", "gmLI", "WAAadj", "WAR", "RAR", "waaWL%",
                      "162WL%", "Salary", "Acquired", "RA9extras")) %>%
  dplyr::mutate(Tm = case_when(Tm.x == "TOT" ~ Tm.y,
                               TRUE ~ Tm.x), .after = 5) %>%
  dplyr::mutate(Traded = case_when(Tm.x == "TOT" ~ 1,
                                   TRUE ~ 0), .after = 6) %>%
  dplyr::mutate(Lg = Lg.x, .after = 6) %>%
  dplyr::mutate(rm_idx = case_when(bbref_id == lag(bbref_id) ~ 1,
                                   TRUE ~ 0), .after = 6) %>%
  dplyr::filter( 
    # (Traded == 0 & rm_idx == 0) |
    #                (Traded == 1) 
                 rm_idx == 0 ) %>%
  dplyr::select(-Lg.x, -Lg.y, -rm_idx, -Tm.x, -Tm.y) #%>%
  # dplyr::filter( (Traded == 1 & Lg == "MLB") | (Traded == 0) )

rm(x)
rm(y)
```

## Scrape Splits Data

Show the scrape code for acquiring every players’ platoon splits

``` r
for(i in 1:nrow(Player_Pitching_ALL2)){
  if (i %% 17 == 0) {
    
    Sys.sleep(60)
    BR_PLAYER_SPLITS_IMPORT_fn(Year = str_extract(as.character(Player_Pitching_ALL2$Year[i]),
                                                  pattern = "[0-9]{2}$"),
                               Player_data_type = "pitching",
                               playerid = Player_Pitching_ALL2$bbref_id[i])
  } else {
    
    BR_PLAYER_SPLITS_IMPORT_fn(Year = str_extract(as.character(Player_Pitching_ALL2$Year[i]),
                                                  pattern = "[0-9]{2}$"),
                               Player_data_type = "pitching",
                               playerid = Player_Pitching_ALL2$bbref_id[i])
    
  }
}


Pitcher_splits = purrr::reduce(mget(ls()[which(grepl(ls(), pattern = "pitching_splits") == T)]), full_join)
rm(list = ls()[which(grepl(ls(), pattern = "pitching_splits") == T)])
```

Wrangle the splits data to add more player information

``` r
## Get more player info, like the handedness of the pitcher
Pitcher_splits = Pitcher_splits %>%
  dplyr::mutate(across(where(is.numeric), as.character)) %>%
  dplyr::mutate(Year = as.double(Year)) %>%
  left_join(Player_Pitching_ALL2, by = c("bbref_id", "Year")) %>%
  dplyr::relocate(c(Year, Name, bbref_id, lastinit, Throws, Age, Tm, Traded, Split), .before = 1) %>%
  dplyr::rename_at(vars(contains(".x")), ~ str_remove(., ".x")) %>%
  dplyr::select(c(1:36))

## Group by platoon year to derive lg avg splits
LgAvg_Pitcher_Splits = Pitcher_splits %>%
  dplyr::mutate(across(c(10:36), as.numeric)) %>%
  dplyr::group_by(Year, Throws, Split) %>%
  dplyr::summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Throws)) %>%
  dplyr::mutate(bbref_id = "LgAvg", .before = 1) %>%
  dplyr::mutate(across(c("BA", "OBP", "SLG", "OPS", "BAbip", "tOPS+", "sOPS+"),
                       round, 3)) %>%
  dplyr::mutate(across(c("SO/W"), round, 2)) %>%
  dplyr::mutate(across(c("G", "PA", "AB", "R", "H", "2B", "3B", "HR", "SB", "CS",
                         "BB", "SO", "TB", "GDP", "HBP", "SH", "SF", "IBB", "ROE",
                         "tOPS+", "sOPS+", "Traded"), round, 0)) %>%
  dplyr::arrange(desc(`sOPS+`))

## Add the Lg Avg back to the splits
Pitcher_splits = Pitcher_splits %>%
  dplyr::mutate(across(c(10:36), as.numeric)) %>%
  full_join(LgAvg_Pitcher_Splits) %>%
  dplyr::arrange(OPS)

## Add searchable pitcher name, FG id
Pitcher_splits = Pitcher_splits %>%
  dplyr::rename(key_bbref = bbref_id) %>%
  left_join(CHAD_LU, by = c("key_bbref")) %>%
  dplyr::rename(Name = Name.y) %>%
  dplyr::select(-Name.x) %>%
  dplyr::relocate(Name, .before = 2) %>%
  dplyr::rename(bbref_id = key_bbref,
                playerid = key_fangraphs) %>%
  dplyr::relocate(playerid, .after = "lastinit") %>%
  dplyr::select(-contains("name_"), -contains("key"), -contains("mlb"))


## Group by platoon year to derive lg avg splits
LgAvg_Hitter_splits = Hitter_splits %>%
  dplyr::mutate(key_bbref = bbref_id) %>%
  dplyr::left_join(CHAD_LU, by = c("key_bbref")) %>%
  dplyr::rename(Name = Name.x) %>%
  dplyr::select(-contains("key"), -contains("name_"), -contains("mlb"), -contains(".y")) %>%
  dplyr::group_by(Year, Bats, Split) %>%
  dplyr::summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(bbref_id = "LgAvg", .after = 1) %>%
  dplyr::mutate(lastinit = "LgAvg", .after = 2) %>%
  dplyr::mutate(playerid = "LgAvg", .after = 3) %>%
  dplyr::relocate(c(Bats, Split), .after = "playerid") %>%
  dplyr::mutate(across(c("BA", "OBP", "SLG", "OPS", "BAbip", "tOPS+", "sOPS+"), round, 3)) %>%
  dplyr::mutate(across(c("Traded", "G", "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI",
                         "SB", "CS", "BB", "SO", "TB", "GDP", "HBP", "SH", "SF", "IBB",
                         "ROE", "tOPS+", "sOPS+"), round, 0))

## Add the lg avg back to the splits
Hitter_splits = Hitter_splits %>%
  dplyr::mutate(key_bbref = bbref_id) %>%
  left_join(CHAD_LU, by = "key_bbref") %>%
  dplyr::mutate(playerid = as.character(key_fangraphs), .after = "lastinit") %>%
  full_join(LgAvg_Hitter_splits) %>%
  dplyr::mutate(Name = Name.y, .after = 1) %>%
  dplyr::select(-contains("key"), -contains("name_"), -contains("mlb"), -contains(".x"), -contains(".y")) %>%
  dplyr::arrange(desc(`sOPS+`))
```

# Processed DFs

Show the resulting data frames:

## Hitters’ splits

    ##   Year           Name  bbref_id lastinit playerid Bats Age  Tm Traded  Split
    ## 1 2018   Ryan Schimpf schimry01        s     9953    L  30 LAA      0 vs LHP
    ## 2 2016 Chris Parmelee parmech01        p     2554    L  28 NYY      0 vs LHP
    ## 3 2016   Andrew Susac susacan01        s    13132    R  26 MIL      0 vs LHP
    ## 4 2017   Mike Marjama marjami01        m    12594    R  27 SEA      0 vs LHP
    ## 5 2020 Mason Williams willima10        w    11859    L  28 BAL      0 vs LHP
    ##     Positions G PA AB R H 2B 3B HR RBI SB CS BB SO BA OBP SLG OPS TB GDP HBP SH
    ## 1 2B 3B LF RF 1  1  1 1 1  0  0  1   1  0  0  0  0  1   1   4   5  4   0   0  0
    ## 2          1B 1  1  1 1 1  0  0  1   2  0  0  0  0  1   1   4   5  4   0   0  0
    ## 3           C 1  1  1 2 1  0  0  1   2  0  0  0  0  1   1   4   5  4   0   0  0
    ## 4           C 1  1  1 1 1  0  0  1   1  0  0  0  0  1   1   4   5  4   0   0  0
    ## 5    LF CF RF 1  1  1 0 1  0  1  0   0  0  1  0  0  1   1   3   4  3   0   0  0
    ##   SF IBB ROE BAbip tOPS+ sOPS+
    ## 1  0   0   0    NA   633  1210
    ## 2  0   0   0    NA   391  1176
    ## 3  0   0   0    NA  1066  1176
    ## 4  0   0   0    NA   714  1161
    ## 5  0   0   0     1  2152   929

## Pitchers’ splits

    ##   Year           Name  bbref_id lastinit playerid Throws Age  Tm Traded  Split
    ## 1 2017  Andrew Bailey bailean01        b     1368      R  33 LAA      0 vs LHB
    ## 2 2017    Tyler Cloyd cloydty01        c     8536      R  30 SEA      0 vs LHB
    ## 3 2018 Danny Farquhar farquda01        f     8501      R  31 CHW      0 vs LHB
    ## 4 2020  James Bourque bourqja01        b    16668      R  26 WSN      0 vs LHB
    ## 5 2019     Nate Karns karnsna01        k    12638      R  31 BAL      0 vs LHB
    ##   G PA AB R H 2B 3B HR SB CS BB SO SO/W BA OBP SLG OPS TB GDP HBP SH SF IBB ROE
    ## 1 4  5  5 0 0  0  0  0  0  0  0  0   NA  0   0   0   0  0   0   0  0  0   0   0
    ## 2 1  2  2 0 0  0  0  0  0  0  0  1   NA  0   0   0   0  0   0   0  0  0   0   0
    ## 3 5  5  4 1 0  0  0  0  0  0  0  2   NA  0   0   0   0  0   0   0  0  1   0   0
    ## 4 4  4  4 0 0  0  0  0  0  0  0  0   NA  0   0   0   0  0   1   0  0  0   0   0
    ## 5 4  8  8 0 0  0  0  0  0  1  0  4   NA  0   0   0   0  0   0   0  0  0   0   0
    ##   BAbip tOPS+ sOPS+
    ## 1     0  -100  -100
    ## 2     0  -100  -100
    ## 3     0  -100  -100
    ## 4     0  -100  -100
    ## 5     0  -100  -100

## Aggregated hitters’ data

    ##               Name lastinit  bbref_id Age Year  Tm Lg Traded Bats  WAR RAR
    ## 1       José Abreu        a abreujo02  29 2016 CHW AL      0    R  3.1  30
    ## 2    Dustin Ackley        a ackledu01  28 2016 NYY AL      0    L -0.2  -2
    ## 3 Cristhian Adames        a adamecr01  24 2016 COL NL      0    S -0.8  -7
    ## 4       Matt Adams        a adamsma01  27 2016 STL NL      0    L  0.5   7
    ## 5   Ehire Adrianza        a adriaeh01  26 2016 SFG NL      0    S -0.1   0
    ##   Positions   G  PA  AB  R   H 2B 3B HR RBI SB CS BB  SO   BA  OBP  SLG  OPS
    ## 1        1B 159 695 624 67 183 32  1 25 100  0  2 47 125 .293 .353 .468 .820
    ## 2  1B RF 2B  28  70  61  6   9  0  0  0   4  0  0  8   9 .148 .243 .148 .390
    ## 3  SS 2B 3B 121 256 225 25  49  7  3  2  17  2  3 24  47 .218 .304 .302 .607
    ## 4        1B 118 327 297 37  74 18  0 16  54  0  1 25  81 .249 .309 .471 .780
    ## 5  SS 2B 3B  40  71  63  3  16  2  0  2   7  0  1  2  13 .254 .299 .381 .679
    ##   OPS+  TB GDP HBP SH SF IBB Rbat Rbaser Rdp Rfield Rpos RAA Rrep oWAR dWAR
    ## 1  124 292  21  15  0  9   7   22     -3  -3     -1   -9   6   24  3.2 -1.0
    ## 2    9   9   0   0  0  1   0   -7      1   0      4   -1  -4    3 -0.6  0.3
    ## 3   53  68   5   4  3  0   0  -15     -1   1     -3    3 -15    8 -0.5 -0.1
    ## 4  106 140   5   2  0  3   1    3     -2   0     -1   -4  -4   11  0.6 -0.5
    ## 5   83  24   0   2  4  0   0   -2     -1   0     -1    1  -3    2  0.0  0.0
    ##   oRAR  WAA
    ## 1   31  0.7
    ## 2   -5 -0.4
    ## 3   -4 -1.6
    ## 4    8 -0.5
    ## 5    1 -0.3

## Aggregated pitchers’ data

    ##             Name lastinit  bbref_id Throws Age  Tm Lg Traded W L  W-L%  ERA  G
    ## 1  Fernando Abad        a  abadfe01      L  30 BOS AL      1 1 6  .143 3.66 57
    ## 2    A.J. Achter        a achteaj01      R  27 LAA AL      0 1 0 1.000 3.11 27
    ## 3    Tim Adleman        a adlemti01      R  28 CIN NL      0 4 4  .500 4.00 13
    ## 4    Matt Albers        a alberma01      R  33 CHW AL      0 2 6  .250 6.31 58
    ## 5 Raúl Alcántara        a alcanra01      R  23 OAK AL      0 1 3  .250 7.25  5
    ##   GS GF CG SHO SV   IP  H  R ER HR BB IBB SO HBP BK WP  BF ERA+  FIP  WHIP   H9
    ## 1  0 15  0   0  1 46.2 40 20 19  4 22   2 41   1  1  1 198  118 3.98 1.329  7.7
    ## 2  0 17  0   0  0 37.2 43 13 13  7 12   1 14   1  0  0 160  130 5.85 1.460 10.3
    ## 3 13  0  0   0  0 69.2 64 32 31 13 20   1 47   5  0  0 287  107 5.30 1.206  8.3
    ## 4  1 11  0   0  0 51.1 67 44 36 10 19   1 30   3  0  4 237   64 5.80 1.675 11.7
    ## 5  5  0  0   0  0 22.1 31 18 18  9  4   0 14   4  1  1 103   57 8.21 1.567 12.5
    ##   HR9 BB9 SO9 SO/W Year  RA9 RA9opp RA9def RA9role  PPFp RA9avg RAA  WAA gmLI
    ## 1 0.8 4.2 7.9 1.86 2016 3.86   4.66  -0.11   -0.34 101.3   4.49   3  0.4 1.37
    ## 2 1.7 2.9 3.3 1.17 2016 3.11   4.57   0.25   -0.33  92.8   3.70   3  0.3  .43
    ## 3 1.7 2.6 6.1 2.35 2016 4.13   4.55  -0.01    0.19 100.3   4.77   6  0.6     
    ## 4 1.8 3.3 5.3 1.58 2016 7.71   4.59   0.00   -0.31  97.0   4.15 -20 -2.0 1.32
    ## 5 3.6 1.6 5.6 3.50 2016 7.25   4.72  -0.47    0.14  98.4   5.25  -5 -0.5     
    ##   WAAadj  WAR RAR waaWL% 162WL%     Salary    Acquired RA9extras
    ## 1    0.0  0.8   8   .506   .502 $1,250,000      Traded      <NA>
    ## 2   -0.2  0.5   6   .510   .502                Waivers      <NA>
    ## 3   -0.1  1.2  11   .548   .504            Free Agency      <NA>
    ## 4   -0.4 -2.0 -15   .465   .487 $2,000,000 Free Agency      <NA>
    ## 5    0.0 -0.3  -3   .405   .497                 Traded      <NA>

# Integrate CVSL Rosters and Rules

## Import Previous CVSL Rosters Environment

``` r
load("~/GitHub/Baseball/CVSL/2022/CVSLenv.RData")
```

## Plot Some Draft Results

Scatter plot of some previous draft results

![](CVSL-Universes-and-Leaderboards_files/figure-gfm/Join%20Draft%20history%20with%20stats-1.png)<!-- -->

## Eligible MLB Teams

The MLB teams from which CVSL Managers can draft players

``` r
CVSL_universe = c("SFG", "SDP", "LAD", "ARI", "COL",
                   "OAK", "TEX", "LAA",
                   "MIN", "CLE",
                   "STL",
                   "PHI", "NYM", "MIA", "ATL", "WSN",
                   "NYY", "TBR", "TOR", "BOS")
```

## Filter by “Service Time” Criteria

Filter hitters by:

-   `C`s need **\>= 100 ABs** to be eligible to catch in CVSL games
-   all other hitters need **\>= 175 ABs** to be eligible to play in
    CVSL games

Filter pitchers by:

-   `SP`s need **\>= 8 starts (GS)** *AND* **\>= 30 IP** to be eligible
    to start in CVSL games

-   `RP`s need **\>= 30 IP** to be eligible to pitch in CVSL games

-   **code below is filtered to include only 2023 MLB stats**

``` r
eligible_hitters_2024 = Player_Batting %>%
  dplyr::filter(Year == 2023) %>%
  dplyr::mutate(AB = as.numeric(AB),
                eligible = case_when(AB < 100 ~ 0,
                                     grepl(Positions, pattern = "(C)") &
                                       AB >= 100 ~ 1,
                                     grepl(Positions, pattern = "1B") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "2B") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "3B") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "SS") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "LF") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "(CF)") &
                                       AB >= 175 ~ 1,
                                     grepl(Positions, pattern = "RF") &
                                       AB >= 175 ~ 1,
                                     
                                     TRUE ~ 0), .after = 1) %>%
  dplyr::filter(eligible == 1) %>%
  dplyr::select(Name, bbref_id, Tm, Positions, AB) %>%
  dplyr::filter(Tm %in% CVSL_universe) %>%
  dplyr::distinct()


eligible_pitchers_2024 = Player_Pitching %>%
  dplyr::filter(Year == 2023) %>%
  dplyr::mutate(G = as.numeric(G),
                GS = as.numeric(GS),
                IP = as.numeric(IP),
                eligible = case_when(GS >= 8 &
                                       IP >= 30 ~ 1,
                                     IP >= 30 ~ 1,
                                     TRUE ~ 0), .after = 1) %>%
  dplyr::select(Name, bbref_id, Tm, eligible, G, GS, IP) %>%
  dplyr::filter(Tm %in% CVSL_universe) %>%
  dplyr::filter(eligible == 1) %>%
  dplyr::distinct()
```

Show hitter splits for **2024** (based on 2023 MLB stats)

``` r
Hitter_splits %>%
  dplyr::mutate(Bats = case_when(Bats == "S" &
                                   Split == "vs LHP" ~ "R",
                                 Bats == "S" &
                                   Split == "vs RHP" ~ "L",
                                 TRUE ~ Bats)) %>%
  dplyr::filter(Year == 2023) %>%
  dplyr::filter(bbref_id %in% eligible_hitters_2024$bbref_id) %>%
  dplyr::arrange(desc(`sOPS+`)) %>%
  dplyr::filter(Tm %in% CVSL_universe) %>%
  dplyr::slice(c(1:5))
```

    ##   Year           Name  bbref_id lastinit playerid Bats Age  Tm Traded  Split
    ## 1 2023     Nick Allen allenni01        a  1000155    R  24 OAK      0 vs RHP
    ## 2 2023 Luis Campusano campulu01        c    22217    R  24 SDP      0 vs LHP
    ## 3 2023     Yandy Díaz  diazya01        d    16578    R  31 TBR      0 vs LHP
    ## 4 2023   Mookie Betts bettsmo01        b    13611    R  30 LAD      0 vs LHP
    ## 5 2023   Corey Seager seageco01        s    13624    L  29 TEX      0 vs RHP
    ##   Positions   G  PA  AB  R   H 2B 3B HR RBI SB CS BB SO    BA   OBP   SLG   OPS
    ## 1     SS 2B  10  17  14  4   6  1  1  0   0  0  0  2  3 0.429 0.529 0.643 1.172
    ## 2         C  27  47  42 11  17  2  0  3   9  0  0  3  7 0.405 0.447 0.667 1.114
    ## 3     1B 3B  59 139 121 22  43  8  0 10  32  0  0 16 22 0.355 0.432 0.669 1.101
    ## 4  RF 2B SS  75 184 150 42  45 14  0 14  33  6  0 30 22 0.300 0.424 0.673 1.097
    ## 5        SS 110 370 321 64 107 29  0 26  71  2  0 42 55 0.333 0.408 0.667 1.075
    ##    TB GDP HBP SH SF IBB ROE BAbip tOPS+ sOPS+
    ## 1   9   0   1  0  0   0   0 0.546   251   236
    ## 2  28   2   1  0  1   0   0 0.424   161   198
    ## 3  81   1   1  0  1   0   0 0.367   134   194
    ## 4 101   2   3  0  1   3   1 0.270   120   192
    ## 5 214   5   2  0  5   9   1 0.331   112   189

Show pitcher splits for **2024** (based on 2023 MLB stats)

``` r
Pitcher_splits %>%
  dplyr::select(-contains(".y")) %>%
  dplyr::filter(Year == 2023) %>%
  dplyr::filter(bbref_id %in% eligible_pitchers_2024$bbref_id) %>%
  dplyr::arrange(`sOPS+`) %>%
  left_join(Player_Pitching, by = c("bbref_id", "Year")) %>%
  dplyr::rename_at(vars(contains(".x")), ~ str_remove(., ".x")) %>%
  dplyr::relocate(Tm, .after = "Year") %>%
  dplyr::select(-c(36:88)) %>%
  dplyr::filter(Tm %in% CVSL_universe) %>%
  dplyr::mutate(`BB%` = round(BB / PA, 3),
                `K%` = round(SO / PA, 3)) %>%
  dplyr::arrange(OBP) %>%
  dplyr::slice(c(1:5))
```

    ##   Year  Tm               Name  bbref_id lastinit playerid Throws Age Traded
    ## 1 2023 MIN     Caleb Thielbar thielca01        t    10078      L  36      0
    ## 2 2023 LAD        Ryan Pepiot pepiory01        p    26221      R  25      0
    ## 3 2023 ATL      Nick Anderson anderni01        a    18337      R  32      0
    ## 4 2023 STL        JoJo Romero romerjo01        r    19574      L  26      0
    ## 5 2023 PHI Cristopher Sánchez sanchcr01        s    20778      L  26      0
    ##    Split  G PA AB R  H 2B 3B HR SB CS BB SO SO/W    BA   OBP   SLG   OPS TB GDP
    ## 1 vs LHB 29 47 47 3  6  2  1  0  2  1  0 14   NA 0.128 0.128 0.213 0.341 10   0
    ## 2 vs LHB  8 75 72 3  8  0  0  3  1  0  2 13  6.5 0.111 0.147 0.236 0.383 17   1
    ## 3 vs LHB 31 58 57 7  8  4  0  0  0  0  1 17 17.0 0.140 0.155 0.211 0.366 12   1
    ## 4 vs LHB 25 41 38 2  4  0  0  0  0  1  3 12  4.0 0.105 0.171 0.105 0.276  4   2
    ## 5 vs LHB 16 80 75 4 10  1  1  1  0  0  5 27  5.4 0.133 0.188 0.213 0.401 16   6
    ##   HBP SH SF IBB ROE BAbip     Salary      Acquired RA9extras   BB%    K%
    ## 1   0  0  0   0   0 0.182 $2,400,000   Free Agency      0.37 0.000 0.298
    ## 2   1  0  0   0   0 0.089            Amateur Draft      0.00 0.027 0.173
    ## 3   0  0  0   0   1 0.200   $875,000   Free Agency      0.16 0.017 0.293
    ## 4   0  0  0   0   0 0.154                   Traded      0.62 0.073 0.293
    ## 5   0  0  0   0   0 0.192   $725,000        Traded      0.00 0.062 0.338
