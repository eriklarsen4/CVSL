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
  dplyr::select(-Tm.x, -Tm.y) %>%
  dplyr::rename(Tm.x = Tm)

  ## replace the "TOT" with the team they ended the season with
y = Player_Batting_ALL %>%
  dplyr::filter(Tm == "TOT") %>%
  dplyr::distinct() %>%
  left_join(x) %>%
  dplyr::mutate(Tm = Tm.x, .after = 5) %>%
  dplyr::filter(!is.na(Tm)) %>%
  dplyr::select(-Tm.x)

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
  dplyr::filter( (Traded == 0 & rm_idx == 0) |
                   (Traded == 1) ) %>%
  dplyr::select(Name, lastinit, bbref_id, Age, Year, Tm, Lg,
                Traded, Bats, WAR, RAR, Positions, G, PA, AB, R, H, `2B`,
                `3B`, HR, RBI, SB, CS, BB, SO, BA, OBP, SLG, OPS,
                `OPS+`, TB, GDP, HBP, SH, SF, IBB, Rbat, Rbaser,
                Rdp, Rfield, Rpos, RAA, Rrep, oWAR, dWAR, oRAR, WAA,
                -Lg.x, -Lg.y, -rm_idx, -Tm.x, -Tm.y, -Pos.Summary) %>%
  dplyr::filter( (Traded == 1 & Lg == "MLB") | (Traded == 0))

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
  dplyr::right_join(Player_Pitching_ALL, by = c("Name", "lastinit", "bbref_id", "Year"))

  ## replace the "TOT" with the team they ended the season with
y = Player_Pitching_ALL %>%
  dplyr::filter(Tm == "TOT") %>%
  dplyr::distinct() %>%
  left_join(x) %>%
  dplyr::select(-Tm, -Tm.y) %>%
  dplyr::mutate(Tm = Tm.x, .after = 3) %>%
  dplyr::select(-Tm.x)

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
  dplyr::filter( (Traded == 0 & rm_idx == 0) |
                   (Traded == 1) ) %>%
  dplyr::select(-Lg.x, -Lg.y, -rm_idx, -Tm.x, -Tm.y) %>%
  dplyr::filter( (Traded == 1 & Lg == "MLB") | (Traded == 0) )

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
  dplyr::mutate(key_bbref = bbref_id) %>%
  dplyr::mutate(across(where(is.numeric), as.character)) %>%
  dplyr::mutate(Year = as.double(Year)) %>%
  left_join(Player_Pitching_ALL2, by = c("bbref_id", "Year")) %>%
  dplyr::rename_at(vars(contains(".x")), ~ str_remove(., ".x")) %>%
  dplyr::relocate(lastinit, .after = "bbref_id") %>%
  dplyr::relocate(Year, .before = 1) %>%
  dplyr::relocate(Age, .after = "lastinit") %>%
  dplyr::relocate(Tm, .after = "Throws") %>%
  dplyr::relocate(Traded, .after = "Tm") %>%
  dplyr::relocate(Throws, .after = "Traded") %>%
  dplyr::relocate(Split, .after = "Throws") %>%
  dplyr::select(c(1:36))

## Group by platoon year to derive lg avg splits
LgAvg_Pitcher_Splits = Pitcher_splits %>%
  dplyr::mutate(across(c(10:36), as.numeric)) %>%
  dplyr::group_by(Year, Throws, Split) %>%
  dplyr::summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  dplyr::ungroup() %>%
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

## Get the handedness, name, team, trade status, positions of the batter
Hitter_splits = Hitter_splits %>%
  dplyr::relocate(Year, .before = 1) %>%
  dplyr::relocate(lastinit, .after = "bbref_id") %>%
  dplyr::mutate(key_bbref = bbref_id) %>%
  left_join(CHAD_LU, by = c("key_bbref")) %>%
  dplyr::rename(playerid = key_fangraphs) %>%
  dplyr::relocate(playerid, .after = "lastinit") %>%
  dplyr::relocate(Age, .after = "playerid") %>%
  dplyr::relocate(Tm, .after = "Age") %>%
  dplyr::relocate(Traded, .after = "Tm") %>%
  dplyr::rename(Name = Name.x) %>%
  dplyr::select(-contains("key"), -contains("name_"), -contains("mlb"), -contains(".y"))

## Group by platoon year to derive lg avg splits
LgAvg_Hitter_splits = Hitter_splits %>%
  dplyr::group_by(Year, Bats, Split) %>%
  dplyr::summarize(across(where(is.numeric), mean, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(bbref_id = "LgAvg", .before = 3) %>%
  dplyr::mutate(lastinit = "LgAvg", .before = 4) %>%
  dplyr::mutate(playerid = "LgAvg") %>%
  dplyr::relocate(Split, .after = "playerid") %>%
  dplyr::relocate(Bats, .after = "Split") %>%
  dplyr::mutate(across(c("BA", "OBP", "SLG", "OPS", "BAbip", "tOPS+", "sOPS+"), round, 3)) %>%
  dplyr::mutate(across(c("Traded", "G", "PA", "AB", "R", "H", "2B", "3B", "HR", "RBI",
                         "SB", "CS", "BB", "SO", "TB", "GDP", "HBP", "SH", "SF", "IBB",
                         "ROE", "tOPS+", "sOPS+"), round, 0))

## Add the lg avg back to the splits
Hitter_splits = Hitter_splits %>%
  dplyr::mutate(playerid = as.character(playerid)) %>%
  full_join(LgAvg_Hitter_splits) %>%
  dplyr::arrange(desc(`sOPS+`))
```

# Integrate CVSL Rosters and Rules

## Import Historic CVSL Roster Environment

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

Show top 5 `hitter` splits for **2024** (based on 2023 MLB stats)

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

    ##             Name  bbref_id lastinit playerid  Split Bats Age Year  Tm Traded
    ## 1     Nick Allen allenni01        a  1000155 vs RHP    R  24 2023 OAK      0
    ## 2 Luis Campusano campulu01        c    22217 vs LHP    R  24 2023 SDP      0
    ## 3     Yandy Díaz  diazya01        d    16578 vs LHP    R  31 2023 TBR      0
    ## 4   Mookie Betts bettsmo01        b    13611 vs LHP    R  30 2023 LAD      0
    ## 5   Corey Seager seageco01        s    13624 vs RHP    L  29 2023 TEX      0
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

Show top 5 `pitcher` splits for **2024** (based on 2023 MLB stats)

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

    ##                 Name  bbref_id lastinit playerid  Split Throws Year  Tm  G PA
    ## 1     Caleb Thielbar thielca01        t    10078 vs LHB      L 2023 MIN 29 47
    ## 2        Ryan Pepiot pepiory01        p    26221 vs LHB      R 2023 LAD  8 75
    ## 3      Nick Anderson anderni01        a    18337 vs LHB      R 2023 ATL 31 58
    ## 4        JoJo Romero romerjo01        r    19574 vs LHB      L 2023 STL 25 41
    ## 5 Cristopher Sánchez sanchcr01        s    20778 vs LHB      L 2023 PHI 16 80
    ##   AB R  H 2B 3B HR SB CS BB SO SO/W    BA   OBP   SLG   OPS TB GDP HBP SH SF
    ## 1 47 3  6  2  1  0  2  1  0 14   NA 0.128 0.128 0.213 0.341 10   0   0  0  0
    ## 2 72 3  8  0  0  3  1  0  2 13  6.5 0.111 0.147 0.236 0.383 17   1   1  0  0
    ## 3 57 7  8  4  0  0  0  0  1 17 17.0 0.140 0.155 0.211 0.366 12   1   0  0  0
    ## 4 38 2  4  0  0  0  0  1  3 12  4.0 0.105 0.171 0.105 0.276  4   2   0  0  0
    ## 5 75 4 10  1  1  1  0  0  5 27  5.4 0.133 0.188 0.213 0.401 16   6   0  0  0
    ##   IBB ROE BAbip tOPS+ sOPS+   BB%    K%
    ## 1   0   0 0.182     0   -10 0.000 0.298
    ## 2   0   0 0.089    32     2 0.027 0.173
    ## 3   0   1 0.200    14    -2 0.017 0.293
    ## 4   0   0 0.154    -3   -22 0.073 0.293
    ## 5   0   0 0.192    20     9 0.062 0.338
