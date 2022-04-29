Tanking Data Wrangling
================
Erik Larsen
4/23/2022

This snippet was used in my [article](https://www.theleftycatcher.com/post/diving-deep-into-tanking) on
[TheLeftyCatcher](https://www.theleftycatcher.com), where I compared and
contrasted how `Jeff Luhnow` and `Theo Epstein` tanked to rebuild the
`Chicago Cubs` and `Houston Astros`, beginning in \~2011 into World
Series winners. I acquired `MLB Free Agent` data, `MLB Amateur Draft`
data, `International Signing` data, `MLB Standings` data, `MLB Payroll`
data, and “`MLB Value`” data. I scraped or downloaded this data from
multiple sites, including `Baseball Reference`, `ESPN`, and
`Baseball Almanac`. It required some processing to compile and analyze
with or without graphs.

Subsequent pieces and snippets will revolve around this data.

## Load packages

``` r
  ## Data wrangling; df re-arrangement, string manipulations
library(readr)
library(reshape2)
library(tidyverse)
library(stringr)

  ## Web scraping
library(rvest)
library(XML)

  ## Plotting
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggpubr)
library(plotly)

  ## Modeling
library(mgcv)
library(splines)
```

## Scrape and Clean Amateur Draft Data

Amateur draft data from Baseball Almanac

``` r
  ## Create a selector for the relevant element
almanac_selector = "#wrapper > div.container > div.ba-table > table"

    ## Create a function to scrape the Baseball Almanac draft data
BBA_import_fn = function(Year, selector){
    ## Read in the URL with desired year and selector
  df = read_html(
    paste("https://www.baseball-almanac.com/draft/baseball-draft.php?yr=20", Year, sep = ""), encoding = "UTF8"
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
    ## Coerce Rd and pick (`#`) numbers to be numeric (integers)
  df$Rd = as.numeric(df$Rd)
  df$`#` = as.numeric(df$`#`)
    ## Co-erce the Name strings into UTF-8 encoding
  Encoding(df$`Player Name`) = "UTF-8"
  df$`Player Name` = iconv(df$`Player Name`, "UTF-8", "UTF-8", sub = " ")
    ## Add a "year" column for eventual df merging
  df$Year = as.numeric(paste("20", Year, sep = ""))
    ## Save the dataframe as a variable to the global environment
  assign(paste("BBA20", Year, sep = ""), df, envir = .GlobalEnv)
}
  ## Loop through and scrape the data starting from Epstein and Luhnow hires with Cubs/Astros (2011) to present
for ( i in 11:21 ){
  BBA_import_fn(Year = i, selector = almanac_selector)
}
  ## Merge the dataframes together
BBA_DRAFT_ALL = rbind.data.frame(BBA2011, BBA2012, BBA2013, BBA2014, BBA2015, BBA2016, BBA2017, BBA2018, BBA2019, BBA2020, BBA2021)
rm(BBA2011, BBA2012, BBA2013, BBA2014, BBA2015, BBA2016, BBA2017, BBA2018, BBA2019, BBA2020, BBA2021)

Encoding(BBA_DRAFT_ALL$`Player Name`) = "UTF-8"
BBA_DRAFT_ALL$`Player Name` = iconv(BBA_DRAFT_ALL$`Player Name`, "UTF-8", "UTF-8", sub = " ")

  ## Fix team name mistakes
BBA_DRAFT_ALL$`Drafted By`[ which(BBA_DRAFT_ALL$`Drafted By` == "Chicago") ] = "Chicago Cubs"
BBA_DRAFT_ALL$`Drafted By`[ which(BBA_DRAFT_ALL$`Drafted By` == "Florida Marlins") ] = "Miami Marlins"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Miami") ] = "Miami Marlins"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Phillies") ] = "Philadelphia Phillies"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Pittsburg") ] = "Pittsburg Pirates"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Baltimore") ] = "Baltimore Orioles"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Athletics") ] = "Oakland Athletics"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Anaheim") ] = "Los Angeles Angels"
BBA_DRAFT_ALL$`Drafted By`[ grep(BBA_DRAFT_ALL$`Drafted By`, pattern = "Senators") ] = "Oakland Athletics"

  ## Change " `Drafted By` " to "Tm", and " `Player Name` " to "Name"
colnames(BBA_DRAFT_ALL)[4] = "Name"
colnames(BBA_DRAFT_ALL)[5] = "Tm"
```

Small function to analyze college draft selections

``` r
  ## Create a function that analyzes the number of college picks by position of a given team in a given year
Draft_df_Analysis = function(df, Team){
  
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
  ## Note that there may be duplicates in some dfs; better to perform analysis after duplicates are removed
Draft_df_Analysis(df = BBA_DRAFT_ALL, Team = "New York Yankees")
```

    ## [1] "1 1b ; 197 / 386 College Picks"
    ## [1] "18 1B ; 197 / 386 College Picks"
    ## [1] "17 2B ; 197 / 386 College Picks"
    ## [1] "15 3B ; 197 / 386 College Picks"
    ## [1] "1 3B-1B ; 197 / 386 College Picks"
    ## [1] "1 3B-2B ; 197 / 386 College Picks"
    ## [1] "31 C ; 197 / 386 College Picks"
    ## [1] "8 CF ; 197 / 386 College Picks"
    ## [1] "4 IF ; 197 / 386 College Picks"
    ## [1] "2 LF ; 197 / 386 College Picks"
    ## [1] "28 LHP ; 197 / 386 College Picks"
    ## [1] "52 OF ; 197 / 386 College Picks"
    ## [1] "229 P ; 197 / 386 College Picks"
    ## [1] "1 P-1B ; 197 / 386 College Picks"
    ## [1] "1 P-OF ; 197 / 386 College Picks"
    ## [1] "3 RF ; 197 / 386 College Picks"
    ## [1] "72 RHP ; 197 / 386 College Picks"
    ## [1] "18 SS ; 197 / 386 College Picks"

Check the details on the Astros’ and Cubs’ First 5 Rd selections from
2011-2021

``` r
print(BBA_DRAFT_ALL[ which(BBA_DRAFT_ALL$Tm == "Houston Astros" & BBA_DRAFT_ALL$Rd <= 5 ), ], n = 59)
```

    ## # A tibble: 59 x 8
    ##       Rd   `#` Phase Name             Tm             POS   `Drafted From`   Year
    ##    <dbl> <dbl> <chr> <chr>            <chr>          <chr> <chr>           <dbl>
    ##  1     1    11 JR    George Springer  Houston Astros OF    "University of~  2011
    ##  2     2    69 JR    Adrian Houser    Houston Astros P     "Locust Grove ~  2011
    ##  3     3    99 JR    Jack Armstrong   Houston Astros P     "Vanderbilt Un~  2011
    ##  4     4   130 JR    Christopher Lee  Houston Astros P     "Santa Fe Comm~  2011
    ##  5     5   160 JR    Nick Tropeano    Houston Astros P     "Stony Brook U~  2011
    ##  6     1     1 JR    Carlos Correa    Houston Astros SS    "Puerto Rico B~  2012
    ##  7     1    41 JR    Lance McCullers  Houston Astros P     "Jesuit High S~  2012
    ##  8     2    61 JR    Nolan Fontana    Houston Astros SS    "University of~  2012
    ##  9     3    96 JR    Brady Rodgers    Houston Astros P     "Arizona State~  2012
    ## 10     4   129 JR    Rio Ruiz         Houston Astros 3B    "Bishop Amat M~  2012
    ## 11     5   159 JR    Andrew Aplin     Houston Astros OF    "Arizona State~  2012
    ## 12     1     1 JR    Mark Appel       Houston Astros RHP   "Stanford"       2013
    ## 13     2    40 JR    Andrew Thurman   Houston Astros RHP   "UC Irvine"      2013
    ## 14     3    74 JR    Kent Emanuel     Houston Astros LHP   "North Carolin~  2013
    ## 15     4   107 JR    Conrad Gregor    Houston Astros 1B    "Vanderbilt Un~  2013
    ## 16     5   137 JR    Tony Kemp        Houston Astros 2B    "Vanderbilt Un~  2013
    ## 17     1     1 JR    Brady Aiken      Houston Astros P     "Cathedral Cat~  2014
    ## 18     1    37 JR    Derek Fisher     Houston Astros OF    "University of~  2014
    ## 19     2    42 JR    A.J. Reed        Houston Astros 1B    "University of~  2014
    ## 20     3    75 JR    J.D. Davis       Houston Astros 3B    "Cal State Ful~  2014
    ## 21     4   106 JR    Daniel Mengden   Houston Astros RHP   "Texas A&M Uni~  2014
    ## 22     5   136 JR    Jacob Nix        Houston Astros RHP   "Los Alamitos ~  2014
    ## 23     1     2 JR    Alex Bregman     Houston Astros SS    "Louisiana Sta~  2015
    ## 24     1     5 JR    Kyle Tucker      Houston Astros OF    "Plant High Sc~  2015
    ## 25     1    37 JR    Daz Cameron      Houston Astros OF    "Eagle's Landi~  2015
    ## 26     2    46 JR    Tom Eshelman     Houston Astros RHP   "Cal State Ful~  2015
    ## 27     3    79 JR    Riley Ferrell    Houston Astros P     "Texas Christi~  2015
    ## 28     4   109 JR    Anthony Hermelyn Houston Astros C     "University of~  2015
    ## 29     5   139 JR    Trent Thornton   Houston Astros P     "University of~  2015
    ## 30     1    17 JR    Forrest Whitley  Houston Astros P     "Alamo Heights~  2016
    ## 31     2    61 JR    Ronnie Dawson    Houston Astros OF    "Ohio State Un~  2016
    ## 32     3    97 JR    Jake Rogers      Houston Astros C     "Tulane Univer~  2016
    ## 33     4   127 JR    Brett Adcock     Houston Astros P     "University of~  2016
    ## 34     5   157 JR    Abraham Toro     Houston Astros 3B    "Seminole Stat~  2016
    ## 35     1    15 JR    J.B. Bukauskas   Houston Astros RHP   "North Carolin~  2017
    ## 36     2    53 JR    Joe Perez        Houston Astros 3B    "Archbishop Mc~  2017
    ## 37     2    56 JR    Corbin Martin    Houston Astros RHP   "Texas A&M"      2017
    ## 38     2    75 JR    J.J. Matijevic   Houston Astros 2B    "Arizona"        2017
    ## 39     3    91 JR    Tyler Ivey       Houston Astros RHP   "Grayson Count~  2017
    ## 40     4   121 JR    Peter Solomon    Houston Astros RHP   "University of~  2017
    ## 41     5   151 JR    Nathan Perry     Houston Astros C     "Bassett High ~  2017
    ## 42     1    28 JR    Seth Beer        Houston Astros OF    "Clemson"        2018
    ## 43     2    66 JR    Jayson Schroeder Houston Astros RHP   "\"Juanita HS,~  2018
    ## 44     3   102 JR    Jeremy Pena      Houston Astros SS    "Maine"          2018
    ## 45     4   132 JR    Alex McKenna     Houston Astros OF    "Cal Poly"       2018
    ## 46     5   162 JR    Cody Deason      Houston Astros RHP   "Arizona"        2018
    ## 47     1    32 JR    Korey Lee        Houston Astros C     "California"     2019
    ## 48     2    68 JR    Grae Kessinger   Houston Astros SS    "Mississippi"    2019
    ## 49     3   106 JR    Jordan Brewer    Houston Astros OF    "Michigan"       2019
    ## 50     4   136 JR    Colin Barber     Houston Astros OF    "\"Pleasant Va~  2019
    ## 51     5   166 JR    Hunter Brown     Houston Astros RHP   "Wayne State"    2019
    ## 52     2    72 JR    Alex Santos II   Houston Astros RHP   "Mount Saint M~  2020
    ## 53     3   101 JR    Tyler Brown      Houston Astros RHP   "Vanderbilt Un~  2020
    ## 54     4   131 JR    Zach Daniels     Houston Astros OF    "University of~  2020
    ## 55     5   160 JR    Shay Whitcomb    Houston Astros SS    "UC San Diego"   2020
    ## 56     3    87 JR    Tyler Whitaker   Houston Astros OF    "Bishop Gorman~  2021
    ## 57     4   117 JR    Alex Ulloa       Houston Astros SS    "Calvary Chris~  2021
    ## 58     4   132 JR    Chayce McDermott Houston Astros RHP   "Ball State Un~  2021
    ## 59     5   148 JR    Quincy Hamilton  Houston Astros OF    "Wright State ~  2021

``` r
print(BBA_DRAFT_ALL[ which(BBA_DRAFT_ALL$Tm == "Chicago Cubs" & BBA_DRAFT_ALL$Rd <= 5), ], n = 58)
```

    ## # A tibble: 58 x 8
    ##       Rd   `#` Phase Name                Tm           POS   `Drafted From`  Year
    ##    <dbl> <dbl> <chr> <chr>               <chr>        <chr> <chr>          <dbl>
    ##  1     1     9 JR    Javier Baez         Chicago Cubs SS    "Arlington Co~  2011
    ##  2     2    68 JR    Daniel Vogelbach    Chicago Cubs 1B    "Bishop Verot~  2011
    ##  3     3    98 JR    Ezekiel DeVoss      Chicago Cubs CF    "University o~  2011
    ##  4     4   129 JR    Tony Zych           Chicago Cubs P     "University o~  2011
    ##  5     5   159 JR    Tayler Scott        Chicago Cubs RHP   "Notre Dame P~  2011
    ##  6     1     6 JR    Albert Almora       Chicago Cubs OF    "Mater Academ~  2012
    ##  7     1    43 JR    Pierce Johnson      Chicago Cubs P     "Missouri Sta~  2012
    ##  8     1    56 JR    Paul Blackburn      Chicago Cubs P     "Heritage Hig~  2012
    ##  9     2    67 JR    Duane Underwood Jr. Chicago Cubs RHP   "Pope High Sc~  2012
    ## 10     3   101 JR    Ryan McNeil         Chicago Cubs P     "Nipomo High ~  2012
    ## 11     4   134 JR    Josh Conway         Chicago Cubs P     "Coastal Caro~  2012
    ## 12     5   164 JR    Anthony Prieto      Chicago Cubs P     "Americas (El~  2012
    ## 13     1     2 JR    Kris Bryant         Chicago Cubs 3B    "University o~  2013
    ## 14     2    41 JR    Rob Zastryzny       Chicago Cubs LHP   "Missouri"      2013
    ## 15     3    75 JR    Jacob Hannemann     Chicago Cubs CF    "BYU"           2013
    ## 16     4   108 JR    Tyler Skulina       Chicago Cubs RHP   "Kent State U~  2013
    ## 17     5   138 JR    Trey Masek          Chicago Cubs RHP   "Texas Tech U~  2013
    ## 18     1     4 JR    Kyle Schwarber      Chicago Cubs C     "Indiana Univ~  2014
    ## 19     2    45 JR    Jake Stinnett       Chicago Cubs P     "University o~  2014
    ## 20     3    78 JR    Mark Zagunis        Chicago Cubs C     "Virginia Tec~  2014
    ## 21     4   109 JR    Carson Sands        Chicago Cubs P     "North Florid~  2014
    ## 22     5   139 JR    Justin Steele       Chicago Cubs LHP   "George Count~  2014
    ## 23     1     9 JR    Ian Happ            Chicago Cubs OF    "University o~  2015
    ## 24     2    47 JR    Donnie Dewees       Chicago Cubs OF    "University o~  2015
    ## 25     3    82 JR    Bryan Hudson        Chicago Cubs P     "Alton High S~  2015
    ## 26     4   113 JR    Darryl Wilson       Chicago Cubs OF    "Canton South~  2015
    ## 27     5   143 JR    Ryan Kellogg        Chicago Cubs P     "Arizona Stat~  2015
    ## 28     3   104 JR    Tom Hatch           Chicago Cubs RHP   "Oklahoma Sta~  2016
    ## 29     4   134 JR    Tyson Miller        Chicago Cubs RHP   "California B~  2016
    ## 30     5   164 JR    Bailey Clark        Chicago Cubs P     "Duke Univers~  2016
    ## 31     1    27 JR    Brendon Little      Chicago Cubs LHP   "State Colleg~  2017
    ## 32     1    30 JR    Alex Lange          Chicago Cubs RHP   "LSU"           2017
    ## 33     2    67 JR    Cory Abbott         Chicago Cubs RHP   "Loyola Marym~  2017
    ## 34     3   105 JR    Keegan Thompson     Chicago Cubs RHP   "Auburn Unive~  2017
    ## 35     4   135 JR    Erich Uelmen        Chicago Cubs RHP   "Cal Poly - S~  2017
    ## 36     5   165 JR    Nelson Velazquez    Chicago Cubs OF    "PJ Education~  2017
    ## 37     1    24 JR    Nico Hoerner        Chicago Cubs SS    "Stanford"      2018
    ## 38     2    62 JR    Brennen Davis       Chicago Cubs OF    "\"Basha HS, ~  2018
    ## 39     2    77 JR    Cole Roederer       Chicago Cubs OF    "\"Hart HS, S~  2018
    ## 40     2    78 JR    Paul Richan         Chicago Cubs RHP   "San Diego"     2018
    ## 41     3    98 JR    Jimmy Herron        Chicago Cubs OF    "Duke"          2018
    ## 42     4   128 JR    Ethan Roberts       Chicago Cubs RHP   "Tennessee Te~  2018
    ## 43     5   158 JR    Andy Weber          Chicago Cubs 2B    "Virginia"      2018
    ## 44     1    27 JR    Ryan Jensen         Chicago Cubs RHP   "Fresno State"  2019
    ## 45     2    64 JR    Chase Strumpf       Chicago Cubs 2B    "UCLA"          2019
    ## 46     3   103 JR    Michael McAvene     Chicago Cubs RHP   "Louisville"    2019
    ## 47     4   132 JR    Chris Clarke        Chicago Cubs RHP   "University o~  2019
    ## 48     5   162 JR    Josh Burgmann       Chicago Cubs RHP   "Washington"    2019
    ## 49     1    16 JR    Ed Howard           Chicago Cubs SS    "Mount Carmel~  2020
    ## 50     2    51 JR    Burl Carraway       Chicago Cubs LHP   "Dallas Bapti~  2020
    ## 51     3    88 JR    Jordan Nwogu        Chicago Cubs OF    "University o~  2020
    ## 52     4   117 JR    Luke Little         Chicago Cubs LHP   "San Jacinto ~  2020
    ## 53     5   147 JR    Koen Moreno         Chicago Cubs RHP   "Panther Cree~  2020
    ## 54     1    21 JR    Jordan Wicks        Chicago Cubs LHP   "Kansas State~  2021
    ## 55     2    56 JR    James Triantos      Chicago Cubs 3B    "James Madiso~  2021
    ## 56     3    93 JR    Drew Gray           Chicago Cubs LHP   "IMG Academy ~  2021
    ## 57     4   123 JR    Christian Franklin  Chicago Cubs OF    "University o~  2021
    ## 58     5   154 JR    Liam Spence         Chicago Cubs SS    "University o~  2021

## Scrape and Clean Baseball Reference Standings Data

``` r
  ## Create a function to import the standings from 2011-2021 according to Baseball Reference
br_stand_fn = function(Year){
  df = read_html(
    paste("https://www.baseball-reference.com/leagues/majors/20",
          Year,
          "-standings.shtml#expanded_standings_overall",
          sep = "")
  ) %>%
    html_table()
  ## Combine the divisions into one standings dataframe
  df = reduce(df[][], full_join, by = c("Tm", "W", "L", "W-L%", "GB"))
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
  ## Loop through and import the standings from 2011-2021
for ( i in 11:21 ) {
  br_stand_fn(Year = i)
}

StandALL = rbind.data.frame(Standings_2011, Standings_2012, Standings_2013, Standings_2014, Standings_2015, Standings_2016, Standings_2017, Standings_2018, Standings_2019, Standings_2020, Standings_2021)

StandALL$Tm[ which(StandALL$Tm == "Los Angeles Angels of Anaheim")] = "Los Angeles Angels"
StandALL$Tm[ which(StandALL$Tm == "Florida Marlins")] = "Miami Marlins"

rm(Standings_2011, Standings_2012, Standings_2013, Standings_2014, Standings_2015, Standings_2016, Standings_2017, Standings_2018, Standings_2019, Standings_2020, Standings_2021)
```

## Import and clean “player value” Data from `Baseball Reference`

Import

``` r
  ## Import player value stats downloaded from Baseball Reference
setwd("C:/Users/Erik/Desktop/BoxCopy/Programming Scripts and Data/Baseball/Projects/Tanking/Tanking/Player Value files/")
files = list.files(pattern = "Player_Value")
for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = files[i]))
  )
}
```

Clean the data

``` r
## Add "playerID" to the playerID column
  ## For position players
PP_val_colnames = colnames(Player_Value_Batting_2011)
PP_val_colnames = c(PP_val_colnames[c(1,2)], "playerID", PP_val_colnames[c(3:(length(PP_val_colnames)-1))])
  ## For pitchers
PO_val_colnames = colnames(Player_Value_Pitching_2011)
PO_val_colnames = c(PO_val_colnames[c(1,2)], "playerID", PO_val_colnames[c(3:(length(PO_val_colnames)-1))])
  ## 2020, 2021 have Xinn value added; account for the extra column
Player_Value_Pitching_2020 = Player_Value_Pitching_2020[ , -13]
Player_Value_Pitching_2021 = Player_Value_Pitching_2021[ , -13]

  ## Correct column names
colnames(Player_Value_Batting_2011) = PP_val_colnames
colnames(Player_Value_Pitching_2011) = PO_val_colnames
```

Repeat the column name revisions for all the dfs (not shown)

Put the hitter and pitcher value dfs into separate lists to loop through
for cleaning

``` r
Hitter_Value_df_list = ls()[
  which(grepl(ls(), pattern = "Player_Value_Batting") == TRUE)
]
Pitcher_Value_df_list = ls()[
  which(grepl(ls(), pattern = "Player_Value_Pitching") == TRUE)
]

## Create a function to clean the hitter dfs
clean_br_hitter_value_df_fn = function(df){
  ## Remove the "Rk" column
  df = df[ , -1 ]
  ## Remove the handedness coded into the "Name" column
  df$Name = gsub(df$Name, pattern = "\\*|\\#", replacement = "")
  Encoding(df$Name) = "UTF-8"
  df$Name = iconv(df$Name, "UTF-8", "UTF-8", sub = " ")
  colnames(df)[24] = "Pos.Summary"
  ## Remove pitchers from the hitters df
  df = df[ -which(grepl(df$Pos.Summary, pattern = "1|\\/1|([0-9]-Jan)" ) == TRUE), ]
  ## Convert the "salary" column
  df$Salary = as.numeric(gsub(df$Salary, pattern = "\\$|\\,", replacement = ""))/1000000
  rownames(df) = NULL
  return(df)
}

## Create functions to clean the pitcher dfs
clean_br_pitcher_value_df_fn = function(df){
  ## Remove the "Rk" column
  df = df[ , -1 ]
  ## Remove the handedness coded into the "Name" column
  df$Name = gsub(df$Name, pattern = "\\*|\\#", replacement = "")
  ## Convert to UTF-8 and remove special characters
  Encoding(df$Name) = "UTF-8"
  df$Name = iconv(df$Name, "UTF-8", "UTF-8", sub = " ")
  ## Convert the "salary" column
  df$Salary = as.numeric(gsub(df$Salary, pattern = "\\$|\\,", replacement = ""))/1000000
  rownames(df) = NULL
  return(df)
}
## Loop through and concatenate the df variable names into a list
for (i in 1:length(Hitter_Value_df_list) ){
  Hitter_Value_df_list[i] = rbind.data.frame(Hitter_Value_df_list[i])
  Pitcher_Value_df_list[i] = rbind.data.frame(Pitcher_Value_df_list[i])
}
## Clean the dfs
cleaned_br_hitter_val_df_list = lapply(mget(unlist(Hitter_Value_df_list)), clean_br_hitter_value_df_fn)
cleaned_br_pitcher_val_df_list = lapply(mget(unlist(Pitcher_Value_df_list)), clean_br_pitcher_value_df_fn)

## Add each df's Year as a column
cleaned_br_hitter_val_df_list[[1]]$Year = 2011
cleaned_br_pitcher_val_df_list[[1]]$Year = 2011
```

Add each df’s `Year` as a column for all `cleaned_br_hitter_val_df_list`
or `cleaned_br_pitcher_val_df_list` objects (not shown)

Loop through and find **position players** in the
`player value pitching df`. Remove them

``` r
  ## Initialize vector
temp = vector()
for ( i in 1:length(cleaned_br_pitcher_val_df_list) ) {
  for ( j in 1:nrow(cleaned_br_pitcher_val_df_list[[i]])) {
    if ( cleaned_br_pitcher_val_df_list[[i]]$playerID[j] %in% cleaned_br_hitter_val_df_list[[i]]$playerID ){
      ## Identify matches of playerID, store in temp vector
      temp[j] = which(cleaned_br_hitter_val_df_list[[i]]$playerID == cleaned_br_pitcher_val_df_list[[i]]$playerID[j])
      temp = temp[which(!is.na(temp))]
      ## Remove the position players from the pitcher dfs
      cleaned_br_pitcher_val_df_list[[i]] = cleaned_br_pitcher_val_df_list[[i]][ -temp, ]
    }
  }
}
#sort(unique(cleaned_br_hitter_val_df_list[[1]]$Tm))
```

Put the cleaned Hitter Value dfs into one df for easier calculation of
career `bWAR`s

``` r
CAREER_hitter_WAR_df = data.frame()
CAREER_hitter_WAR_df = rbind.data.frame(cleaned_br_hitter_val_df_list[[1]][c(1,2,16,22,25)], cleaned_br_hitter_val_df_list[[2]][c(1,2,16,22,25)],
                                        cleaned_br_hitter_val_df_list[[3]][c(1,2,16,22,25)], cleaned_br_hitter_val_df_list[[4]][c(1,2,16,22,25)],
                                        cleaned_br_hitter_val_df_list[[5]][c(1,2,16,22,25)], cleaned_br_hitter_val_df_list[[6]][c(1,2,16,22,25)],
                                        cleaned_br_hitter_val_df_list[[7]][c(1,2,16,22,25)], cleaned_br_hitter_val_df_list[[8]][c(1,2,16,22,25)],
                                        cleaned_br_hitter_val_df_list[[9]][c(1,2,16,22,25)], cleaned_br_hitter_val_df_list[[10]][c(1,2,16,22,25)],
                                        cleaned_br_hitter_val_df_list[[11]][c(1,2,16,22,25)])
## Calculate hitter career WAR and put it into a dataframe
HitterCareerWAR = aggregate(x = CAREER_hitter_WAR_df$WAR, by = list(CAREER_hitter_WAR_df$playerID), FUN = sum)
HitterCareerWAR = as.data.frame(HitterCareerWAR)
colnames(HitterCareerWAR) = c("playerID", "Career WAR")
HitterCareerWAR$`Career WAR` = round(HitterCareerWAR$`Career WAR`, digits = 3)
HitterCareerWAR = add_column(HitterCareerWAR, Name = "", .before = 1)
## Edit hitter names into the dataframe by looping through
dummy = vector()
for (i in HitterCareerWAR$playerID ){
  dummy[i] = CAREER_hitter_WAR_df$Name[which(CAREER_hitter_WAR_df$playerID == i)]
}
HitterCareerWAR$Name = dummy
```

Repeat the cleaning process above for pitchers (not shown)

Combine Career bWAR dfs

``` r
## Combine the career WAR dfs
PLAYER_CAREER_WAR = rbind.data.frame(HitterCareerWAR, PitcherCareerWAR)
PLAYER_CAREER_WAR = PLAYER_CAREER_WAR %>%
  arrange(desc(`Career WAR`))

Encoding(PLAYER_CAREER_WAR$Name) = "UTF-8"
PLAYER_CAREER_WAR$Name = iconv(PLAYER_CAREER_WAR$Name, "UTF-8", "UTF-8", sub = " ")

PLAYER_CAREER_WAR = PLAYER_CAREER_WAR[ -which(duplicated(PLAYER_CAREER_WAR$Name) == TRUE), ]
```

## Scrape and Clean Team bWAR and salary/payroll data from `Baseball Reference`

``` r
  ## Create a function that will import team value tables from Baseball Reference (for pitching stats and hitting stats)
br_team_value_import_fn = function(Year, Player_data_type) {
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
  ## Loop through the relevant years and import the tables as dataframes
for ( i in 11:21 ){
  br_team_value_import_fn(Year = i, Player_data_type = "batting")
}
  ## Repeat the loop for pitching stats
for ( i in 11:21 ){
  br_team_value_import_fn(Year = i, Player_data_type = "pitching")
}
  ## Concatenate into a aggregate dataframes by player type
TeamBatALL = rbind.data.frame(Team_battingvalue_2011[c(1:30),],Team_battingvalue_2012[c(1:30),], Team_battingvalue_2013[c(1:30),],
                              Team_battingvalue_2014[c(1:30),],Team_battingvalue_2015[c(1:30),], Team_battingvalue_2016[c(1:30),],
                              Team_battingvalue_2017[c(1:30),],Team_battingvalue_2018[c(1:30),], Team_battingvalue_2019[c(1:30),],
                              Team_battingvalue_2020[c(1:30),],Team_battingvalue_2021[c(1:30),])

TeamBatALL$Tm[ which(TeamBatALL$Tm == "Los Angeles Angels of Anaheim")] = "Los Angeles Angels"
TeamBatALL$Tm[ which(TeamBatALL$Tm == "Florida Marlins")] = "Miami Marlins"

  ## BR added a value metric to Xinn pitching (col 10)
    ## Remove to concatenate
TeamPitALL = rbind.data.frame(Team_pitchingvalue_2011[c(1:30),], Team_pitchingvalue_2012[c(1:30),], Team_pitchingvalue_2013[c(1:30),],
                              Team_pitchingvalue_2014[c(1:30),], Team_pitchingvalue_2015[c(1:30),], Team_pitchingvalue_2016[c(1:30),],
                              Team_pitchingvalue_2017[c(1:30),], Team_pitchingvalue_2018[c(1:30),], Team_pitchingvalue_2019[c(1:30),],
                              Team_pitchingvalue_2020[c(1:30),-10], Team_pitchingvalue_2021[c(1:30),-10])

TeamPitALL$Tm[ which(TeamPitALL$Tm == "Los Angeles Angels of Anaheim")] = "Los Angeles Angels"
TeamPitALL$Tm[ which(TeamPitALL$Tm == "Florida Marlins")] = "Miami Marlins"

  ## Convert the WAR and Salary columns to numerical data to add together in a new df
TeamBatALL$WAR = as.numeric(TeamBatALL$WAR)
TeamBatALL$Salary = as.numeric(TeamBatALL$Salary)
TeamPitALL$WAR = as.numeric(TeamPitALL$WAR)
TeamPitALL$Salary = as.numeric(TeamPitALL$Salary)
```

Add player acquisition data to the cleaned `Baseball Reference` player
value data

``` r
  ## Iniitialize vector
df = vector()
  ## Loop through and tab each team's number of acqusitions for the main acq. types in each year;
    ## Put them in their own dfs
      ## For hitters
for(i in 1:11){
  df = cleaned_br_hitter_val_df_list[[i]][-which(cleaned_br_hitter_val_df_list[[i]]$Tm == "2TM" |
                                                   cleaned_br_hitter_val_df_list[[i]]$Tm == "3TM" |
                                                   cleaned_br_hitter_val_df_list[[i]]$Tm == "4TM" |
                                                   cleaned_br_hitter_val_df_list[[i]]$Tm == "5TM"),
  ] %>%
    group_by(Tm, Year) %>%
    summarise(Sum_Num_Trades = sum(length(which(Acquired == "Traded"))),
              Sum_Num_FAs = sum(length(which(Acquired == "Free Agency"))),
              Sum_Num_Ams = sum(length(which(Acquired == "Amateur Draft" | Acquired == "Amateur Free Agent"))),
              Sum_Sal = sum(Salary, na.rm = T)) %>%
    arrange(Tm) %>%
    as.data.frame()
  assign(paste("HitterAcqs_", i, sep = ""), df, .GlobalEnv)
}
```

Repeat for pitchers

``` r
df = vector()
for(i in 1:11){
  df = cleaned_br_pitcher_val_df_list[[i]][-which(cleaned_br_pitcher_val_df_list[[i]]$Tm == "2TM" |
                                                    cleaned_br_pitcher_val_df_list[[i]]$Tm == "3TM" | 
                                                    cleaned_br_pitcher_val_df_list[[i]]$Tm == "4TM" |
                                                    cleaned_br_pitcher_val_df_list[[i]]$Tm == "5TM"),] %>%
    group_by(Tm, Year) %>%
    summarise(Sum_Num_Trades = sum(length(which(Acquired == "Traded"))),
              Sum_Num_FAs = sum(length(which(Acquired == "Free Agency"))),
              Sum_Num_Ams = sum(length(which(Acquired == "Amateur Draft" | Acquired == "Amateur Free Agent"))),
              Sum_Sal = sum(Salary, na.rm = T)) %>%
    arrange(Tm) %>%
    as.data.frame()
  assign(paste("PitcherAcqs_", i, sep = ""), df, .GlobalEnv)
}
```

Concatenate the acquisition dfs into a list of dfs; prep the data to be
joined to the `TeamBatALL`, `TeamPitALL` dfs

``` r
  ## Concatenate into a list of dfs
HitterAcqs = mget(
  unlist(
    ls()[
      which(grepl(ls(), pattern = "HitterAcqs_") == TRUE)
    ][c(1,4:11,2,3)]
  )
)
PitcherAcqs = mget(
  unlist(
    ls()[
      which(grepl(ls(), pattern = "PitcherAcqs_") == TRUE)
    ][c(1,4:11,2,3)]
  )
)
  ## Remove the individual dfs
rm(list = 
     ls()[grep(ls()[], pattern = "HitterAcqs_|PitcherAcqs_")]
)
  ## Re-name the Marlins
PitcherAcqs[[1]]$Tm[which(PitcherAcqs[[1]]$Tm == "FLA")] = "MIA"
HitterAcqs[[1]]$Tm[which(HitterAcqs[[1]]$Tm == "FLA")] = "MIA"
  ## Re-order the list of dfs alphabetically
PitcherAcqs[[1]] = PitcherAcqs[[1]][order(PitcherAcqs[[1]]$Tm),]
HitterAcqs[[1]] = HitterAcqs[[1]][order(HitterAcqs[[1]]$Tm),]
  ## Match names with full official names
for(i in 1:11){
  PitcherAcqs[[i]]$Tm = unique(sort(StandALL$Tm))
  HitterAcqs[[i]]$Tm = unique(sort(StandALL$Tm))
}
  ## Re-arrange dfs into one df, housing the acq. data
PitcherAcqs = bind_rows(PitcherAcqs, .id = "column_label")
PitcherAcqs = PitcherAcqs %>%
  select(Tm, Year, Sum_Num_Trades, Sum_Num_FAs, Sum_Num_Ams, Sum_Sal)

HitterAcqs = bind_rows(HitterAcqs, .id = "column_label")
HitterAcqs = HitterAcqs %>%
  select(Tm, Year, Sum_Num_Trades, Sum_Num_FAs, Sum_Num_Ams, Sum_Sal)

  ## Join with Team Value data and remove extra data
TeamBatALL = HitterAcqs %>%
  left_join(TeamBatALL) %>%
  select(Tm, Year, Sum_Num_Trades, Sum_Num_FAs, Sum_Num_Ams, Sum_Sal, WAR, Salary)
TeamPitALL = PitcherAcqs %>%
  left_join(TeamPitALL) %>%
  select(Tm, Year, Sum_Num_Trades, Sum_Num_FAs, Sum_Num_Ams, Sum_Sal, WAR, Salary)
```

Combine into one `TeamValALL` df

``` r
  ## Combine team pitching and team hitting WAR, salary
TeamValALL = TeamBatALL
TeamValALL[,c(3:8)] = TeamBatALL[,c(3:8)] + TeamPitALL[,c(3:8)]

TeamValALL = StandALL %>%
  inner_join(TeamValALL)
colnames(TeamValALL)[4] = "W_L"

  ## Remove temp dfs
rm(Team_battingvalue_2011, Team_battingvalue_2012, Team_battingvalue_2013, Team_battingvalue_2014, Team_battingvalue_2015, Team_battingvalue_2016, Team_battingvalue_2017, Team_battingvalue_2018, Team_battingvalue_2019, Team_battingvalue_2020, Team_battingvalue_2021)
rm(Team_pitchingvalue_2011, Team_pitchingvalue_2012, Team_pitchingvalue_2013, Team_pitchingvalue_2014, Team_pitchingvalue_2015, Team_pitchingvalue_2016, Team_pitchingvalue_2017, Team_pitchingvalue_2018, Team_pitchingvalue_2019, Team_pitchingvalue_2020, Team_pitchingvalue_2021)

head(TeamValALL)
```

    ##                      Tm   W  L   W_L GB DivRank Year Sum_Num_Trades Sum_Num_FAs
    ## 1 Philadelphia Phillies 102 60 0.630 --       5 2011              6          15
    ## 2      New York Yankees  97 65 0.599 --       1 2011              6          18
    ## 3         Texas Rangers  96 66 0.593 --       1 2011             10          11
    ## 4     Milwaukee Brewers  96 66 0.593 --       5 2011              5          18
    ## 5        Detroit Tigers  95 67 0.586 --       1 2011              7          11
    ## 6  Arizona Diamondbacks  94 68 0.580 --       1 2011             12          14
    ##   Sum_Num_Ams   Sum_Sal  WAR    Salary
    ## 1          14 170.79838 54.1 266.43571
    ## 2          15 194.10463 53.6 269.04539
    ## 3          11  85.62610 54.6 117.40537
    ## 4          14  83.39733 40.0 126.64633
    ## 5          17 105.05723 41.8 140.49623
    ## 6          13  46.78983 34.1  74.30183

## Scrape MLB Free-Agent Data from ESPN

``` r
## Create a function to scrape Free-Agent Data from ESPN
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
  ## Create a variable for a selector
ESPN_selector = "#my-players-table"
  ## Loop through and import the data for the relevant time-frame
for ( i in 11:21 ) {
  ESPN_FA_import_fn(Year = i, selector = ESPN_selector)
}
  ## Combine the dfs into one df
ESPN = bind_rows(ESPN_FA_2011, ESPN_FA_2012, ESPN_FA_2013, ESPN_FA_2014, ESPN_FA_2015,
                 ESPN_FA_2016, ESPN_FA_2017, ESPN_FA_2018, ESPN_FA_2019, ESPN_FA_2020,
                 ESPN_FA_2021)
  ## Remove FA deals to Japanese baseball league and "undisclosed" deals
ESPN = ESPN[ -which(ESPN$Team == "Japan"), ]
ESPN = ESPN[ -which(ESPN$Type == "Undisclosed"), ]
rm(ESPN_FA_2011, ESPN_FA_2012, ESPN_FA_2013, ESPN_FA_2014, ESPN_FA_2015, ESPN_FA_2016, ESPN_FA_2017, ESPN_FA_2018, ESPN_FA_2019, ESPN_FA_2020, ESPN_FA_2021)
```

## Scrape and Clean International Signing Data from Spotrac

``` r
  ## Create a selector from the spotrac website
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
  ## Loop through the scraping function by the relevant years
for ( i in 11:21 ) {
  Spotrac_Intl_import_fn(Year = i, spotrac_selector = spotrac_Intl_selector)
}
  ## Put all the dfs into one df and remove the separate dfs
IntlALL = rbind.data.frame(Intl_Signings_2011, Intl_Signings_2012, Intl_Signings_2013, Intl_Signings_2014, Intl_Signings_2015,
                           Intl_Signings_2016, Intl_Signings_2017, Intl_Signings_2018, Intl_Signings_2019, Intl_Signings_2020,
                           Intl_Signings_2021)
rm(Intl_Signings_2011, Intl_Signings_2012, Intl_Signings_2013, Intl_Signings_2014, Intl_Signings_2015, Intl_Signings_2016, Intl_Signings_2017, Intl_Signings_2018, Intl_Signings_2019, Intl_Signings_2020, Intl_Signings_2021)
```

## Create and Populate DFs for Plotting

Create dfs

``` r
  ## Remove "Carlos Pena" from Chicago Cubs draft records
BBA_DRAFT_ALL = BBA_DRAFT_ALL[ -which(BBA_DRAFT_ALL$Name == "Carlos Pena"), ]

  ## Remove duplicates
BBA_DRAFT_ALL_no_dup = BBA_DRAFT_ALL[ -which(duplicated(BBA_DRAFT_ALL$Name) == TRUE), ]

  ## Correct Lance McCullers' name
BBA_DRAFT_ALL$Name[ grep(BBA_DRAFT_ALL$Name, pattern = "McCullers") ] = "Lance McCullers Jr."
BBA_DRAFT_ALL_no_dup$Name[ grep(BBA_DRAFT_ALL_no_dup$Name, pattern = "McCullers") ] = "Lance McCullers Jr."

  ## Subset the relevant draft data
ASTROS_CUBS_DRAFT_SUB =
  BBA_DRAFT_ALL_no_dup[ which(BBA_DRAFT_ALL_no_dup$Tm == "Houston Astros" |
                                BBA_DRAFT_ALL_no_dup$Tm == "Chicago Cubs") , ]

LEAGUE_DRAFT_SUB = BBA_DRAFT_ALL_no_dup[ which(BBA_DRAFT_ALL_no_dup$Tm != "Houston Astros" &
                                                 BBA_DRAFT_ALL_no_dup$Tm != "Chicago Cubs"), ]

  ## Join draft subset dfs with career WAR data -> How good drafting was
ASTROS_CUBS_DRAFT_SUB = ASTROS_CUBS_DRAFT_SUB %>%
  left_join(PLAYER_CAREER_WAR)
ASTROS_CUBS_DRAFT_SUB[ -which(ASTROS_CUBS_DRAFT_SUB$Year == 2011), ] %>%
  group_by(Tm) %>%
  summarize(Career_WAR = sum(`Career WAR`, na.rm = T)) %>%
  arrange(desc(Career_WAR))
```

    ## # A tibble: 2 x 2
    ##   Tm             Career_WAR
    ##   <chr>               <dbl>
    ## 1 Houston Astros       79.4
    ## 2 Chicago Cubs         57

``` r
LEAGUE_DRAFT_SUB = LEAGUE_DRAFT_SUB %>%
  left_join(PLAYER_CAREER_WAR)
Draft_Results = LEAGUE_DRAFT_SUB %>%
  group_by(Tm, Year) %>%
  summarize(Career_WAR = sum(`Career WAR`, na.rm = T)) %>%
  arrange(desc(Career_WAR))

  ## Create subset dfs of standings, value, international signing and free-agent data for the rest of the league
LEAGUE_STAND_SUB = StandALL[ which(StandALL$Tm != "Houston Astros" & StandALL$Tm != "Chicago Cubs"), ]
LEAGUE_VAL_SUB = TeamValALL[ which(TeamValALL$Tm != "Houston Astros" & TeamValALL$Tm != "Chicago Cubs"), ]
LEAGUE_INTL_SUB = IntlALL[ which(IntlALL$Team != "HOU" & IntlALL$Team != "CHC"), ]
LEAGUE_ESPN_SUB = ESPN[ which(ESPN$Team != "Astros" & ESPN$Team != "Cubs"), ]
```

Populate Astros, Cubs, and League “Tanking” dfs

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
ASTROS_TANK_DF[,2] = StandALL$`W-L%`[ which(StandALL$Tm == "Houston Astros") ]
CUBS_TANK_DF[,2] = StandALL$`W-L%`[ which(StandALL$Tm == "Chicago Cubs") ]
LG_TANK_DF$`Win%` = 0.500
```

Fill \# of picks in First 5 rds

``` r
  ## Fill # of Picks in First 5 rds
ASTROS_TANK_DF[1,3] = length( which( ASTROS_CUBS_DRAFT_SUB$Tm == "Houston Astros" &
                                       ASTROS_CUBS_DRAFT_SUB$Year == 2011 &
                                       ASTROS_CUBS_DRAFT_SUB$Rd <= 5 ) )
```

Repeat for rest, for `CUBS_TANK_DF` and `LG_TANK_DF` (not shown)

Fill \# SSs drafted

``` r
## Fill # of SSs drafted
ASTROS_TANK_DF[1,4] = length( which( ASTROS_CUBS_DRAFT_SUB$Tm == "Houston Astros" &
                                       ASTROS_CUBS_DRAFT_SUB$Year == 2011 &
                                       ASTROS_CUBS_DRAFT_SUB$POS == "SS" ) )
```

Repeat for rest, for `CUBS_TANK_DF` and `LG_TANK_DF` (not shown)

Populate the rest of the dfs (not shown)

Add career bWAR of draftees to the dfs

``` r
## Add Career WAR of draftees
ASTROS_TANK_DF = ASTROS_CUBS_DRAFT_SUB %>%
  group_by(Year) %>%
  filter(Tm == "Houston Astros") %>%
  summarize(Draft_Class_Career_WAR = sum(`Career WAR`, na.rm = T)) %>%
  select(Draft_Class_Career_WAR) %>%
  bind_cols(ASTROS_TANK_DF)
ASTROS_TANK_DF = ASTROS_TANK_DF %>%
  relocate(Draft_Class_Career_WAR, .after = 19)

CUBS_TANK_DF = ASTROS_CUBS_DRAFT_SUB %>%
  group_by(Year) %>%
  filter(Tm == "Chicago Cubs") %>%
  summarize(Draft_Class_Career_WAR = sum(`Career WAR`, na.rm = T)) %>%
  select(Draft_Class_Career_WAR) %>%
  bind_cols(CUBS_TANK_DF)
CUBS_TANK_DF = CUBS_TANK_DF %>%
    relocate(Draft_Class_Career_WAR, .after = 19)

LG_TANK_DF = LEAGUE_DRAFT_SUB %>%
  group_by(Year) %>%
  summarize(Draft_Class_Career_WAR = sum(`Career WAR`, na.rm = T)/28) %>%
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
```

## Create and Clean the df for Tankers

``` r
REBUILDERS_df = TeamValALL %>%
  group_by(`Tm`, Year) %>%
  filter(L >= 92)

  ## Remove the Red Sox, Indians/Guardians, Brewers, Mets, Giants, Rays, Jays, Nats (idx = 5, 7, 23, 35, 40, 30, 52, 58)
REBUILDERS_df = REBUILDERS_df[ -c(5, 7, 23, 35, 40, 30, 52, 58), ]

REBUILDERS_df = REBUILDERS_df %>%
  group_by(`Tm`, Year) %>%
  arrange(`Tm`, Year) %>%
  ungroup()

REBUILDERS_df$GB = as.double(REBUILDERS_df$GB)
colnames(REBUILDERS_df)[c(8:13)] = c("MLB Trades Made", "FA Signings", "Minors Callups", "Payroll Sum", "bWAR", "Payroll (in M $US)")
head(REBUILDERS_df[c(4,5,7:11,13:16,20:27,31:38,41,42,45,46,52:54),-11])
```

    ## # A tibble: 6 x 12
    ##   Tm            W     L   W_L    GB DivRank  Year `MLB Trades Ma~` `FA Signings`
    ##   <chr>     <int> <int> <dbl> <dbl>   <int> <dbl>            <int>         <int>
    ## 1 Atlanta ~    67    95 0.414  23         4  2015                8            18
    ## 2 Atlanta ~    68    93 0.422  26.5       5  2016               17            16
    ## 3 Baltimor~    47   115 0.29   61         5  2018                9            13
    ## 4 Baltimor~    54   108 0.333  49         5  2019               12            10
    ## 5 Baltimor~    52   110 0.321  48         5  2021                6            13
    ## 6 Chicago ~    61   101 0.377  36         4  2012                7            10
    ## # ... with 3 more variables: `Minors Callups` <int>, bWAR <dbl>,
    ## #   `Payroll (in M $US)` <dbl>

``` r
  ## Remove non-qualifiers
Printable_REBUILD = REBUILDERS_df[c(4,5,7:11,13:16,20:27,31:38,41,42,45,46),-11]

  ## Re-arrange for display
Printable_REBUILD = Printable_REBUILD %>%
  select(`Tm`, Year, W, L, W_L, GB, DivRank, `MLB Trades Made`, `FA Signings`, `Minors Callups`, bWAR, `Payroll (in M $US)`)

  ## For plotting
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
    values = rbind(Printable_REBUILD$Tm,
                   Printable_REBUILD$Year,
                   Printable_REBUILD$W,
                   Printable_REBUILD$L,
                   Printable_REBUILD$W_L,
                   Printable_REBUILD$GB,
                   Printable_REBUILD$DivRank,
                   Printable_REBUILD$`MLB Trades Made`,
                   Printable_REBUILD$`FA Signings`,
                   Printable_REBUILD$`Minors Callups`,
                   Printable_REBUILD$bWAR,
                   Printable_REBUILD$`Payroll (in M $US)`),
    align = c("center", "center"),
    line = list(color = "black", width = 1),
    font = list(family = "Times", size = 12, color = c("black"))
  ))
```
