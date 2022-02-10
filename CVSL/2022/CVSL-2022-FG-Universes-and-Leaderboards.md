Central Valley Strat-O-Matic League 2022 Universes and Leaderboards
================
Erik Larsen
2/9/2022

This snippet cleans `FanGraphs (FG)` and `Baseball-Reference (BR)`
player Leaderboards (`catchers`, `pitchers`, and `non-catchers`). When
scraping, the `FG Leaderboards` do not return the full tables. The
`BR Leaderboards` cannot be scraped; thus, all were downloaded as
`CSV`s.  
This snippet also incorporates the `MLB` franchises from which our
`Strat-O-Matic` baseball dynasty league, [The Central Valley
Strat-O-Matic Baseball League
(CVSL)](https://sites.google.com/view/cvslbaseball/home), can draft
players.  
This snippet is based on the `R script`, [CVSL 2022 FG Universes and
Leaderboards](C:/Users/Erik/Desktop/BoxCopy/Programming%20Scripts%20and%20Data/Baseball/CVSL/CVSL%202022%20FG%20Universes%20and%20Leaderboards.R).

Required packages: [readr](https://cran.r-project.org/package=readr),
[tidyverse](https://cran.r-project.org/package=tidyverse)

## Environment Prep

Set the working directory and import the `FG 2021 Leaderboards` for
`Hitters`, `Pitchers`, and `Catchers`.  
`CVSL` rules require  
+ `eligible pitchers` to have accrued `30 IP`  
+ `eligible non-catchers` to have accrued `175 AB`  
+ `catchers` to have accrued `100 AB`

So these criteria were applied to `FG Leaderboard` queries.

## Data import

``` r
  ### FG 2021 Pitchers and Hitters totals and splits ###
files = list.files(path = "C:/Users/Erik/Desktop/BoxCopy/Strato/2022/", pattern = "2021 FG")
for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = paste("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/",files[i], sep = "")))
  )
}
  ## Re-name the FG tables containing minima for splits in 2021
Hitters_total = `2021 FG Hitters min 50 PA`
Hitters_split = `2021 FG Hitters min 50 PA Split teams`
Hitters_adv = `2021 FG Adv 100 PA min`
Pitchers_total = `2021 FG Pitchers min 30 IP`
Pitchers_split = `2021 FG Pitchers min 30 IP Split teams`
Pitchers_adv_2021 = `2021 FG Adv 30 IP min`
Catchers_total = `2021 FG Catchers min 100 PA`
  ## Remove the old objects
rm(`2021 FG Hitters min 50 PA`,
   `2021 FG Hitters min 50 PA Split teams`,
   `2021 FG Pitchers min 30 IP`,
   `2021 FG Pitchers min 30 IP Split teams`,
   `2021 FG Catchers min 100 PA`,
   `2021 FG Adv 30 IP min`,
   `2021 FG Adv 100 PA min`)
```

Import `FG Hitters Advanced` Leaderboards for advanced metrics
`from the last 6 seasons`.

``` r
  ### FG Hitters advanced metrics for each season in the league
files = list.files(path = "C:/Users/Erik/Desktop/BoxCopy/Strato/2022/", pattern = "FG Adv 100 PA min")
for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = paste("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/",files[i], sep = "")))
  )
}
  ## Re-name the FG tables for hitters containing advanced metrics for each season
Hitters_adv_2016 = `2016 FG Adv 100 PA min`
Hitters_adv_2017 = `2017 FG Adv 100 PA min`
Hitters_adv_2018 = `2018 FG Adv 100 PA min`
Hitters_adv_2019 = `2019 FG Adv 100 PA min`
Hitters_adv_2020 = `2020 FG Adv 100 PA min`
Hitters_adv_2021 = `2021 FG Adv 100 PA min`
    ## delete the old objects
rm(`2016 FG Adv 100 PA min`,
   `2017 FG Adv 100 PA min`,
   `2018 FG Adv 100 PA min`,
   `2019 FG Adv 100 PA min`,
   `2020 FG Adv 100 PA min`,
   `2021 FG Adv 100 PA min`)
  ## Fix the "Name" column names
colnames(Hitters_adv_2016)[1] = "Name"
colnames(Hitters_adv_2017)[1] = "Name"
colnames(Hitters_adv_2018)[1] = "Name"
colnames(Hitters_adv_2019)[1] = "Name"
colnames(Hitters_adv_2020)[1] = "Name"
colnames(Hitters_adv_2021)[1] = "Name"
```

Import `BR Leaderboards` that contain handedness and position
information. Wrangle the data into a format to manipulate.

``` r
files = list.files(path = "C:/Users/Erik/Desktop/BoxCopy/Strato/2022/", pattern = "BR")
for (i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = paste("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/",files[i], sep = ""), encoding = "UTF8")
    )
  )
}
  ## Re-name them to include underscores
    ##  First, concatenate the dataframe names into a list
BR_list = ls()[
  which(grepl(ls(), pattern = "BR") == TRUE)
]
    ## Replace the spaces with underscores and assign the files to the names
for(i in 1:length(unlist(BR_list))){
    
    BR_list[i] = gsub(" ", "_", ls()[
      which(grepl(ls(), pattern = "BR") == TRUE)[i]
    ], fixed = TRUE)
    
    as.data.frame(
      assign(unlist(BR_list)[i], read.csv(file = paste("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/", files[i], sep = ""), encoding = "UTF8")
    )
    )
}
```

Create a function that cleans the `BR` data (not shown).  
The data’s `encoding` is recognized as “unknown”, though it is `latin-1`
and co-erced into `UTF-8` upon import.  
This creates problems with strings/characters. The subsequent function
largely corrects for this, along with also decoding `BR`’s position and
handedness encodings.

Call all the `BR` data into the newly-created function.

``` r
  ## Clean the dfs; remove unrecognized characters due to improper encoding,
    ##  remove extra columns and rows; re-name strings, convert handedness
BR_2016_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2016_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2016")
BR_2017_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2017_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2017")
BR_2018_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2018_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2018")
BR_2019_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2019_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2019")
BR_2020_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2020_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2020")
BR_2021_Standard_Pitching_Leaders = clean_BR_df_fn(df = BR_2021_Standard_Pitching_Leaders, Player_type = "Pitcher", Year = "2021")

BR_2016_Standard_Pitching_Leaders = BR_2016_Standard_Pitching_Leaders[ -which(duplicated(BR_2016_Standard_Pitching_Leaders$Name)), ]
BR_2017_Standard_Pitching_Leaders = BR_2017_Standard_Pitching_Leaders[ -which(duplicated(BR_2017_Standard_Pitching_Leaders$Name)), ]
BR_2018_Standard_Pitching_Leaders = BR_2018_Standard_Pitching_Leaders[ -which(duplicated(BR_2018_Standard_Pitching_Leaders$Name)), ]
BR_2019_Standard_Pitching_Leaders = BR_2019_Standard_Pitching_Leaders[ -which(duplicated(BR_2019_Standard_Pitching_Leaders$Name)), ]
BR_2020_Standard_Pitching_Leaders = BR_2020_Standard_Pitching_Leaders[ -which(duplicated(BR_2020_Standard_Pitching_Leaders$Name)), ]
BR_2021_Standard_Pitching_Leaders = BR_2021_Standard_Pitching_Leaders[ -which(duplicated(BR_2021_Standard_Pitching_Leaders$Name)), ]

BR_2016_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2016_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2016")
BR_2017_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2017_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2017")
BR_2018_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2018_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2018")
BR_2019_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2019_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2019")
BR_2020_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2020_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2020")
BR_2021_Standard_Batting_Leaders = clean_BR_df_fn(df = BR_2021_Standard_Batting_Leaders, Player_type = "Hitter", Year = "2021")

  ## Check that strings are compatible to merge BR info with FG info
BR_2021_Standard_Batting_Leaders$Name[which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Whit Merrifield") == TRUE)]
```

    ## [1] "Whit Merrifield"

``` r
BR_2021_Standard_Pitching_Leaders$Name[grepl(BR_2021_Standard_Pitching_Leaders$Name, pattern = "\\s")][c(1:10)]
```

    ##  [1] "Fernando Abad"   "Cory Abbott"     "Albert Abreu"    "Bryan Abreu"    
    ##  [5] "Domingo Acevedo" "Jason Adam"      "Austin Adams"    "Joan Adon"      
    ##  [9] "Miguel Aguilar"  "Keegan Akin"

Import the `FG` `Pitcher Leaderboards`.

``` r
  ## FanGraphs pitcher leaderboards downloads
files = list.files(path = "C:/Users/Erik/Desktop/BoxCopy/Strato/2022/", pattern = "FG Pitchers min 30 IP")
for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = paste("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/",files[i], sep = "")))
  )
}
Pitchers_2016 = `2016 FG Pitchers min 30 IP`
Pitchers_2017 = `2017 FG Pitchers min 30 IP`
Pitchers_2018 = `2018 FG Pitchers min 30 IP`
Pitchers_2019 = `2019 FG Pitchers min 30 IP`
Pitchers_2020 = `2020 FG Pitchers min 30 IP`
Pitchers_2021 = `2021 FG Pitchers min 30 IP`
rm(`2016 FG Pitchers min 30 IP`,
   `2017 FG Pitchers min 30 IP`,
   `2018 FG Pitchers min 30 IP`,
   `2019 FG Pitchers min 30 IP`,
   `2020 FG Pitchers min 30 IP`,
   `2021 FG Pitchers min 30 IP`,
   `2021 FG Pitchers min 30 IP Split teams`)
  ## Re-name all the other FG df "Name" columns;
colnames(Pitchers_2016)[1] = "Name"
colnames(Pitchers_2017)[1] = "Name"
colnames(Pitchers_2018)[1] = "Name"
colnames(Pitchers_2019)[1] = "Name"
colnames(Pitchers_2020)[1] = "Name"
colnames(Pitchers_2021)[1] = "Name"

  ## Re-name all the other FG df "Name" columns; these are all dfs of stats from 2021
colnames(Hitters_total)[1] = "Name"
colnames(Hitters_split)[1] = "Name"
colnames(Hitters_adv)[1] = "Name"
colnames(Pitchers_total)[1] = "Name"
colnames(Pitchers_split)[1] = "Name"
colnames(Pitchers_adv_2021)[1] = "Name"
colnames(Catchers_total)[1] = "Name"

  ## Create a function that enables to pipe an object's strings through another object's strings
"%notin%" = Negate("%in%")
```

## Establish “Universes”

The “`CVSL Universe`” consists of the following teams:

``` r
CVSL_universe = c("LAA", "OAK", "TEX", "SFG", "COL",
                  "LAD", "SDP", "ARI", "MIN", "CLE",
                  "STL", "NYY", "BOS", "TOR", "TBR",
                  "NYM", "PHI", "ATL", "MIA", "WSN")
```

Process the `FG` `Hitters_total` df to include **only hitters with &gt;=
100 ABs**

``` r
  ## Remove hitters with < 100 ABs
Hitters_total = Hitters_total[ which(Hitters_total$AB >= 100), ]
rownames(Hitters_total) = NULL
  ## Confirm pitchers are not included in the df (should only be Shohei Ohtani)
Hitters_total$Name[ which(Hitters_total$playerid %in% Pitchers_total$playerid) ]
```

    ## [1] "Shohei Ohtani"

Find which hitters changed teams.

``` r
  ## Find which hitters changed teams
dashed_hitters = Hitters_total$Name[ which( Hitters_total$Team == "- - -") ]
  ## Iterate through to subset the split df
    ## by those that have been traded and accrued >= 100 ABs
dummy = vector()
for ( i in 1:length(dashed_hitters) ){
  dummy[i] = list(which(Hitters_split$Name == dashed_hitters[i]))
}
Traded_hitters = Hitters_split[ unlist(dummy), ]
rm(dummy)
#Traded_hitters
dashed_hitters
```

    ##  [1] "Trea Turner"      "Frank Schwindel"  "Starling Marte"   "Adam Frazier"    
    ##  [5] "Josh Harrison"    "Myles Straw"      "Jose Iglesias"    "Corey Dickerson" 
    ##  [9] "Kyle Schwarber"   "Nelson Cruz"      "Kris Bryant"      "Javier Baez"     
    ## [13] "Willy Adames"     "Eddie Rosario"    "Eduardo Escobar"  "Yan Gomes"       
    ## [17] "Anthony Rizzo"    "Michael Chavis"   "Jorge Mateo"      "Ben Gamel"       
    ## [21] "Rowdy Tellez"     "Freddy Galvis"    "Abraham Toro"     "Joc Pederson"    
    ## [25] "Albert Pujols"    "Lane Thomas"      "John Nogowski"    "Kelvin Gutierrez"
    ## [29] "Cesar Hernandez"  "Asdrubal Cabrera" "Adam Duvall"      "Jorge Soler"     
    ## [33] "Jack Mayfield"    "Yoshi Tsutsugo"   "Jake Marisnick"   "Jake Bauers"     
    ## [37] "Joe Panik"        "Jarrod Dyson"     "Khris Davis"      "Wilson Ramos"    
    ## [41] "Jordan Luplow"    "Adam Eaton"       "Travis Shaw"      "Marwin Gonzalez" 
    ## [45] "Joey Gallo"       "DJ Peters"        "Hoy Park"         "Stephen Vogt"    
    ## [49] "Jake Lamb"        "Billy McKinney"   "Mike Tauchman"    "Tim Locastro"    
    ## [53] "Rio Ruiz"         "Ka'ai Tom"        "Alex Jackson"

Manually curate a list of the teams to which these hitters went.  
Convert the dashes in the `Hitters_total` df to these team names.

``` r
dashed_hitters_to = c("LAD", "CHC", "OAK", "SDP", "OAK",
                      "CLE", "BOS", "TOR", "BOS", "TBR",
                      "SFG", "NYM", "MIL", "ATL", "MIL",
                      "OAK", "NYY", "PIT", "BAL", "PIT",
                      "MIL", "PHI", "SEA", "ATL", "LAD",
                      "WSN", "PIT", "BAL", "CHW", "CIN",
                      "ATL", "ATL", "LAA", "PIT", "SDP",
                      "SEA", "MIA", "TOR", "OAK", "CLE",
                      "TBR", "LAA", "BOS", "HOU", "NYY",
                      "TEX", "PIT", "ATL", "TOR", "LAD",
                      "SFG", "NYY", "COL", "PIT", "MIA")

dummy = vector()

for (i in dashed_hitters){
    dummy[i] = which(Hitters_total$Name == i)
}
for (i in 1:length(dashed_hitters_to)){
    Hitters_total$Team[dummy[i]] = dashed_hitters_to[i]
}
rm(dummy)
```

Slice eligible catchers.

``` r
  ## Slice eligible catchers ( >= 100 ABs)
Catchers_total = Catchers_total[ which(Catchers_total$AB >= 100), ]
  ## Concatenate eligible catchers into a list
eligible_Catchers = Catchers_total$Name
```

Concatenate and slice all eligible hitters/catchers.

``` r
  ## Concatenate eligible hitters ( >= 175 ABs, or catchers with >= 100 ABs) into a list
eligible_Hitters = Hitters_total$Name[ which(Hitters_total$AB >= 175 | Hitters_total$Name %in% eligible_Catchers) ]

  ## Slice all eligible hitters
Hitters_total = Hitters_total[ which(Hitters_total$Name %in% eligible_Hitters) , ]
```

Find which pitchers changed teams.

``` r
  ## Find which pitchers changed teams
dashed_pitchers = Pitchers_total$Name[ which( Pitchers_total$Team == "- - -") ]

  ## Iterate through to subset the split data frame by those that have been traded and accrued >= 30 IP
dummy = vector()
for ( i in 1:length(dashed_pitchers) ){
  dummy[i] = list(which(Pitchers_split$Name == dashed_pitchers[i]))
}
Traded_pitchers = Pitchers_split[ unlist(dummy), ]
rm(dummy)
#Traded_pitchers
dashed_pitchers
```

    ##  [1] "Max Scherzer"        "Jose Berrios"        "Kyle Gibson"        
    ##  [4] "Craig Kimbrel"       "Tyler Anderson"      "Rich Hill"          
    ##  [7] "Ryan Tepera"         "Drew Rasmussen"      "Andrew Chafin"      
    ## [10] "Trevor Williams"     "Adam Cimber"         "Matt Wisler"        
    ## [13] "Kendall Graveman"    "Clay Holmes"         "Daniel Hudson"      
    ## [16] "Andrew Heaney"       "Luis Cessa"          "Tony Watson"        
    ## [19] "Richard Rodriguez"   "Codi Heuer"          "Diego Castillo"     
    ## [22] "Phil Maton"          "Joely Rodriguez"     "Noe Ramirez"        
    ## [25] "J.A. Happ"           "John Gant"           "Domingo Tapia"      
    ## [28] "Phil Bickford"       "Rafael Montero"      "Yimi Garcia"        
    ## [31] "Joel Payamps"        "Sean Doolittle"      "Joe Smith"          
    ## [34] "Hunter Strickland"   "Spencer Howard"      "Jose Quintana"      
    ## [37] "JT Chargois"         "Heath Hembree"       "Trevor Richards"    
    ## [40] "J.P. Feyereisen"     "John Curtiss"        "J.B. Wendelken"     
    ## [43] "Wandy Peralta"       "Tyler Chatwood"      "Mychal Givens"      
    ## [46] "Hansel Robles"       "Joakim Soria"        "Bryse Wilson"       
    ## [49] "Matt Andriese"       "Dennis Santana"      "Ian Kennedy"        
    ## [52] "Jon Lester"          "Wade LeBlanc"        "Josiah Gray"        
    ## [55] "Daniel Norris"       "Ashton Goudeau"      "Brad Hand"          
    ## [58] "Enyel De Los Santos" "Ralph Garza"         "Anthony Banda"      
    ## [61] "Jesus Luzardo"       "Justin Wilson"       "Ross Detwiler"      
    ## [64] "Vince Velasquez"     "Shawn Armstrong"     "Brett de Geus"      
    ## [67] "Jake Arrieta"        "Alex Young"

Manually curate a list of the teams to which these pitchers went.  
Convert the dashes in the `Pitchers_total` df to these team names.

``` r
dashed_pitchers_to = c("LAD", "TOR", "PHI", "CHW", "SEA",
                       "NYM", "CHW", "TBR", "OAK", "NYM",
                       "TOR", "TBR", "HOU", "NYY", "SDP",
                       "NYY", "CIN", "SFG", "ATL", "CHC",
                       "SEA", "HOU", "NYY", "ARI", "STL",
                       "MIN", "KCR", "LAD", "HOU", "HOU",
                       "KCR", "SEA", "SEA", "MIL", "TEX",
                       "SFG", "TBR", "NYM", "TOR", "TBR",
                       "MIL", "ARI", "NYY", "SFG", "CIN",
                       "BOS", "TOR", "PIT", "SEA", "TEX",
                       "PHI", "STL", "STL", "WSN", "MIL",
                       "COL", "NYM", "PIT", "MIN", "PIT",
                       "MIA", "CIN", "SDP", "SDP", "TBR",
                       "ARI", "SDP", "CLE")
dummy = vector()

for (i in dashed_pitchers){
  dummy[i] = which(Pitchers_total$Name == i)
}
for (i in 1:length(dashed_pitchers_to)){
  Pitchers_total$Team[dummy[i]] = dashed_pitchers_to[i]
}
rm(dummy)
```

Slice all eligible `P`s within the `Pitchers_total` df.  
Concatenate all eligible `SP`s from this df into a list,
`eligible_SPs`.  
Concatenate all eligible `RP`s from the same `Pitchers_total` df into a
list, `eligible_RPs`.

``` r
  ## Slice eligible pitchers
Pitchers_total = Pitchers_total[ which(Pitchers_total$IP >= 30), ]

  ## Concatenate eligible starters into a list
eligible_SPs = Pitchers_total$Name[ which(Pitchers_total$IP >= 30 & Pitchers_total$GS >= 8) ]
  ## Concatenate eligible relievers into a list
eligible_RPs = Pitchers_total$Name[ which(Pitchers_total$Name %notin% eligible_SPs == TRUE) ]
```

Filter the lists by the `CVSL_universe`.

``` r
  ## Find eligible catchers that are outside the CVSL universe
Hitters_total$Name[ which( Hitters_total$Name %in% eligible_Catchers == TRUE )[
  which(
    Hitters_total$Team[ which( Hitters_total$Name %in% eligible_Catchers == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]
```

    ##  [1] "Tyler Stephenson"  "Salvador Perez"    "Omar Narvaez"     
    ##  [4] "Cam Gallagher"     "Pedro Severino"    "Tucker Barnhart"  
    ##  [7] "Jacob Stallings"   "Luis Torrens"      "Yasmani Grandal"  
    ## [10] "Jake Rogers"       "Willson Contreras" "Jason Castro"     
    ## [13] "Eric Haase"        "Zack Collins"      "Tom Murphy"       
    ## [16] "Manny Pina"        "Austin Wynns"      "Cal Raleigh"      
    ## [19] "Martin Maldonado"  "Michael Perez"

``` r
eligible_Catchers_out = Hitters_total$Name[ which( Hitters_total$Name %in% eligible_Catchers == TRUE )[
  which(
    Hitters_total$Team[ which( Hitters_total$Name %in% eligible_Catchers == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]

  ## Find eligible hitters that are outside the CVSL universe
Hitters_total$Name[ which( Hitters_total$Name %in% eligible_Hitters == TRUE )[
  which(
    Hitters_total$Team[ which( Hitters_total$Name %in% eligible_Hitters == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
][c(1:15)]
```

    ##  [1] "Luis Robert"       "Frank Schwindel"   "Yuli Gurriel"     
    ##  [4] "Michael Brantley"  "Tim Anderson"      "Nick Castellanos" 
    ##  [7] "Nick Madrigal"     "Jesse Winker"      "Bryan Reynolds"   
    ## [10] "Nicky Lopez"       "Kyle Tucker"       "Ty France"        
    ## [13] "Cedric Mullins II" "Rafael Ortega"     "Matt Duffy"

``` r
eligible_Hitters_out = Hitters_total$Name[ which( Hitters_total$Name %in% eligible_Hitters == TRUE )[
  which(
    Hitters_total$Team[ which( Hitters_total$Name %in% eligible_Hitters == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]

  ## Find eligible starting pitchers that are outside the CVSL universe
Pitchers_total$Name[ which( Pitchers_total$Name %in% eligible_SPs == TRUE )[
  which(
    Pitchers_total$Team[ which( Pitchers_total$Name %in% eligible_SPs == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]
```

    ##  [1] "Corbin Burnes"       "Carlos Rodon"        "Brandon Woodruff"   
    ##  [4] "Dylan Cease"         "Lance Lynn"          "Lucas Giolito"      
    ##  [7] "Freddy Peralta"      "Tyler Mahle"         "Luis Castillo"      
    ## [10] "Lance McCullers Jr." "Luis Garcia"         "Chris Flexen"       
    ## [13] "Wade Miley"          "John Means"          "Sonny Gray"         
    ## [16] "Mike Minor"          "Logan Gilbert"       "Tyler Anderson"     
    ## [19] "Brady Singer"        "Framber Valdez"      "Matt Harvey"        
    ## [22] "Jose Urquidy"        "Eric Lauer"          "Danny Duffy"        
    ## [25] "Spencer Turnbull"    "Adrian Houser"       "Matthew Boyd"       
    ## [28] "Casey Mize"          "Zack Greinke"        "Kyle Hendricks"     
    ## [31] "Yusei Kikuchi"       "Mitch Keller"        "Jake Odorizzi"      
    ## [34] "Brad Keller"         "Alec Mills"          "Tyler Alexander"    
    ## [37] "Adbert Alzolay"      "Carlos Hernandez"    "Brett Anderson"     
    ## [40] "Matt Manning"        "Jorge Lopez"         "Tarik Skubal"       
    ## [43] "Wily Peralta"        "Reynaldo Lopez"      "Cristian Javier"    
    ## [46] "Keegan Akin"         "Vladimir Gutierrez"  "Dallas Keuchel"     
    ## [49] "Marco Gonzales"      "Daniel Lynch"        "Trevor Cahill"      
    ## [52] "Justin Dunn"         "Max Kranick"         "Bruce Zimmermann"   
    ## [55] "Jose Urena"          "JT Brubaker"         "Kris Bubic"         
    ## [58] "Jeff Hoffman"        "Bryse Wilson"        "Zach Davies"        
    ## [61] "Spenser Watkins"     "Alexander Wells"     "Justin Steele"      
    ## [64] "Chad Kuhl"           "Jackson Kowar"       "Wil Crowe"          
    ## [67] "Chase De Jong"       "Justus Sheffield"    "Dean Kremer"

``` r
eligible_SPs_out = Pitchers_total$Name[ which( Pitchers_total$Name %in% eligible_SPs == TRUE )[
  which(
    Pitchers_total$Team[ which( Pitchers_total$Name %in% eligible_SPs == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]

  ## Find eligible relievers that are outside the CVSL universe
Pitchers_total$Name[ which( Pitchers_total$Name %in% eligible_RPs == TRUE )[
  which(
    Pitchers_total$Team[ which( Pitchers_total$Name %in% eligible_RPs == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
][c(1:15)]
```

    ##  [1] "Liam Hendriks"     "Josh Hader"        "Ryan Pressly"     
    ##  [4] "Craig Kimbrel"     "Scott Barlow"      "Michael Kopech"   
    ##  [7] "Ryan Tepera"       "Michael Fulmer"    "Cole Sulser"      
    ## [10] "Paul Sewald"       "Devin Williams"    "Garrett Crochet"  
    ## [13] "Drew Steckenrider" "Lucas Sims"        "Aaron Bummer"

``` r
eligible_RPs_out = Pitchers_total$Name[ which( Pitchers_total$Name %in% eligible_RPs == TRUE )[
  which(
    Pitchers_total$Team[ which( Pitchers_total$Name %in% eligible_RPs == TRUE ) ] %notin% CVSL_universe == TRUE
  )
]
]

eligible_Ps = c(eligible_SPs, eligible_RPs)
```
