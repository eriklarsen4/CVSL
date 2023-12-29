

##### CVSL Rosters #####
 ## Upload the environment from the "Universes Script"
load("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/CVSL2022UniverseEnviro.RData")

## Create a function that will clean/prep the current roster dataframes
clean_current_df_fn = function(df) {
  ## Remove gap columns
  df = df[,-c(2:4)]
  ## Remove the "Pitchers" row
  df = df[ -which(df[,1] == "Pitchers"), ]
  ## Re-order the indeces
  rownames(df) = NULL
  ## Change the player names to first name and last name
  df[,1] = gsub(df$Position.Players, pattern = "(^.*),\\s(.*$)", replacement ="\\2 \\1")  
  ## Re-name columns
  colnames(df)[c(1:4)] = c("Name", "On 40-man?", "ArbYears\nLeft", "NTC")
  ## Replace NAs in Arb col
  df$`On 40-man?`[ which( is.na( df$`On 40-man?` ) == TRUE) ] = "N"
  ## Replace Arb col vals with strings
  df$`On 40-man?`[ which( df$`On 40-man?` == 1) ] = "Y"
  ## Remove dollar signs and co-erce salaries as integers for math ops
  for ( i in 5:ncol(df) ){
    df[,i] = gsub(df[,i], pattern = "\\$", replacement ="")
    df[,i] = as.integer(df[,i])
  }
  return(df)
}

## Import current rosters
DODGERS = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Dodgers%20Roster%20for%20import.csv")
BISON = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Bison%20Roster%20for%20import.csv")
CATTLEMEN = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Cattlemen%20Roster%20for%20import.csv")
ARSENAL = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Arsenal%20Roster%20for%20import.csv")
WOMBATS = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Wombats%20Roster%20for%20import.csv")
PHILLIES = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Phillies%20Roster%20for%20import.csv")
NUGGETS = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Nuggets%20Roster%20for%20import.csv")
DUCKS = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Ducks%20Roster%20for%20import.csv")
MATADORS = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Matadors%20Roster%20for%20import.csv")
RENEGADES = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Renegades%20Roster%20for%20import.csv")

## Process current rosters
DODGERS = clean_current_df_fn(df = DODGERS)
BISON = clean_current_df_fn(df = BISON)
CATTLEMEN = clean_current_df_fn(df = CATTLEMEN)
ARSENAL = clean_current_df_fn(df = ARSENAL)
WOMBATS = clean_current_df_fn(df = WOMBATS)
PHILLIES = clean_current_df_fn(df = PHILLIES)
NUGGETS = clean_current_df_fn(df = NUGGETS)
DUCKS = clean_current_df_fn(df = DUCKS)
MATADORS = clean_current_df_fn(df = MATADORS)
RENEGADES = clean_current_df_fn(df = RENEGADES)

## Adjust Arsenal column names
colnames(ARSENAL)[c(5:10)] = c("X2021", "X2022", "X2023", "X2024", "X2025", "X2026")
## Same for Bison and Cattlemen
colnames(BISON)[c(5:10)] = c("X2021", "X2022", "X2023", "X2024", "X2025", "X2026")
colnames(CATTLEMEN)[c(5:10)] = c("X2021", "X2022", "X2023", "X2024", "X2025", "X2026")

## Edit the Ducks; impute the 2021 column
DUCKS = add_column(DUCKS, X2021 = 0, .before = 5)
## Impute
DUCKS[ , 5] = DUCKS[ ,6]
## Remove the 2027 column
DUCKS = DUCKS[ , -11 ]


## Remove duplicated Ramon Laureano from Phillies roster; traded to Renegades mid-season
PHILLIES = PHILLIES[ -which(PHILLIES$Name == "Ramon Laureano"), ]

##### Spelling errors #####
## Fix name spellings
## Gerrit Cole
DODGERS$Name[ which(grepl(DODGERS$Name, pattern = "Cole"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = " Cole")) ]
## Zach Eflin
DODGERS$Name[ which(grepl(DODGERS$Name, pattern = "Eflin"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Eflin"))]
## Dylan Bundy
DODGERS$Name[ which(grepl(DODGERS$Name, pattern = "Byndy"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Bundy"))]

## Matt Olson
BISON$Name[ which(grepl(BISON$Name, pattern = "Matt Olsen"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Olson"))]
## Nathaniel Lowe
BISON$Name[ which(grepl(BISON$Name, pattern = "Lowe")) ] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Lowe"))][1]
## J.B. Wendelken
BISON$Name[ which(grepl(BISON$Name, pattern = "Wend"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Wend"))][2]
## Caleb Baragar
BISON$Name[ which(grepl(BISON$Name, pattern = "Barag"))] = Pitchers_split$Name[ which(grepl(Pitchers_split$Name, pattern = "Barag"))]
## Mike Mayers
BISON$Name[ which(grepl(BISON$Name, pattern = "May"))] = Pitchers_split$Name[ which(grepl(Pitchers_split$Name, pattern = "May"))][2]
## Mike Yaz
BISON$Name[ which(grepl(BISON$Name, pattern = "Yas"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Yas"))][2]
## Christian Vazquez
BISON$Name[ which(grepl(BISON$Name, pattern = "Va"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Vaz"))]
## Joshua Fuentes
BISON$Name[ which(grepl(BISON$Name, pattern = "Fuen"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Fuen"))]
## Leody Taveras
BISON$Name[ which(grepl(BISON$Name, pattern = "Leody"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Leody"))]

## Sean Newcomb
CATTLEMEN$Name[ which(grepl(CATTLEMEN$Name, pattern = "Sean"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Newc"))]


## Randal Grichuk
ARSENAL$Name[ which(grepl(ARSENAL$Name, pattern = "Grichuk"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Grichuk"))]

## Jo Adell
WOMBATS$Name[ which(grepl(WOMBATS$Name, pattern = "Adell"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Adell"))]
## Jazz Chisholm Jr
WOMBATS$Name[ which(grepl(WOMBATS$Name, pattern = "Jazz"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Jazz"))]
## Triston McKenzie
WOMBATS$Name[ which(grepl(WOMBATS$Name, pattern = "Tris"))] = Pitchers_split$Name[ which(grepl(Pitchers_split$Name, pattern = "Tris"))]

## Chris Paddack
PHILLIES$Name[ which(grepl(PHILLIES$Name, pattern = "Pad"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Pad"))]
## Edwin Encarnacion
PHILLIES$Name[ which(grepl(PHILLIES$Name, pattern = "Edwin"))] = "Edwin Encarnacion"

## Zach Plesac
NUGGETS$Name[ which(grepl(NUGGETS$Name, pattern = "Zach"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Plesac"))]
## Cristian Pache
NUGGETS$Name[ which(grepl(NUGGETS$Name, pattern = "Pache"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Pach"))]

## Pete Alonso
DUCKS$Name[ which(grepl(DUCKS$Name, pattern = "Alonso"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Alonso"))]
## Jedd Gyorko
DUCKS$Name[ which(grepl(DUCKS$Name, pattern = "Gyorko"))] = "Jedd Gyorko"

## Carter Kieboom
MATADORS$Name[ which(grepl(MATADORS$Name, pattern = "Kieboom"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Kieboom"))]

## Adam Haseley
RENEGADES$Name[ which(grepl(RENEGADES$Name, pattern = "Hase"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Hase"))]
## Garrett Hampson
RENEGADES$Name[ which(grepl(RENEGADES$Name, pattern = "Hamp"))] = Hitters_split$Name[ which(grepl(Hitters_split$Name, pattern = "Hamp"))]


## Add team name column for future merging
DODGERS$Team = "Dodgers"
BISON$Team = "Bison"
CATTLEMEN$Team = "Cattlemen"
ARSENAL$Team = "Arsenal"
WOMBATS$Team = "Wombats"
PHILLIES$Team = "Phillies"
NUGGETS$Team = "Nuggets"
DUCKS$Team = "Ducks"
MATADORS$Team = "Matadors"
RENEGADES$Team = "Renegades"

## Add playerid column for future merging
DODGERS$playerid = 0
BISON$playerid = 0
CATTLEMEN$playerid = 0
ARSENAL$playerid = 0
WOMBATS$playerid = 0
PHILLIES$playerid = 0
NUGGETS$playerid = 0
DUCKS$playerid = 0
MATADORS$playerid = 0
RENEGADES$playerid = 0

##### Integrate Off-season moves #####

## Dodgers, 2/6 ##
  ## Arb to Nick Ahmed ($9)
  ## Arb to Chris Taylor ($8)
DODGERS = add_row(DODGERS, Name = "Brandon Belt", `On 40-man?` = "Y", NTC = "", X2021 = 14, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "Lourdes Gurriel Jr.", `On 40-man?` = "Y", NTC = "", X2021 = 8, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "German Marquez", `On 40-man?` = "Y", NTC = "", X2021 = 12, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "Nick Ahmed", `On 40-man?` = "Y", NTC = "", X2021 = 9, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "Archie Bradley", `On 40-man?` = "Y", NTC = "", X2021 = 8, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "Dylan Floro", `On 40-man?` = "Y", NTC = "", X2021 = 8, Length = 1, Team = "Dodgers")
DODGERS = add_row(DODGERS, Name = "Chris Taylor", `On 40-man?` = "Y", NTC = "", X2021 = 8, Length = 1, Team = "Dodgers")
DODGERS[c(29:32),3] = 2
DODGERS[c(26:28),3] = 1

## Bison and Cattlemen blockbuster 1/24 ##
  ##  Max Scherzer from Cattlemen to Bison
  ##  Fernando Tatis Jr from Bison to Cattlemen
CATTLEMEN = add_row(.data = CATTLEMEN, BISON[ which(BISON$Name == "Fernando Tatis Jr."), ] )
CATTLEMEN$Team[ which(CATTLEMEN$Name == "Fernando Tatis Jr.")] = "Cattlemen"
BISON = add_row(.data = BISON, CATTLEMEN[ which(CATTLEMEN$Name == "Max Scherzer"), ] )
BISON$Team[ which(BISON$Name == "Max Scherzer")] = "Bison"

CATTLEMEN = CATTLEMEN[ -which(CATTLEMEN$Name == "Max Scherzer"), ]
BISON = BISON[ -which(BISON$Name == "Fernando Tatis Jr."), ]

## Cattlemen and Nuggets trade 1/26
  ## Joey Gallo from Nuggets to Cattlemen
  ## Brandon Lowe from Cattlemen to Nuggets
CATTLEMEN = add_row(.data = CATTLEMEN, NUGGETS[ which(NUGGETS$Name == "Joey Gallo"), ] )
CATTLEMEN$Team[ which(CATTLEMEN$Name == "Joey Gallo")] = "Cattlemen"
NUGGETS = add_row(.data = NUGGETS, CATTLEMEN[ which(CATTLEMEN$Name == "Brandon Lowe"), ] )
NUGGETS$Team[ which(NUGGETS$Name == "Brandon Lowe")] = "Nuggets"

CATTLEMEN = CATTLEMEN[ -which(CATTLEMEN$Name == "Brandon Lowe"), ]
NUGGETS = NUGGETS[ -which(NUGGETS$Name == "Joey Gallo"), ]

## Ducks Arbitration 1/25
  ## Arb to Rafael Devers ($12)
  ## Arb to Chris Bassitt ($12)

DUCKS = add_row(DUCKS, Name = "Rafael Devers", `On 40-man?` = "Y", NTC = "Y", X2021 = 12, Length = 1, Team = "Ducks")
DUCKS = add_row(DUCKS, Name = "Chris Bassitt", `On 40-man?` = "Y", NTC = "Y", X2021 = 12, Length = 1, Team = "Ducks")
DUCKS[c(27,28),3] = 2


## Nuggets Arbitration 1/26 ##
  ## Arb to Mike Zunino ($8)
  ## Arb to Willy Adames ($8)
NUGGETS = add_row(NUGGETS, Name = "Mike Zunino", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Nuggets")
NUGGETS = add_row(NUGGETS, Name = "Willy Adames", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Nuggets")
NUGGETS[ c(34,35), 3] = 2

## Wombats Arbitration 1/26 ##
  ## Arb to Aaron Loup ($12)
  ## Arb to Brian Goodwin ($8)
WOMBATS = add_row(WOMBATS, Name = "Aaron Loup", `On 40-man?` = "Y", NTC = "Y", X2021 = 12, Length = 1, Team = "Wombats")
WOMBATS = add_row(WOMBATS, Name = "Brian Goodwin", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Wombats")
WOMBATS = WOMBATS[ -c(34,35), ]
WOMBATS[ c(24,25), 3] = 2

## Bison Arbitration 1/26 ##
  ## Arb to Kolten Wong ($12)
BISON = add_row(BISON, Name = "Kolten Wong", `On 40-man?` = "Y", NTC = "Y", X2021 = 12, Length = 1, Team = "Bison")
BISON[31,3] = 1

## Matadors Arbitration 1/26 ##
  ## Arb to Yasmani Grandal ($14)
  ## Arb to Kevin Gausman ($)
  ## Arb to Jake Cronenworth ($)
MATADORS = add_row(MATADORS, Name = "Yasmani Grandal", `On 40-man?` = "Y", NTC = "Y", X2021 = 14, Length = 1, Team = "Matadors")
MATADORS = add_row(MATADORS, Name = "Kevin Gausman", `On 40-man?` = "Y", NTC = "Y", X2021 = 12, Length = 1, Team = "Matadors")
MATADORS = add_row(MATADORS, Name = "Jake Cronenworth", `On 40-man?` = "Y", NTC = "Y", X2021 = 29, Length = 1, Team = "Matadors")
MATADORS[c(28:30),3] = 2

## Renegades Arbitration 1/26 ##
  ## Arb to Amed Rosario ($8)
RENEGADES = add_row(RENEGADES, Name = "Amed Rosario", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Renegades")
RENEGADES[40,3] = 2

## Phillies Arbitration 1/29
  ##  Arb to Jarlin Garcia ($8)
PHILLIES = add_row(PHILLIES, Name = "Jarlin Garcia", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Phillies")
PHILLIES[30,3] = 2

## Arsenal Arbitration 1/29
  ## Arb to Starling Marte ($23)
  ## Arb to Tommy Edman ($8)
  ## Arb to Kyle Gibson ($8)
ARSENAL = add_row(ARSENAL, Name = "Starling Marte", `On 40-man?` = "Y", NTC = "Y", X2021 = 23, Length = 1, Team = "Arsenal")
ARSENAL = add_row(ARSENAL, Name = "Tommy Edman", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Arsenal")
ARSENAL = add_row(ARSENAL, Name = "Kyle Gibson", `On 40-man?` = "Y", NTC = "Y", X2021 = 8, Length = 1, Team = "Arsenal")
ARSENAL[c(24:26),3] = 2


## Nuggets Arbitration 1/30
  ## Arb to Tanner Houck ($15)
NUGGETS = add_row(NUGGETS, Name = "Tanner Houck", `On 40-man?` = "Y", NTC = "Y", X2021 = 15, Length = 1, Team = "Nuggets")
NUGGETS[ 36, 3] = 2

## Create a function to add playerids
Add_playerid_fn = function(df) {
  dummy = vector()
  for (i in df$Name) {
    dummy[i] = which(Hitters_split$Name == i)[1]
    dummy[i] = Hitters_split$playerid[dummy[i]]
    dummy = as.numeric(dummy)
  }
  df$playerid = dummy
  return(df)
}

DODGERS = Add_playerid_fn(df = DODGERS)
BISON = Add_playerid_fn(df = BISON)
CATTLEMEN = Add_playerid_fn(df = CATTLEMEN)
ARSENAL = Add_playerid_fn(df = ARSENAL)
WOMBATS = Add_playerid_fn(df = WOMBATS)
PHILLIES = Add_playerid_fn(df = PHILLIES)
NUGGETS = Add_playerid_fn(df = NUGGETS)
DUCKS = Add_playerid_fn(df = DUCKS)
MATADORS = Add_playerid_fn(df = MATADORS)
RENEGADES = Add_playerid_fn(df = RENEGADES)


## Create a function that will clean/prep the arbitration and blind bid roster dataframes
clean_arb_and_bid_df_fn = function(df) {
  ## Remove the extra column
  df = df[,-5]
  
  ## Change the player names to first name and last name
  df[,1] = gsub(df$Player, pattern = "(^.*),\\s(.*$)", replacement ="\\2 \\1")
  ## Remove dollar signs and co-erce salaries as integers for math ops
  for ( i in 4:ncol(df) ){
    df[,i] = gsub(df[,i], pattern = "\\$", replacement ="")
    df[,i] = as.integer(df[,i])
  }
  colnames(df)[1] = "Name"
  return(df)
}

## Import arb and bid rosters
DODGERS_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Dodgers%20Arb%20and%20Bid.csv")
BISON_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Bison%20Arb%20and%20Bid.csv")
CATTLEMEN_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Cattlemen%20Arb%20and%20Bid.csv")
ARSENAL_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Arsenal%20Arb%20and%20Bid.csv")
WOMBATS_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Wombats%20Arb%20and%20Bid.csv")
PHILLIES_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Phillies%20Arb%20and%20Bid.csv")
NUGGETS_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Nuggets%20Arb%20and%20Bid.csv")
DUCKS_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Ducks%20Arb%20and%20Bid.csv")
MATADORS_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Matadors%20Arb%20and%20Bid.csv")
RENEGADES_off = read.csv("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/OffSeason%20Rosters/2022%20Renegades%20Arb%20and%20Bid.csv")

## Process arb/bid rosters and overwrite the dataframe
DODGERS_off = clean_arb_and_bid_df_fn(df = DODGERS_off)
BISON_off = clean_arb_and_bid_df_fn(df = BISON_off)
CATTLEMEN_off = clean_arb_and_bid_df_fn(df = CATTLEMEN_off)
ARSENAL_off = clean_arb_and_bid_df_fn(df = ARSENAL_off)
WOMBATS_off = clean_arb_and_bid_df_fn(df = WOMBATS_off)
PHILLIES_off = clean_arb_and_bid_df_fn(df = PHILLIES_off)
NUGGETS_off = clean_arb_and_bid_df_fn(df = NUGGETS_off)
DUCKS_off = clean_arb_and_bid_df_fn(df = DUCKS_off)
MATADORS_off = clean_arb_and_bid_df_fn(df = MATADORS_off)
RENEGADES_off = clean_arb_and_bid_df_fn(df = RENEGADES_off)

## Fix DUCKS_off, NUGGETS_off colnames
colnames(DUCKS_off)[4] = "Contract"
colnames(NUGGETS_off)[4] = "Contract"

## Add team name column for future merging
DODGERS_off$Team = "Dodgers"
BISON_off$Team = "Bison"
CATTLEMEN_off$Team = "Cattlemen"
ARSENAL_off$Team = "Arsenal"
WOMBATS_off$Team = "Wombats"
PHILLIES_off$Team = "Phillies"
NUGGETS_off$Team = "Nuggets"
DUCKS_off$Team = "Ducks"
MATADORS_off$Team = "Matadors"
RENEGADES_off$Team = "Renegades"

## Impute Ronald Acuna Jr. onto the Nuggets' off-season roster as type B
NUGGETS_off = add_row(NUGGETS_off, Name = "Ronald Acuna Jr.", Classification = "Type B", Previous.Salary = 6, Team = "Nuggets",
                    .after = nrow(NUGGETS_off))


  ## Incorporate Dodgers' Arb deals 1/24
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Nick Ahmed"), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Chris Taylor"), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Brandon Belt"), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Lourdes Gurriel Jr."), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "German Marquez"), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Archie Bradley"), ]
DODGERS_off = DODGERS_off[ -which(DODGERS_off$Name == "Dylan Floro"), ]


  ## Ducks arbitration 1/25 ##
    ## Arb to Rafael Devers and Chris Bassitt
DUCKS_off = DUCKS_off[ -which(DUCKS_off$Name == "Rafael Devers"), ]
DUCKS_off = DUCKS_off[ -which(DUCKS_off$Name == "Chris Bassitt"), ]

  ## Nuggets arbitration 1/26
NUGGETS_off = NUGGETS_off[ -which(NUGGETS_off$Name == "Mike Zunino"), ]
NUGGETS_off = NUGGETS_off[ -which(NUGGETS_off$Name == "Willy Adames"), ]

  ## Wombats arbitration 1/26
WOMBATS_off = WOMBATS_off[ -which(WOMBATS_off$Name == "Aaron Loup"), ]
  ## Brian Goodwin not included

  ## Matadors arbitration 1/26
MATADORS_off = MATADORS_off[ -which(MATADORS_off$Name == "Yasmani Grandal"), ]
MATADORS_off = MATADORS_off[ -which(MATADORS_off$Name == "Kevin Gausman"), ]
MATADORS_off = MATADORS_off[ -which(MATADORS_off$Name == "Jake Cronenworth"), ]

  ## Renegades Arbitration 1/26 ##
RENEGADES_off = RENEGADES_off[ -which(RENEGADES_off$Name == "Ahmed Rosario"), ]

  ## Bison Arbitration 1/26
BISON_off = BISON_off[ -which(BISON_off$Name == "Kolten Wong"), ]

  ## Phillies Arbitration 1/29
PHILLIES_off = PHILLIES_off[ -which(PHILLIES_off$Name == "Jarlin Garcia"), ]

  ## Arsenal Arbitration 1/29
ARSENAL_off = ARSENAL_off[ -which(ARSENAL_off$Name == "Starling Marte"), ]
ARSENAL_off = ARSENAL_off[ -which(ARSENAL_off$Name == "Tommy Edman"), ]
ARSENAL_off = ARSENAL_off[ -which(ARSENAL_off$Name == "Kyle Gibson"), ]

  ## Nuggets Arbitration 1/30
NUGGETS_off = NUGGETS_off[ -which(grepl(NUGGETS_off$Name, pattern = "Tanner") == TRUE), ]


  ## Wombats cuts 1/31
    ## Darren O'Day (+$10)
    ## Austin Nola (+$13)
WOMBATS = WOMBATS[ -which(WOMBATS$Name == "Darren O'Day"), ]
WOMBATS = WOMBATS[ -which(WOMBATS$Name == "Austin Nola"), ]

  ## Nuggets cuts 1/31
    ## Carlos Santana (+$2)
    ## Mike Soroka (+$1)
    ## Kyle Wright (+$0)
      ## Call up Dylan Carlson
      ## Call up Brendan Rodgers
      ## Call up Randy Arozarena
NUGGETS = NUGGETS[ -which(NUGGETS$Name == "Carlos Santana"), ]
NUGGETS = NUGGETS[ -which(NUGGETS$Name == "Mike Soroka"), ]
NUGGETS = NUGGETS[ -which(NUGGETS$Name == "Kyle Wright"), ]
NUGGETS[ which(NUGGETS$Name == "Dylan Carlson"), 2] = "Y"
NUGGETS[ which(NUGGETS$Name == "Brendan Rodgers"), 2] = "Y"
NUGGETS[ which(NUGGETS$Name == "Randy Arozarena"), 2] = "Y"

  ## Renegades cuts 2/3
    ## Sean Doolittle (+$1)
    ## Brendan McKay (+$4)
    ## Cam Hill (+$3)
    ## Adam Ottavino (+$1)
    ## Ryan Yarbrough (+$15)
    ## Jake Odorizzi (+$6)
    ## Khris Davis (+$9)
    ## Mitch Moreland (+$15)
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Sean Doolittle"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Brenden McKay"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Cam Hill"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Adam Ottavino"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Ryan Yarbrough"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Jake Odorizzi"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Khris Davis"), ]
RENEGADES = RENEGADES[ -which(RENEGADES$Name == "Mitch Moreland"), ]

length(which(RENEGADES$`On 40-man?` == "Y"))

##### Merge dfs #####
CVSL_MASTER = bind_rows(DODGERS, BISON, CATTLEMEN, ARSENAL, WOMBATS,
                        PHILLIES, NUGGETS, DUCKS, MATADORS, RENEGADES)
## Remove the 2026, 2025 columns
CVSL_MASTER = CVSL_MASTER[ , -c(9,10)]
## Re-name the year columns, team column
colnames(CVSL_MASTER)[c(5:8, 10)] = c("2021", "2022", "2023", "2024", "CVSLTeam")


##### Find who is arb eligible ####

## Fix spellings
DUCKS_off$Name[ which(grepl(DUCKS_off$Name, pattern = "Kwang"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Kwang")) ]
RENEGADES_off$Name[ which(grepl(RENEGADES_off$Name, pattern = "Curt"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Curt"))]
DODGERS_off$Name[ which(grepl(DODGERS_off$Name, pattern = "Michael Taylor"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Michael "))][2]
DUCKS_off$Name[ which(grepl(DUCKS_off$Name, pattern = "Travis"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Travis d'"))]
PHILLIES_off$Name[ which(grepl(PHILLIES_off$Name, pattern = "Ruf"))] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Ruf"))]
WOMBATS_off$Name[ which(grepl(WOMBATS_off$Name, pattern = "Loaisiga"))] = Pitchers_total$Name[ which(grepl(Pitchers_total$Name, pattern = "Loaisiga"))]


## Concatenate each team's arb eligible players into lists
Dodgers_arb_players = DODGERS_off$Name[which(DODGERS_off$Classification == "Type C")]
Bison_arb_players = BISON_off$Name[which(BISON_off$Classification == "Type C")]
Cattlemen_arb_players = CATTLEMEN_off$Name[which(CATTLEMEN_off$Classification == "Type C")]
Arsenal_arb_players = ARSENAL_off$Name[which(ARSENAL_off$Classification == "Type C")]
Wombats_arb_players = WOMBATS_off$Name[which(WOMBATS_off$Classification == "Type C")]
Phillies_arb_players = PHILLIES_off$Name[which(PHILLIES_off$Classification == "Type C")]
Nuggets_arb_players = NUGGETS_off$Name[which(NUGGETS_off$Classification == "Type C")]
Ducks_arb_players = DUCKS_off$Name[which(DUCKS_off$Classification == "Type C")]
Matadors_arb_players = MATADORS_off$Name[which(MATADORS_off$Classification == "Type C")]
Renegades_arb_players = RENEGADES_off$Name[which(RENEGADES_off$Classification == "Type C")]

## Compile a list of all arb eligible players
Arb_players = c(Dodgers_arb_players, Bison_arb_players, Cattlemen_arb_players, Arsenal_arb_players, Wombats_arb_players,
                Phillies_arb_players, Nuggets_arb_players, Ducks_arb_players, Matadors_arb_players, Renegades_arb_players)

## Find which potential arb-eligible hitters are outside the universe; if not offered arbitration, not bid nor draft-eligible
Arb_players[ which(Arb_players %in% eligible_Hitters_out == TRUE) ]
## Find which potential arb-eligible SPs are outside the universe; if not offered arbitration, not bid nor draft-eligible
Arb_players[ which(Arb_players %in% eligible_SPs_out == TRUE) ]
## Find which potential arb-eligible RPs are outside the universe; if not offered arbitration, not bid nor draft-eligible
Arb_players[ which(Arb_players %in% eligible_RPs_out == TRUE) ]



Ducks_arb_players = DUCKS_off$Name[which(DUCKS_off$Classification == "Type C")]
Renegades_arb_players = RENEGADES_off$Name[which(RENEGADES_off$Classification == "Type C")]
Nuggets_arb_players = NUGGETS_off$Name[which(NUGGETS_off$Classification == "Type C")]
Dodgers_arb_players = DODGERS_off$Name[which(DODGERS_off$Classification == "Type C")]

Arb_players = c(Dodgers_arb_players, Bison_arb_players, Cattlemen_arb_players, Arsenal_arb_players, Wombats_arb_players,
                Phillies_arb_players, Nuggets_arb_players, Ducks_arb_players, Matadors_arb_players, Renegades_arb_players)
##### Find who is bid eligible #####

## Repeat for bid only
Dodgers_bid_players = DODGERS_off$Name[which(DODGERS_off$Classification == "Type B")]
Bison_bid_players = BISON_off$Name[which(BISON_off$Classification == "Type B")]
Cattlemen_bid_players = CATTLEMEN_off$Name[which(CATTLEMEN_off$Classification == "Type B")]
Arsenal_bid_players = ARSENAL_off$Name[which(ARSENAL_off$Classification == "Type B")]
Wombats_bid_players = WOMBATS_off$Name[which(WOMBATS_off$Classification == "Type B")]
Phillies_bid_players = PHILLIES_off$Name[which(PHILLIES_off$Classification == "Type B")]
Nuggets_bid_players = NUGGETS_off$Name[which(NUGGETS_off$Classification == "Type B")]
Ducks_bid_players = DUCKS_off$Name[which(DUCKS_off$Classification == "Type B")]
Matadors_bid_players = MATADORS_off$Name[which(MATADORS_off$Classification == "Type B")]
Renegades_bid_players = RENEGADES_off$Name[which(RENEGADES_off$Classification == "Type B")]

  ## Compile a list of all bid eligible players
Bid_players = c(Dodgers_bid_players, Bison_bid_players, Cattlemen_bid_players, Arsenal_bid_players, Wombats_bid_players,
                Phillies_bid_players, Nuggets_bid_players, Ducks_bid_players, Matadors_bid_players, Renegades_bid_players)

##### Create the DRAFT ELIGIBLE HITTER DF #####
  ## Hitters
    ## Create the Draft-Eligible Hitters df
    ## First subset by which players are in the CVSL universe
DRAFT_Hs = Hitters_total[ which(Hitters_total$Team %in% CVSL_universe == TRUE), ]
  ## Subset by which are not rostered
DRAFT_Hs = DRAFT_Hs[ which(DRAFT_Hs$playerid %notin% CVSL_MASTER$playerid == TRUE), ]
  ## Add a column to insert agency type
DRAFT_Hs$Class = ""

  ## Overwrite the HITTER DRAFT UNIVERSE df to contain advanced metrics, merge their free agency status
DRAFT_Hs = DRAFT_Hs %>%
  select(Name, Age, Bats, Positions, Team, Class, AB) %>%
  inner_join(Hitters_adv, by = "Name") %>%
  arrange(desc(WAR)) %>%
  collect()
  ## Co-erce the rate columns to be numeric/decimals
DRAFT_Hs$BB. = as.numeric(gsub(DRAFT_Hs$BB., pattern = "%", replacement = ""))
DRAFT_Hs$K. = as.numeric(gsub(DRAFT_Hs$K., pattern = "%", replacement = ""))
  ## Remove a duplicate column
DRAFT_Hs = DRAFT_Hs[,-8]
  ## Re-name the Team column
colnames(DRAFT_Hs)[5] = "Team"

  ## Find players may have different spellings that may preclude their inclusion in a merged df
DRAFT_Hs[which(DRAFT_Hs$Name %in% BR_2021_Standard_Batting_Leaders$Name == FALSE), ]
  ## Re-name them in the BR df
## AJ Pollock
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Pollock") == TRUE) ] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Pollock") == TRUE) ]
## Andy Ibanez
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Andy I") == TRUE) ] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Ibanez") == TRUE) ]
## Yadiel Hernandez
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Yadiel") == TRUE) ] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Yadiel") == TRUE) ]
## Jose Iglesias
#BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Jos Iglesias") == TRUE) ][1] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Iglesias") == TRUE) ]
## LaMonte Wade Jr
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Wade") == TRUE) ][1] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Wade") == TRUE) ]
## Adolis Garcia
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Adolis") == TRUE) ] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Adolis") == TRUE) ]
## Ha-seong Kim
BR_2021_Standard_Batting_Leaders$Name[ which(grepl(BR_2021_Standard_Batting_Leaders$Name, pattern = "Kim") == TRUE) ][1] = Hitters_total$Name[ which(grepl(Hitters_total$Name, pattern = "Kim") == TRUE) ]

  ## Re-run the data
DRAFT_Hs = Hitters_total[ which(Hitters_total$Team %in% CVSL_universe == TRUE), ]
  ## Subset by those not already rostered
DRAFT_Hs = DRAFT_Hs[ which(DRAFT_Hs$playerid %notin% CVSL_MASTER$playerid == TRUE), ]
  ## Add a column to insert agency type
DRAFT_Hs$Class = ""

BR_2021_Standard_Batting_Leaders = BR_2021_Standard_Batting_Leaders[ -which(duplicated(BR_2021_Standard_Batting_Leaders$Name) == TRUE), ]

## Add their handedness and defense columns from BR
DRAFT_Hs = BR_2021_Standard_Batting_Leaders %>%
  select(Name, Age, Bats, Positions) %>%
  inner_join(DRAFT_Hs) %>%
  collect()

DRAFT_Hs$Positions = as.character(DRAFT_Hs$Positions)

## Convert BR position code to strings
listy = vector()
for (i in 1:nrow(DRAFT_Hs)) {
  listy[i] = list(
    as.numeric(unlist(str_extract_all(
      unlist(strsplit(DRAFT_Hs$Positions[i], split = "\\D"))[
        which(unlist(strsplit(DRAFT_Hs$Positions[i], split = "\\D")) != "")
      ], pattern = "[:digit:]"))))
  for (j in 1:length(listy[[i]])){
    if (listy[[i]][j] == 1) {
      listy[[i]][j] = "P"
    }
    if (listy[[i]][j] == 2) {
      listy[[i]][j] = "C"
    }
    if (listy[[i]][j] == 3) {
      listy[[i]][j] = "1B"
    }
    if (listy[[i]][j] == 4) {
      listy[[i]][j] = "2B"
    }
    if (listy[[i]][j] == 5) {
      listy[[i]][j] = "3B"
    }
    if (listy[[i]][j] == 6) {
      listy[[i]][j] = "SS"
    }
    if (listy[[i]][j] == 7) {
      listy[[i]][j] = "LF"
    }
    if (listy[[i]][j] == 8) {
      listy[[i]][j] = "CF"
    }
    if (listy[[i]][j] == 9) {
      listy[[i]][j] = "RF"
    }
  }
  DRAFT_Hs$Positions[i] = paste(unlist(listy[[i]]), collapse = " ")
}

## Loop through the list of Type B hitters
dummy = vector()
for (i in Bid_players) {
  dummy[i] = list(which(DRAFT_Hs$Name == i))
}
print(DRAFT_Hs[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_Hs$Class[ as.integer(unlist(dummy)) ] = "Type B"

## Loop through the list of Type C hitters
dummy = vector()
for (i in Arb_players) {
  dummy[i] = list(which(DRAFT_Hs$Name == i))
}
print(DRAFT_Hs[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_Hs$Class[ as.integer(unlist(dummy)) ] = "Type C"
## Annotate the rest
DRAFT_Hs$Class[ which(DRAFT_Hs$Class == "" )] = "Type A"

  ## Add advanced metrics
DRAFT_Hs = Hitters_adv_2021 %>%
  select(Name, G, PA, HR, SB, BB., K., AVG, OBP, SLG, wRC., Off, Def, WAR,) %>%
  inner_join(DRAFT_Hs) %>%
  select(Name, Age, Team, Bats, Positions, WAR, G, AB, HR, SB, BB., K., AVG, OBP, SLG, wRC., Off, Def, Class) %>%
  collect()

##### Create the DRAFT ELIGIBLE PITCHER DF #####
  ## Subset starters by those on the appropriate teams
DRAFT_Ps = Pitchers_total[ which(Pitchers_total$Team %in% CVSL_universe == TRUE), ]
  ## Subset by those not already rostered
DRAFT_Ps = DRAFT_Ps[ which(DRAFT_Ps$playerid %notin% CVSL_MASTER$playerid == TRUE), ]

BR_2021_Standard_Pitching_Leaders = BR_2021_Standard_Pitching_Leaders[ -which(duplicated(BR_2021_Standard_Pitching_Leaders$Name) == TRUE), ]

  ## Add their handedness and defense columns from BR
DRAFT_Ps = BR_2021_Standard_Pitching_Leaders %>%
  select(Name, Age, Throws) %>%
  inner_join(DRAFT_Ps) %>%
  collect()

  ## Add advanced metrics
DRAFT_Ps = Pitchers_adv_2021 %>%
  select(Name, Team, K.9, BB.9, HR.9, WHIP, ERA, FIP, xFIP, SIERA) %>%
  inner_join(DRAFT_Ps) %>%
  select(Name, Age, Throws, Team, IP, G, GS, SV, K.9, BB.9, HR.9, WHIP, ERA, xERA, FIP, xFIP, SIERA, WAR) %>%
  arrange(WHIP) %>%
  collect()

DRAFT_Ps = add_column(DRAFT_Ps, Role = "", .after = 4)

for(i in 1:nrow(DRAFT_Ps)){
  if (DRAFT_Ps$G[i] == DRAFT_Ps$GS[i]){
    DRAFT_Ps$Role[i] = "SP"
  }
  if (DRAFT_Ps$GS[i] < DRAFT_Ps$G[i]){
    if (DRAFT_Ps$GS[i] >= 8) {
      DRAFT_Ps$Role[i] = "SWING"
    }
    if (DRAFT_Ps$GS[i] < 8) {
      DRAFT_Ps$Role[i] = "LRP"
    }
  }
  if (DRAFT_Ps$GS[i] == 0){
    DRAFT_Ps$Role[i] = "FIRE"
  }
}


  ## Add a column to insert agency type
DRAFT_Ps$Class = ""

## Loop through the list of Type B Ps
dummy = vector()
for (i in Bid_players) {
  dummy[i] = list(which(DRAFT_Ps == i))
}
print(DRAFT_Ps[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_Ps$Class[ as.integer(unlist(dummy))] = "Type B"

## Loop through the list of Type C Ps
dummy = vector()
for (i in Arb_players) {
  dummy[i] = list(which(DRAFT_Ps$Name == i))
}
print(DRAFT_Ps[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_Ps$Class[ as.integer(unlist(dummy))] = "Type C"
## Annotate the rest
DRAFT_Ps$Class[ which(DRAFT_Ps$Class == "")] = "Type A"

## Edit Jonathan Loaisiga's classification, according to Reggie
DRAFT_Ps$Class[ which(DRAFT_Ps$Name == "Jonathan Loaisiga")] = "Type A"


##### Create the Bid Universe DF
  ## Put it all into a df
BID_UNI = rbind.data.frame(DODGERS_off, BISON_off, CATTLEMEN_off, ARSENAL_off, WOMBATS_off,
                           PHILLIES_off, NUGGETS_off, DUCKS_off, MATADORS_off, RENEGADES_off)
  ## Subset the arb and bid players
BID_UNI = BID_UNI[ which(BID_UNI$Classification != "Type A"), ]
colnames(BID_UNI)[c(2,7)] = c("Class", "CVSLTeam")

  ## Add the players' MLB Teams
BID_UNI = Hitters_split %>%
  select(Name, Team) %>%
  full_join(Pitchers_total) %>%
  inner_join(BID_UNI) %>%
  select(Name, Team, Class, CVSLTeam)
  ## Convert the arb guys to bid guys
BID_UNI$Class[which(BID_UNI$Class == "Type C" & BID_UNI$CVSLTeam != "Dodgers")] = "Type B"
  ## Find the duplicates
BID_UNI$Name[ which(duplicated(BID_UNI$Name) == TRUE) ]

  ## Remove the duplicates and others outside the universe
BID_UNI = BID_UNI[ -c(1,3,38,40,48,51,55,56,78,82,83,62,90,101,102),  ]
rownames(BID_UNI) = NULL

BID_UNI = BID_UNI[ -which(BID_UNI$Team %notin% CVSL_universe), ]



  ## Combine hitter data and pitcher data
BID_UNI = DRAFT_Hs %>%
  full_join(DRAFT_Ps) %>%
  inner_join(BID_UNI) %>%
  select(Name, Age, Bats, Positions, Team, CVSLTeam, WAR, AB, wRC., Off, Def, Role, GS, IP, K.9, WHIP, ERA, xERA, FIP, xFIP, Class) %>%
  arrange(desc(WAR)) %>%
  collect()
  




## Find which potential bid-eligible hitters are outside the universe
Bid_players[ which(Bid_players %in% eligible_Hitters_out == TRUE) ]
Bid_players[ which(Bid_players %notin% eligible_Hitters == TRUE) ]

Wombats_arb_players[ which(Wombats_arb_players %in% eligible_Hitters_out == TRUE) ]
Wombats_arb_players[ which(Wombats_arb_players %in% eligible_Catchers_out == TRUE) ]
Wombats_arb_players[ which(Wombats_arb_players %in% eligible_Ps == FALSE) ]
Wombats_bid_players[ which(Wombats_bid_players %in% eligible_Hitters_out == TRUE)]
Wombats_bid_players[ which(Wombats_bid_players %in% eligible_Ps == FALSE)]

#Pitchers_split[ which(grepl(Pitchers_split$Name, pattern = "Clippard")), ]

## Find which potential bid-eligible SPs are outside the universe
Bid_players[ which(Bid_players %in% eligible_SPs_out == TRUE) ]
Bid_players[ which(Bid_players %notin% eligible_SPs == TRUE) ]

## Find which potential bid-eligible RPs are outside the universe
Bid_players[ which(Bid_players %in% eligible_RPs_out == TRUE) ]
Bid_players[ which(Bid_players %notin% eligible_RPs == TRUE) ]

Bid_players
Ineligible_Bid_players = Bid_players[c(1,9,29,30,33)]

Arb_players
Arb_players_ineligible_if_denied = Arb_players[c(17,21,23,25,38,41,45,46,48,63,64,80,82,85,88)]


#####

rownames(Hitters_adv_2018) = NULL
rownames(Hitters_adv_2019) = NULL
rownames(Hitters_adv_2020) = NULL

Hitters_adv_2018[ which(Hitters_adv_2018$Team == "- - -"),]
DRAFT_2019_Hs  = Hitters_adv_2018[ which(Hitters_adv_2018$Team %in% CVSL_universe == TRUE | Hitters_adv_2018$Team == "- - -"), ]
## Subset by those not already rostered
DRAFT_2019_Hs = DRAFT_2019_Hs[ which(DRAFT_2019_Hs$Name %notin% CVSL_all_DRAFTS_hitters$Name == TRUE), ]

## Add their handedness and defense columns from BR
DRAFT_2019_Hs = BR_2018_Standard_Batting_Leaders %>%
  select(Name, Age, Bats, Positions) %>%
  inner_join(DRAFT_2019_Hs) %>%
  collect()

DRAFT_2019_Hs$Positions = as.character(DRAFT_2019_Hs$Positions)

## Convert BR position code to strings
listy = vector()
for (i in 1:nrow(DRAFT_2019_Hs)) {
  listy[i] = list(
    as.numeric(unlist(str_extract_all(
      unlist(strsplit(DRAFT_2019_Hs$Positions[i], split = "\\D"))[
        which(unlist(strsplit(DRAFT_2019_Hs$Positions[i], split = "\\D")) != "")
      ], pattern = "[:digit:]"))))
  for (j in 1:length(listy[[i]])){
    if (listy[[i]][j] == 1) {
      listy[[i]][j] = "P"
    }
    if (listy[[i]][j] == 2) {
      listy[[i]][j] = "C"
    }
    if (listy[[i]][j] == 3) {
      listy[[i]][j] = "1B"
    }
    if (listy[[i]][j] == 4) {
      listy[[i]][j] = "2B"
    }
    if (listy[[i]][j] == 5) {
      listy[[i]][j] = "3B"
    }
    if (listy[[i]][j] == 6) {
      listy[[i]][j] = "SS"
    }
    if (listy[[i]][j] == 7) {
      listy[[i]][j] = "LF"
    }
    if (listy[[i]][j] == 8) {
      listy[[i]][j] = "CF"
    }
    if (listy[[i]][j] == 9) {
      listy[[i]][j] = "RF"
    }
  }
  DRAFT_2019_Hs$Positions[i] = paste(unlist(listy[[i]]), collapse = " ")
}

## Loop through the list of Type B hitters
dummy = vector()
for (i in Bid_players) {
  dummy[i] = list(which(DRAFT_2019_Hs$Name == i))
}
print(DRAFT_2019_Hs[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_2019_Hs$Class[ as.integer(unlist(dummy)) ] = "Type B"

## Loop through the list of Type C hitters
dummy = vector()
for (i in Arb_players) {
  dummy[i] = list(which(DRAFT_2019_Hs$Name == i))
}
print(DRAFT_2019_Hs[ as.integer(unlist(dummy)), ])
## Annotate them in the df
DRAFT_2019_Hs$Class[ as.integer(unlist(dummy)) ] = "Type C"
## Annotate the rest
DRAFT_2019_Hs$Class[ which(DRAFT_2019_Hs$Class == "" )] = "Type A"


## Subset SPs
DRAFT_SPs = DRAFT_Ps
DRAFT_SPs = subset(DRAFT_SPs, DRAFT_SPs$GS > 8)
## Remove %s from GB% to perform math ops; add an additional Inn column
DRAFT_SPs$GB. = as.numeric(gsub(DRAFT_SPs$GB., pattern = "%", replacement = ""))
DRAFT_SPs = add_column(DRAFT_SPs, innings = 0, .after = 4)
DRAFT_SPs$innings = DRAFT_SPs$IP

## Subset long relievers
DRAFT_LRPs = DRAFT_Ps
DRAFT_LRPs = subset(DRAFT_LRPs, DRAFT_LRPs$G > DRAFT_LRPs$GS & DRAFT_LRPs$GS > 1)
DRAFT_LRPs = DRAFT_LRPs[ which(DRAFT_LRPs$Name %notin% DRAFT_SPs$Name == TRUE), ]
## Remove %s from GB% to perform math ops; add an additional Inn column
DRAFT_LRPs$GB. = as.numeric(gsub(DRAFT_LRPs$GB., pattern = "%", replacement = ""))
DRAFT_LRPs = add_column(DRAFT_LRPs, innings = 0, .after = 4)
DRAFT_LRPs$innings = DRAFT_LRPs$IP

## Subset 1 INN relievers
DRAFT_FIREs = DRAFT_Ps
DRAFT_FIREs = subset(DRAFT_FIREs, DRAFT_FIREs$GS == 0)
## Remove %s from GB% to perform math ops; add an additional Inn column
DRAFT_FIREs$GB. = as.numeric(gsub(DRAFT_FIREs$GB., pattern = "%", replacement = ""))
DRAFT_FIREs = add_column(DRAFT_FIREs, innings = 0, .after = 4)
DRAFT_FIREs$innings = DRAFT_FIREs$IP

## Create a function that Z-scores and ranks the pitcher metrics
Pitcher_Z_fn = function(Pitcher_df) {
  for (i in 1:nrow(Pitcher_df)) {
    for (j in 6:17) {
      if (j == 12){
        next
      }
      Pitcher_df[i,j] = (Pitcher_df[i,j] - mean(Pitcher_df[,j])) / sd(Pitcher_df[,j])
    }
  }
  colnames(Pitcher_df)[c(5:18)] = c("IP", "ZIP", "ZK9", "ZBB9", "ZHR9", "ZBABIP", "ZGB", "vFA..pi.", "ZERA", "ZxERA", "ZFIP", "ZxFIP", "ZWAR", "Class")
  
  Pitcher_df = Pitcher_df[ order(-Pitcher_df$ZWAR, -Pitcher_df$ZK9), ]
  return(Pitcher_df)
}

Z_FIRE = Pitcher_Z_fn(Pitcher_df = DRAFT_FIREs)
Z_SP = Pitcher_Z_fn(Pitcher_df = DRAFT_SPs)
Z_LRP = Pitcher_Z_fn(Pitcher_df = DRAFT_LRPs)

## Remove the duplicate columns
DRAFT_FIREs = DRAFT_FIREs[,-5]
DRAFT_LRPs = DRAFT_FIREs[,-5]
DRAFT_SPs = DRAFT_FIREs[,-5]

## Import data
files = list.files(path =  "https://github.com/eriklarsen4/Baseball/tree/main/CVSL/FG%20and%20BR%20Data", pattern = "2021 Splits")

for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(paste("https://github.com/eriklarsen4/Baseball/tree/main/CVSL/FG%20and%20BR%20Data", file = files[i], sep = "")))
  )
}
vLHP = `2021 Splits Leaders vs LHP`
vRHP = `2021 Splits Leaders vs RHP`
vLHH = `2021 Splits Leaders vs LHH`
vRHH = `2021 Splits Leaders vs RHH`

rm(`2021 Splits Leaders vs LHP`,
   `2021 Splits Leaders vs RHP`,
   `2021 Splits Leaders vs LHH`,
   `2021 Splits Leaders vs RHH`)

## Create a dataframe of advanced metrics for all rostered CVSL hitters
CVSL_Hs = Hitters_total[ which(Hitters_total$playerid %in% unique(CVSL_MASTER$playerid) == TRUE), ]

## Add salaries
CVSL_Hs = CVSL_MASTER %>%
  select(Name, `2021`, CVSLTeam) %>%
  inner_join(CVSL_Hs) %>%
  collect()
## Re-name the column
colnames(CVSL_Hs)[2] = "Salary"

CVSL_Hs = CVSL_Hs %>%
  select(Name, Salary, CVSLTeam, Team) %>%
  inner_join(BR_2021_Standard_Batting_Leaders) %>%
  collect()

## Remove Will Smith, the pitcher
CVSL_Hs = CVSL_Hs[ -17,]
rownames(CVSL_Hs) = NULL

listy = vector()
for (i in 1:nrow(CVSL_Hs)) {
  listy[i] = list(
    as.numeric(unlist(str_extract_all(
      unlist(strsplit(CVSL_Hs$Positions[i], split = "\\D"))[
        which(unlist(strsplit(CVSL_Hs$Positions[i], split = "\\D")) != "")
      ], pattern = "[:digit:]"))))
  for (j in 1:length(listy[[i]])){
    if (listy[[i]][j] == 1) {
      listy[[i]][j] = "P"
    }
    if (listy[[i]][j] == 2) {
      listy[[i]][j] = "C"
    }
    if (listy[[i]][j] == 3) {
      listy[[i]][j] = "1B"
    }
    if (listy[[i]][j] == 4) {
      listy[[i]][j] = "2B"
    }
    if (listy[[i]][j] == 5) {
      listy[[i]][j] = "3B"
    }
    if (listy[[i]][j] == 6) {
      listy[[i]][j] = "SS"
    }
    if (listy[[i]][j] == 7) {
      listy[[i]][j] = "LF"
    }
    if (listy[[i]][j] == 8) {
      listy[[i]][j] = "CF"
    }
    if (listy[[i]][j] == 9) {
      listy[[i]][j] = "RF"
    }
  }
  CVSL_Hs$Positions[i] = paste(unlist(listy[[i]]), collapse = " ")
}

CVSL_Hs = CVSL_Hs %>%
  select(Name, Age, Bats, Positions, Salary, CVSLTeam, Team) %>%
  inner_join(Hitters_adv_2021) %>%
  arrange(desc(WAR)) %>%
  collect()

## Co-erce %s to be numeric/decimals
CVSL_Hs$BB. = as.numeric(gsub(CVSL_Hs$BB., pattern = "%", replacement = ""))
CVSL_Hs$K. = as.numeric(gsub(CVSL_Hs$K., pattern = "%", replacement = ""))

CVSL_Hs[ which(CVSL_Hs$Salary == max(CVSL_Hs$Salary)), ]





CVSL_Ps = Pitchers_total[ which(Pitchers_total$playerid %in% CVSL_MASTER$playerid == TRUE), ]
CVSL_Ps$Name[ which(CVSL_Ps$Name == "Craig Kimbrel")]

## Add their handedness and defense columns from BR
CVSL_Ps = BR_2021_Standard_Pitching_Leaders %>%
  select(Name, Age, Throws) %>%
  inner_join(CVSL_Ps) %>%
  collect()

## Add advanced metrics
CVSL_Ps = Pitchers_adv_2021 %>%
  select(Name, K.9, BB.9, HR.9, WHIP, ERA, FIP, xFIP, SIERA) %>%
  inner_join(CVSL_Ps) %>%
  select(Name, Age, Throws, Team, IP, G, GS, SV, K.9, BB.9, HR.9, WHIP, ERA, xERA, FIP, xFIP, SIERA, WAR) %>%
  arrange(WHIP) %>%
  collect()

CVSL_Ps = add_column(CVSL_Ps, Role = "", .after = 4)

for(i in 1:nrow(CVSL_Ps)){
  if (CVSL_Ps$G[i] == CVSL_Ps$GS[i]){
    CVSL_Ps$Role[i] = "SP"
  }
  if (CVSL_Ps$GS[i] < CVSL_Ps$G[i]){
    if (CVSL_Ps$GS[i] >= 8) {
      CVSL_Ps$Role[i] = "SWING"
    }
    if (CVSL_Ps$GS[i] < 8) {
      CVSL_Ps$Role[i] = "LRP"
    }
  }
  if (CVSL_Ps$GS[i] == 0){
    CVSL_Ps$Role[i] = "FIRE"
  }
}

vRHP$Name[which(grepl(vRHP$Name, pattern = "Pollock"))] = DRAFT_Hs$Name[which(grepl(DRAFT_Hs$Name, pattern = "Pollock") == TRUE)]
vLHP$Name[which(grepl(vLHP$Name, pattern = "Pollock"))] = DRAFT_Hs$Name[which(grepl(DRAFT_Hs$Name, pattern = "Pollock") == TRUE)]


  ## Add salaries and CVSL team
CVSL_Ps = CVSL_MASTER %>%
  select(Name, `2021`, CVSLTeam) %>%
  inner_join(CVSL_Ps) %>%
  arrange(WHIP) %>%
  collect()
colnames(CVSL_Ps)[2] = "Salary"

colnames(CVSL_Hs)[21] = "wRCp"
CVSL_LHP_LEADERS = CVSL_Hs %>% 
  select(Name, Age, Bats, Positions, CVSLTeam, wRCp, WAR) %>%
  inner_join(vLHP) %>%
  arrange(desc(wRC.)) %>%
  collect()
CVSL_LHP_LEADERS = CVSL_LHP_LEADERS[, -8]
CVSL_LHP_LEADERS[,c(9,10)] = CVSL_LHP_LEADERS[,c(9,10)]*100
CVSL_LHP_LEADERS[,c(9,10)] = round(CVSL_LHP_LEADERS[,c(9,10)],digits=1)
CVSL_LHP_LEADERS[,c(12:17,20,21)] = round(CVSL_LHP_LEADERS[,c(12:17,20,21)],digits=3)
colnames(CVSL_LHP_LEADERS)[c(9,10)] = c("BB%", "K%")
CVSL_LHP_LEADERS = CVSL_LHP_LEADERS[, -c(17:20)]



CVSL_RHP_LEADERS = CVSL_Hs %>%
  select(Name, Age, Bats, Positions, CVSLTeam, wRCp, WAR) %>%
  inner_join(vRHP) %>%
  arrange(desc(wRC.)) %>%
  collect()
CVSL_RHP_LEADERS = CVSL_RHP_LEADERS[, -8]
CVSL_RHP_LEADERS[,c(9,10)] = CVSL_RHP_LEADERS[,c(9,10)]*100
CVSL_RHP_LEADERS[,c(9,10)] = round(CVSL_RHP_LEADERS[,c(9,10)],digits=1)
CVSL_RHP_LEADERS[,c(12:17,20,21)] = round(CVSL_RHP_LEADERS[,c(12:17,20,21)],digits=3)
colnames(CVSL_RHP_LEADERS)[c(9,10)] = c("BB%", "K%")
CVSL_RHP_LEADERS = CVSL_RHP_LEADERS[, -c(17:20)]


CVSL_LHH_LEADERS = CVSL_Ps %>%
  select(Name, CVSLTeam, Throws, WAR) %>%
  left_join(vLHH) %>%
  arrange(WHIP) %>%
  collect()
CVSL_LHH_LEADERS = CVSL_LHH_LEADERS[,-c(5,7)]
CVSL_LHH_LEADERS[,c(10,11,16)] = CVSL_LHH_LEADERS[,c(10,11,16)]*100
CVSL_LHH_LEADERS[,c(6:8,10,11,16)] = round(CVSL_LHH_LEADERS[,c(6:8,10,11,16)], digits = 1)
CVSL_LHH_LEADERS[,c(9,17,18)] = round(CVSL_LHH_LEADERS[,c(9,17,18)], digits = 2)
CVSL_LHH_LEADERS[,c(13,14)] = round(CVSL_LHH_LEADERS[,c(13,14)], digits = 3)
colnames(CVSL_LHH_LEADERS)[c(6:12,16)] = c("K/9", "BB/9", "K/BB", "HR/9", "K%", "BB%", "K/BB%", "LOB%")

CVSL_RHH_LEADERS = CVSL_Ps %>%
  select(Name, CVSLTeam, Throws, WAR) %>%
  left_join(vRHH) %>%
  arrange(WHIP) %>%
  collect()
CVSL_RHH_LEADERS = CVSL_RHH_LEADERS[,-c(5,7)]
CVSL_RHH_LEADERS[,c(10,11,16)] = CVSL_RHH_LEADERS[,c(10,11,16)]*100
CVSL_RHH_LEADERS[,c(6:8,10,11,16)] = round(CVSL_RHH_LEADERS[,c(6:8,10,11,16)], digits = 1)
CVSL_RHH_LEADERS[,c(9,17,18)] = round(CVSL_RHH_LEADERS[,c(9,17,18)], digits = 2)
CVSL_RHH_LEADERS[,c(13,14)] = round(CVSL_RHH_LEADERS[,c(13,14)], digits = 3)
colnames(CVSL_RHH_LEADERS)[c(6:12,16)] = c("K/9", "BB/9", "K/BB", "HR/9", "K%", "BB%", "K/BB%", "LOB%")

DRAFT_LHP_LEADERS = DRAFT_Hs %>%
  select(Name, Age, Bats, Positions, Class, Team, Def, WAR) %>%
  inner_join(vLHP) %>%
  arrange(desc(wRC.)) %>%
  collect()
DRAFT_LHP_LEADERS = DRAFT_LHP_LEADERS[ , -c(9,13,19,21)]
DRAFT_LHP_LEADERS$Class[ which(DRAFT_LHP_LEADERS$Name == "Ronald Acuna Jr.") ] = "Type B"
DRAFT_LHP_LEADERS[,c(10,11)] = DRAFT_LHP_LEADERS[,c(10,11)]*100
DRAFT_LHP_LEADERS[,c(10,11)] = round(DRAFT_LHP_LEADERS[,c(10,11)],digits = 2)
colnames(DRAFT_LHP_LEADERS)[c(10,11)] = c("BB%", "K%")
DRAFT_LHP_LEADERS[,c(12:18)] = round(DRAFT_LHP_LEADERS[,c(12:18)],digits = 3)
DRAFT_LHP_LEADERS[,19] = round(DRAFT_LHP_LEADERS[,19],digits = 1)


DRAFT_RHP_LEADERS = DRAFT_Hs %>%
  select(Name, Age, Bats, Positions, Class, Team, Def, WAR) %>%
  left_join(vRHP) %>%
  arrange(desc(wRC.)) %>%
  collect()
DRAFT_RHP_LEADERS = DRAFT_RHP_LEADERS[ , -c(9,13,19,21)]
DRAFT_RHP_LEADERS$Class[ which(DRAFT_RHP_LEADERS$Name == "Ronald Acuna Jr.") ] = "Type B"
DRAFT_RHP_LEADERS[,c(10,11)] = DRAFT_RHP_LEADERS[,c(10,11)]*100
DRAFT_RHP_LEADERS[,c(10,11)] = round(DRAFT_RHP_LEADERS[,c(10,11)],digits = 2)
colnames(DRAFT_RHP_LEADERS)[c(10,11)] = c("BB%", "K%")
DRAFT_RHP_LEADERS[,c(12:18)] = round(DRAFT_RHP_LEADERS[,c(12:18)],digits = 3)
DRAFT_RHP_LEADERS[,19] = round(DRAFT_RHP_LEADERS[,19],digits = 1)

DRAFT_LHH_LEADERS = DRAFT_Ps %>%
  select(Name, Throws, Team, WAR, Class, G, GS) %>%
  left_join(vLHH) %>%
  arrange(WHIP) %>%
  collect()
DRAFT_LHH_LEADERS[,c(15:17,21)] = DRAFT_LHH_LEADERS[,c(15:17,21)]*100
DRAFT_LHH_LEADERS[,c(14:20,22,23)] = round(DRAFT_LHH_LEADERS[,c(14:20,22,23)], digits = 3)
DRAFT_LHH_LEADERS[,c(11:13,15:17,21)] = round(DRAFT_LHH_LEADERS[,c(11:13,15:17,21)], digits = 1)
colnames(DRAFT_LHH_LEADERS)[c(11:17)] = c("K/9IP", "BB/9IP", "K/BB", "HR/9IP", "K%", "BB%", "K/BB%")
DRAFT_LHH_LEADERS = DRAFT_LHH_LEADERS[,-8]

DRAFT_RHH_LEADERS = DRAFT_Ps %>%
  select(Name, Throws, Team, WAR, Class, G, GS) %>%
  left_join(vRHH) %>%
  arrange(WHIP) %>%
  collect()
DRAFT_RHH_LEADERS[,c(15:16,21)] = DRAFT_RHH_LEADERS[,c(15:17,21)]*100
DRAFT_RHH_LEADERS[,c(14:20,22,23)] = round(DRAFT_RHH_LEADERS[,c(14:20,22,23)], digits = 3)
DRAFT_RHH_LEADERS[,c(11:13,15:17,21)] = round(DRAFT_RHH_LEADERS[,c(11:13,15:17,21)], digits = 1)
colnames(DRAFT_RHH_LEADERS)[c(11:17)] = c("K/9IP", "BB/9IP", "K/BB", "HR/9IP", "K%", "BB%", "K/BB%")
DRAFT_RHH_LEADERS = DRAFT_RHH_LEADERS[,-8]


DRAFT_LHH_LEADERS = add_column(DRAFT_LHH_LEADERS, Role = "", .after = 5)

for(i in 1:nrow(DRAFT_LHH_LEADERS)){
  if (DRAFT_LHH_LEADERS$G[i] == DRAFT_LHH_LEADERS$GS[i]){
    DRAFT_LHH_LEADERS$Role[i] = "SP"
  }
  if (DRAFT_LHH_LEADERS$GS[i] < DRAFT_LHH_LEADERS$G[i]){
    if (DRAFT_LHH_LEADERS$GS[i] >= 8) {
      DRAFT_LHH_LEADERS$Role[i] = "SWING"
    }
    if (DRAFT_LHH_LEADERS$GS[i] < 8) {
      DRAFT_LHH_LEADERS$Role[i] = "LRP"
    }
  }
  if (DRAFT_LHH_LEADERS$GS[i] == 0){
    DRAFT_LHH_LEADERS$Role[i] = "FIRE"
  }
}

DRAFT_RHH_LEADERS = add_column(DRAFT_RHH_LEADERS, Role = "", .after = 5)

for(i in 1:nrow(DRAFT_RHH_LEADERS)){
  if (DRAFT_RHH_LEADERS$G[i] == DRAFT_RHH_LEADERS$GS[i]){
    DRAFT_RHH_LEADERS$Role[i] = "SP"
  }
  if (DRAFT_RHH_LEADERS$GS[i] < DRAFT_RHH_LEADERS$G[i]){
    if (DRAFT_RHH_LEADERS$GS[i] >= 8) {
      DRAFT_RHH_LEADERS$Role[i] = "SWING"
    }
    if (DRAFT_RHH_LEADERS$GS[i] < 8) {
      DRAFT_RHH_LEADERS$Role[i] = "LRP"
    }
  }
  if (DRAFT_RHH_LEADERS$GS[i] == 0){
    DRAFT_RHH_LEADERS$Role[i] = "FIRE"
  }
}


rownames(DRAFT_Ps) = NULL
