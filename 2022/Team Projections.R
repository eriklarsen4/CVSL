
library(XML)
library(rvest)
library(tidyverse)

read_html("https://docs.google.com/spreadsheets/d/1zoVdfl2rAWNR1IZuw3baZFrQzWmloftIMO9Rkgy91ts/edit#gid=1661330822") %>%
  html_table()

read_html("https://docs.google.com/spreadsheets/d/15H5xt2vMUPp6Z8Enb1tYqOA1M71MmJXeFr4oJpEM1s0/edit#gid=1768177686") %>%
  html_table()

##### Import the drafts #####
setwd("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/")
files = list.files(pattern = "CVSL Draft")

for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = files[i]))
  )
}
CVSL_2022_DRAFT = read.csv("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/CVSL Draft 2022.csv")

CVSL_2019_DRAFT = `CVSL Draft 2019`
CVSL_2020_DRAFT = `CVSL Draft 2020`
CVSL_2021_DRAFT = `CVSL Draft 2021`
CVSL_2022_DRAFT = `CVSL Draft 2022`
rm(`CVSL Draft 2019`, `CVSL Draft 2020`, `CVSL Draft 2021`, `CVSL Draft 2022`)

##### Clean the 2019 dataframe #####
colnames(CVSL_2019_DRAFT) = CVSL_2019_DRAFT[1,]

## Remove dollar signs and co-erce salaries as integers for math ops
for ( i in seq(2, ncol((CVSL_2019_DRAFT)), 2) ){
  CVSL_2019_DRAFT[,i] = gsub(CVSL_2019_DRAFT[,i], pattern = "\\$", replacement ="")
  CVSL_2019_DRAFT[,i] = as.integer(CVSL_2019_DRAFT[,i])
}
## Re-name the salary columns
colnames(CVSL_2019_DRAFT)[seq(2, ncol((CVSL_2019_DRAFT)), 2)] = "Salary"
CVSL_2019_DRAFT = CVSL_2019_DRAFT[ -1, ]

## Re-arrange the dataframe to put all players into a column
dummy = as.data.frame(
  cbind(c(CVSL_2019_DRAFT[,1], CVSL_2019_DRAFT[,3], CVSL_2019_DRAFT[,5], CVSL_2019_DRAFT[,7], CVSL_2019_DRAFT[,9],
          CVSL_2019_DRAFT[,11], CVSL_2019_DRAFT[,13], CVSL_2019_DRAFT[,15], CVSL_2019_DRAFT[,17], CVSL_2019_DRAFT[,19]))
)
## Create a second column of integers
dummy$Salary = 0
## Create a third column of strings
dummy$CVSLTeam = ""
## Create a fourth column and fill it with the year
dummy$Year = 2019

## Replace the second column with the players' auction values
dummy[,2] = cbind(c(CVSL_2019_DRAFT[,2], CVSL_2019_DRAFT[,4], CVSL_2019_DRAFT[,6], CVSL_2019_DRAFT[,8], CVSL_2019_DRAFT[,10],
                    CVSL_2019_DRAFT[,12], CVSL_2019_DRAFT[,14], CVSL_2019_DRAFT[,16], CVSL_2019_DRAFT[,18], CVSL_2019_DRAFT[,20]))

## Overwrite the CVSL draft object
dummy = dummy[ -which(is.na(dummy$Salary)), ]

## Fill the third column with appropriate team names
dummy[1:length(which(CVSL_2019_DRAFT$Dodgers != "")),3] = "Twins"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$`Golden Bears`[ which(CVSL_2019_DRAFT$`Golden Bears` != "")] == TRUE) ] = "Golden Bears"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Phillies[ which(CVSL_2019_DRAFT$Phillies != "")] == TRUE) ] = "Phillies"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Nuggets[ which(CVSL_2019_DRAFT$Nuggets != "")] == TRUE) ] = "Nuggets"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Cattlemen[ which(CVSL_2019_DRAFT$Cattlemen != "")] == TRUE) ] = "Cattlemen"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Bison[ which(CVSL_2019_DRAFT$Bison != "")] == TRUE) ] = "Bison"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Arsenal[ which(CVSL_2019_DRAFT$Arsenal != "")] == TRUE) ] = "Arsenal"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Ducks[ which(CVSL_2019_DRAFT$Ducks != "")] == TRUE) ] = "Ducks"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Tamales[ which(CVSL_2019_DRAFT$Tamales != "")] == TRUE) ] = "Tamales"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2019_DRAFT$Matadors[ which(CVSL_2019_DRAFT$Matadors != "")] == TRUE) ] = "Matadors"

## Overwrite the CVSL draft object and delete the duplicate
CVSL_2019_DRAFT = dummy
rm(dummy)
CVSL_2019_DRAFT$Salary = as.numeric(CVSL_2019_DRAFT$Salary)

CVSL_2019_DRAFT$CVSLTeam[ which(CVSL_2019_DRAFT$CVSLTeam == "Dodgers") ] = "Twins"

##### Clean the 2020 dataframe #####
## Remove dollar signs and co-erce salaries as integers for math ops
for ( i in seq(2, ncol((CVSL_2020_DRAFT)), 2) ){
  CVSL_2020_DRAFT[,i] = gsub(CVSL_2020_DRAFT[,i], pattern = "\\$", replacement ="")
  CVSL_2020_DRAFT[,i] = as.integer(CVSL_2020_DRAFT[,i])
}
## Re-name the salary columns
colnames(CVSL_2020_DRAFT)[seq(2, ncol((CVSL_2020_DRAFT)), 2)] = "Salary"
## Re-name the Team columns
colnames(CVSL_2020_DRAFT)[seq(1, ncol(CVSL_2020_DRAFT), 2)] = CVSL_2020_DRAFT[ 1, seq(1, ncol(CVSL_2020_DRAFT), 2)]
## remove the first row
CVSL_2020_DRAFT = CVSL_2020_DRAFT[ -1, ]

## Re-arrange the dataframe to put all players into a column
dummy = as.data.frame(
  cbind(c(CVSL_2020_DRAFT[,1], CVSL_2020_DRAFT[,3], CVSL_2020_DRAFT[,5], CVSL_2020_DRAFT[,7], CVSL_2020_DRAFT[,9],
          CVSL_2020_DRAFT[,11], CVSL_2020_DRAFT[,13], CVSL_2020_DRAFT[,15], CVSL_2020_DRAFT[,17], CVSL_2020_DRAFT[,19]))
)
## Create a second column of integers
dummy$Salary = 0
## Create a third column of strings
dummy$CVSLTeam = ""
## Create a fourth column and fill it with the year
dummy$Year = 2020

## Replace the second column with the players' auction values
dummy[,2] = cbind(c(CVSL_2020_DRAFT[,2], CVSL_2020_DRAFT[,4], CVSL_2020_DRAFT[,6], CVSL_2020_DRAFT[,8], CVSL_2020_DRAFT[,10],
                    CVSL_2020_DRAFT[,12], CVSL_2020_DRAFT[,14], CVSL_2020_DRAFT[,16], CVSL_2020_DRAFT[,18], CVSL_2020_DRAFT[,20]))
## Overwrite the CVSL draft object
dummy = dummy[ -which(is.na(dummy$Salary)), ]

## Fill the third column with appropriate team names
dummy[1:length(which(CVSL_2020_DRAFT$Phillies != "")),3] = "Phillies"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Ducks[ which(CVSL_2020_DRAFT$Ducks != "")] == TRUE) ] = "Ducks"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Bison[ which(CVSL_2020_DRAFT$Bison != "")] == TRUE) ] = "Bison"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Dodgers[ which(CVSL_2020_DRAFT$Dodgers != "")] == TRUE) ] = "Twins"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Bears[ which(CVSL_2020_DRAFT$Bears != "")] == TRUE) ] = "Bears"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Matadors[ which(CVSL_2020_DRAFT$Matadors != "")] == TRUE) ] = "Matadors"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Cattlemen[ which(CVSL_2020_DRAFT$Cattlemen != "")] == TRUE) ] = "Cattlemen"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Nuggets[ which(CVSL_2020_DRAFT$Nuggets != "")] == TRUE) ] = "Nuggets"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Copperheads[ which(CVSL_2020_DRAFT$Copperheads != "")] == TRUE) ] = "Copperheads"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Tamales[ which(CVSL_2020_DRAFT$Tamales != "")] == TRUE) ] = "Tamales"

CVSL_2020_DRAFT = dummy
rm(dummy)
CVSL_2020_DRAFT$Salary = as.numeric(CVSL_2020_DRAFT$Salary)

##### Clean the 2021 dataframe #####
## Remove the first 4 rows and the extra columns
CVSL_2021_DRAFT = CVSL_2021_DRAFT[ -c(1:4), -c(seq(2, ncol(CVSL_2021_DRAFT), 2)) ]

## Remove the dollar signs and co-erce salaries as integers for math ops
for ( i in seq(2, ncol((CVSL_2021_DRAFT)), 2) ){
  CVSL_2021_DRAFT[,i] = gsub(CVSL_2021_DRAFT[,i], pattern = "\\$", replacement ="")
  CVSL_2021_DRAFT[,i] = as.integer(CVSL_2021_DRAFT[,i])
}
## Re-name the salary columns
colnames(CVSL_2021_DRAFT)[seq(2, ncol((CVSL_2021_DRAFT)), 2)] = "Salary"
rownames(CVSL_2021_DRAFT) = NULL

## Re-arrange the dataframe to put all players into a column
dummy = as.data.frame(
  cbind(c(CVSL_2021_DRAFT[,1], CVSL_2021_DRAFT[,3], CVSL_2021_DRAFT[,5], CVSL_2021_DRAFT[,7], CVSL_2021_DRAFT[,9],
          CVSL_2021_DRAFT[,11], CVSL_2021_DRAFT[,13], CVSL_2021_DRAFT[,15], CVSL_2021_DRAFT[,17], CVSL_2021_DRAFT[,19]))
)
## Create a second column of integers
dummy$Salary = 0
## Create a third column of strings
dummy$CVSLTeam = ""

## Create a fourth column and fill it with the year
dummy$Year = 2021

## Replace the second column with the players' auction values
dummy[,2] = cbind(c(CVSL_2021_DRAFT[,2], CVSL_2021_DRAFT[,4], CVSL_2021_DRAFT[,6], CVSL_2021_DRAFT[,8], CVSL_2021_DRAFT[,10],
                    CVSL_2021_DRAFT[,12], CVSL_2021_DRAFT[,14], CVSL_2021_DRAFT[,16], CVSL_2021_DRAFT[,18], CVSL_2021_DRAFT[,20]))
## Overwrite the CVSL draft object
dummy = dummy[ -which(is.na(dummy$Salary)), ]

## Fill the third column with appropriate team names
dummy[1:length(which(CVSL_2021_DRAFT$Dodgers != "")),3] = "Twins"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Phillies[ which(CVSL_2021_DRAFT$Phillies != "")] == TRUE) ] = "Phillies"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Renegades[ which(CVSL_2021_DRAFT$Renegades != "")] == TRUE) ] = "Renegades"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Matadors[ which(CVSL_2021_DRAFT$Matadors != "")] == TRUE) ] = "Matadors"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Ducks[ which(CVSL_2021_DRAFT$Ducks != "")] == TRUE) ] = "Ducks"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Tamales[ which(CVSL_2021_DRAFT$Tamales != "")] == TRUE) ] = "Tamales"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Bison[ which(CVSL_2021_DRAFT$Bison != "")] == TRUE) ] = "Bison"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Nuggets[ which(CVSL_2021_DRAFT$Nuggets != "")] == TRUE) ] = "Nuggets"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Cattlemen[ which(CVSL_2021_DRAFT$Cattlemen != "")] == TRUE) ] = "Cattlemen"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2021_DRAFT$Copperheads[ which(CVSL_2021_DRAFT$Copperheads != "")] == TRUE) ] = "Copperheads"

CVSL_2021_DRAFT = dummy
rm(dummy)
CVSL_2021_DRAFT$Salary = as.numeric(CVSL_2021_DRAFT$Salary)

## Re-name the player/name column in each dataframe
colnames(CVSL_2019_DRAFT)[1] = "Name"
colnames(CVSL_2020_DRAFT)[1] = "Name"
colnames(CVSL_2021_DRAFT)[1] = "Name"

##### Clean the 2022 dataframe #####
## Remove the first 4 rows and the extra columns
CVSL_2022_DRAFT = CVSL_2022_DRAFT[ , -which(grepl(colnames(CVSL_2022_DRAFT), pattern = "X") == TRUE) ]
CVSL_2022_DRAFT = CVSL_2022_DRAFT[ -c(1:4), -c(seq(3, ncol(CVSL_2022_DRAFT), 3)) ]

## Remove the dollar signs and co-erce salaries as integers for math ops
for ( i in seq(2, ncol((CVSL_2022_DRAFT)), 2) ){
  CVSL_2022_DRAFT[,i] = gsub(CVSL_2022_DRAFT[,i], pattern = "\\$", replacement ="")
  CVSL_2022_DRAFT[,i] = as.integer(CVSL_2022_DRAFT[,i])
}
## Re-name the salary columns
colnames(CVSL_2022_DRAFT)[seq(2, ncol((CVSL_2022_DRAFT)), 2)] = "Salary"
rownames(CVSL_2022_DRAFT) = NULL

## Re-arrange the dataframe to put all players into a column
dummy = as.data.frame(
  cbind(c(CVSL_2022_DRAFT[,1], CVSL_2022_DRAFT[,3], CVSL_2022_DRAFT[,5], CVSL_2022_DRAFT[,7], CVSL_2022_DRAFT[,9],
          CVSL_2022_DRAFT[,11], CVSL_2022_DRAFT[,13], CVSL_2022_DRAFT[,15], CVSL_2022_DRAFT[,17], CVSL_2022_DRAFT[,19]))
)
## Create a second column of integers
dummy$Salary = 0
## Create a third column of strings
dummy$CVSLTeam = ""

## Create a fourth column and fill it with the year
dummy$Year = 2022

## Replace the second column with the players' auction values
dummy[,2] = cbind(c(CVSL_2022_DRAFT[,2], CVSL_2022_DRAFT[,4], CVSL_2022_DRAFT[,6], CVSL_2022_DRAFT[,8], CVSL_2022_DRAFT[,10],
                    CVSL_2022_DRAFT[,12], CVSL_2022_DRAFT[,14], CVSL_2022_DRAFT[,16], CVSL_2022_DRAFT[,18], CVSL_2022_DRAFT[,20]))
## Overwrite the CVSL draft object
dummy = dummy[ -which(is.na(dummy$Salary)), ]

## Fill the third column with appropriate team names
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Renegades[ which(CVSL_2022_DRAFT$Renegades != "")] == TRUE) ] = "Renegades"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Phillies[ which(CVSL_2022_DRAFT$Phillies != "")] == TRUE) ] = "Phillies"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Twins[ which(CVSL_2022_DRAFT$Twins != "")] == TRUE) ] = "Twins"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Matadors[ which(CVSL_2022_DRAFT$Matadors != "")] == TRUE) ] = "Matadors"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Ducks[ which(CVSL_2022_DRAFT$Ducks != "")] == TRUE) ] = "Ducks"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Wombats[ which(CVSL_2022_DRAFT$Wombats != "")] == TRUE) ] = "Wombats"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Bison[ which(CVSL_2022_DRAFT$Bison != "")] == TRUE) ] = "Bison"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Nuggets[ which(CVSL_2022_DRAFT$Nuggets != "")] == TRUE) ] = "Nuggets"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Cattlemen[ which(CVSL_2022_DRAFT$Cattlemen != "")] == TRUE) ] = "Cattlemen"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2022_DRAFT$Arsenal[ which(CVSL_2022_DRAFT$Arsenal != "")] == TRUE) ] = "Arsenal"

CVSL_2022_DRAFT = dummy
rm(dummy)
CVSL_2022_DRAFT$Salary = as.numeric(CVSL_2022_DRAFT$Salary)

## Re-name the player/name column in each dataframe
colnames(CVSL_2022_DRAFT)[1] = "Name"
##### Merge drafts into hitters and pitchers dfs #####
CVSL_2019_DRAFT_hitters = Hitters_adv_2018[ which(Hitters_adv_2018$Team %in% CVSL_universe == TRUE | Hitters_adv_2018$Team == "- - -"), ] %>%
  inner_join(CVSL_2019_DRAFT)

CVSL_2020_DRAFT_hitters = Hitters_adv_2019[ which(Hitters_adv_2019$Team %in% CVSL_universe == TRUE | Hitters_adv_2019$Team == "- - -"), ] %>%
  inner_join(CVSL_2020_DRAFT)

CVSL_2021_DRAFT_hitters = Hitters_adv_2020[ which(Hitters_adv_2020$Team %in% CVSL_universe == TRUE | Hitters_adv_2020$Team == "- - -"), ] %>%
  inner_join(CVSL_2021_DRAFT)

CVSL_2022_DRAFT_hitters = Hitters_adv_2021[ which(Hitters_adv_2021$Team %in% CVSL_universe == TRUE | Hitters_adv_2021$Team == "- - -"), ] %>%
  inner_join(CVSL_2022_DRAFT)

CVSL_all_DRAFTS_hitters = rbind.data.frame(CVSL_2019_DRAFT_hitters, CVSL_2020_DRAFT_hitters, CVSL_2021_DRAFT_hitters)



CVSL_2019_DRAFT_pitchers = Pitchers_2018[ which(Pitchers_2018$Team %in% CVSL_universe == TRUE | Pitchers_2018$Team == "- - -"), ] %>%
  inner_join(CVSL_2019_DRAFT)

CVSL_2020_DRAFT_pitchers = Pitchers_2019[ which(Pitchers_2019$Team %in% CVSL_universe == TRUE | Pitchers_2019$Team == "- - -"), ] %>%
  inner_join(CVSL_2020_DRAFT)

CVSL_2021_DRAFT_pitchers = Pitchers_2020[ which(Pitchers_2020$Team %in% CVSL_universe == TRUE | Pitchers_2020$Team == "- - -"), ] %>%
  inner_join(CVSL_2021_DRAFT)

CVSL_2022_DRAFT_pitchers = Pitchers_2021[ which(Pitchers_2021$Team %in% CVSL_universe == TRUE | Pitchers_2021$Team == "- - -"), ] %>%
  inner_join(CVSL_2022_DRAFT)

CVSL_all_DRAFTS_pitchers = rbind.data.frame(CVSL_2019_DRAFT_pitchers, CVSL_2020_DRAFT_pitchers, CVSL_2021_DRAFT_pitchers)

##### Twins #####
TWINS = DODGERS

TWINS$Team = "Twins"

TWINS_Hitters = HITTING_LEADERS_UNIVERSE %>%
  inner_join(TWINS, by = "Name") %>%
  filter(Year == 2021)

TWINS_Pitchers = PITCHING_LEADERS_UNIVERSE %>%
  inner_join(TWINS, by = "Name")

TWINS_new = rbind.data.frame(TWINS_Pitchers[,c(1,18,24)], TWINS_Hitters[,c(1,22,30)])
colnames(TWINS_new)[3] = "Salary"

TWINS_new = CVSL_all_DRAFTS_hitters %>%
  filter(CVSLTeam == "Twins") %>%
  select(Name, WAR, Salary) %>%
  full_join(TWINS_new)

TWINS_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Twins") %>%
  select(Name, WAR, Salary) %>%
  full_join(TWINS_new)

TWINS_new$POS = ""
TWINS_new$POS[ c(
  which(TWINS_new$Name == CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Twins" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(TWINS_new$Name %in% TWINS_Pitchers$Name[ which(TWINS_Pitchers$GS >= 8) ]) ) ] = "SP"

TWINS_new$POS[ c(
  which(TWINS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Twins" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(TWINS_new$Name %in% TWINS_Pitchers$Name[ which(TWINS_Pitchers$GS < 8) ]) )] = "RP"

TWINS_new$POS[ which(TWINS_new$POS == "")] = "PP"

TWINS_by_pos = TWINS_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
TWINS_by_pos = column_to_rownames(TWINS_by_pos, var = "POS")

TWINS_proj = add_row(TWINS_by_pos)
rownames(TWINS_proj)[4] = "Tot"

TWINS_proj[4,] = colSums(TWINS_proj, na.rm = T)
TWINS_proj$Team = "Twins"

##### Renegades #####

RENEGADES_Hitters = Hitters_adv_2021 %>%
  inner_join(RENEGADES, by = "Name")
RENEGADES_Pitchers = Pitchers_2021 %>%
  inner_join(RENEGADES, by = "Name")
RENEGADES_new = rbind.data.frame(RENEGADES_Pitchers[,c(1,21,26)], RENEGADES_Hitters[,c(1,20,25)])
colnames(RENEGADES_new)[3] = "Salary"

RENEGADES_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Renegades") %>%
  select(Name, WAR, Salary) %>%
  full_join(RENEGADES_new)

RENEGADES_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Renegades") %>%
  select(Name, WAR, Salary) %>%
  full_join(RENEGADES_new)

RENEGADES_new$POS = ""
RENEGADES_new$POS[ c(
  which(RENEGADES_new$Name == CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Renegades" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(RENEGADES_new$Name %in% RENEGADES_Pitchers$Name[ which(RENEGADES_Pitchers$GS >= 8) ]) ) ] = "SP"

RENEGADES_new$POS[ c(
  which(RENEGADES_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Renegades" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(RENEGADES_new$Name %in% RENEGADES_Pitchers$Name[ which(RENEGADES_Pitchers$GS < 8) ]) )] = "RP"

RENEGADES_new$POS[ which(RENEGADES_new$POS == "")] = "PP"

RENEGADES_by_pos = RENEGADES_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
RENEGADES_by_pos = column_to_rownames(RENEGADES_by_pos, var = "POS")

RENEGADES_proj = add_row(RENEGADES_by_pos)
rownames(RENEGADES_proj)[4] = "Tot"

RENEGADES_proj[4,] = colSums(RENEGADES_proj, na.rm = T)
RENEGADES_proj$Team = "Renegades"
##### Wombats #####

WOMBATS_Hitters = Hitters_adv_2021 %>%
  inner_join(WOMBATS, by = "Name")
WOMBATS_Pitchers = Pitchers_2021 %>%
  inner_join(RENEGADES, by = "Name")
WOMBATS_new = rbind.data.frame(WOMBATS_Pitchers[,c(1,21,26)], WOMBATS_Hitters[,c(1,20,25)])
colnames(WOMBATS_new)[3] = "Salary"

WOMBATS_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Wombats") %>%
  select(Name, WAR, Salary) %>%
  full_join(WOMBATS_new)

WOMBATS_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Wombats") %>%
  select(Name, WAR, Salary) %>%
  full_join(WOMBATS_new)

WOMBATS_new$POS = ""
WOMBATS_new$POS[ c(
  which(WOMBATS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Wombats" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(WOMBATS_new$Name %in% WOMBATS_Pitchers$Name[ which(WOMBATS_Pitchers$GS >= 8) ]) ) ] = "SP"

WOMBATS_new$POS[ c(
  which(WOMBATS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Wombats" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(WOMBATS_new$Name %in% WOMBATS_Pitchers$Name[ which(WOMBATS_Pitchers$GS < 8) ]) )] = "RP"

WOMBATS_new$POS[ which(WOMBATS_new$POS == "")] = "PP"

WOMBATS_by_pos = WOMBATS_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
WOMBATS_by_pos = column_to_rownames(WOMBATS_by_pos, var = "POS")

WOMBATS_proj = add_row(WOMBATS_by_pos)
rownames(WOMBATS_proj)[4] = "Tot"

WOMBATS_proj[4,] = colSums(WOMBATS_proj, na.rm = T)
WOMBATS_proj$Team = "Wombats"
##### Phillies #####

PHILLIES_Hitters = Hitters_adv_2021 %>%
  inner_join(PHILLIES, by = "Name")
PHILLIES_Pitchers = Pitchers_2021 %>%
  inner_join(PHILLIES, by = "Name")
PHILLIES_new = rbind.data.frame(PHILLIES_Pitchers[,c(1,21,26)], PHILLIES_Hitters[,c(1,20,25)])
colnames(PHILLIES_new)[3] = "Salary"

PHILLIES_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Phillies") %>%
  select(Name, WAR, Salary) %>%
  full_join(PHILLIES_new)

PHILLIES_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Phillies") %>%
  select(Name, WAR, Salary) %>%
  full_join(PHILLIES_new)

PHILLIES_new$POS = ""
PHILLIES_new$POS[ c(
  which(PHILLIES_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Phillies" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(PHILLIES_new$Name %in% PHILLIES_Pitchers$Name[ which(PHILLIES_Pitchers$GS >= 8) ]) ) ] = "SP"

PHILLIES_new$POS[ c(
  which(PHILLIES_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Phillies" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(PHILLIES_new$Name %in% PHILLIES_Pitchers$Name[ which(PHILLIES_Pitchers$GS < 8) ]) )] = "RP"

PHILLIES_new$POS[ which(PHILLIES_new$POS == "")] = "PP"

PHILLIES_by_pos = PHILLIES_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
PHILLIES_by_pos = column_to_rownames(PHILLIES_by_pos, var = "POS")

PHILLIES_proj = add_row(PHILLIES_by_pos)
rownames(PHILLIES_proj)[4] = "Tot"

PHILLIES_proj[4,] = colSums(PHILLIES_proj, na.rm = T)
PHILLIES_proj$Team = "Phillies"
##### Arsenal #####

ARSENAL_Hitters = HITTING_LEADERS_UNIVERSE %>%
  inner_join(ARSENAL, by = "Name") %>%
  filter(Year == 2021)
ARSENAL_Pitchers = PITCHING_LEADERS_UNIVERSE %>%
  inner_join(ARSENAL, by = "Name")
ARSENAL_new = rbind.data.frame(ARSENAL_Pitchers[,c(1,21,26)], ARSENAL_Hitters[,c(1,20,25)])
colnames(ARSENAL_new)[3] = "Salary"

ARSENAL_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Arsenal") %>%
  select(Name, WAR, Salary) %>%
  full_join(ARSENAL_new)

ARSENAL_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Arsenal") %>%
  select(Name, WAR, Salary) %>%
  full_join(ARSENAL_new)

ARSENAL_new$POS = ""
ARSENAL_new$POS[ c(
  which(ARSENAL_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Arsenal" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(ARSENAL_new$Name %in% ARSENAL_Pitchers$Name[ which(ARSENAL_Pitchers$GS >= 8) ]) ) ] = "SP"

ARSENAL_new$POS[ c(
  which(ARSENAL_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Arsenal" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(ARSENAL_new$Name %in% ARSENAL_Pitchers$Name[ which(ARSENAL_Pitchers$GS < 8) ]) )] = "RP"

ARSENAL_new$POS[ which(ARSENAL_new$POS == "")] = "PP"

ARSENAL_by_pos = ARSENAL_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
ARSENAL_by_pos = column_to_rownames(ARSENAL_by_pos, var = "POS")

ARSENAL_proj = add_row(ARSENAL_by_pos)
rownames(ARSENAL_proj)[4] = "Tot"

ARSENAL_proj[4,] = colSums(ARSENAL_proj, na.rm = T)
ARSENAL_proj$Team = "Arsenal"
##### Matadors #####

MATADORS_Hitters = Hitters_adv_2021 %>%
  inner_join(MATADORS, by = "Name")
MATADORS_Pitchers = Pitchers_2021 %>%
  inner_join(MATADORS, by = "Name")
MATADORS_new = rbind.data.frame(MATADORS_Pitchers[,c(1,21,26)], MATADORS_Hitters[,c(1,20,25)])
colnames(MATADORS_new)[3] = "Salary"

MATADORS_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Matadors") %>%
  select(Name, WAR, Salary) %>%
  full_join(MATADORS_new)

MATADORS_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Matadors") %>%
  select(Name, WAR, Salary) %>%
  full_join(MATADORS_new)

MATADORS_new$POS = ""
MATADORS_new$POS[ c(
  which(MATADORS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Matadors" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(MATADORS_new$Name %in% MATADORS_Pitchers$Name[ which(MATADORS_Pitchers$GS >= 8) ]) ) ] = "SP"

MATADORS_new$POS[ c(
  which(MATADORS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Matadors" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(MATADORS_new$Name %in% MATADORS_Pitchers$Name[ which(MATADORS_Pitchers$GS < 8) ]) )] = "RP"

MATADORS_new$POS[ which(MATADORS_new$POS == "")] = "PP"

MATADORS_by_pos = MATADORS_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
MATADORS_by_pos = column_to_rownames(MATADORS_by_pos, var = "POS")

MATADORS_proj = add_row(MATADORS_by_pos)
rownames(MATADORS_proj)[4] = "Tot"

MATADORS_proj[4,] = colSums(MATADORS_proj, na.rm = T)
MATADORS_proj$Team = "Matadors"
##### Nuggets #####

NUGGETS_Hitters = Hitters_adv_2021 %>%
  inner_join(NUGGETS, by = "Name")
NUGGETS_Pitchers = Pitchers_2021 %>%
  inner_join(NUGGETS, by = "Name")
NUGGETS_new = rbind.data.frame(NUGGETS_Pitchers[,c(1,21,26)], NUGGETS_Hitters[,c(1,20,25)])
colnames(NUGGETS_new)[3] = "Salary"

NUGGETS_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Nuggets") %>%
  select(Name, WAR, Salary) %>%
  full_join(NUGGETS_new)

NUGGETS_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Nuggets") %>%
  select(Name, WAR, Salary) %>%
  full_join(NUGGETS_new)

NUGGETS_new$POS = ""
NUGGETS_new$POS[ c(
  which(NUGGETS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Nuggets" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(NUGGETS_new$Name %in% NUGGETS_Pitchers$Name[ which(NUGGETS_Pitchers$GS >= 8) ]) ) ] = "SP"

NUGGETS_new$POS[ c(
  which(NUGGETS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Nuggets" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(NUGGETS_new$Name %in% NUGGETS_Pitchers$Name[ which(NUGGETS_Pitchers$GS < 8) ]) )] = "RP"

NUGGETS_new$POS[ which(NUGGETS_new$POS == "")] = "PP"

NUGGETS_by_pos = NUGGETS_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
NUGGETS_by_pos = column_to_rownames(NUGGETS_by_pos, var = "POS")

NUGGETS_proj = add_row(NUGGETS_by_pos)
rownames(NUGGETS_proj)[4] = "Tot"

NUGGETS_proj[4,] = colSums(NUGGETS_proj, na.rm = T)
NUGGETS_proj$Team = "Nuggets"
##### Cattlemen #####

CATTLEMEN_Hitters = Hitters_adv_2021 %>%
  inner_join(CATTLEMEN, by = "Name")
CATTLEMEN_Pitchers = Pitchers_2021 %>%
  inner_join(CATTLEMEN, by = "Name")
CATTLEMEN_new = rbind.data.frame(CATTLEMEN_Pitchers[,c(1,21,26)], CATTLEMEN_Hitters[,c(1,20,25)])
colnames(CATTLEMEN_new)[3] = "Salary"

CATTLEMEN_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Cattlemen") %>%
  select(Name, WAR, Salary) %>%
  full_join(CATTLEMEN_new)

CATTLEMEN_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Cattlemen") %>%
  select(Name, WAR, Salary) %>%
  full_join(CATTLEMEN_new)

CATTLEMEN_new$POS = ""
CATTLEMEN_new$POS[ c(
  which(CATTLEMEN_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Cattlemen" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(CATTLEMEN_new$Name %in% CATTLEMEN_Pitchers$Name[ which(CATTLEMEN_Pitchers$GS >= 8) ]) ) ] = "SP"

CATTLEMEN_new$POS[ c(
  which(CATTLEMEN_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Cattlemen" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(CATTLEMEN_new$Name %in% CATTLEMEN_Pitchers$Name[ which(CATTLEMEN_Pitchers$GS < 8) ]) )] = "RP"

CATTLEMEN_new$POS[ which(CATTLEMEN_new$POS == "")] = "PP"

CATTLEMEN_by_pos = CATTLEMEN_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
CATTLEMEN_by_pos = column_to_rownames(CATTLEMEN_by_pos, var = "POS")

CATTLEMEN_proj = add_row(CATTLEMEN_by_pos)
rownames(CATTLEMEN_proj)[4] = "Tot"

CATTLEMEN_proj[4,] = colSums(CATTLEMEN_proj, na.rm = T)
CATTLEMEN_proj$Team = "Cattlemen"

##### Bison #####

BISON_Hitters = Hitters_adv_2021 %>%
  inner_join(BISON, by = "Name")
BISON_Pitchers = Pitchers_2021 %>%
  inner_join(BISON, by = "Name")
BISON_new = rbind.data.frame(BISON_Pitchers[,c(1,21,26)], BISON_Hitters[,c(1,20,25)])
colnames(BISON_new)[3] = "Salary"

BISON_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Bison") %>%
  select(Name, WAR, Salary) %>%
  full_join(BISON_new)

BISON_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Bison") %>%
  select(Name, WAR, Salary) %>%
  full_join(BISON_new)

BISON_new$POS = ""
BISON_new$POS[ c(
  which(BISON_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Bison" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(BISON_new$Name %in% BISON_Pitchers$Name[ which(BISON_Pitchers$GS >= 8) ]) ) ] = "SP"

BISON_new$POS[ c(
  which(BISON_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Bison" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(BISON_new$Name %in% BISON_Pitchers$Name[ which(BISON_Pitchers$GS < 8) ]) )] = "RP"

BISON_new$POS[ which(BISON_new$POS == "")] = "PP"

BISON_by_pos = BISON_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
BISON_by_pos = column_to_rownames(BISON_by_pos, var = "POS")

BISON_proj = add_row(BISON_by_pos)
rownames(BISON_proj)[4] = "Tot"

BISON_proj[4,] = colSums(BISON_proj, na.rm = T)
BISON_proj$Team = "Bison"
##### Ducks #####

DUCKS_Hitters = Hitters_adv_2021 %>%
  inner_join(DUCKS, by = "Name")
DUCKS_Pitchers = Pitchers_2021 %>%
  inner_join(DUCKS, by = "Name")
DUCKS_new = rbind.data.frame(DUCKS_Pitchers[,c(1,21,26)], DUCKS_Hitters[,c(1,20,25)])
colnames(DUCKS_new)[3] = "Salary"

DUCKS_new = CVSL_2022_DRAFT_hitters %>%
  filter(CVSLTeam == "Ducks") %>%
  select(Name, WAR, Salary) %>%
  full_join(DUCKS_new)

DUCKS_new = CVSL_2022_DRAFT_pitchers %>%
  filter(CVSLTeam == "Ducks") %>%
  select(Name, WAR, Salary) %>%
  full_join(DUCKS_new)

DUCKS_new$POS = ""
DUCKS_new$POS[ c(
  which(DUCKS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Ducks" & CVSL_2022_DRAFT_pitchers$GS >= 8) ]),
  which(DUCKS_new$Name %in% DUCKS_Pitchers$Name[ which(DUCKS_Pitchers$GS >= 8) ]) ) ] = "SP"

DUCKS_new$POS[ c(
  which(DUCKS_new$Name %in% CVSL_2022_DRAFT_pitchers$Name[ which(CVSL_2022_DRAFT_pitchers$CVSLTeam == "Ducks" & CVSL_2022_DRAFT_pitchers$GS < 8) ]),
  which(DUCKS_new$Name %in% DUCKS_Pitchers$Name[ which(DUCKS_Pitchers$GS < 8) ]) )] = "RP"

DUCKS_new$POS[ which(DUCKS_new$POS == "")] = "PP"

DUCKS_by_pos = DUCKS_new %>% 
  group_by(POS) %>%
  summarize(WARsum = sum(WAR),
            Salsum = sum(Salary),
            WARavg = mean(WAR),
            Salavg = mean(Salary),
            DpWAR = mean(Salary)/mean(WAR))
DUCKS_by_pos = column_to_rownames(DUCKS_by_pos, var = "POS")

DUCKS_proj = add_row(DUCKS_by_pos)
rownames(DUCKS_proj)[4] = "Tot"

DUCKS_proj[4,] = colSums(DUCKS_proj, na.rm = T)
DUCKS_proj$Team = "Ducks"

##### Put them together #####

PROJECTIONS = reduce(mget(ls()[
  which(grepl(ls(), pattern = "proj") == TRUE)
])[][], full_join)

PROJECTIONS$POS = rep(c("PP", "RP", "SP", "Tot"), 10)

PROJECTIONS %>%
  group_by(Team) %>%
  filter(POS == "Tot") %>%
  arrange(desc(WARsum))

PROJECTIONS %>%
  group_by(Team) %>%
  filter(POS == "PP") %>%
  arrange(desc(WARsum))

PROJECTIONS %>%
  group_by(Team) %>%
  filter(POS == "SP") %>%
  arrange(desc(WARsum))

PROJECTIONS %>%
  group_by(Team) %>%
  filter(POS == "RP") %>%
  arrange(desc(WARsum))