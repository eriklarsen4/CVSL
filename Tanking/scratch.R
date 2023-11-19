
# Old player BBREF import ----

```{r Old BR Import, include = F, warning = F, message = F, echo = F}
## Import player value stats downloaded from Baseball Reference
# setwd("C:/Users/Erik/Desktop/BoxCopy/Programming Scripts and Data/Baseball/Projects/Tanking/")
# files = list.files(pattern = "Player_Value")
# for ( i in 1:length(files) ) {
#   as.data.frame(
#     assign(
#       gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = files[i]))
#   )
# }

# files = list.files(path = "https://raw.githubusercontent.com/eriklarsen4/Baseball/main/Tanking/Player%20Value%20files")
# for (i in 1:length(files) ) {
#   as.data.frame(
#     assign(
#       gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = paste("https://github.com/eriklarsen4/Baseball/tree/main/Tanking/Player%20Value%20files",files[i], sep = ""), encoding = "UTF8")
#     )
#   )
# }

#   ## Re-name them to include underscores
#     ##  First, concatenate the dataframe names into a list
# BR_list = ls()[
#   which(grepl(ls(), pattern = "Value") == TRUE)
# ]
#     ## Replace the spaces with underscores and assign the files to the names
# for(i in 1:length(unlist(BR_list))){
#     
#     BR_list[i] = gsub(" ", "_", ls()[
#       which(grepl(ls(), pattern = "BR") == TRUE)[i]
#     ], fixed = TRUE)
#     
#     as.data.frame(
#       assign(unlist(BR_list)[i], read.csv(file = paste("https://github.com/eriklarsen4/Baseball/tree/main/CVSL/FG%20and%20BR%20Data", files[i], sep = ""), encoding = "UTF8")
#     )
#     )
# }
```

```{r Import player WAR and salary data from BBRef, include = F, warning = F, message = F, echo = F}
rm(list = ls()[which(grepl(ls(), pattern = "Player_Value_"))])
rm(Player_Value_Pitching_ALL)


batter_files = vector()
pitcher_files = vector()

for (i in 1:length(2011:2021)){
  batter_files[i] = c(
    paste("https://raw.githubusercontent.com/eriklarsen4/Baseball/main/Tanking/Player%20Value%20files/Player_Value_Batting_",2010+i,".csv", sep = "")
  )
  
  as.data.frame(
    assign(
      str_extract(batter_files[i], pattern = paste("Player_Value_Batting_", 2010+i, sep = "")),
      read.csv(file = batter_files[i], encoding = "UTF-8")
    )
  )
  
  pitcher_files[i] = c(
    paste("https://raw.githubusercontent.com/eriklarsen4/Baseball/main/Tanking/Player%20Value%20files/Player_Value_Pitching_",2010+i,".csv", sep = "")
  )
  
  as.data.frame(
    assign(
      str_extract(pitcher_files[i], pattern = paste("Player_Value_Pitching_", 2010+i, sep = "")),
      read.csv(file = pitcher_files[i], encoding = "UTF-8")
    )
  )
}

## 2020, 2021 have Xinn value added for pitchers; remove it to join and clean
Player_Value_Pitching_2020 = Player_Value_Pitching_2020[ , -13]
Player_Value_Pitching_2021 = Player_Value_Pitching_2021[ , -13]

## Add Year column
Batting.list = mget(ls()[which(grepl(ls(), pattern = "Batting"))])
Pitching.list = mget(ls()[which(grepl(ls(), pattern = "Pitching"))])

Batting.list = lapply(Batting.list, transform, Year = 0)
Pitching.list = lapply(Pitching.list, transform, Year = 0)

for (i in 1:length(Batting.list)) {
  
  Batting.list[[i]]$Year = as.numeric(paste(2010+i, sep = ""))
  Pitching.list[[i]]$Year = as.numeric(paste(2010+i, sep = ""))
  
  assign(ls()[which(grepl(ls(), pattern = paste("Batting_", 2010+i, sep = "")) == T)],
         as.data.frame(Batting.list[[i]])
  )
  
  assign(ls()[which(grepl(ls(), pattern = paste("Pitching_", 2010+i, sep = "")) == T)],
         as.data.frame(Pitching.list[[i]])
  )
  
}


```

# ----

CHAD_names = list()
CHAD_first_names = str_split_i(CHAD_LU$Name[grepl(CHAD_LU$Name, pattern = "\\s+")], pattern = "\\s+", i = 1)
CHAD_second_names = str_split_i(CHAD_LU$Name[grepl(CHAD_LU$Name, pattern = "\\s+")], pattern = "\\s+", i = 2)
CHAD_third_names = str_split_i(CHAD_LU$Name[grepl(CHAD_LU$Name, pattern = "\\s+")], pattern = "\\s+", i = 3)
CHAD_fourth_names = str_split_i(CHAD_LU$Name[grepl(CHAD_LU$Name, pattern = "\\s+")], pattern = "\\s+", i = 4)


missing_astros_cubs_identifiers

for (i in 1:length(grepl(CHAD_LU$Name, pattern = "\\s+")) ){
  
  CHAD_names[[i]] = c(unlist(str_split(CHAD_LU$Name[i], pattern = "\\s+")))
}

str_split_1(ASTROS_CUBS_DRAFT_SUB$Name[1], pattern = "\\s+") %in% str_split_i(CHAD_LU$Name[grepl(CHAD_LU$Name, pattern = "\\s+")], pattern = "\\s+", i = 1)
str_extract(ASTROS_CUBS_DRAFT_SUB$Name[1], pattern = "[A-Za-z].+?\\S")

## gets any matches between first/last names of both lists
unlist(CHAD_names[
  grep(CHAD_names, pattern = paste(unlist(str_split(missing_astros_cubs_identifiers[1], pattern = "\\s+")), collapse = "|"))
])


CHAD_LU = CHAD_LU %>%
  dplyr::mutate(CHAD_name = Name, .before = 7) %>%
  dplyr::mutate(Name = case_when(grepl(Name, pattern = "<a0>") ~ gsub(Name, pattern = "<a0>", replacement = " "),
                                                                  grepl(Name, pattern = "<ed>") ~ gsub(Name, pattern = "<ed>", replacement = "i"),
                                                                  grepl(Name, pattern = "<e1>") ~ gsub(Name, pattern = "<e1>", replacement = "a"),
                                                                  grepl(Name, pattern = "<fa>") ~ gsub(Name, pattern = "<fa>", replacement = "u"),
                                                                  grepl(Name, pattern = "<e9>") ~ gsub(Name, pattern = "<e9>", replacement = "e"),
                                                                  TRUE ~ Name))
CHAD_LU %>%
  dplyr::filter(Name == "Javier Baez") %>%
  dplyr::select(Name, CHAD_name)

CHAD_LU %>%
  left_join(BBA_DRAFT_ALL)

ASTROS_CUBS_DRAFT_SUB %>%
  left_join(CHAD_LU)

LEAGUE_DRAFT_SUB %>%
  dplyr::filter(!(grepl(key_bbref, pattern = "davisau0") & grepl(Tm, pattern = "Boston")))

LEAGUE_DRAFT_SUB %>%
  dplyr::filter(!(grepl(key_bbref, pattern = "smithjo11") & grepl(Tm, pattern = "Pirates")))

LEAGUE_DRAFT_SUB %>%
  dplyr::filter(grepl(Name, pattern = "Kiner") & grepl(Name, pattern = "Falefa"))

LEAGUE_DRAFT_SUB %>%
  dplyr::filter(grepl(Name, pattern = "Murphy") & grepl(Name, pattern = "John") & grepl(Tm, pattern = "Yankees"))

LEAGUE_DRAFT_SUB %>%
  dplyr::filter(grepl(Name, pattern = "Jake Miller"))


baseballr::mlb_people(c(CHAD_LU$key_mlbam[!is.na(CHAD_LU$key_mlbam)][c(1:5)])) %>%
  dplyr::select(id, full_name, height, weight)


round(median(TeamValALL %>% ungroup() %>%
        dplyr::filter(!grepl(Team, pattern = "(Houston Astros)|(Chicago Cubs)") &
                        Year == as.numeric(paste(20,17,sep = ""))) %>%
        dplyr::select(Salary) %>% unlist() %>% as.numeric() ), digits = 3)


round(mean(ESPN %>% ungroup() %>%
        dplyr::filter(Yrs != "" &
                        !grepl(Team, pattern = "(Astros)|(Cubs)") &
                        Year == as.numeric(paste(20,17,sep = ""))) %>%
        dplyr::select(Yrs) %>% unlist() %>% as.numeric(), na.rm = T), digits = 2)

TANK_DF %>%
  dplyr::filter(Year != 2020) %>%
  ggplot() +
  # geom_point(aes(x = Year,
  #                y = `Team bWAR`,
  #                color = Tm) ) +
  # geom_line(aes(x = Year,
  #               y = `Team bWAR`,
  #               color = Tm) ) +
  # geom_point(aes(x = Year,
  #                y = `Team payroll`,
  #                color = Tm) ) +
  # geom_line(aes(x = Year,
  #               y = `Team payroll`,
  #               color = Tm) ) +
  geom_point(aes(x = Year,
                 y = WARPM,
                 color = Tm) ) +
  geom_line(aes(x = Year,
                y = WARPM,
                color = Tm) )

Player_Value_Batting_ALL %>%
  dplyr::mutate(WAR = as.numeric(WAR),
                Age = as.numeric(Age)) %>%
  dplyr::select(bbref_id, Age, WAR, Year) %>%
  left_join(PLAYER_YEAR_WAR_df, by = c("bbref_id", "WAR", "Year")) %>%
  ggplot() +
  geom_point(aes(x = Age.y,
                 y = WAR)) +
  geom_smooth(aes(x = Age.y,
                  y = WAR), method= "gam")
