

library(readr)
library(tidyverse)


library(rvest)
library(XML)

##### Scrape baseball-reference's free-agent contract data #####
css_selector = "#fa_signings"

FA_2018 = read_html("https://www.baseball-reference.com/leagues/majors/2018-free-agents.shtml") %>% 
  html_element(css = css_selector) %>%
  html_table()

FA_2019 = read_html("https://www.baseball-reference.com/leagues/majors/2019-free-agents.shtml") %>% 
  html_element(css = css_selector) %>%
  html_table()

FA_2017 = read_html("https://www.baseball-reference.com/leagues/majors/2017-free-agents.shtml") %>% 
  html_element(css = css_selector) %>%
  html_table()

FA_2016 = read_html("https://www.baseball-reference.com/leagues/majors/2016-free-agents.shtml") %>% 
  html_element(css = css_selector) %>%
  html_table()

##### Scrape ESPN's free-agent contract data #####
ESPN2019_selector = "#my-players-table"

ESPN2019_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2019/type/signed") %>%
  html_element(css = ESPN2019_selector) %>%
  html_table()

## ESPN 2018 free-agent signings
ESPN2018_selector = "#my-players-table"

ESPN2018_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2018/type/signed") %>%
  html_element(css = ESPN2018_selector) %>%
  html_table()

## ESPN 2017 free-agent signings
ESPN2017_selector = "#my-players-table"

ESPN2017_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2017/type/signed") %>%
  html_element(css = ESPN2017_selector) %>%
  html_table()

## ESPN 2016 free-agent signings
ESPN2016_selector = "#my-players-table"

ESPN2016_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2016/type/signed") %>%
  html_element(css = ESPN2016_selector) %>%
  html_table()

##### clean the data #####
  ## Create a function that cleans the ESPN FA dfs
clean_ESPN_FA_fn = function(df, Year){
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
  df$TotalValue = as.numeric(df$TotalValue) / 1000000
  df$X9 = (as.numeric(df$X9) / as.numeric(df$X7)) / 1000000
  df = add_column(df, Year = "", .before = 2)
  df[,2] = Year
  colnames(df)[c(1,3:7)] = c("Name", "Position", "Age", "Team", "Years", "AAV")
  
  return(df)
}
  ## Call each df through the function and overwrite the variable
ESPN2016_FAs = clean_ESPN_FA_fn(df = ESPN2016_FAs, Year = "2016")
ESPN2017_FAs = clean_ESPN_FA_fn(df = ESPN2017_FAs, Year = "2017")
ESPN2018_FAs = clean_ESPN_FA_fn(df = ESPN2018_FAs, Year = "2018")
ESPN2019_FAs = clean_ESPN_FA_fn(df = ESPN2019_FAs, Year = "2019")

FA_2016 = FA_2016[  , -c(9:32) ]
FA_2017 = FA_2017[  , -c(9:32) ]
FA_2018 = FA_2018[  , -c(9:32) ]
FA_2019 = FA_2019[  , -c(9:32) ]

##### Merge the data #####
## Remove MiLB deals by merging the MiLB info from ESPN dfs with the BR dfs
FA_2016 = ESPN2016_FAs %>%
  select(Name, Type) %>%
  filter(Type == "MLB") %>%
  select(Name) %>%
  inner_join(FA_2016, by = "Name") %>%
  select(Name, Date, `To Team`) %>%
  collect()

FA_2017 = ESPN2017_FAs %>%
  select(Name, Type) %>%
  filter(Type == "MLB") %>%
  select(Name) %>%
  inner_join(FA_2017, by = "Name") %>%
  select(Name, Date, `To Team`) %>%
  collect()

FA_2018 = ESPN2018_FAs %>%
  select(Name, Type) %>%
  filter(Type == "MLB") %>%
  select(Name) %>%
  inner_join(FA_2018, by = "Name") %>%
  select(Name, Date, `To Team`) %>%
  collect()

FA_2019 = ESPN2019_FAs %>%
  select(Name, Type) %>%
  filter(Type == "MLB") %>%
  select(Name) %>%
  inner_join(FA_2019, by = "Name") %>%
  select(Name, Date, `To Team`) %>%
  collect()


##### Add year and month strings for plotting #####
## Create a year grouping column for the BR dfs
FA_2016$Year = "2016"
FA_2017$Year = "2017"
FA_2018$Year = "2018"
FA_2019$Year = "2019"

  ## Replace the dates with months for each month in each BR df
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2016-10") == TRUE) ] = "Oct"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2016-11") == TRUE) ] = "Nov"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2016-12") == TRUE) ] = "Dec"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2017-01") == TRUE) ] = "Jan"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2017-02") == TRUE) ] = "Feb"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2017-03") == TRUE) ] = "Mar"
FA_2016$Date[ which(grepl(FA_2016$Date, pattern = "2017-04") == TRUE) ] = "Apr"

FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2017-10") == TRUE) ] = "Oct"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2017-11") == TRUE) ] = "Nov"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2017-12") == TRUE) ] = "Dec"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2018-01") == TRUE) ] = "Jan"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2018-02") == TRUE) ] = "Feb"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2018-03") == TRUE) ] = "Mar"
FA_2017$Date[ which(grepl(FA_2017$Date, pattern = "2018-04") == TRUE) ] = "Apr"

FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2018-10") == TRUE) ] = "Oct"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2018-11") == TRUE) ] = "Nov"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2018-12") == TRUE) ] = "Dec"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2019-01") == TRUE) ] = "Jan"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2019-02") == TRUE) ] = "Feb"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2019-03") == TRUE) ] = "Mar"
FA_2018$Date[ which(grepl(FA_2018$Date, pattern = "2019-04") == TRUE) ] = "Apr"

FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2019-10") == TRUE) ] = "Oct"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2019-11") == TRUE) ] = "Nov"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2019-12") == TRUE) ] = "Dec"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2020-01") == TRUE) ] = "Jan"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2020-02") == TRUE) ] = "Feb"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2020-03") == TRUE) ] = "Mar"
FA_2019$Date[ which(grepl(FA_2019$Date, pattern = "2020-04") == TRUE) ] = "Apr"

  ## Print the numbers for each month in each BR df
length(which(FA_2016$Date == "Oct"))
length(which(FA_2016$Date == "Nov"))
length(which(FA_2016$Date == "Dec"))
length(which(FA_2016$Date == "Jan"))
length(which(FA_2016$Date == "Feb"))
length(which(FA_2016$Date == "Mar"))
length(which(FA_2016$Date == "Apr"))

length(which(FA_2017$Date == "Oct"))
length(which(FA_2017$Date == "Nov"))
length(which(FA_2017$Date == "Dec"))
length(which(FA_2017$Date == "Jan"))
length(which(FA_2017$Date == "Feb"))
length(which(FA_2017$Date == "Mar"))
length(which(FA_2017$Date == "Apr"))

length(which(FA_2018$Date == "Oct"))
length(which(FA_2018$Date == "Nov"))
length(which(FA_2018$Date == "Dec"))
length(which(FA_2018$Date == "Jan"))
length(which(FA_2018$Date == "Feb"))
length(which(FA_2018$Date == "Mar"))
length(which(FA_2018$Date == "Apr"))

length(which(FA_2019$Date == "Oct"))
length(which(FA_2019$Date == "Nov"))
length(which(FA_2019$Date == "Dec"))
length(which(FA_2019$Date == "Jan"))
length(which(FA_2019$Date == "Feb"))
length(which(FA_2019$Date == "Mar"))
length(which(FA_2019$Date == "Apr"))

##### Combine data into one df for ggplotting ##### 

FA_2016_CBA = bind_rows(FA_2016, FA_2017, FA_2018, FA_2019)
FA_2016_CBA = FA_2016_CBA[ -which(grepl(FA_2016_CBA$Date, pattern = "2") == TRUE), ]

##### GGplot #####

FAhist = ggplot(FA_2016_CBA, aes(x = factor(Date, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")), color = Year, fill = Year)) +
  geom_histogram(position = "dodge", alpha = 0.2, stat = "count") +
  scale_color_manual(values = c("firebrick", "darkgoldenrod1", "darkgreen", "navyblue")) +
  scale_fill_manual(values = c("firebrick", "darkgoldenrod1", "darkgreen", "navyblue")) +
  labs(title = " Free-Agent Contracts\nfrom 2016-2019", x = "Month Contract was Signed", y = "Number of Contracts", hjust = 0.5) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = c(0.8,0.8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
FAhist