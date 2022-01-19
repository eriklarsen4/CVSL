MLB Free Agency during 2016 CBA
================

## Load packages

``` r
library(readr)
library(tidyverse)
library(rvest)
library(XML)
library(ggplot2)
```

## Scrape Baseball Reference’s Free Agent Contract Data

``` r
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
```

## Scrape ESPN’s Free Agent Tracker Data

``` r
##### Scrape ESPN's free-agent contract data #####
## ESPN 2019 free-agent signings
ESPN_selector = "#my-players-table"

ESPN2019_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2019/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2018 free-agent signings
ESPN2018_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2018/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2017 free-agent signings
ESPN2017_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2017/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2016 free-agent signings
ESPN2016_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2016/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()
```

## Clean ESPN’s data

Create a function that “cleans” ESPN’s dfs

Call each df into the function, and overwrite the stored variables

``` r
ESPN2016_FAs = clean_ESPN_FA_fn(df = ESPN2016_FAs, Year = "2016")
ESPN2017_FAs = clean_ESPN_FA_fn(df = ESPN2017_FAs, Year = "2017")
ESPN2018_FAs = clean_ESPN_FA_fn(df = ESPN2018_FAs, Year = "2018")
ESPN2019_FAs = clean_ESPN_FA_fn(df = ESPN2019_FAs, Year = "2019")

  ## Remove extra columns (metrics)
FA_2016 = FA_2016[  , -c(9:32) ]
FA_2017 = FA_2017[  , -c(9:32) ]
FA_2018 = FA_2018[  , -c(9:32) ]
FA_2019 = FA_2019[  , -c(9:32) ]
```

## Merge the Data

``` r
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
```

Add year and month strings for plotting (not shown)

Combine the dfs of each year

``` r
##### Combine data into one df for ggplotting ##### 

FA_2016_CBA = bind_rows(FA_2016, FA_2017, FA_2018, FA_2019)
FA_2016_CBA = FA_2016_CBA[ -which(grepl(FA_2016_CBA$Date, pattern = "2") == TRUE), ]
```

## Plot the results

``` r
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
```

![](Free-Agency_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
