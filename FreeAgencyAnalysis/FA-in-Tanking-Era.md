Free-Agency in the Tanking Era
================
Erik Larsen
1/16/2021

## Load packages

``` r
library(readr)
library(tidyverse)
library(rvest)
library(XML)
library(ggplot2)
library(ggrepel)
library(ggpubr)
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
## ESPN 2021 free-agent signings
ESPN_selector = "#my-players-table"

ESPN2021_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2021/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2020 free-agent signings
ESPN2020_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2020/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2019 free-agent signings
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

## ESPN 2015 free-agent signings
ESPN2015_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2015/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2014 free-agent signings
ESPN2014_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2014/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2013 free-agent signings
ESPN2013_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2013/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2012 free-agent signings
ESPN2012_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2012/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()

## ESPN 2011 free-agent signings
ESPN2011_FAs = read_html("https://www.espn.com/mlb/freeagents/_/year/2011/type/signed") %>%
  html_element(css = ESPN_selector) %>%
  html_table()
```

## Clean ESPN’s data

Create a function that “cleans” ESPN’s dfs (not shown)

Call each df into the function and overwrite the variable

``` r
ESPN2011_FAs = clean_ESPN_FA_fn(df = ESPN2011_FAs, Year = "2011")
ESPN2012_FAs = clean_ESPN_FA_fn(df = ESPN2012_FAs, Year = "2012")
ESPN2013_FAs = clean_ESPN_FA_fn(df = ESPN2013_FAs, Year = "2013")
ESPN2014_FAs = clean_ESPN_FA_fn(df = ESPN2014_FAs, Year = "2014")
ESPN2015_FAs = clean_ESPN_FA_fn(df = ESPN2015_FAs, Year = "2015")
ESPN2016_FAs = clean_ESPN_FA_fn(df = ESPN2016_FAs, Year = "2016")
ESPN2017_FAs = clean_ESPN_FA_fn(df = ESPN2017_FAs, Year = "2017")
ESPN2018_FAs = clean_ESPN_FA_fn(df = ESPN2018_FAs, Year = "2018")
ESPN2019_FAs = clean_ESPN_FA_fn(df = ESPN2019_FAs, Year = "2019")
ESPN2020_FAs = clean_ESPN_FA_fn(df = ESPN2020_FAs, Year = "2020")
ESPN2021_FAs = clean_ESPN_FA_fn(df = ESPN2021_FAs, Year = "2021")
```

Add an “Age” column that contains the player’s age at the time of
signing. All values are relative to when the data was accessed (year
2021), so adjust each df’s values accordingly.

``` r
ESPN2011_FAs$Age = as.numeric(ESPN2011_FAs$Age) - 10
ESPN2012_FAs$Age = as.numeric(ESPN2012_FAs$Age) - 9
ESPN2013_FAs$Age = as.numeric(ESPN2013_FAs$Age) - 8
ESPN2014_FAs$Age = as.numeric(ESPN2014_FAs$Age) - 7
ESPN2015_FAs$Age = as.numeric(ESPN2015_FAs$Age) - 6
ESPN2016_FAs$Age = as.numeric(ESPN2016_FAs$Age) - 5
ESPN2017_FAs$Age = as.numeric(ESPN2017_FAs$Age) - 4
ESPN2018_FAs$Age = as.numeric(ESPN2018_FAs$Age) - 3
ESPN2019_FAs$Age = as.numeric(ESPN2019_FAs$Age) - 2
ESPN2020_FAs$Age = as.numeric(ESPN2020_FAs$Age) - 1
ESPN2021_FAs$Age = as.numeric(ESPN2021_FAs$Age)
```

Combine the dfs

``` r
ESPN = bind_rows(ESPN2011_FAs, ESPN2012_FAs, ESPN2013_FAs, ESPN2014_FAs, ESPN2015_FAs,
          ESPN2016_FAs, ESPN2017_FAs, ESPN2018_FAs, ESPN2019_FAs, ESPN2020_FAs,
          ESPN2021_FAs)
```

Create a df to house compiled contract data from the ESPN dfs

``` r
  ## Create a 11x14 matrix
FREE_AGENTS = matrix(nrow = 11, ncol = 14)
  ## Convert to dataframe
FREE_AGENTS = as.data.frame(FREE_AGENTS)
  ## Add column names
colnames(FREE_AGENTS) = c("Year", "ML Deals", "MiL Deals", "Total Deals", "% ML Deals", "Total Spent",
                          "Avg. Total Contract", "Avg. Length", "Med. Length", "Avg. AAV",
                          "Med. Total Contract", "Med. AAV", "Avg. Age", "Med. Age")
  ## Loop through and add year values to the year column
for (i in 1:nrow(FREE_AGENTS)){
  FREE_AGENTS[i,1] = seq(2011, 2021, 1)[i]
}
```

Populate the dataframe (not shown)

## Graphs

Graph the total number of major league contracts signed and the total
money spent on those contracts (from 2011-2021)
![](https://github.com/eriklarsen4/Baseball/blob/main/FreeAgencyAnalysis/FA%20in%20Tanking%20Era%20Figs/Global%20Major%20League%20FA%20Contract%20Trends-1.png)<!-- -->

Graph the means and medians of total contract value, AAV, length of
contract, and age of player for all major league free-agent deals from
2011-2021

``` r
  ## Graph on one plot
ggarrange(MeanMedLength, MeanMedAAV, MeanMedTotVal, MeanMedAge,
          ncol = 2, nrow = 2, common.legend = FALSE, labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/FreeAgencyAnalysis/FA%20in%20Tanking%20Era%20Figs/Mean%20and%20Median%20MLB%20FA%20Total%20Contract%20Value%20AAV%20Contract%20Length%20and%20Player%20Age-1.png)<!-- -->

Graph minor league contract trends.

-   First add the data (not shown)

-   Now graph that data

``` r
ggarrange(MiLdealPerCentTrend, MiLMedMeanAge, ncol = 2, nrow = 1, common.legend = FALSE, labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/FreeAgencyAnalysis/FA%20in%20Tanking%20Era%20Figs/Minor%20League%20Contract%20data-1.png)<!-- -->

Graph the distributions of contracts; density plots by year, and
boxplots of all teams from 2011-2019

``` r
ggarrange(AAVdensity, FA_AAV_bp, ncol = 2, nrow = 1, common.legend = FALSE, labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/FreeAgencyAnalysis/FA%20in%20Tanking%20Era%20Figs/MLB%20FA%20Contract%20AAV%20Distributions-1.png)<!-- -->
