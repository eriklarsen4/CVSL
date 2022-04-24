Tanking Graphs
================
Erik Larsen
4/23/2022

This snippet was used to generate the graphs in my article (currently
under review) on [TheLeftyCatcher](https://www.theleftycatcher.com),
where I compared and contrasted how `Jeff Luhnow` and `Theo Epstein`
tanked to rebuild the `Chicago Cubs` and `Houston Astros`, beginning in
\~2011 into World Series winners. I acquired `MLB Free Agent` data,
`MLB Amateur Draft` data, `International Signing` data, `MLB Standings`
data, `MLB Payroll` data, and “`MLB Value`” data. I scraped or
downloaded this data from multiple sites, including
`Baseball Reference`, `ESPN`, and `Baseball Almanac`. It required some
processing to compile and analyze with or without graphs.

The data wrangling script to generate the data used for plotting is
located
[here](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Data%20Wrangling.R).
The global environment is located
[here](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/tanking2E.RData),
and the downloaded data files are located
[here](https://github.com/eriklarsen4/Baseball/tree/main/Tanking/Player%20Value%20files).

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

## Load the Tanking Data Wrangling Environment

``` r
load("https://github.com//eriklarsen4//Baseball//blob//main//Tanking//tanking2E.RData")
```

## Build models

Payroll, W%

``` r
Sal_W_model = lm(W_L~Salary, data = TeamValALL[-which(TeamValALL$Year == 2020), ])
round(summary(Sal_W_model)$r.squared, digits = 3)
```

    ## [1] 0.176

bWAR, W%

``` r
WL_WAR_lm = lm(data = TeamValALL[-which(TeamValALL$Year == 2020),], formula = W_L~WAR)
summary(WL_WAR_lm)$r.squared
```

    ## [1] 0.8385079

## Derive Team Color Codes

Use colors from previous piece on [TheLeftyCatcher.com]()

``` r
color_code = matrix(nrow = 30, ncol = 4)
color_code = as.data.frame(color_code)
colnames(color_code) = c("ESPNTeam", "ESPNcolor", "currentTeam", "currentcolor")
color_code$ESPNTeam = unique(ESPN$Team)[order(unique(ESPN$Team))]
color_code$ESPNcolor = c("firebrick1", "orange3", "forestgreen", "deepskyblue", "firebrick", "navy", "firebrick3",
                         "dodgerblue", "darkred", "dodgerblue", "darkorange1", "darkblue", "mediumseagreen", "black",
                         "blue", "red2", "darkorange1", "tan4", "red", "black", "blue", "navy", "firebrick", "red",
                         "black", "royalblue", "midnightblue", "navy", "black", "navyblue")
  ## For Current piece
color_code$currentTeam = unique(StandALL$Tm)[order(unique(StandALL$Tm))]
color_code$currentcolor = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                            "black", "red", "darkblue", "purple", "midnightblue",
                            "orange3", "royalblue", "firebrick1", "dodgerblue", "turquoise",
                            "goldenrod", "navy", "blue", "navyblue", "forestgreen",
                            "red", "black", "tan4", "darkorange1", "mediumseagreen",
                            "firebrick3", "navy", "blue", "deepskyblue", "red2")
```

## W%s 2011-2021

Graph every team’s win percentage from 2011-2021, omitting 2020. Fit the
`Sal_W_model` line

``` r
  ## Graph all W-L %s
WL = ggplot(data = TeamValALL[-which(TeamValALL$Year == 2020),]) +
  geom_point(aes(x = Salary, y = W_L, color = Tm), size = 4, alpha = 0.7) +
  geom_abline(slope = as.numeric(coefficients(Sal_W_model)[2]),
              intercept = as.numeric(coefficients(Sal_W_model)[1]),
              color = "black",
              size = 1) +
  geom_text(x = 380, y = 0.620,
            label = expression(paste(("R")^"2", "= 0.176")), color = "black") +
  coord_cartesian(xlim = c(17,420), ylim = c(0.3, 0.750)) +
  labs(title = "MLB Team Performance Regression\n2011-2021",
       x = "Team Payroll (in M $US)",
       y = "W-L %",
       color = "Team") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                                "black", "red", "darkblue", "purple", "midnightblue",
                                "orange3", "royalblue", "firebrick1", "dodgerblue", "turquoise",
                                "goldenrod", "navy", "blue", "navyblue", "forestgreen",
                                "red", "black", "tan4", "darkorange1", "mediumseagreen",
                                "firebrick3", "navy", "blue", "deepskyblue", "red2"))
WL
```

![](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Figs/Wps%20and%20payroll%20wo2020%20w475h475.jpeg)<!-- -->

## Astros and Cubs W%s, Payrolls, and FA Contracts

Create the ggplot objects

`W%`

``` r
  ## Graph the Astros and Cubs' W-L %s
CubsStros_WL = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `Win%`, color = Tm)) +
  geom_point(aes(y = `Win%`, color = Tm)) +
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2014, y = 0.7, label = "Epstein, Luhnow\nHired in Off-Season", color = "firebrick", size = 3) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0.280, 0.750)) +
  labs(title = "Cubs & Astros' W%s\n2011-2021", x = "Season", y = "Win %", color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.position = c(0.20,0.85),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(labels = c("Cubs", "Astros", "League Average"),
                     values = c("dodgerblue", "orange1", "black"))
```

`Payrolls`

``` r
  ## Graph the Astros and Cubs' Payrolls
CubsStros_Payrolls = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `Team payroll`, color = Tm)) +
  geom_point(aes(y = `Team payroll`, color = Tm)) +
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2014, y = 315, label = "Epstein, Luhnow\nHired in Off-Season", color = "firebrick", size = 3) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(15, 350)) +
  labs(title = "Cubs & Astros' Team\nPayrolls, 2011-2021", x = "Season", y = "Team Payroll (in M $US)", color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.position = c(0.20,0.85),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("dodgerblue", "orange1", "black"))
```

`Mean MLB Free-Agent Contract AAVs`

``` r
CubsStros_FA = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `Mean FA AAV`, color = Tm)) +
  geom_point(aes(y = `Mean FA AAV`, color = Tm)) +
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2014, y = 15.3, label = "Epstein, Luhnow\nHired in Off-Season", color = "firebrick", size = 3) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 17)) +
  labs(title = "Cubs & Astros' MLB\nFA Contracts, 2011-2021", x = "Off-Season", y = "Mean MLB Free-Agent Contract AAV (in M $US)", color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.position = c(0.20,0.85),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(labels = c("Cubs", "Astros", "League Average"),
                     values = c("dodgerblue", "orange1", "black"))
```

Plot the objects together

``` r
ggarrange(CubsStros_WL, CubsStros_Payrolls, CubsStros_FA,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Figs/AstrosCubsGroupTrends%20wo2020%20w900h480.jpeg)<!-- -->

## Astros and Cubs Amateur Data

Create the `College Picks` `ggplot` object

``` r
CubsStros_College_Picks = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `College %`, color = Tm)) +
  geom_point(aes(y = `College %`, color = Tm)) + 
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2013, y = 50,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick",
            size = 3) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 100)) +
  labs(title = "College Amateurs Drafted\n2011-2021",
       x = "Season",
       y = "College Players Taken in Amateur Draft (%)",
       color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.position = c(0.35,0.20),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(labels = c("Cubs", "Astros", "League Average"),
                     values = c("dodgerblue", "orange1", "black"))
```

Create the `Intl Signs` `ggplot` object

``` r
CubsStros_Intl_Signs = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),],
                              aes(x = Year)) +
  geom_line(aes(y = `# Int'l Signings`, color = Tm)) +
  geom_point(aes(y = `# Int'l Signings`, color = Tm)) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 20)) +
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2013, y = 10,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick", size = 3) +
  labs(title = "International Signings\n2011-2021",
       x = "Season", y = "# International Signings",
       color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.position = c(0.20,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(labels = c("Cubs", "Astros", "League Average"),
                     values = c("dodgerblue", "orange1", "black"))
```

Plot the objects together

``` r
ggarrange(CubsStros_College_Picks, CubsStros_Intl_Signs,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Figs/AstrosCubsTalentAcqGroup%20wo2020%20w800h480.jpeg)<!-- -->

## Which Teams Qualify as Tankers

Subset the `TeamValALL` `df` to prep the `tankingtable`

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
colnames(REBUILDERS_df)[c(8:13)] = c("MLB Trades Made", "FA Signings",
                                     "Minors Callups", "Payroll Sum",
                                     "bWAR", "Payroll (in M $US)")
print(REBUILDERS_df[c(4,5,7:11,13:16,20:27,31:38,41,42,45,46,52:54),-11], n = 34)
```

    ## # A tibble: 34 x 16
    ##    Tm           W     L   W_L    GB DivRank  Year `MLB Trades Ma~` `FA Signings`
    ##    <chr>    <int> <int> <dbl> <dbl>   <int> <dbl>            <int>         <int>
    ##  1 Atlanta~    67    95 0.414  23         4  2015                8            18
    ##  2 Atlanta~    68    93 0.422  26.5       5  2016               17            16
    ##  3 Baltimo~    47   115 0.29   61         5  2018                9            13
    ##  4 Baltimo~    54   108 0.333  49         5  2019               12            10
    ##  5 Baltimo~    52   110 0.321  48         5  2021                6            13
    ##  6 Chicago~    61   101 0.377  36         4  2012                7            10
    ##  7 Chicago~    66    96 0.407  31         5  2013                5            16
    ##  8 Chicago~    67    95 0.414  35         4  2017                8            10
    ##  9 Chicago~    62   100 0.383  29         4  2018               13            11
    ## 10 Cincinn~    64    98 0.395  36         5  2015                7            15
    ## 11 Cincinn~    68    94 0.42   35.5       5  2016               12            15
    ## 12 Colorad~    66    96 0.407  28         4  2014               13            15
    ## 13 Colorad~    68    94 0.42   24         5  2015                6            17
    ## 14 Detroit~    64    98 0.395  38         5  2017               17            11
    ## 15 Detroit~    64    98 0.395  27         3  2018               13            11
    ## 16 Detroit~    47   114 0.292  53.5       5  2019               11            15
    ## 17 Houston~    56   106 0.346  40         5  2011               11             9
    ## 18 Houston~    55   107 0.34   42         5  2012                7            10
    ## 19 Houston~    51   111 0.315  45         5  2013               15             7
    ## 20 Miami M~    69    93 0.426  29         4  2012                8            12
    ## 21 Miami M~    62   100 0.383  34         5  2013               13            19
    ## 22 Miami M~    63    98 0.391  26.5       5  2018               17            11
    ## 23 Miami M~    57   105 0.352  40         5  2019               15            12
    ## 24 Miami M~    67    95 0.414  21.5       4  2021               21            12
    ## 25 Minneso~    63    99 0.389  32         5  2011                5             9
    ## 26 Minneso~    66    96 0.407  22         5  2012                2            13
    ## 27 Minneso~    66    96 0.407  27         4  2013                3            14
    ## 28 Oakland~    68    94 0.42   20         5  2015               17            10
    ## 29 Oakland~    69    93 0.426  26         5  2016               19             6
    ## 30 Pittsbu~    69    93 0.426  22         5  2019               13             7
    ## 31 Pittsbu~    61   101 0.377  34         5  2021               13             7
    ## 32 Texas R~    67    95 0.414  31         5  2014               10            20
    ## 33 Texas R~    67    95 0.414  36         5  2018                3            12
    ## 34 Texas R~    60   102 0.37   35         5  2021               11            14
    ## # ... with 7 more variables: `Minors Callups` <int>, bWAR <dbl>,
    ## #   `Payroll (in M $US)` <dbl>, WARPM <dbl>, labs <chr>, ExpW_L <dbl>,
    ## #   Diff_W_L <dbl>

``` r
  ## Remove non-qualifiers
Printable_REBUILD = REBUILDERS_df[c(4,5,7:11,13:16,20:27,31:38,41,42,45,46),-11]

  ## Re-arrange for display
Printable_REBUILD = Printable_REBUILD %>%
  select(`Tm`, Year, W, L, W_L, GB, DivRank,
         `MLB Trades Made`, `FA Signings`,
         `Minors Callups`, bWAR, `Payroll (in M $US)`)
```

Create the `Rebiuld_T` table

``` r
  ## plot
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

## W%s and bWAR Data

Create the `WL_WAR` `ggplot` object; graphs `W%` and team `bWAR`

``` r
WL_WAR = ggplot(data = TeamValALL[-which(TeamValALL$Year == 2020),]) +
  geom_point(aes(x = TeamValALL$WAR[-which(TeamValALL$Year == 2020)],
                 y = TeamValALL$W_L[-which(TeamValALL$Year == 2020)],
                 color = TeamValALL$Tm[-which(TeamValALL$Year == 2020)]),
             size = 4,
             alpha = 0.5) +
  geom_abline(slope = as.numeric(coefficients(WL_WAR_lm)[2]),
              intercept = as.numeric(coefficients(WL_WAR_lm)[1]),
              color = "black",
              size = 1) +
  geom_text(x = 59, y = 0.7, label = expression(paste(("R")^"2", "= 0.822")), color = "black") +
  coord_cartesian(xlim = c(7,68), ylim = c(0.280, 0.750)) +
  labs(title = "MLB Team Performance Regression\n2011-2021",
       x = "Team bWAR",
       y = "W-L %",
       color = "Team") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                                "black", "red", "darkblue", "purple", "midnightblue",
                                "orange3", "royalblue", "firebrick1", "dodgerblue", "turquoise",
                                "goldenrod", "navy", "blue", "navyblue", "forestgreen",
                                "red", "black", "tan4", "darkorange1", "mediumseagreen",
                                "firebrick3", "navy", "blue", "deepskyblue", "red2"))
```

Correct the `Guardians` team name; create the `WARPM` column Add
appropriate labels to the teams with the top `WARPM`

``` r
TeamValALL$Tm[ which(TeamValALL$Tm == "Cleveland Indians") ] = "Cleveland Guardians"
TeamValALL$WARPM = as.numeric(TeamValALL$WAR) / as.numeric(TeamValALL$Salary)
TeamValALL$labs = ""
TeamValALL$labs[ order(TeamValALL$WARPM, decreasing = TRUE)[c(1:14)] ] = c("'11 Rays", "'18 Rays", "'21 Guardians", "'11 Royals", "'21 Rays", "'21 Marlins", "'12 A's", "'13 Marlins", "'14 Marlins", "'19 Rays", "'12 Rays", "'13 A's", "'15 Rays", "'18 A's")
```

Create the `W%` and `WARPM` `ggplot` object

``` r
WL_WARPM = ggplot(data = TeamValALL[ -which(TeamValALL$Year == 2020), ]) +
  geom_point(aes(x = WARPM, y = W_L, color = Tm), alpha = 0.7, size = 4) +
  coord_cartesian(xlim = c(min(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM ),
                           max(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM )),
                  ylim = c(0.280, 0.750)) +
  labs(title = "MLB Team W%s \nand Payroll Efficiencies, 2011-2021",
       x = "WARPM: Team bWAR / Team Payroll (in M $US)",
       y = "W-L %",
       color = "Team") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none",
        legend.title = element_text(size = 13),
        #legend.direction = "horizontal",
        legend.text = element_text(size = 11),
        #legend.position = c(0.30,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c(color_code$currentcolor))

WL_WARPM = WL_WARPM +
  geom_text_repel(data = TeamValALL[ -which(TeamValALL$Year == 2020), ],
                  x = TeamValALL$WARPM[ -which(TeamValALL$Year == 2020) ],
                  y = TeamValALL$W_L[ -which(TeamValALL$Year == 2020) ],
                  color = "black", aes(label = labs), size = 3, max.overlaps = Inf,
                  min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)
```

Now prep data for the `WARPM` and `W-L%` plot subsetted on the
rebuilding teams

``` r
      ## Filter by teams with the same or worse win total as the 2011 Cubs; outside the pandemic
rebuild_idx = which(TeamValALL$W <= 70 & TeamValALL$Year != 2020)
rebuild_list = unique(TeamValALL$Tm[rebuild_idx])
  ## Subset a new df, add in the "efficiency" stat
rebuild_filtered_StandALL = TeamValALL[ rebuild_idx, ]
rebuild_filtered_StandALL$WARPM =
  as.numeric(rebuild_filtered_StandALL$WAR) / as.numeric(rebuild_filtered_StandALL$Salary)
rebuild_filtered_StandALL$labs = ""
rebuild_filtered_StandALL$labs[ which(rebuild_filtered_StandALL$Salary <= 100)] =
  c("'11 Astros", "'12 Astros", "'13 Twins", "'13 Marlins", "'13 Astros",
    "'14 Astros", "'15 A's", "'15 Phils", "'16 A's", "'16 Rays",
    "'16 Braves", "'16 Padres", "'19 Pirates", "'21 Marlins", "'21 Pirates",
    "'21 Rangers", "'21 O's")
rebuild_filtered_StandALL$Tm[ which(rebuild_filtered_StandALL$Tm == "Cleveland Indians")] = "Cleveland Guardians"
```

Create the tanking subset `ggplot` object

``` r
WL_rebuild = ggplot(data = rebuild_filtered_StandALL) +
  geom_point(aes(x = WARPM, y = W_L, color = Tm), alpha = 0.7, size = 4) +
  coord_cartesian(xlim = c(min(rebuild_filtered_StandALL$WARPM ),
                           max(rebuild_filtered_StandALL$WARPM )),
                  ylim = c(0.280, 0.750)) +
  labs(title = "Re-building Teams' W%s \nand Payroll Efficiencies, 2011-2021",
       x = "WARPM: Team bWAR / Team Payroll (in M $US)",
       y = "W-L %",
       color = "Team") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                                "black", "red", "darkblue", "purple", "midnightblue",
                                "orange3", "royalblue", "turquoise", "goldenrod", "navy",
                                "blue", "forestgreen", "red", "black", "tan4",
                                "darkorange1", "mediumseagreen", "navy", "blue", "deepskyblue", "red2"))
WL_rebuild = WL_rebuild +
  geom_text_repel(data = rebuild_filtered_StandALL,
                  x = rebuild_filtered_StandALL$WARPM,
                  y = rebuild_filtered_StandALL$W_L,
                  color = "black",
                  aes(label = labs),
                  size = 3,
                  max.overlaps = Inf,
                  min.segment.length = 0,
                  xlim = c(0,NA),
                  ylim = c(0,NA),
                  box.padding = 0.5)
```

Plot the `W%` and `WARPM` objects together in a plot

``` r
ggarrange(WL_WAR, WL_WARPM, WL_rebuild,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking%20Figs/Ws%20bWAR%20and%20Payroll%20wo2020%20w1550h500.jpeg)<!-- -->

## Grading Tanks

Create the `Grading_Tanks` `df` that houses the tanking clubs’ `Win`
totals, `Years in Tank`, `Years Not in Tank`, and `Rebuild Grade`

`Rebuild Grade` is the average of `Win`s across subsequent seasons after
tanking

``` r
Grading_Tanks = matrix(nrow = 10, ncol = 3)
Grading_Tanks = as.data.frame(Grading_Tanks)
colnames(Grading_Tanks) = c("Tm", "Win Sum", "Years in Tank")
Grading_Tanks$Tm = c("Atlanta Braves", "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Miami Marlins", "Minnesota Twins", "Oakland Athletics")

Grading_Tanks$`Win Sum` = c(sum(72,90,97,88),
                            sum(73,97,103,92,95,84,71),
                            sum(72,93),
                            sum(68,67,75,83),
                            sum(75,87,91,71,74),
                            sum(77),
                            sum(86,84,101,103,107,95),
                            sum(77,71,79,77),
                            sum(70,83),
                            sum(75,97,97,86))
Grading_Tanks$`Years in Tank` = c(2,2,2,2,2,1,3,2,2,2)
Grading_Tanks$`Years Not in Tank` = c(4,7,2,4,5,1,6,4,2,4)
Grading_Tanks$`Rebuild Grade` = round(Grading_Tanks$`Win Sum` / Grading_Tanks$`Years Not in Tank`, digits = 1)
```

Re-arrange the `df` in order of `Rebuild Grade` and add the tank years
to the team name

``` r
Grading_Tanks = Grading_Tanks %>%
  arrange(desc(`Rebuild Grade`))

Grading_Tanks$Tm = c("Houston Astros ('11-'13)", "Oakland Athletics ('15-'16)", "Chicago Cubs ('12-'13)", "Atlanta Braves ('15-'16)",
                     "Chicago White Sox ('17-'18)", "Colorado Rockies ('14-'15)", "Detroit Tigers ('17-'19)", "Minnesota Twins ('11-'13)",
                     "Miami Marlins ('12-'13)", "Cincinnati Reds ('15-'16)")
```

Create the `plotly` object (table), `Grading_T`

``` r
Grading_T = plot_ly(
  type = 'table',
  columnwidth = c(rep(20,5)),
  columnorder = c(seq(1,5,1)),
  header = list(
    values = c(colnames(Grading_Tanks)),
    align = c("center", "center"),
    line = list(width = 1, color = 'black'),
    fill = list(color = c("grey", "grey")),
    font = list(family = "Times", size = 19, color = "black")
  ),
  cells = list(
    values = rbind(Grading_Tanks$Tm,
                   Grading_Tanks$`Win Sum`,
                   Grading_Tanks$`Years in Tank`,
                   Grading_Tanks$`Years Not in Tank`,
                   Grading_Tanks$`Rebuild Grade`
                   ),
    align = c("center", "center"),
    line = list(color = "black", width = 1),
    font = list(family = "Times", size = 17, color = c("black"))
  ))
```
