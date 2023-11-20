Tanking Graphs
================
Erik Larsen
4/23/2022

# Overview

This snippet was used to generate the graphs in [my
article](https://www.theleftycatcher.com/post/diving-deep-into-tanking)
on [TheLeftyCatcher](https://www.theleftycatcher.com), where I compared
and contrasted how `Jeff Luhnow` and `Theo Epstein` tanked to rebuild
the `Chicago Cubs` and `Houston Astros` into World Series winners.

To analyze tanking, I acquired:

-   `MLB Free Agent` data (via `ESPN`)

-   `MLB Amateur Draft` data (via `Baseball Almanac`)

-   `International Signing` data (via `Spotrac`)

-   `MLB Standings` data, `MLB Payroll` data, and “`MLB Value`” data
    (via `Baseball Reference`)

The data wrangling markdown file to acquire and prepare the data for
plotting is located
[here](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/Tanking-Data-Wrangling.md).

The global environment is located
[here](https://github.com/eriklarsen4/Baseball/blob/main/Tanking/WranglingEnvironment.RData).

# Environment Prep

## Load packages

``` r
  ## Data wrangling; df re-arrangement, string manipulations
library(readr)
# library(reshape2)
library(tidyverse)
# library(stringr)

  ## Web scraping
library(rvest)
library(XML)

  ## Plotting
library(ggplot2)
library(ggrepel)
# library(GGally)
library(ggpubr)
library(plotly)

  ## Modeling
library(mgcv)
library(splines)
```

## Load the Tanking Data Wrangling Environment

``` r
load("~/GitHub/Baseball/Tanking/WranglingEnvironment.RData")
```

# Build Models

## Build the WL% \~ Payroll Model

Model `team win-loss percentage`, using linear regression with `payroll`
as the sole predictor

``` r
Sal_W_model = lm(W_L~Salary, data = TeamValALL[-which(TeamValALL$Year == 2020), ])
round(summary(Sal_W_model)$r.squared, digits = 3)
```

    ## [1] 0.176

## Build bWAR \~ W% Model

Model `team win-loss percentage`, using linear regression with `WAR` as
the sole predictor

``` r
WL_WAR_lm = lm(data = TeamValALL[-which(TeamValALL$Year == 2020),], formula = W_L~WAR)
summary(WL_WAR_lm)$r.squared
```

    ## [1] 0.8374522

## Derive Team Color Codes

Use colors from previous piece on
[TheLeftyCatcher.com](https://www.theleftycatcher.com/post/free-agency-spending-trends-in-the-tanking-era)

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

# Plot Figure 1

## Creat ggplot objects

Plot every team’s win percentage from 2011-2021, omitting 2020

-   Fit the `Sal_W_model` line

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
  coord_cartesian(xlim = c(17,420),
                  ylim = c(0.3, 0.750)) +
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
```

## Render the data

``` r
WL
```

![](Tanking-Graphs_files/figure-gfm/Figure%201%20plot-1.png)<!-- -->

**Salary isn’t predictive of team performance in terms of wins and
losses**

# Plot Figure 2

## Create ggplot objects

Create the ggplot objects

`W%`

``` r
  ## Graph the Astros and Cubs' W-L %s
CubsStros_WL = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),],
                      aes(x = Year)) +
  geom_line(aes(y = `Win%`,
                color = Tm)) +
  geom_point(aes(y = `Win%`,
                 color = Tm)) +
  geom_vline(xintercept = 2011.5,
             linetype = "dashed",
             color = "firebrick")+
  geom_text(x = 2014,
            y = 0.7,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick",
            size = 3) +
  coord_cartesian(xlim = c(2011,2021),
                  ylim = c(0.280, 0.750)) +
  labs(title = "Cubs & Astros' W%s\n2011-2021",
       x = "Season",
       y = "Win %",
       color = "Stat") + 
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
CubsStros_Payrolls = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),],
                            aes(x = Year)) +
  geom_line(aes(y = `Team payroll`,
                color = Tm)) +
  geom_point(aes(y = `Team payroll`,
                 color = Tm)) +
  geom_vline(xintercept = 2011.5,
             linetype = "dashed",
             color = "firebrick")+
  geom_text(x = 2014,
            y = 315,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick",
            size = 3) +
  coord_cartesian(xlim = c(2011,2021),
                  ylim = c(15, 350)) +
  labs(title = "Cubs & Astros' Team\nPayrolls, 2011-2021",
       x = "Season",
       y = "Team Payroll (in M $US)",
       color = "Stat") + 
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
CubsStros_FA = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),],
                      aes(x = Year)) +
  geom_line(aes(y = `Mean FA AAV`,
                color = Tm)) +
  geom_point(aes(y = `Mean FA AAV`,
                 color = Tm)) +
  geom_vline(xintercept = 2011.5,
             linetype = "dashed",
             color = "firebrick")+
  geom_text(x = 2014,
            y = 15.3,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick",
            size = 3) +
  coord_cartesian(xlim = c(2011,2021),
                  ylim = c(0, 17)) +
  labs(title = "Cubs & Astros' MLB\nFA Contracts, 2011-2021",
       x = "Off-Season",
       y = "Mean MLB Free-Agent Contract AAV (in M $US)",
       color = "Stat") + 
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

## Render the data

Render the Astros’ and Cubs’ `W%`s, `Payroll`s, `FA Contract AAV`s
objects together

-   compare against league average

``` r
ggarrange(CubsStros_WL, CubsStros_Payrolls, CubsStros_FA,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](Tanking-Graphs_files/figure-gfm/Figure%202-1.png)<!-- -->

# Plot Figure 3

## Create ggplot objects

Create the `College Picks` `ggplot` object

``` r
CubsStros_College_Picks = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `College %`, color = Tm)) +
  geom_point(aes(y = `College %`, color = Tm)) + 
  geom_vline(xintercept = 2011.5,
             linetype = "dashed",
             color = "firebrick")+
  geom_text(x = 2013, y = 50,
            label = "Epstein, Luhnow\nHired in Off-Season",
            color = "firebrick",
            size = 3) +
  coord_cartesian(xlim = c(2011,2021),
                  ylim = c(0, 100)) +
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
  coord_cartesian(xlim = c(2011,2021),
                  ylim = c(0, 20)) +
  geom_vline(xintercept = 2011.5,
             linetype = "dashed",
             color = "firebrick")+
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

## Render the data

Render the objects together

``` r
ggarrange(CubsStros_College_Picks, CubsStros_Intl_Signs,
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](Tanking-Graphs_files/figure-gfm/Figure%203-1.png)<!-- -->

# Plot Table 1

Which Teams Qualify as Tankers?

## Create the plotly object

Subset the `TeamValALL` `df` to prep the `tankingtable`

``` r
REBUILDERS_df = TeamValALL %>%
  dplyr::filter(L >= 92) %>%
  dplyr::mutate(GB = as.double(GB)) %>%
  dplyr::arrange(Year) %>%
  dplyr::slice(-c(9,10,19,22,26,29,36,40,47,53,54))

## Re-arrange for display
Printable_REBUILD = REBUILDERS_df %>%
  dplyr::rename(`MLB Trades Made` = Sum_Num_Trades,
                `FA Signings` = Sum_Num_FAs,
                `Waiver Claims` = Sum_Num_Claims,
                `Total Acquisitions` = Sum_Acqs,
                `Payroll (in M $US)` = Salary,
                `Payroll Change (in M $US)` = Sum_Sal_Change,
                `Net Payroll (in M $US)` = Net_Salary,
                bWAR = WAR) %>%
  # dplyr::slice(-c(4,5,7:11,13:16,20:27,31:38,41,42,45,46)) %>%
  dplyr::select(Team, Year, W, L, `W-L%`,
                GB, DivRank, `MLB Trades Made`,
                `FA Signings`, `Waiver Claims`,
                `Total Acquisitions`,
                bWAR,
                `Payroll (in M $US)`,
                `Payroll Change (in M $US)`,
                `Net Payroll (in M $US)`) %>%
  dplyr::arrange(`Payroll Change (in M $US)`)
```

## Render the data

(table will not render approrpiately in github markdown format)

``` r
# Rebuild_T
Printable_REBUILD %>%
  dplyr::slice(c(1:10))
```

    ##                     Team Year  W   L  W-L%   GB DivRank MLB Trades Made
    ## 1   Washington Nationals 2021 65  97 0.401 23.5       5              12
    ## 2       San Diego Padres 2016 68  94 0.420 23.0       5              10
    ## 3         Detroit Tigers 2017 64  98 0.395 38.0       5               4
    ## 4      Chicago White Sox 2017 67  95 0.414 35.0       4               5
    ## 5  Philadelphia Phillies 2017 66  96 0.407 31.0       5               3
    ## 6      Baltimore Orioles 2018 47 115 0.290 61.0       5               7
    ## 7      Oakland Athletics 2016 69  93 0.426 26.0       5               7
    ## 8          Texas Rangers 2021 60 102 0.370 35.0       5               6
    ## 9      Chicago White Sox 2013 63  99 0.389 30.0       5               4
    ## 10          Chicago Cubs 2012 61 101 0.377 36.0       4               9
    ##    FA Signings Waiver Claims Total Acquisitions bWAR Payroll (in M $US)
    ## 1            3             2                 17 25.7          292.09029
    ## 2            5             3                 18 19.2           65.80140
    ## 3            2             0                  6 23.1          167.93920
    ## 4            6             1                 12 25.3          147.15100
    ## 5            4             3                 10 26.5          141.99050
    ## 6            1             2                 10 13.2          193.33083
    ## 7            3             0                 10 25.0           60.82413
    ## 8            3             0                  9 15.5           79.46533
    ## 9            0             1                  5 23.8          111.82930
    ## 10           3             2                 14 16.6          124.45520
    ##    Payroll Change (in M $US) Net Payroll (in M $US)
    ## 1                  -78.00348              214.08681
    ## 2                  -67.70570               -1.90430
    ## 3                  -66.57500              101.36420
    ## 4                  -59.44350               87.70750
    ## 5                  -45.91250               96.07800
    ## 6                  -44.71500              148.61583
    ## 7                  -42.50450               18.31963
    ## 8                  -37.96667               41.49867
    ## 9                  -31.52000               80.30930
    ## 10                 -30.63100               93.82420

# Plot Figure 4

## Create ggplot objects

Create the `WL_WAR` ggplot object:

-   graphs `W%` and team `bWAR`

-   include the model fit

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
  geom_text(x = 59,
            y = 0.7,
            label = expression(paste(("R")^"2", "= 0.822")),
            color = "black") +
  coord_cartesian(xlim = c(7,68),
                  ylim = c(0.280, 0.750)) +
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

-   correct the `Guardians` team name

-   create the `WARPM` column

-   add appropriate labels to the teams with the top `WARPM`

``` r
TeamValALL$Tm[ which(TeamValALL$Tm == "Cleveland Indians") ] = "Cleveland Guardians"
TeamValALL$WARPM = as.numeric(TeamValALL$WAR) / as.numeric(TeamValALL$Salary)
TeamValALL$labs = ""
TeamValALL$labs[ order(TeamValALL$WARPM, decreasing = TRUE)[c(1:14)] ] =
  c("'11 Rays",
    "'18 Rays",
    "'21 Guardians",
    "'11 Royals",
    "'21 Rays",
    "'21 Marlins",
    "'12 A's",
    "'13 Marlins",
    "'14 Marlins",
    "'19 Rays",
    "'12 Rays",
    "'13 A's",
    "'15 Rays",
    "'18 A's")
```

Create the `W%` and `WARPM` ggplot object

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

Now, prep data for the `WARPM` and `W-L%` plot subsetted on the
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

Create the tanking subset ggplot object

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

## Render the data

Plot the `W%` and `WARPM` objects together in a plot

``` r
ggarrange(WL_WAR, WL_WARPM, WL_rebuild,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
```

![](Tanking-Graphs_files/figure-gfm/Figure%204-1.png)<!-- -->

# Plot Table 2

## Create the Grading Tanks object

Create the `Grading_Tanks` `df`. Houses:

-   the tanking clubs’ `Win` totals

-   the tanking clubs’ `Years in Tank`

-   the tanking clubs’ `Years Not in Tank`

-   the tanking clubs’ `Rebuild Grade`

`Rebuild Grade` is the average of `Win`s across subsequent seasons after
tanking

``` r
Grading_Tanks = matrix(nrow = 10, ncol = 3)
Grading_Tanks = as.data.frame(Grading_Tanks)
colnames(Grading_Tanks) = c("Tm", "Win Sum", "Years in Tank")
Grading_Tanks$Tm = c("Atlanta Braves", "Chicago Cubs",
                     "Chicago White Sox", "Cincinnati Reds",
                     "Colorado Rockies", "Detroit Tigers",
                     "Houston Astros", "Miami Marlins",
                     "Minnesota Twins", "Oakland Athletics")

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
  dplyr::arrange(desc(`Rebuild Grade`))

Grading_Tanks$Tm = c("Houston Astros ('11-'13)",
                     "Oakland Athletics ('15-'16)",
                     "Chicago Cubs ('12-'13)",
                     "Atlanta Braves ('15-'16)",
                     "Chicago White Sox ('17-'18)",
                     "Colorado Rockies ('14-'15)",
                     "Detroit Tigers ('17-'19)",
                     "Minnesota Twins ('11-'13)",
                     "Miami Marlins ('12-'13)",
                     "Cincinnati Reds ('15-'16)")
```

## Render Table 2

(will not render appropriately in github markdown format)

``` r
Grading_Tanks
```

    ##                             Tm Win Sum Years in Tank Years Not in Tank
    ## 1     Houston Astros ('11-'13)     576             3                 6
    ## 2  Oakland Athletics ('15-'16)     355             2                 4
    ## 3       Chicago Cubs ('12-'13)     615             2                 7
    ## 4     Atlanta Braves ('15-'16)     347             2                 4
    ## 5  Chicago White Sox ('17-'18)     165             2                 2
    ## 6   Colorado Rockies ('14-'15)     398             2                 5
    ## 7     Detroit Tigers ('17-'19)      77             1                 1
    ## 8    Minnesota Twins ('11-'13)     153             2                 2
    ## 9      Miami Marlins ('12-'13)     304             2                 4
    ## 10   Cincinnati Reds ('15-'16)     293             2                 4
    ##    Rebuild Grade
    ## 1           96.0
    ## 2           88.8
    ## 3           87.9
    ## 4           86.8
    ## 5           82.5
    ## 6           79.6
    ## 7           77.0
    ## 8           76.5
    ## 9           76.0
    ## 10          73.2
