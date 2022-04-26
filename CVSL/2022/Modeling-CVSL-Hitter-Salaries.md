Modeling CVSL Hitters Salaries
================
Erik Larsen
4/26/2022

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
This snippet is based on the `R script`, [Modeling Strat
Salaries](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/Modeling%20Strat%20Salaries.R).

Required packages: [readr](https://cran.r-project.org/package=readr),
[tidyverse](https://cran.r-project.org/package=tidyverse)

## Environment Prep

``` r
library(tidyverse)
library(readr)
library(mgcv)
library(splines)
library(ggplot2)
library(ggrepel)

##### Model CVSL Draft salaries #####
  ##### Import the drafts #####
files = list.files(path = "https://github.com/eriklarsen4/Baseball/tree/main/CVSL/2022", pattern = "CVSL Draft")

for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = files[i]))
  )
}
CVSL_2019_DRAFT = `CVSL Draft 2019`
CVSL_2020_DRAFT = `CVSL Draft 2020`
CVSL_2021_DRAFT = `CVSL Draft 2021`
rm(`CVSL Draft 2019`, `CVSL Draft 2020`, `CVSL Draft 2021`)

load("https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/CVSLenv.RData")
```

Clean the `CVSL_2019_DRAFT` df

``` r
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
dummy[1:length(which(CVSL_2019_DRAFT$Dodgers != "")),3] = "Dodgers"
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
```

Each draft file is different, but repeat the process for each of the
last three years (not shown)

## Add fWAR to the dfs

Add `fWAR` to all the dfs for each year (pitcher and hitter)

``` r
  ## Re-name the player/name column in each dataframe
colnames(CVSL_2019_DRAFT)[1] = "Name"
colnames(CVSL_2020_DRAFT)[1] = "Name"
colnames(CVSL_2021_DRAFT)[1] = "Name"

  ##### Add fWAR to each df #####

CVSL_2019_DRAFT_Hitters = CVSL_2019_DRAFT %>%
  inner_join(Hitters_adv_2018) %>%
  select(Name, Salary, CVSLTeam, Year, wRC., Off, Def, WAR) %>%
  collect()
#CVSL_2019_DRAFT_Hitters = DRAFT_Hs %>%
#  select(Name, Age, Bats, Positions) %>%
#  inner_join(CVSL_2019_DRAFT_Hitters) %>%
#  collect()

CVSL_2020_DRAFT_Hitters = CVSL_2020_DRAFT %>%
  inner_join(Hitters_adv_2019) %>%
  select(Name, Salary, CVSLTeam, Year, wRC., Off, Def, WAR) %>%
  collect()
#CVSL_2020_DRAFT_Hitters = DRAFT_Hs %>%
#  select(Name, Age, Bats, Positions) %>%
#  inner_join(CVSL_2020_DRAFT_Hitters) %>%
#  collect()

CVSL_2021_DRAFT_Hitters = CVSL_2021_DRAFT %>%
  inner_join(Hitters_adv_2020) %>%
  select(Name, Salary, CVSLTeam, Year, wRC., Off, Def, WAR) %>%
  collect()
#CVSL_2021_DRAFT_Hitters = DRAFT_Hs %>%
#  select(Name, Age, Bats, Positions) %>%
#  inner_join(CVSL_2021_DRAFT_Hitters) %>%
#  collect()

CVSL_2019_DRAFT_Pitchers = CVSL_2019_DRAFT %>%
  inner_join(BR_2018_Standard_Pitching_Leaders) %>%
  select(Name, Salary, CVSLTeam, Year, G, GS, SO9, WHIP) %>%
  arrange(WHIP) %>%
  filter(G >= 8) %>%
  collect()

CVSL_2020_DRAFT_Pitchers = CVSL_2020_DRAFT %>%
  inner_join(BR_2019_Standard_Pitching_Leaders) %>%
  select(Name, Salary, CVSLTeam, Year, G, GS, SO9, WHIP) %>%
  arrange(WHIP) %>%
  filter(G >= 8) %>%
  collect()

CVSL_2021_DRAFT_Pitchers = CVSL_2021_DRAFT %>%
  inner_join(BR_2020_Standard_Pitching_Leaders) %>%
  select(Name, Salary, CVSLTeam, Year, G, GS, SO9, WHIP) %>%
  arrange(WHIP) %>%
  filter(G >= 8) %>%
  collect()
```

## Build linear regression and generalized additive models

``` r
##### Build and Visualize the linear models and GAMs of WAR or wRC+ predicting hitter's salary #####
  ## Combine the drafts
CVSL_all_DRAFTS_hitters = rbind.data.frame(CVSL_2019_DRAFT_Hitters, CVSL_2020_DRAFT_Hitters, CVSL_2021_DRAFT_Hitters)
  ## Co-erce salaries to be numeric for math operations
CVSL_all_DRAFTS_hitters$Salary = as.numeric(CVSL_all_DRAFTS_hitters$Salary)
  ## Re-name the franchise names
CVSL_all_DRAFTS_hitters$CVSLTeam[ which(CVSL_all_DRAFTS_hitters$CVSLTeam == "Copperheads") ] = "Arsenal"
CVSL_all_DRAFTS_hitters$CVSLTeam[ which(CVSL_all_DRAFTS_hitters$CVSLTeam == "Bears") ] = "Renegades"
CVSL_all_DRAFTS_hitters$CVSLTeam[ which(CVSL_all_DRAFTS_hitters$CVSLTeam == "Golden Bears") ] = "Renegades"
CVSL_all_DRAFTS_hitters$CVSLTeam[ which(CVSL_all_DRAFTS_hitters$CVSLTeam == "Tamales") ] = "Wombats"
```

Build models on hitters’ `CVSL` salaries based on `wRC+`: linear, and
multiple polynomials

``` r
##### Build wRC+ models #####
  ## Build a linear model of wRC+ predicting Salary
DRAFT_wRCh_lm = lm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC.)
  ## Check the model
#coef(summary(DRAFT_wRCh_lm))
#summary(DRAFT_wRCh_lm)

  ## Build the generalized linear model; a 2nd-degree polynomial of wRC+ predicting Salary
DRAFT_wRCh_glm2 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC. + I(wRC.^2))
  ## Check the model
#coef(summary(DRAFT_wRCh_glm2))
#summary(DRAFT_wRCh_glm2)

  ## Build the generalized linear model; a 3rd-degree polynomial of wRC+ predicting Salary
DRAFT_wRCh_glm3 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC. + I(wRC.^2) + I(wRC.^3))
  ## Check the model
#coef(summary(DRAFT_wRCh_glm3))
#summary(DRAFT_wRCh_glm3)
```

Repeat as above, but for `fWAR`

``` r
  ## Build a generalized additive model
gammy_wRCh = gam(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ bs(wRC., k = 3))
#summary(gammy_wRCh)

  ##### Build WAR models #####
  ## Linear
DRAFT_WARh_lm = lm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR)
  ## Check the model
#coef(summary(DRAFT_WARh_lm))
#summary(DRAFT_WARh_lm)

  ## 2nd-deg polyn.
DRAFT_WARh_glm2 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR + I(WAR^2))
  ## Check the model
#coef(summary(DRAFT_WARh_glm2))
#summary(DRAFT_WARh_glm2)

  ## 3rd-deg polyn.
DRAFT_WARh_glm3 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR + I(WAR^2) + I(WAR^3))
#coef(summary(DRAFT_WARh_glm3))
#summary(DRAFT_WARh_glm3)

gammy_WARh = gam(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ bs(WAR, k = 3) + bs(Year, k = 3))
#summary(gammy_WARh)

## Create sorted indeces for plotting
idx = sort(CVSL_all_DRAFTS_hitters$Salary, index.return = T)$ix
```

Plot some simple base plots of the data and the fitted models derived
above.

First, `wRC+`

``` r
  ##### Plot the data and the three models fit to them ####
    ## wRC+
par(mfrow = c(1,1))
plot(CVSL_all_DRAFTS_hitters$wRC., CVSL_all_DRAFTS_hitters$Salary, xlim = range(CVSL_all_DRAFTS_hitters$wRC.),
     cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
     main = "CVSL Auction Draft\nHitter Salaries by wRC+", xlab = "wRC+", ylab = "Salary ($)")
#lines( sort(CVSL_all_DRAFTS_hitters
#$wRC.[idx]) , fitted(DRAFT_lm)[order(CVSL_all_DRAFTS_hitters$wRC.)], col = "red", lty = 1)
lines( sort(CVSL_all_DRAFTS_hitters$wRC.[idx]) , fitted(DRAFT_wRCh_glm2)[order(CVSL_all_DRAFTS_hitters$wRC.)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$wRC.[idx]), fitted(DRAFT_wRCh_glm3)[ order(CVSL_all_DRAFTS_hitters$wRC.) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$wRC.[idx]), fitted(gammy_WARh)[ order(CVSL_all_DRAFTS_hitters$wRC.) ], col = "navy", lty = 4)
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/Base%20CVSL%20Hitter%20Salary%20by%20wRC%2B-1.png)<!-- -->

Now, `fWAR`

``` r
  ## Plot the data and the three models fit to it
    ## WAR
plot(CVSL_all_DRAFTS_hitters$WAR, CVSL_all_DRAFTS_hitters$Salary, xlim = range(CVSL_all_DRAFTS_hitters$WAR),
     cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
     main = "CVSL Auction Draft\nHitter Salaries by WAR", xlab = "WAR", ylab = "Salary ($)")
#lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]) , fitted(DRAFT_WARh_lm)[order(CVSL_all_DRAFTS_hitters$WAR)], col = "red", lty = 1)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]) , fitted(DRAFT_WARh_glm2)[order(CVSL_all_DRAFTS_hitters$WAR)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]), fitted(DRAFT_WARh_glm3)[ order(CVSL_all_DRAFTS_hitters$WAR) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]), fitted(gammy_WARh)[ order(CVSL_all_DRAFTS_hitters$WAR) ], col = "navy", lty = 4)
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/Base%20CVSL%20Hitter%20Salary%20by%20fWAR-1.png)<!-- -->

Seems like the 3rd degree polynomial fits both `wRC+` and `fWAR` as
predictors of `CVSL Salary` best

Add predictions to the data

``` r
  ## Add the predictions of those models into the dataframe
CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     wRC.PredSal3 =
                                       round(as.numeric(predict(DRAFT_wRCh_glm3,
                                                                CVSL_all_DRAFTS_hitters,
                                                                type = "response"))),
                                     .after = 2)

CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     WARPredSal3 =
                                       round(as.numeric(predict(DRAFT_WARh_glm3,
                                                                CVSL_all_DRAFTS_hitters,
                                                                type = "response"))), 
                                     .after = 2)

CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     WARPredSalgam =
                                       round(as.numeric(predict(gammy_WARh,
                                                                CVSL_all_DRAFTS_hitters,
                                                                type = "response"))), 
                                     .after = 2)

CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     wRCPredSalgam =
                                       round(as.numeric(predict(gammy_wRCh,
                                                                CVSL_all_DRAFTS_hitters,
                                                                type = "response"))), 
                                     .after = 2)

CVSL_Hs = add_column(CVSL_Hs,
                     WARPredSal3 = round(as.numeric(predict(DRAFT_WARh_glm3, CVSL_Hs, type = "response"))),
                     .after = 2)


## Plot the residuals
## Find the residuals
CVSL_all_DRAFTS_hitters$WARResid =  CVSL_all_DRAFTS_hitters$Salary - CVSL_all_DRAFTS_hitters$WARPredSal3
CVSL_all_DRAFTS_hitters$wRC.Resid =  CVSL_all_DRAFTS_hitters$Salary - CVSL_all_DRAFTS_hitters$wRC.PredSal3
```

Plot model residuals

`wRC+` model first

``` r
## Plot residuals by wRC+
plot( CVSL_all_DRAFTS_hitters$wRC.PredSal3, CVSL_all_DRAFTS_hitters$wRC.Resid, xlim = range(CVSL_all_DRAFTS_hitters$wRC.PredSal3),
      ylim = range(CVSL_all_DRAFTS_hitters$wRC.Resid), cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\nHitter Salaries When Fit to a 3rd deg. GLM",
      xlab = "Predicted Salary by wRC+ ($)", ylab = "Residuals   (Predicted $ - Actual $)")
abline(0,0, col = "black", lwd = 2)
text(x = 8, y = 0, "Overpay\nUnderpay", col = "black")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/Base%20CVSL%20Hitter%20Salary%20by%20fWAR-1.png)<!-- -->

`fWAR` model second

``` r
## Plot residuals by WAR
plot( CVSL_all_DRAFTS_hitters$WARPredSal3, CVSL_all_DRAFTS_hitters$WARResid, xlim = range(CVSL_all_DRAFTS_hitters$WARPredSal3),
      ylim = range(CVSL_all_DRAFTS_hitters$WARResid), cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\n Hitter Salaries When Fit to a 3rd deg. GLM",
      xlab = "Predicted Salary by WAR ($)", ylab = "Residuals   (Actual $ - Predicted $)")
abline(0,0, col = "black", lwd = 2)
text(x = 8, y = 0, "Overpay\nUnderpay", col = "black")
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/Base%20CVSL%20Hitter%20Salary%20by%20fWAR%20residuals-1.png)<!-- -->

``` r
## See how the residuals are distributed
#res_es = resid(DRAFT_glm2)
#plot(density(res_es))
```

Create the labels to be used for plotting. These will be names of more
interesting points/players in the model (not shown)

Create the `ggplot` object of the data, add labels, and plot

``` r
  ## Create the object
gg = ggplot(CVSL_all_DRAFTS_hitters,
            aes(x = `wRC.`, y = `Salary`)) + 
  geom_point(aes(color = `CVSLTeam`), alpha = 0.7, size = 3) +
  coord_cartesian(xlim = c(min(CVSL_all_DRAFTS_hitters$wRC.),max(CVSL_all_DRAFTS_hitters$wRC.)),
                  ylim = c(min(CVSL_all_DRAFTS_hitters$Salary),max(CVSL_all_DRAFTS_hitters$Salary))) +
  labs(title = "Modeling CVSL Auction Salaries for Hitters\n2019-2021 Seasons", x = "wRC+", y = "Salary ($)") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("yellow3", "violetred2", "saddlebrown", "dodgerblue", "darkgreen", "black", "darkorange1", "red", "darkgrey", "navyblue")) +
  geom_line(aes(x = `wRC.`,
                 y = `wRC.PredSal3`), color = "salmon", size = 1.2, alpha = 0.3)
  ## Add the labels
gg = gg + geom_text_repel(data = CVSL_all_DRAFTS_hitters, x = CVSL_all_DRAFTS_hitters$wRC., y = CVSL_all_DRAFTS_hitters$Salary, color = "black", aes(label = Labels), size = 3, max.overlaps = Inf, box.padding = 0.5)
  ## Plot
gg
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/GG%20CVSL%20Hitters%20wRC%2B-1.png)<!-- -->

Plot model residuals with `ggplot`

``` r
  ## ggplot residuals
ggresid = ggplot(CVSL_all_DRAFTS_hitters,
            aes(x = `wRC.PredSal3`, y = `wRC.Resid`)) + 
  geom_point(aes(color = `CVSLTeam`), alpha = 0.7, size = 3) +
  coord_cartesian(xlim = c(min(CVSL_all_DRAFTS_hitters$wRC.PredSal3),max(CVSL_all_DRAFTS_hitters$wRC.PredSal3)),
                  ylim = c(min(CVSL_all_DRAFTS_hitters$wRC.Resid),max(CVSL_all_DRAFTS_hitters$wRC.Resid))) +
  labs(title = "wRC+ Residuals of CVSL Auction Salaries\n for Hitters 2019-2021 Seasons", x = "Predicted Salary ($)", y = "Residuals") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("yellow3", "violetred2", "saddlebrown", "dodgerblue", "darkgreen", "black", "darkorange1", "red", "darkgrey", "navyblue")) +
  geom_hline(yintercept = 0, color = "black", size = 1.2, alpha = 0.3) +
  geom_text(x = 49, y = 0, label = "Overpay\n\nUnderpay", size = 4)

ggresid = ggresid + geom_text_repel(data = CVSL_all_DRAFTS_hitters, x = CVSL_all_DRAFTS_hitters$wRC.PredSal3, y = CVSL_all_DRAFTS_hitters$wRC.Resid, color = "black", aes(label = Labels), size = 3, max.overlaps = Inf)

ggresid
```

![](https://github.com/eriklarsen4/Baseball/blob/main/CVSL/2022/GG%20CVSL%20Hitters%20wRC%2B%20residuals-1.png)<!-- -->
