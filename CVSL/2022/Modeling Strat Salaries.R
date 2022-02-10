
library(tidyverse)
library(readr)
library(mgcv)
library(splines)
library(ggplot2)
library(ggrepel)

##### Model CVSL Draft salaries #####
  ##### Import the drafts #####
setwd("C:/Users/Erik/Desktop/BoxCopy/Strato/2022/")
files = list.files(pattern = "CVSL Draft")

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
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Dodgers[ which(CVSL_2020_DRAFT$Dodgers != "")] == TRUE) ] = "Dodgers"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Bears[ which(CVSL_2020_DRAFT$Bears != "")] == TRUE) ] = "Bears"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Matadors[ which(CVSL_2020_DRAFT$Matadors != "")] == TRUE) ] = "Matadors"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Cattlemen[ which(CVSL_2020_DRAFT$Cattlemen != "")] == TRUE) ] = "Cattlemen"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Nuggets[ which(CVSL_2020_DRAFT$Nuggets != "")] == TRUE) ] = "Nuggets"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Copperheads[ which(CVSL_2020_DRAFT$Copperheads != "")] == TRUE) ] = "Copperheads"
dummy$CVSLTeam[which(dummy$V1 %in% CVSL_2020_DRAFT$Tamales[ which(CVSL_2020_DRAFT$Tamales != "")] == TRUE) ] = "Tamales"

CVSL_2020_DRAFT = dummy
rm(dummy)


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
dummy[1:length(which(CVSL_2021_DRAFT$Dodgers != "")),3] = "Dodgers"
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

##### Build wRC+ models #####
  ## Build a linear model of wRC+ predicting Salary
DRAFT_wRCh_lm = lm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC.)
  ## Check the model
coef(summary(DRAFT_wRCh_lm))
summary(DRAFT_wRCh_lm)

  ## Build the generalized linear model; a 2nd-degree polynomial of wRC+ predicting Salary
DRAFT_wRCh_glm2 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC. + I(wRC.^2))
  ## Check the model
coef(summary(DRAFT_wRCh_glm2))
summary(DRAFT_wRCh_glm2)

  ## Build the generalized linear model; a 3rd-degree polynomial of wRC+ predicting Salary
DRAFT_wRCh_glm3 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ wRC. + I(wRC.^2) + I(wRC.^3))
  ## Check the model
coef(summary(DRAFT_wRCh_glm3))
summary(DRAFT_wRCh_glm3)

  ## Build a ggeneralized additive model
gammy_wRCh = gam(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ bs(wRC., k = 3))
summary(gammy_wRCh)

  ##### Build WAR models #####
  ## Linear
DRAFT_WARh_lm = lm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR)
  ## Check the model
coef(summary(DRAFT_WARh_lm))
summary(DRAFT_WARh_lm)

  ## 2nd-deg polyn.
DRAFT_WARh_glm2 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR + I(WAR^2))
  ## Check the model
coef(summary(DRAFT_WARh_glm2))
summary(DRAFT_WARh_glm2)

  ## 3rd-deg polyn.
DRAFT_WARh_glm3 = glm(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ WAR + I(WAR^2) + I(WAR^3))
coef(summary(DRAFT_WARh_glm3))
summary(DRAFT_WARh_glm3)

gammy_WARh = gam(data = CVSL_all_DRAFTS_hitters, formula = Salary ~ bs(WAR, k = 3) + bs(Year, k = 3))
summary(gammy_WARh)

## Create sorted indeces for plotting
idx = sort(CVSL_all_DRAFTS_hitters$Salary, index.return = T)$ix



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


  ## Plot the data and the three models fit to it
    ## WAR
plot(CVSL_all_DRAFTS_hitters$WAR, CVSL_all_DRAFTS_hitters$Salary, xlim = range(CVSL_all_DRAFTS_hitters$WAR),
     cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
     main = "CVSL Auction Draft\nHitter Salaries by WAR", xlab = "WAR", ylab = "Salary ($)")
#lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]) , fitted(DRAFT_WARh_lm)[order(CVSL_all_DRAFTS_hitters$WAR)], col = "red", lty = 1)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]) , fitted(DRAFT_WARh_glm2)[order(CVSL_all_DRAFTS_hitters$WAR)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]), fitted(DRAFT_WARh_glm3)[ order(CVSL_all_DRAFTS_hitters$WAR) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(CVSL_all_DRAFTS_hitters$WAR[idx]), fitted(gammy_WARh)[ order(CVSL_all_DRAFTS_hitters$WAR) ], col = "navy", lty = 4)


## Seems like the 3rd degree polynomial fits both wRC+ and WAR as predictors of Salary best;
  ## Add the predictions of those models into the dataframe
CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     wRC.PredSal3 = round(as.numeric(predict(DRAFT_wRCh_glm3, CVSL_all_DRAFTS_hitters, type = "response"))), 
                                     .after = 2)
CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     WARPredSal3 = round(as.numeric(predict(DRAFT_WARh_glm3, CVSL_all_DRAFTS_hitters, type = "response"))), 
                                     .after = 2)
CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     WARPredSalgam = round(as.numeric(predict(gammy_WARh, CVSL_all_DRAFTS_hitters, type = "response"))), 
                                     .after = 2)

CVSL_all_DRAFTS_hitters = add_column(CVSL_all_DRAFTS_hitters, 
                                     wRCPredSalgam = round(as.numeric(predict(gammy_wRCh, CVSL_all_DRAFTS_hitters, type = "response"))), 
                                     .after = 2)

CVSL_Hs = add_column(CVSL_Hs,
                     WARPredSal3 = round(as.numeric(predict(DRAFT_WARh_glm3, CVSL_Hs, type = "response"))),
                     .after = 2)


## Plot the residuals
## Find the residuals
CVSL_all_DRAFTS_hitters$WARResid =  CVSL_all_DRAFTS_hitters$Salary - CVSL_all_DRAFTS_hitters$WARPredSal3
CVSL_all_DRAFTS_hitters$wRC.Resid =  CVSL_all_DRAFTS_hitters$Salary - CVSL_all_DRAFTS_hitters$wRC.PredSal3

## Plot residuals by wRC+
plot( CVSL_all_DRAFTS_hitters$wRC.PredSal3, CVSL_all_DRAFTS_hitters$Resid, xlim = range(CVSL_all_DRAFTS_hitters$wRC.PredSal3),
      ylim = range(CVSL_all_DRAFTS_hitters$Resid), cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\nHitter Salaries When Fit to a 3rd deg. GLM",
      xlab = "Predicted Salary by wRC+ ($)", ylab = "Residuals   (Predicted $ - Actual $)")
abline(0,0, col = "black", lwd = 2)
text(x = 8, y = 0, "Overpay\nUnderpay", col = "black")


## Plot residuals by WAR
plot( CVSL_all_DRAFTS_hitters$WARPredSal3, CVSL_all_DRAFTS_hitters$Resid, xlim = range(CVSL_all_DRAFTS_hitters$WARPredSal3),
      ylim = range(CVSL_all_DRAFTS_hitters$Resid), cex = 0.5, data = CVSL_all_DRAFTS_hitters, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\n Hitter Salaries When Fit to a 3rd deg. GLM",
      xlab = "Predicted Salary by WAR ($)", ylab = "Residuals   (Actual $ - Predicted $)")
abline(0,0, col = "black", lwd = 2)
text(x = 8, y = 0, "Overpay\nUnderpay", col = "black")
## See how the residuals are distributed
#res_es = resid(DRAFT_glm2)
#plot(density(res_es))



  ## Create labeling objects/variables
CVSL_all_DRAFTS_hitters$Labels = ""
which(CVSL_all_DRAFTS_hitters$Name == "Mike Trout")
which(CVSL_all_DRAFTS_hitters$Name == "Manny Machado")
which(CVSL_all_DRAFTS_hitters$Name == "Juan Soto")
which(CVSL_all_DRAFTS_hitters$Name == "Jose Ramirez")
which(CVSL_all_DRAFTS_hitters$Name == "Freddie Freeman")
which(CVSL_all_DRAFTS_hitters$Name == "Raimel Tapia")
which(CVSL_all_DRAFTS_hitters$Name == "Bryce Harper")
which(CVSL_all_DRAFTS_hitters$Name == "Yasmani Grandal")
which(CVSL_all_DRAFTS_hitters$Name == "Ramon Laureano")

  ## Assign them into the same dataframe
CVSL_all_DRAFTS_hitters$Labels[ c(11,29,42,52,62,66,134,140,159,166) ] = paste(CVSL_all_DRAFTS_hitters$Name[ c(11,29,42,52,62,66,134,140,159,166) ], CVSL_all_DRAFTS_hitters$Year[ c(11,29,42,52,62,66,134,140,159,166) ])

gg = ggplot(CVSL_all_DRAFTS_hitters,
            aes(x = `wRC.`, y = `Salary`)) + 
  geom_point(aes(color = `CVSLTeam`), alpha = 0.7) +
  coord_cartesian(xlim = c(min(CVSL_all_DRAFTS_hitters$wRC.),max(CVSL_all_DRAFTS_hitters$wRC.)),
                  ylim = c(min(CVSL_all_DRAFTS_hitters$Salary),max(CVSL_all_DRAFTS_hitters$Salary))) +
  labs(title = "Modeling CVSL Auction Salaries for Hitters\n2019-2021 Seasons", x = "wRC+", y = "Salary ($)") + 
  theme_gray() +
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

gg = gg + geom_text_repel(data = CVSL_all_DRAFTS_hitters, x = CVSL_all_DRAFTS_hitters$wRC., y = CVSL_all_DRAFTS_hitters$Salary, color = "black", aes(label = Labels), size = 3, max.overlaps = Inf, box.padding = 0.5)

gg




  ## ggplot residuals
ggresid = ggplot(CVSL_all_DRAFTS_hitters,
            aes(x = `wRC.PredSal3`, y = `wRC.Resid`)) + 
  geom_point(aes(color = `CVSLTeam`), alpha = 0.7) +
  coord_cartesian(xlim = c(min(CVSL_all_DRAFTS_hitters$wRC.PredSal3),max(CVSL_all_DRAFTS_hitters$wRC.PredSal3)),
                  ylim = c(min(CVSL_all_DRAFTS_hitters$wRC.Resid),max(CVSL_all_DRAFTS_hitters$wRC.Resid))) +
  labs(title = "wRC+ Residuals of CVSL Auction Salaries\n for Hitters 2019-2021 Seasons", x = "Predicted Salary ($)", y = "Residuals") + 
  theme_gray() +
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


##### Build and Visualize the generalized linear model of WAR predicting pitcher's salary #####
## Combine the 2019 and 2020 drafts
CVSL_all_DRAFTS_pitchers = rbind.data.frame(CVSL_2019_DRAFT_Pitchers, CVSL_2020_DRAFT_Pitchers, CVSL_2021_DRAFT_Pitchers)
## Co-erce salaries to be numeric for math operations
CVSL_all_DRAFTS_pitchers$Salary = as.numeric(CVSL_all_DRAFTS_pitchers$Salary)

## Build a linear model
DRAFT_WHIPp_lm = lm(data = CVSL_all_DRAFTS_pitchers, formula = Salary ~ WHIP)
## Check the model
coef(summary(DRAFT_WHIPp_lm))
summary(DRAFT_WHIPp_lm)

## Build the generalized linear model; a 2nd-degree polynomial
DRAFT_WHIPp_glm2 = glm(data = CVSL_all_DRAFTS_pitchers, formula = Salary ~ WHIP + I(WHIP^2))
## Check the model
coef(summary(DRAFT_WHIPp_glm2))
summary(DRAFT_WHIPp_glm2)

## Build the generalized linear model; a 3rd-degree polynomial
DRAFT_WHIPp_glm3 = glm(data = CVSL_all_DRAFTS_pitchers, formula = Salary ~ WHIP + I(WHIP^2) + I(WHIP^3))
## Check the model
coef(summary(DRAFT_WHIPp_glm3))
summary(DRAFT_WHIPp_glm3)

## Create Salary estimates from the models
p_lmWHIPPredSal = round(as.numeric(predict(DRAFT_WHIPp_lm)))

p_glm2WHIPPredSal = round(as.numeric(predict(DRAFT_WHIPp_glm2)))

p_glm3WHIPPredSal = round(as.numeric(predict(DRAFT_WHIPp_glm3)))

## Build a ggeneralized additive model
gammy_WHIPp = gam(data = CVSL_all_DRAFTS_pitchers, formula = Salary ~ bs(WHIP, k = 3) + SO9 )
summary(gammy_WHIPp)

## Create sorted indeces for plotting
idx = sort(CVSL_all_DRAFTS_pitchers$Salary, index.return = T)$ix

## Plot the data and the three models fit to it
plot(CVSL_all_DRAFTS_pitchers$WHIP, CVSL_all_DRAFTS_pitchers$Salary, xlim = range(CVSL_all_DRAFTS_pitchers$WHIP),
     cex = 0.5, data = CVSL_all_DRAFTS_pitchers, col = "darkgray",
     main = "CVSL Auction Draft\nPitcher Salaries by WHIP", xlab = "WHIP", ylab = "Salary")
lines( sort(CVSL_all_DRAFTS_pitchers$WHIP[idx]) , fitted(DRAFT_WHIPp_lm)[order(CVSL_all_DRAFTS_pitchers$WHIP)], col = "red", lty = 1)
lines( sort(CVSL_all_DRAFTS_pitchers$WHIP[idx]) , fitted(DRAFT_WHIPp_glm2)[order(CVSL_all_DRAFTS_pitchers$WHIP)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(CVSL_all_DRAFTS_pitchers$WHIP[idx]), fitted(DRAFT_WHIPp_glm3)[ order(CVSL_all_DRAFTS_pitchers$WHIP) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(CVSL_all_DRAFTS_pitchers$WHIP[idx]), fitted(gammy_WHIPp)[ order(CVSL_all_DRAFTS_pitchers$WHIP) ], col = "navy", lty = 4)

## Seems like the 2nd degree polynomial fits best;
## add the predictions of that model into the dataframe
CVSL_all_DRAFTS_pitchers = add_column(CVSL_all_DRAFTS_pitchers, 
                                      WHIPPredSal2 = round(as.numeric(predict(DRAFT_WHIPp_glm2, CVSL_all_DRAFTS_pitchers, type = "response"))), 
                                     .after = 2)

CVSL_Ps = add_column(CVSL_Ps,
                     WHIPPredSal2 = round(as.numeric(predict(DRAFT_WHIPp_glm2, CVSL_Ps, type = "response"))),
                     .after = 2)

## Plot the residuals
## Find the residuals
CVSL_all_DRAFTS_pitchers$Resid =  CVSL_all_DRAFTS_pitchers$Salary - CVSL_all_DRAFTS_pitchers$WHIPPredSal2

## Plot the residuals
plot( CVSL_all_DRAFTS_pitchers$PredSal, CVSL_all_DRAFTS_pitchers$Resid, xlim = range(CVSL_all_DRAFTS_pitchers$PredSal),
      ylim = range(CVSL_all_DRAFTS_pitchers$Resid), cex = 0.5, data = CVSL_all_DRAFTS_pitchers, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\nPitcher Salaries When Fit to a 2nd deg. GLM",
      xlab = "Predicted Pitcher Salary ($)", ylab = "Residuals   (Predicted $ - Actual $)")

## Plot residuals by WHIP
plot( CVSL_all_DRAFTS_pitchers$WHIP, CVSL_all_DRAFTS_pitchers$Resid, xlim = range(CVSL_all_DRAFTS_pitchers$WHIP),
      ylim = range(CVSL_all_DRAFTS_pitchers$Resid), cex = 0.5, data = CVSL_all_DRAFTS_pitchers, col = "darkgray",
      main = "Residuals of CVSL Auction Draft\nPitcher Salaries When Fit to a 2nd deg. GLM",
      xlab = "WHIP", ylab = "Residual Salary ($)")
abline(0,0, col = "black", lwd = 2)
text(x = 5, y = 0, "Overpay\nUnderpay", col = "black")
## See how the residuals are distributed
#res_es = resid(DRAFT_glm2)
#plot(density(res_es))


DRAFT_Hs = add_column(DRAFT_Hs,
                      wRCPredSal3 = round(as.numeric(predict(DRAFT_wRCh_glm3, DRAFT_Hs, type = "response"))),
                      .after = 2)
DRAFT_Hs = add_column(DRAFT_Hs,
                      WARPredSal3 = round(as.numeric(predict(DRAFT_WARh_glm3, DRAFT_Hs, type = "response"))),
                      .after = 2)

DRAFT_Ps = add_column(DRAFT_Ps,
                      WHIPPredSal2 = round(as.numeric(predict(DRAFT_WHIPp_glm2, DRAFT_Ps, type = "response"))),
                      .after = 2)


DRAFT_LHP_LEADERS = DRAFT_Hs %>%
  select(Name, WARPredSal3, wRCPredSal3, Def, WAR) %>%
  left_join(DRAFT_LHP_LEADERS) %>% 
  arrange(desc(wRC.)) %>%
  collect()

DRAFT_RHP_LEADERS = DRAFT_Hs %>%
  select(Name, WARPredSal3, wRCPredSal3, Def, WAR) %>%
  left_join(DRAFT_RHP_LEADERS) %>% 
  arrange(desc(wRC.)) %>%
  collect()

DRAFT_LHH_LEADERS = DRAFT_Ps %>%
  select(Name, WARPredSal2, WAR) %>%
  left_join(DRAFT_LHH_LEADERS) %>% 
  arrange(WHIP) %>%
  collect()

DRAFT_RHH_LEADERS = DRAFT_Ps %>%
  select(Name, WARPredSal2, WAR) %>%
  left_join(DRAFT_RHH_LEADERS) %>% 
  arrange(WHIP) %>%
  collect()





##### Model CVSL Blind Bids #####

  ## Import the Bid Results files
files = list.files(pattern = "Blind Bid Results")

for ( i in 1:length(files) ) {
  as.data.frame(
    assign(
      gsub(".csv", "", files[], fixed = TRUE)[i], read.csv(file = files[i]))
  )
}
BLIND_BIDS_2017 = `Blind Bid Results 2017`
BLIND_BIDS_2018 = `Blind Bid Results 2018`
BLIND_BIDS_2019 = `Blind Bid Results 2019`
BLIND_BIDS_2020 = `Blind Bid Results 2020`
BLIND_BIDS_2021 = `Blind Bid Results 2021`
rm(`Blind Bid Results 2017`, `Blind Bid Results 2018`, `Blind Bid Results 2019`, `Blind Bid Results 2020`, `Blind Bid Results 2021`)

  ## Add WAR to the hitter dfs
BLIND_BIDS_h_2017 = BLIND_BIDS_2017 %>%
  inner_join(Hitters_adv_2016) %>%
  select(Name, Salary, wRC., Off, Def, WAR) %>%
  collect()

BLIND_BIDS_h_2018 = BLIND_BIDS_2018 %>%
  inner_join(Hitters_adv_2017) %>%
  select(Name, Salary, wRC., Off, Def, WAR) %>%
  collect()

BLIND_BIDS_h_2019 = BLIND_BIDS_2019 %>%
  inner_join(Hitters_adv_2018) %>%
  select(Name, Salary, wRC., Off, Def, WAR) %>%
  collect()

BLIND_BIDS_h_2020 = BLIND_BIDS_2020 %>%
  inner_join(Hitters_adv_2019) %>%
  select(Name, Salary, wRC., Off, Def, WAR) %>%
  collect()

BLIND_BIDS_h_2021 = BLIND_BIDS_2021 %>%
  inner_join(Hitters_adv_2020) %>%
  select(Name, Salary, wRC., Off, Def, WAR) %>%
  collect()

  ## Add year column
BLIND_BIDS_h_2017$Year = 2017
BLIND_BIDS_h_2018$Year = 2018
BLIND_BIDS_h_2019$Year = 2019
BLIND_BIDS_h_2020$Year = 2020
BLIND_BIDS_h_2021$Year = 2021

BLIND_BIDS_h = rbind.data.frame(BLIND_BIDS_h_2017, BLIND_BIDS_h_2018, BLIND_BIDS_h_2019, BLIND_BIDS_h_2020, BLIND_BIDS_h_2021)

BLIND_BIDS_p_2017 = BLIND_BIDS_2017 %>%
  inner_join(Pitchers_2016) %>%
  select(Name, Salary, K.9, ERA, xFIP, WAR) %>%
  arrange(desc(WAR)) %>%
  collect()

BLIND_BIDS_p_2018 = BLIND_BIDS_2018 %>%
  inner_join(Pitchers_2017) %>%
  select(Name, Salary, K.9, ERA, xFIP, WAR) %>%
  arrange(desc(WAR)) %>%
  collect()

BLIND_BIDS_p_2019 = BLIND_BIDS_2019 %>%
  inner_join(Pitchers_2018) %>%
  select(Name, Salary, K.9, ERA, xFIP, WAR) %>%
  arrange(desc(WAR)) %>%
  collect()

BLIND_BIDS_p_2020 = BLIND_BIDS_2020 %>%
  inner_join(Pitchers_2019) %>%
  select(Name, Salary, K.9, ERA, xFIP, WAR) %>%
  arrange(desc(WAR)) %>%
  collect()

BLIND_BIDS_p_2021 = BLIND_BIDS_2021 %>%
  inner_join(Pitchers_2020) %>%
  select(Name, Salary, K.9, ERA, xFIP, WAR) %>%
  arrange(desc(WAR)) %>%
  collect()


  ## Add year column
BLIND_BIDS_p_2017$Year = 2017
BLIND_BIDS_p_2018$Year = 2018
BLIND_BIDS_p_2019$Year = 2019
BLIND_BIDS_p_2020$Year = 2020
BLIND_BIDS_p_2021$Year = 2021

BLIND_BIDS_p = rbind.data.frame(BLIND_BIDS_p_2017, BLIND_BIDS_p_2018, BLIND_BIDS_p_2019, BLIND_BIDS_p_2020, BLIND_BIDS_p_2021)

##### Build and Visualize the generalized linear model of WAR predicting hitter salary #####

## Build a linear model
BID_h_lm = lm(data = BLIND_BIDS_h, formula = Salary ~ WAR)
## Check the model
coef(summary(BID_h_lm))
summary(BID_h_lm)

## Build the generalized linear model; a 2nd-degree polynomial
BID_h_glm2 = glm(data = BLIND_BIDS_h, formula = Salary ~ WAR + I(WAR^2))
## Check the model
coef(summary(BID_h_glm2))
summary(BID_h_glm2)

## Build the generalized linear model; a 3rd-degree polynomial
BID_h_glm3 = glm(data = BLIND_BIDS_h, formula = Salary ~ WAR + I(WAR^2) + I(WAR^3))
## Check the model
coef(summary(BID_h_glm3))
summary(BID_h_glm3)

## Create Salary estimates from the models
h_lmPredSal = round(as.numeric(predict(BID_h_lm)))

h_glm2PredSal = round(as.numeric(predict(BID_h_glm2)))

h_glm3PredSal = round(as.numeric(predict(BID_h_glm3)))

## Build a ggeneralized additive model
gammy_h = gam(data = BLIND_BIDS_h, formula = Salary ~ bs(WAR, k = 3))
summary(gammy_h)

## Create sorted indeces for plotting
idx = sort(BLIND_BIDS_h$Salary, index.return = T)$ix

## Plot the data and the three models fit to it
plot(BLIND_BIDS_h$WAR, BLIND_BIDS_h$Salary, xlim = range(BLIND_BIDS_h$WAR),
     cex = 0.5, data = BLIND_BIDS_h, col = "darkgray",
     main = "CVSL Blind Bids\nHitter Salaries by WAR", xlab = "WAR", ylab = "Salary")
lines( sort(BLIND_BIDS_h$WAR[idx]) , fitted(BID_h_lm)[order(BLIND_BIDS_h$WAR)], col = "red", lty = 1)
lines( sort(BLIND_BIDS_h$WAR[idx]) , fitted(BID_h_glm2)[order(BLIND_BIDS_h$WAR)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(BLIND_BIDS_h$WAR[idx]), fitted(BID_h_glm3)[ order(BLIND_BIDS_h$WAR) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(BLIND_BIDS_h$WAR[idx]), fitted(gammy_h)[ order(BLIND_BIDS_h$WAR) ], col = "navy", lty = 4)

## Seems like the 2nd degree polynomial fits best;
## add the predictions of that model into the dataframe
BLIND_BIDS_h = add_column(BLIND_BIDS_h, 
                          PredSal = round(as.numeric(predict.gam(gammy_h, BLIND_BIDS_h, type = "response"))), 
                          .after = 2)

## Plot the residuals
## Find the residuals
BLIND_BIDS_h$Resid =  BLIND_BIDS_h$Salary - BLIND_BIDS_h$PredSal

## Plot the residuals
plot( BLIND_BIDS_h$PredSal, BLIND_BIDS_h$Resid, xlim = range(BLIND_BIDS_h$PredSal),
      ylim = range(BLIND_BIDS_h$Resid), cex = 0.5, data = BLIND_BIDS_h, col = "darkgray",
      main = "Residuals of CVSL Blind Bid\nHitter Salaries When Fit to a GAM",
      xlab = "Predicted Hitter Salary ($)", ylab = "Residuals   (Predicted $ - Actual $)")

## Plot residuals by WAR
plot( BLIND_BIDS_h$WAR, BLIND_BIDS_h$Resid, xlim = range(BLIND_BIDS_h$WAR),
      ylim = range(BLIND_BIDS_h$Resid), cex = 0.5, data = BLIND_BIDS_h, col = "darkgray",
      main = "Residuals of CVSL Blind Bid\nHitter Salaries When Fit to a GAM",
      xlab = "WAR", ylab = "Residual Salary ($)")
abline(0,0, col = "black", lwd = 2)
text(x = 5, y = 0, "Overpay\nUnderpay", col = "black")


DRAFT_Hs[ which(DRAFT_Hs$Class != "Type A"), ]
DRAFT_Hs = add_column(DRAFT_Hs, 
                      BidPredSal = round(as.numeric(predict(gammy_h, DRAFT_Hs, type = "response"))), 
                      .after = 2)



##### Build and Visualize the generalized linear model of WAR predicting pitcher salary #####

  ## Build a linear model
BID_p_lm = lm(data = BLIND_BIDS_p, formula = Salary ~ WAR)
  ## Check the model
coef(summary(BID_p_lm))
summary(BID_p_lm)

## Build the generalized linear model; a 2nd-degree polynomial
BID_p_glm2 = glm(data = BLIND_BIDS_p, formula = Salary ~ WAR + I(WAR^2))
## Check the model
coef(summary(BID_p_glm2))
summary(BID_p_glm2)

## Build the generalized linear model; a 3rd-degree polynomial
BID_p_glm3 = glm(data = BLIND_BIDS_p, formula = Salary ~ WAR + I(WAR^2) + I(WAR^3))
## Check the model
coef(summary(BID_p_glm3))
summary(BID_p_glm3)

## Create Salary estimates from the models
p_lmPredSal = round(as.numeric(predict(BID_p_lm)))

p_glm2PredSal = round(as.numeric(predict(BID_p_glm2)))

p_glm3PredSal = round(as.numeric(predict(BID_p_glm3)))

## Build a ggeneralized additive model
gammy_p = gam(data = BLIND_BIDS_p, formula = Salary ~ bs(WAR, k = 3))
summary(gammy_p)

## Create sorted indeces for plotting
idx = sort(BLIND_BIDS_p$Salary, index.return = T)$ix

## Plot the data and the three models fit to it
plot(BLIND_BIDS_p$WAR, BLIND_BIDS_p$Salary, xlim = range(BLIND_BIDS_p$WAR),
     cex = 0.5, data = BLIND_BIDS_p, col = "darkgray",
     main = "CVSL Blind Bids\nPitcher Salaries by WAR", xlab = "WAR", ylab = "Salary")
lines( sort(BLIND_BIDS_p$WAR[idx]) , fitted(BID_p_lm)[order(BLIND_BIDS_p$WAR)], col = "red", lty = 1)
lines( sort(BLIND_BIDS_p$WAR[idx]) , fitted(BID_p_glm2)[order(BLIND_BIDS_p$WAR)], col = "darkorange", lty = 1, lwd = 2)
lines( sort(BLIND_BIDS_p$WAR[idx]), fitted(BID_p_glm3)[ order(BLIND_BIDS_p$WAR) ], col = "forestgreen", lty = 3, lwd = 2)
lines( sort(BLIND_BIDS_p$WAR[idx]), fitted(gammy_p)[ order(BLIND_BIDS_p$WAR) ], col = "navy", lty = 4)

## Seems like the 2nd degree polynomial fits best;
## add the predictions of that model into the dataframe
BLIND_BIDS_p = add_column(BLIND_BIDS_p, 
                                      PredSal = round(as.numeric(predict(BID_p_glm2, BLIND_BIDS_p, type = "response"))), 
                                      .after = 2)

colnames(BID_UNI)[2] = "Class"


## Plot the residuals
## Find the residuals
BLIND_BIDS_p$Resid =  BLIND_BIDS_p$Salary - BLIND_BIDS_p$PredSal

## Plot the residuals
plot( BLIND_BIDS_p$PredSal, BLIND_BIDS_p$Resid, xlim = range(BLIND_BIDS_p$PredSal),
      ylim = range(BLIND_BIDS_p$Resid), cex = 0.5, data = BLIND_BIDS_p, col = "darkgray",
      main = "Residuals of CVSL Blind Bid\nPitcher Salaries When Fit to a 2nd deg. GLM",
      xlab = "Predicted Pitcher Salary ($)", ylab = "Residuals   (Predicted $ - Actual $)")

## Plot residuals by WAR
plot( BLIND_BIDS_p$WAR, BLIND_BIDS_p$Resid, xlim = range(BLIND_BIDS_p$WAR),
      ylim = range(BLIND_BIDS_p$Resid), cex = 0.5, data = BLIND_BIDS_p, col = "darkgray",
      main = "Residuals of CVSL Blind Bid\nPitcher Salaries When Fit to a 2nd deg. GLM",
      xlab = "WAR", ylab = "Residual Salary ($)")
abline(0,0, col = "black", lwd = 2)
text(x = 4, y = 0, "Overpay\nUnderpay", col = "black")


#DRAFT_Ps[ which(DRAFT_Ps$Class != "Type A"), ]
DRAFT_Ps = add_column(DRAFT_Ps, 
                          BidPredSal = round(as.numeric(predict(BID_p_lm, DRAFT_Ps, type = "response"))), 
                          .after = 2)




BID_UNI = DRAFT_Hs %>%
  select(Name, BidPredSal, WARPredSal3, WAR) %>%
  full_join(DRAFT_Ps, keep = FALSE) %>%
  select(Name, BidPredSal, WHIPPredSal2, WHIP, ERA, xFIP, WAR) %>%
  inner_join(BID_UNI) %>%
  arrange(desc(as.numeric(BidPredSal))) %>%
  select(Name, BidPredSal, WARPredSal3, WAR, WHIPPredSal2, WHIP, ERA, xFIP, Class, CVSLTeam, Previous.Salary)


CVSL_MASTER2 = CVSL_Hs %>%
  select(Name, Salary, WARPredSal3, CVSLTeam, wRC., Def, WAR) %>%
  full_join(CVSL_Ps, keep = FALSE) %>%
  select(Name, Salary, WARPredSal2, WARPredSal3, CVSLTeam, wRC., Def, ERA, xFIP, WAR) %>%
  inner_join(CVSL_MASTER) %>%
  select(Name, Salary, WARPredSal2, WARPredSal3, CVSLTeam, wRC., Def, ERA, xFIP, WAR) %>%
  arrange(CVSLTeam)

TEAM_EVAL = CVSL_MASTER2
TEAM_EVAL$TeamWAR = 0
TEAM_EVAL$TeamSal = 0


for(i in levels(as.factor(TEAM_EVAL$CVSLTeam))){
  TEAM_EVAL$TeamSal[ which(TEAM_EVAL$CVSLTeam == i) ] = sum(TEAM_EVAL$Salary[ which(TEAM_EVAL$CVSLTeam == i) ])
  TEAM_EVAL$TeamWAR[ which(TEAM_EVAL$CVSLTeam == i) ] = sum(TEAM_EVAL$WAR[ which(TEAM_EVAL$CVSLTeam == i) ])
  print(paste(i, "Payroll = ", sum(TEAM_EVAL$Salary[ which(TEAM_EVAL$CVSLTeam == i)])))
  print(paste(i, "Roster spots = ", length(which(TEAM_EVAL$CVSLTeam == i))))
  print(paste(i, "Team WAR = ", sum(TEAM_EVAL$WAR[ which(TEAM_EVAL$CVSLTeam == i)])))
  print(paste(i, "$ / WAR = ", round(sum(TEAM_EVAL$Salary[ which(TEAM_EVAL$CVSLTeam == i)] / sum(TEAM_EVAL$WAR[ which(TEAM_EVAL$CVSLTeam == i)])))))
}

