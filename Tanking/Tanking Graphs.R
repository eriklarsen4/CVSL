

  ## Import the tankingstream environment

  ## Build models
  ## W% ~ Salary
ggpairs(TeamValALL[ -which(TeamValALL$Year == 2020), -c(1,5) ])
hist(residuals(Sal_WL_lm), col = "navy")
plot(fitted(Sal_WL_lm), residuals(Sal_WL_lm))
abline(h = 0)

Sal_W_model = lm(W_L~Salary, data = TeamValALL[-which(TeamValALL$Year == 2020), ])
round(summary(Sal_W_model)$r.squared, digits = 3)
Sal_W_gam = gam(data = TeamValALL, formula = W_L ~ bs(Salary, k = 4))
summary(Sal_W_gam)
idx = sort(TeamValALL$Salary, index.return = T)$ix

  ## W% ~ WAR (all)
WL_WAR_lm = lm(data = TeamValALL[-which(TeamValALL$Year == 2020),], formula = W_L~WAR)
summary(WL_WAR_lm)$r.squared
as.numeric(coefficients(WAR_WL_lm)[1])

  ##  W ~ WAR (all)
W_WAR_GAM = gam(data = TeamValALL[-which(TeamValALL$Year == 2020),], formula = TeamValALL$W ~ s(TeamValALL$WAR, by = as.factor(TeamValALL$Tm), k = 3, bs = "cr"))
summary(W_WAR_GAM)
k.check(W_WAR_GAM)
W_WAR_lm = lm(data = TeamValALL[-which(TeamValALL$Year == 2020),], formula = W~WAR)
summary(W_WAR_lm)

##### Team Color codes #####
color_code = matrix(nrow = 30, ncol = 4)
color_code = as.data.frame(color_code)
colnames(color_code) = c("ESPNTeam", "ESPNcolor", "currentTeam", "currentcolor")
color_code$ESPNTeam = unique(ESPN$Team)[order(unique(ESPN$Team))]
color_code$ESPNcolor = c("firebrick1", "orange3", "forestgreen", "deepskyblue", "firebrick", "navy", "firebrick3",
                         "dodgerblue", "darkred", "dodgerblue", "darkorange1", "darkblue", "mediumseagreen", "black",
                         "blue", "red2", "darkorange1", "tan4", "red", "black", "blue", "navy", "firebrick", "red",
                         "black", "royalblue", "midnightblue", "navy", "black", "navyblue")

color_code$currentTeam = unique(StandALL$Tm)[order(unique(StandALL$Tm))]
color_code$currentcolor = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                            "black", "red", "darkblue", "purple", "midnightblue",
                            "orange3", "royalblue", "firebrick1", "dodgerblue", "turquoise",
                            "goldenrod", "navy", "blue", "navyblue", "forestgreen",
                            "red", "black", "tan4", "darkorange1", "mediumseagreen",
                            "firebrick3", "navy", "blue", "deepskyblue", "red2")

#teams_colors_logos = load_mlb_teams() %>%
#  filter(!team_primary_abbr %in% c("AL", "NL", "MLB"))

##### Compiling WARs and Salaries; subsetting Cubs/Astros df #####

  ## Graph the Astros and Cubs W-L %s in the last 10 years
    ## Subset only Cubs and Astros data
Cubs_Astros_filteredStand = StandALL[ which(StandALL$Tm == "Chicago Cubs" | StandALL$Tm == "Houston Astros"), ]

    ## Add WAR, Salary columns
Cubs_Astros_filteredStand$WAR = 0
Cubs_Astros_filteredStand$Salary = 0
    ## Fill columns
Cubs_Astros_filteredStand$WAR[ which(Cubs_Astros_filteredStand$Tm == "Chicago Cubs") ] = TeamValALL[ which(TeamValALL$Tm == "Chicago Cubs"), 2 ]
Cubs_Astros_filteredStand$Salary[ which(Cubs_Astros_filteredStand$Tm == "Chicago Cubs") ] = TeamValALL[ which(TeamValALL$Tm == "Chicago Cubs"), 3 ]
Cubs_Astros_filteredStand$WAR[ which(Cubs_Astros_filteredStand$Tm == "Houston Astros") ] = TeamValALL[ which(TeamValALL$Tm == "Houston Astros"), 2 ]
Cubs_Astros_filteredStand$Salary[ which(Cubs_Astros_filteredStand$Tm == "Houston Astros") ] = TeamValALL[ which(TeamValALL$Tm == "Houston Astros"), 3 ]
    ## Create the WARPM column
Cubs_Astros_filteredStand$WARPM =  round(as.numeric(Cubs_Astros_filteredStand$WAR) / as.numeric(Cubs_Astros_filteredStand$Salary), digits = 3)
    ## Remove 2020
Cubs_Astros_filteredStand = Cubs_Astros_filteredStand[-which(Cubs_Astros_filteredStand$Year == 2020),]



##### W% over time #####

  ## Graph all W-L %s
WL = ggplot(data = TeamValALL[-which(TeamValALL$Year == 2020),]) +
  #geom_line(aes(x = WAR, y = W_L, color = Tm)) +
  geom_point(aes(x = Salary, y = W_L, color = Tm), size = 4, alpha = 0.7) +
  geom_abline(slope = as.numeric(coefficients(Sal_W_model)[2]),
              intercept = as.numeric(coefficients(Sal_W_model)[1]),
              color = "black",
              size = 1) +
  geom_text(x = 380, y = 0.620, label = expression(paste(("R")^"2", "= 0.176")), color = "black") +
  coord_cartesian(xlim = c(17,420), ylim = c(0.3, 0.750)) +
  labs(title = "MLB Team Performance Regression\n2011-2021", x = "Team Payroll (in M $US)", y = "W-L %", color = "Team") + 
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

##### Ws and WAR #####
  ## Graph all Ws and WAR vals
WL_WAR = ggplot(data = TeamValALL[-which(TeamValALL$Year == 2020),]) +
  #geom_line(aes(x = WAR, y = W_L, color = Tm)) +
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
  labs(title = "MLB Team Performance Regression\n2011-2021", x = "Team bWAR", y = "W-L %", color = "Team") + 
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

    ## Graph the teams with the worst W-L %s in the last 10 years
      ## Filter by 100 losses outside of the pandemic
bad_teams_idx = which(TeamValALL$L >= 100 & TeamValALL$Year != 2020)
bad_teams_list = unique(TeamValALL$Tm[bad_teams_idx])
  ## Subset a new df, add in the "efficiency" stat, and label the teams in the years with high efficiency that spent < $100 M
bad_filtered_StandALL = TeamValALL[ bad_teams_idx, ]
bad_filtered_StandALL$WARPM = bad_filtered_StandALL$WAR / bad_filtered_StandALL$Salary
bad_filtered_StandALL$labs = ""
bad_filtered_StandALL$labs[ which(bad_filtered_StandALL$Salary <= 100)] = c("'11 Astros", "'12 Astros", "'13 Marlins", "'13 Astros", "'21 Pirates", "'21 Rangers", "'21 O's")
paste(bad_filtered_StandALL$Year[ which(bad_filtered_StandALL$Salary <= 100)], bad_filtered_StandALL$Tm[ which(bad_filtered_StandALL$Salary <= 100)])

      ## Filter by teams with the same or worse win total as the 2011 Cubs; outside the pandemic
rebuild_idx = which(TeamValALL$W <= 70 & TeamValALL$Year != 2020)
rebuild_list = unique(TeamValALL$Tm[rebuild_idx])
  ## Subset a new df, add in the "efficiency" stat
rebuild_filtered_StandALL = TeamValALL[ rebuild_idx, ]
rebuild_filtered_StandALL$WARPM = as.numeric(rebuild_filtered_StandALL$WAR) / as.numeric(rebuild_filtered_StandALL$Salary)
rebuild_filtered_StandALL$labs = ""
rebuild_filtered_StandALL$labs[ which(rebuild_filtered_StandALL$Salary <= 100)] = c("'11 Astros", "'12 Astros", "'13 Twins", "'13 Marlins", "'13 Astros", "'14 Astros", "'15 A's", "'15 Phils", "'16 A's", "'16 Rays", "'16 Braves", "'16 Padres", "'19 Pirates", "'21 Marlins", "'21 Pirates", "'21 Rangers", "'21 O's")
paste(rebuild_filtered_StandALL$Year[ which(rebuild_filtered_StandALL$Salary <= 100)], rebuild_filtered_StandALL$Tm[ which(rebuild_filtered_StandALL$Salary <= 100)])

rebuild_filtered_StandALL[which(rebuild_filtered_StandALL$Salary <= 100),c(1,7)]
color_code[which(color_code$currentTeam %in% rebuild_list),c(3,4)]

rebuild_filtered_StandALL$Tm[ which(rebuild_filtered_StandALL$Tm == "Cleveland Indians")] = "Cleveland Guardians"
rebuild_filtered_StandALL_add = rebuild_filtered_StandALL %>%
  left_join(teams_colors_logos, by = c("Tm" = "team_name"))


WL_rebuild = ggplot(data = rebuild_filtered_StandALL) +
  geom_point(aes(x = WARPM, y = W_L, color = Tm), alpha = 0.7, size = 4) +
  #geom_mlb_logos(aes(x = WARPM, y = W_L, team_savant_abbr = team_logo_espn), width = 0.075, alpha = 0.7) +
  #geom_image(aes(x = WARPM,
  #               y = W_L,
  #               image = team_logo_espn,
  #               image_fun = transparent(team_logo_espn)),
  #           alpha = 0.3,
  #           size = 0.1,
  #           asp = 1.618
  #           ) +
  coord_cartesian(xlim = c(min(rebuild_filtered_StandALL$WARPM ),
                           max(rebuild_filtered_StandALL$WARPM )),
                  ylim = c(0.280, 0.750)) +
  #scale_x_continuous(breaks = seq(2011,2021,1)) +
  labs(title = "Re-building Teams' W%s \nand Payroll Efficiencies, 2011-2021",
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
  #scale_color_manual(values = c(rebuild_filtered_StandALL_add$team_color))
  scale_color_manual(values = c("darkred", "firebrick", "darkorange1", "firebrick", "dodgerblue",
                                "black", "red", "darkblue", "purple", "midnightblue",
                                "orange3", "royalblue", "turquoise", "goldenrod", "navy",
                                "blue", "forestgreen", "red", "black", "tan4",
                                "darkorange1", "mediumseagreen", "navy", "blue", "deepskyblue", "red2"))
  
  #scale_color_identity() +
  #scale_alpha_identity()
WL_rebuild = WL_rebuild + geom_text_repel(data = rebuild_filtered_StandALL, x = rebuild_filtered_StandALL$WARPM, y = rebuild_filtered_StandALL$W_L, color = "black", aes(label = labs), size = 3, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

TeamValALL$Tm[ which(TeamValALL$Tm == "Cleveland Indians") ] = "Cleveland Guardians"
TeamValALL$WARPM = as.numeric(TeamValALL$WAR) / as.numeric(TeamValALL$Salary)
TeamValALL$labs = ""
TeamValALL$labs[ order(TeamValALL$WARPM, decreasing = TRUE)[c(1:14)] ] = c("'11 Rays", "'18 Rays", "'21 Guardians", "'11 Royals", "'21 Rays", "'21 Marlins", "'12 A's", "'13 Marlins", "'14 Marlins", "'19 Rays", "'12 Rays", "'13 A's", "'15 Rays", "'18 A's")
color_code

WL_WARPM = ggplot(data = TeamValALL[ -which(TeamValALL$Year == 2020), ]) +
  geom_point(aes(x = WARPM, y = W_L, color = Tm), alpha = 0.7, size = 4) +
  coord_cartesian(xlim = c(min(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM ),
                           max(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM )),
                  ylim = c(0.280, 0.750)) +
  #scale_x_continuous(breaks = seq(2011,2021,1)) +
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


WL_WARPM

WL_WARPM = WL_WARPM + geom_text_repel(data = TeamValALL[ -which(TeamValALL$Year == 2020), ], x = TeamValALL$WARPM[ -which(TeamValALL$Year == 2020) ], y = TeamValALL$W_L[ -which(TeamValALL$Year == 2020) ], color = "black", aes(label = labs), size = 3, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)


"%notin%" = Negate("%in%")
color_code$currentTeam[which(color_code$currentTeam %notin% rebuild_list)]


TANK_DF$labs = ""
TANK_DF$labs[ which(TANK_DF$Year == 2016 & TANK_DF$Tm == "Chicago Cubs" | TANK_DF$Year == 2017 & TANK_DF$Tm == "Houston Astros")] = "Won WS"

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
#CubsStros_Payrolls = CubsStros_Payrolls + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`Team payroll`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_Payrolls

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
CubsStros_WL

## Graph the Astros and Cubs' FA spending
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
CubsStros_FA

ggarrange(CubsStros_WL, CubsStros_Payrolls, CubsStros_FA, ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")

  ##### College Picks #####
TANK_DF$labs = ""
TANK_DF$labs[ which(TANK_DF$Year == 2016 & TANK_DF$Tm == "Chicago Cubs")] = "World Series Win"
TANK_DF$labs[ which(TANK_DF$Year == 2017 & TANK_DF$Tm == "Houston Astros")] = "World Series Win"

CubsStros_College_Picks = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `College %`, color = Tm)) +
  geom_point(aes(y = `College %`, color = Tm)) + 
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2013, y = 50, label = "Epstein, Luhnow\nHired in Off-Season", color = "firebrick", size = 3) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 100)) +
  labs(title = "College Amateurs Drafted\n2011-2021", x = "Season", y = "College Players Taken in Amateur Draft (%)", color = "Stat") + 
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
#CubsStros_College_Picks = CubsStros_College_Picks + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`College %`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_College_Picks



##### FA Contracts #####

CubsStros_FA_Contracts = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `# FA Contracts`, color = Tm)) +
  geom_point(aes(y = `# FA Contracts`, color = Tm)) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 10)) +
  labs(title = "# MLB FA Contracts from 2011-2021", x = "Season", y = "# MLB FA Contracts from 2011-2021", color = "Stat") + 
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
CubsStros_FA_Contracts = CubsStros_FA_Contracts + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`# FA Contracts`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_FA_Contracts

##### FA Contract Length #####

CubsStros_FA_Contract_Length = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `Mean FA Length`, color = Tm)) +
  geom_point(aes(y = `Mean FA Length`, color = Tm)) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 4)) +
  labs(title = "Average MLB FA Contract Length from 2011-2021", x = "Season", y = "Average MLB FA Contract Length", color = "Stat") + 
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
CubsStros_FA_Contract_Length = CubsStros_FA_Contract_Length + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`Mean FA Length`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_FA_Contract_Length

##### FA Contract AAV #####

CubsStros_FA_Contract_AAV = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `Mean FA AAV`, color = Tm)) +
  geom_point(aes(y = `Mean FA AAV`, color = Tm)) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 17)) +
  labs(title = "Average MLB FA Contract AAV from 2011-2021", x = "Season", y = "Average MLB FA Contract AAV", color = "Stat") + 
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
CubsStros_FA_Contract_AAV = CubsStros_FA_Contract_AAV + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`Mean FA AAV`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_FA_Contract_AAV

##### Intl Signings #####

CubsStros_Intl_Signs = ggplot(data = TANK_DF[-which(TANK_DF$Year == 2020),], aes(x = Year)) +
  geom_line(aes(y = `# Int'l Signings`, color = Tm)) +
  geom_point(aes(y = `# Int'l Signings`, color = Tm)) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 20)) +
  geom_vline(xintercept = 2011.5, linetype = "dashed", color = "firebrick")+
  geom_text(x = 2013, y = 10, label = "Epstein, Luhnow\nHired in Off-Season", color = "firebrick", size = 3) +
  labs(title = "International Signings\n2011-2021", x = "Season", y = "# International Signings", color = "Stat") + 
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
#CubsStros_Intl_Signs = CubsStros_Intl_Signs + geom_text_repel(data = TANK_DF, x = TANK_DF$Year, y = TANK_DF$`# Int'l Signings`, color = "black", aes(label = labs), size = 4, max.overlaps = Inf, min.segment.length = 0, xlim = c(0,NA), ylim = c(0,NA), box.padding = 0.5)

CubsStros_Intl_Signs

ggarrange(CubsStros_College_Picks, CubsStros_Intl_Signs, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
##### Efficiency #####
CubsStros_Eff = ggplot(data = Printable_REBUILD, aes(x = Year)) +
  #geom_line(aes(y = WARPM, color = Tm, type = "dashed")) +
  geom_point(aes(y = WARPM, color = Tm), size = 2) +
  coord_cartesian(xlim = c(2011,2021), ylim = c(0, 1)) +
  labs(title = "Cubs and Astros' Payroll Efficiencies\n2011-2021", x = "Season", y = "WARPM: Team bWAR / Team payroll (in M $US)", color = "Stat") + 
  scale_x_continuous(breaks = c(seq(2011,2021,1))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        #legend.position = c(0.20,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(2011,2021,1)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13),
        axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("firebrick", "darkorange1", "dodgerblue", "black", "red",
                                "purple", "midnightblue", "orange3", "turquoise", "blue",
                                "forestgreen", "gold"))
CubsStros_Eff


Regress = ggplot(data = Printable_REBUILD, aes(x = `Payroll (in M $US)`)) +
  #geom_line(aes(y = W, color = Tm)) +
  geom_point(aes(y = W, color = Tm), size = 4, alpha = 0.7) +
  coord_cartesian(xlim = c(0,200), ylim = c(40, 80)) +
  labs(title = "Regressing re-builds\n2011-2021", x = "Team payroll (in M $US)", y = "Wins", color = "Team") + 
  #scale_x_continuous(breaks = c(seq(10,200,10))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        #legend.position = c(0.20,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(0,200,10)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13)) +
        #axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("firebrick", "darkorange1", "dodgerblue", "black", "red",
                                "purple", "midnightblue", "orange3", "turquoise", "blue3",
                                "forestgreen", "gold"))
Regress


Regress2 = ggplot(data = Printable_REBUILD, aes(x = bWAR)) +
  #geom_line(aes(y = W, color = Tm)) +
  geom_point(aes(y = W, color = Tm), size = 5, alpha = 0.7) +
  coord_cartesian(xlim = c(7,68), ylim = c(40, 110)) +
  labs(title = "Regressing re-builds\n2011-2021", x = "Team bWAR", y = "Wins", color = "Stat") + 
  #scale_x_continuous(breaks = c(seq(10,200,10))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        #legend.position = c(0.20,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(0,40,5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13)) +
  #axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("firebrick", "darkorange1", "dodgerblue", "black", "red",
                                "purple", "midnightblue", "orange3", "turquoise", "blue3",
                                "forestgreen", "gold"))
Regress2


Regress3 = ggplot(data = Printable_REBUILD, aes(x = `Payroll (in M $US)`)) +
  #geom_line(aes(y = W, color = Tm)) +
  geom_point(aes(y = bWAR, color = Tm), size = 2) +
  coord_cartesian(xlim = c(0,200), ylim = c(0,40)) +
  labs(title = "Regressing re-builds\n2011-2021", x = "Team payroll (in M $US)", y = "Team bWAR", color = "Stat") + 
  #scale_x_continuous(breaks = c(seq(10,200,10))) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.title = element_blank(),
        legend.direction = "vertical",
        legend.text = element_text(size = 11),
        #legend.position = c(0.20,0.8),
        axis.ticks.x = element_blank(),
        panel.grid.major.x =  element_line(color = "gray96", seq(0,200,10)),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13)) +
  #axis.text.x.bottom = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_color_manual(values = c("firebrick", "darkorange1", "dodgerblue", "black", "red",
                                "purple", "midnightblue", "orange3", "turquoise", "blue3",
                                "forestgreen", "gold"))
Regress3

GAM = gam(data = Printable_REBUILD, formula = Printable_REBUILD$W ~ s(Printable_REBUILD$bWAR, by = as.factor(Printable_REBUILD$`Tm`), k = 3, bs = "cr"))
summary(GAM)
k.check(GAM)

Regress2 +
  geom_smooth(method = "gam", aes(y = GAM$fitted.values), color = "gray", size = 1.5, se = F)


ggarrange(WL_WAR, WL_WARPM, WL_rebuild, ncol = 3, nrow = 1, common.legend = TRUE, legend = "right", labels = "AUTO")
##### Salary predicting W % #####

Sal_W = ggplot(data = TeamValALL[ which(TeamValALL$Year >= 2016), ]) +
  geom_point(aes(x = Salary, y = W_L, color = Tm)) +
  #stat_smooth(aes(x = Salary, y = W_L, color = "black"), method = "gam", se = FALSE) +
  geom_abline(slope = as.numeric(coefficients(Sal_W_model)[2]),
              intercept = as.numeric(coefficients(Sal_W_model)[1]),
              color = "black",
              size = 1) +
  geom_text(x = 375, y = 0.610, label = expression(paste(("R")^"2", "= 0.159")), color = "black") +
  coord_cartesian(xlim = c(min(TeamValALL$Salary),
                           max(TeamValALL$Salary)),
                  ylim = c(min(TeamValALL$W_L),
                           max(TeamValALL$W_L))) +
  labs(title = "MLB W%s as a function of Payroll\n2011-2021",
       x = "Team Payroll (M $US)",
       y = "Win %", color = "Stat") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray96"),
        panel.grid.major.y =  element_line(color = "gray96"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 13)) +
  scale_color_manual(values = c("firebrick1", "orange3", "forestgreen", "deepskyblue", "firebrick", "navy", "firebrick3",
                               "dodgerblue", "darkred", "dodgerblue", "darkorange1", "darkblue", "mediumseagreen", "black",
                               "blue", "red2", "darkorange1", "tan4", "red", "black", "blue", "navy", "firebrick", "red",
                               "black", "royalblue", "midnightblue", "navy", "black", "navyblue"))
Sal_W


#TANK_DF$YOYSal = TANK_DF %>%
#  group_by(Tm) %>%
#  for (i in 2021:2012) {
#    as.numeric(TANK_DF$`Team payroll`[i]) / as.numeric(TANK_DF$`Team payroll`[i-1])
#  }
##### #####
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

Grading_Tanks = Grading_Tanks %>%
  arrange(desc(`Rebuild Grade`))

Grading_Tanks$Tm = c("Houston Astros ('11-'13)", "Oakland Athletics ('15-'16)", "Chicago Cubs ('12-'13)", "Atlanta Braves ('15-'16)",
                     "Chicago White Sox ('17-'18)", "Colorado Rockies ('14-'15)", "Detroit Tigers ('17-'19)", "Minnesota Twins ('11-'13)",
                     "Miami Marlins ('12-'13)", "Cincinnati Reds ('15-'16)")


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

Grading_T


TeamValALL$ExpW_L = 0
TeamValALL$ExpW_L[-c(271:300)] = round(as.numeric(WL_WAR_lm$fitted.values), digits = 3)
TeamValALL$Diff_W_L = TeamValALL$ExpW_L / TeamValALL$W_L


Actual_vs_Exp = ggplot(data = TeamValALL[ -which(TeamValALL$Year == 2020), ]) +
  geom_point(aes(x = WARPM, y = Diff_W_L, color = Tm), alpha = 0.7, size = 4) +
  coord_cartesian(xlim = c(min(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM ),
                           max(TeamValALL[ -which(TeamValALL$Year == 2020) ]$WARPM )),
                  ylim = c(0, 1.4)) +
  #scale_x_continuous(breaks = seq(2011,2021,1)) +
  labs(title = "MLB Team W% Residuals \nand Payroll Efficiencies, 2011-2021",
       x = "WARPM: Team bWAR / Team Payroll (in M $US)",
       y = "Actual W-L % - Expected W-L %",
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

Actual_vs_Exp

