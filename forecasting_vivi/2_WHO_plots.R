#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

# setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")
setwd("C:/Users/haowe/Desktop/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "aweek", "maps", "scales", "ggpubr", 
          "cowplot", "aweek", "hrbrthemes", "viridis","RColorBrewer")
load_package(pkgs)

#' Load self-written functions 
source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")

#' Load country list for WHO data.
countryISO <- read.csv("C:/Users/haowe/Desktop/iiag/data_old/country_list_ISO.csv")

#' Load data
fluWHO <- readRDS("./saved_objects/fluWHO.rds")

fluWHO_incidence <- fluWHO[[1]]
sel_iso_xgb <- fluWHO[[2]]

acc_WHO_roll_fix <- readRDS("./saved_objects/acc_WHO_roll_fix.rds")
baseline_acc <- readRDS("./saved_objects/acc_WHO_baseline.rds")
gbm_complex_all <- readRDS("./saved_objects/df_gbm_complex_all.rds")
indi_acc_baseline <- read.csv("./saved_objects/acc_hist_null_byYears.csv")
indi_acc_roll_fix <- read.csv("./saved_objects/indi_acc_roll_fix_by_countryWeek.csv")

countryISO$Country <- ifelse(countryISO$Country=="Moldova, Republic of", "Moldova", countryISO$Country)
countryISO$Country <- ifelse(countryISO$Country=="United States", "USA", countryISO$Country)

############## country map ############
country_for_map <- countryISO[which(countryISO$ISO3 %in% sel_iso_xgb), ]

world_map <- map_data("world")

# which(country_for_map$Country%in%world_map$region == FALSE)
# country_for_map$Country[19] <- "Moldova"
# country_for_map$Country[32] <- "USA"

xgb_world_map <- world_map[which(world_map$region %in% country_for_map$Country), ]

  
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") +
  geom_polygon(data = xgb_world_map, aes(x = long, y = lat, group = group), fill = "red", colour = "white")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank())

ggsave(filename = "./Figures/xgb_country_map.pdf", width = 12, height = 8, dpi = 320, scale = 1)
ggsave(filename = "./Figures/xgb_country_map.png", width = 12, height = 8, dpi = 320, scale = 1)

############ distribution of categories ##########
prop_category <- with(gbm_complex_all, table(Y_week0)) %>% 
  prop.table() %>% 
  as.data.frame() %>% 
  rename(Proportion = Freq,
         Incidence_level = Y_week0) %>% 
  mutate(Prop_percent = percent(Proportion, accuracy = 0.01))

prop_bar <- ggplot(data=prop_category, aes(x=Incidence_level, y=Proportion)) +
  geom_bar(stat="identity", width=1, colour = "white", fill="steelblue")+
  scale_x_discrete("Incidence level", breaks = c(1:10))+
  coord_cartesian(ylim = c(0, 1))+
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1),
                     labels = c(0, 25, 50, 75, 100))+
  ylab("Proportion (%)")+
  # scale_y_continuous("Proportion",expand = c(0, 0),limits = c(0,1.00))+
  geom_text(aes(label=Prop_percent), vjust=-0.2, size=5)+
  theme_bw()+
  theme(text = element_text(size = 18))

prop_bar
ggsave(filename = "./Figures/prop_category_all.pdf", width = 12, height = 8, dpi = 320, scale = 1)
ggsave(filename = "./Figures/prop_category_all.png", width = 12, height = 8, dpi = 320, scale = 1)

#' Epi-like heat chart 
df_epi_heat <- gbm_complex_all %>% 
  dplyr::select(-week_4, -week_5, -month, -season) %>% 
  rename(ISO = Country,
         Incidence_level = Y_week0) %>% 
  mutate(Year = substr(rownames(gbm_complex_all),0,4),
         Week = substr(rownames(gbm_complex_all),6,7)) %>% 
  # mutate(Year = as.numeric(Year),
  #        Week = as.numeric(Week)) %>% 
  filter(Year %in% c(2015:2017)) 


for (i in 1:nrow(df_epi_heat)){
  index <- which(countryISO$ISO3 %in% df_epi_heat$ISO[i])
  df_epi_heat$Country[i] <- countryISO$Country[index]
}

  
df_epi_heat <- df_epi_heat %>% 
  dplyr::select(-ISO) %>% 
  group_by(Country, Year) %>%
  mutate(Week = as.numeric(Week)) %>% 
  tidyr::complete(Week = 1:53) %>% 
  ungroup(Country, Year) %>% 
  mutate(Week = sprintf("%02d", Week)) %>% 
  mutate(Country = factor(Country, levels = unique(Country)),
         Year_week = paste0(Year,"-", Week)) %>% 
  mutate(Year_week = gsub("-", "-W", Year_week, fixed = TRUE),
         Incidence_level = as.numeric(Incidence_level)) %>% 
  mutate(Year_week = week2date(Year_week),
         Incidence_level = factor(Incidence_level, levels = c(10:1))) 

df_epi_heat <- df_epi_heat[-which(df_epi_heat$Year!=2015 & df_epi_heat$Week=="53"), ]

epi_heat <- ggplot(data = df_epi_heat, aes(x = Year_week, y = Country))+
  geom_tile(data = subset(df_epi_heat, !is.na(Incidence_level)), aes(fill = Incidence_level))+
  geom_tile(data = subset(df_epi_heat,  is.na(Incidence_level)), aes(fill = NA, colour = "NA"), 
            linetype = 0, fill = "grey70", alpha = 0.5)+
  labs(x = "Year",
       color = "Incidence level")+
  scale_fill_manual(name="Incidence level",
                      values = c("#fde725", "#b5de2b", "#6ece58", "#35b779", "#1f9e89",
                                 "#26828e", "#26828e", "#3e4989", "#482878", "#440154"))+
  theme_bw()+
  theme(text = element_text(size = 20))

epi_heat

ggsave(filename = "./Figures/category_heatmap.pdf", width = 12, height = 8, dpi = 320, scale = 1)
ggsave(filename = "./Figures/category_heatmap.png", width = 12, height = 8, dpi = 320, scale = 1)

ggarrange(prop_bar, epi_heat, labels = c("A", "B"),
          ncol = 1, nrow = 2, align = "v")

ggsave(filename = "./Figures/category_barchart_heatmap.pdf", width = 8, height = 12, dpi = 320, scale = 1)
ggsave(filename = "./Figures/category_barchart_heatmap.png", width = 8, height = 12, dpi = 320, scale = 1)

########## accuracy comparison ###########

#' MZE baseline model
MZE_basic <- data.frame(Model = c(rep("hist", 12), rep("null", 12)),
                         Year = rep(c("2015","2016", "2017"), 8),
                         Week_ahead = rep(c(rep("1-week",3), rep("2-week", 3), rep("3-week", 3),
                                            rep("4-week", 3)), 2),
                         MZE = c(baseline_acc$acc_1week_pred15_hist$MZE, baseline_acc$acc_1week_pred16_hist$MZE,
                                 baseline_acc$acc_1week_pred17_hist$MZE, baseline_acc$acc_2week_pred15_hist$MZE,
                                 baseline_acc$acc_2week_pred16_hist$MZE, baseline_acc$acc_2week_pred17_hist$MZE,
                                 baseline_acc$acc_3week_pred15_hist$MZE, baseline_acc$acc_3week_pred16_hist$MZE,
                                 baseline_acc$acc_3week_pred17_hist$MZE, baseline_acc$acc_4week_pred15_hist$MZE,
                                 baseline_acc$acc_4week_pred16_hist$MZE, baseline_acc$acc_4week_pred17_hist$MZE,
                                 baseline_acc$acc_1week_pred15_null$MZE, baseline_acc$acc_1week_pred16_null$MZE,
                                 baseline_acc$acc_1week_pred17_null$MZE, baseline_acc$acc_2week_pred15_null$MZE,
                                 baseline_acc$acc_2week_pred16_null$MZE, baseline_acc$acc_2week_pred17_null$MZE,
                                 baseline_acc$acc_3week_pred15_null$MZE, baseline_acc$acc_3week_pred16_null$MZE,
                                 baseline_acc$acc_3week_pred17_null$MZE, baseline_acc$acc_4week_pred15_null$MZE,
                                 baseline_acc$acc_4week_pred16_null$MZE, baseline_acc$acc_4week_pred17_null$MZE)) %>%
  mutate(Year = factor(Year, levels = c("2015","2016", "2017")))

MZE_CI_basic_hist <- NULL
for (week in unique(indi_acc_baseline$Week_ahead)){
  for(year in unique(indi_acc_baseline$Year)){
    temp.CI.hist <- est.CI(indi_acc_baseline$MZE[which(indi_acc_baseline$Year==as.numeric(year)& 
                                                         indi_acc_baseline$Model=="hist"&
                                                         indi_acc_baseline$Week_ahead==as.numeric(week))]) 
    MZE_CI_basic_hist <- rbind(MZE_CI_basic_hist, temp.CI.hist)
  }
}

MZE_CI_basic_null <- NULL
for (week in unique(indi_acc_baseline$Week_ahead)){
  for(year in unique(indi_acc_baseline$Year)){
    temp.CI.null <- est.CI(indi_acc_baseline$MZE[which(indi_acc_baseline$Year==as.numeric(year)& 
                                                         indi_acc_baseline$Model=="null"&
                                                         indi_acc_baseline$Week_ahead==as.numeric(week))]) 
    MZE_CI_basic_null <- rbind(MZE_CI_basic_null, temp.CI.null)
  }
}

MZE_CI_basic <- rbind(MZE_CI_basic_hist, MZE_CI_basic_null)

MZE_basic <- cbind(MZE_basic, MZE_CI_basic) %>% 
  rename(lb = "1",
         ub = "2")

#' overall MZE XGBoost
MZE_score <- data.frame(Model = c(rep("extending", 12), rep("fix", 12)),
                        Year = rep(c("2015","2016", "2017"), 8),
                        Week_ahead = rep(c(rep("1-week",3), rep("2-week", 3), rep("3-week", 3),
                                           rep("4-week", 3)), 2),
                        MZE = c(acc_WHO_roll_fix$acc_1week_pred15_rol$MZE, acc_WHO_roll_fix$acc_1week_pred16_rol$MZE,
                                acc_WHO_roll_fix$acc_1week_pred17_rol$MZE, acc_WHO_roll_fix$acc_2week_pred15_rol$MZE,
                                acc_WHO_roll_fix$acc_2week_pred16_rol$MZE, acc_WHO_roll_fix$acc_2week_pred17_rol$MZE,
                                acc_WHO_roll_fix$acc_3week_pred15_rol$MZE, acc_WHO_roll_fix$acc_3week_pred16_rol$MZE,
                                acc_WHO_roll_fix$acc_3week_pred17_rol$MZE, acc_WHO_roll_fix$acc_4week_pred15_rol$MZE,
                                acc_WHO_roll_fix$acc_4week_pred16_rol$MZE,acc_WHO_roll_fix$acc_4week_pred17_rol$MZE,
                                acc_WHO_roll_fix$acc_1week_pred15_fix$MZE, acc_WHO_roll_fix$acc_1week_pred16_fix$MZE,
                                acc_WHO_roll_fix$acc_1week_pred17_fix$MZE, acc_WHO_roll_fix$acc_2week_pred15_fix$MZE,
                                acc_WHO_roll_fix$acc_2week_pred16_fix$MZE, acc_WHO_roll_fix$acc_2week_pred17_fix$MZE,
                                acc_WHO_roll_fix$acc_3week_pred15_fix$MZE, acc_WHO_roll_fix$acc_3week_pred16_fix$MZE,
                                acc_WHO_roll_fix$acc_3week_pred17_fix$MZE, acc_WHO_roll_fix$acc_4week_pred15_fix$MZE,
                                acc_WHO_roll_fix$acc_4week_pred16_fix$MZE, acc_WHO_roll_fix$acc_4week_pred17_fix$MZE)) %>%
  mutate(Year = factor(Year, levels = c("2015","2016", "2017")))

#' MZE score XGBoost extending window
MZE_score_roll <- MZE_score %>%
  filter(Model == "extending")

MZE_CI_ext <- NULL
for (week in unique(indi_acc_roll_fix$Week_ahead)){
  for(year in unique(indi_acc_roll_fix$Year)){
    temp.CI <- est.CI(indi_acc_roll_fix$MZE[which(indi_acc_roll_fix$Year==as.numeric(year)& 
                                                    indi_acc_roll_fix$Model=="rolling"&
                                                    indi_acc_roll_fix$Week_ahead==as.numeric(week))]) 
    MZE_CI_ext <- rbind(MZE_CI_ext, temp.CI)
  }
}
MZE_score_roll <- cbind(MZE_score_roll, MZE_CI_ext) %>% 
  rename(lb = "1",
         ub = "2")

#' MZE score XGBoost fix window
MZE_score_fix <- MZE_score %>%
  filter(Model == "fix")

MZE_CI_fix <- NULL
for (week in unique(indi_acc_roll_fix$Week_ahead)){
  for(year in unique(indi_acc_roll_fix$Year)){
    temp.CI <- est.CI(indi_acc_roll_fix$MZE[which(indi_acc_roll_fix$Year==as.numeric(year)& 
                                                    indi_acc_roll_fix$Model=="fix"&
                                                    indi_acc_roll_fix$Week_ahead==as.numeric(week))]) 
    MZE_CI_fix <- rbind(MZE_CI_fix, temp.CI)
  }
}
MZE_score_fix <- cbind(MZE_score_fix, MZE_CI_fix) %>% 
  rename(lb = "1",
         ub = "2")

#' macro MAE baseline model
MAE_basic <- data.frame(Model = c(rep("hist", 12), rep("null", 12)),
                        Year = rep(c("2015","2016", "2017"), 8),
                        Week_ahead = rep(c(rep("1-week",3), rep("2-week", 3), rep("3-week", 3),
                                           rep("4-week", 3)), 2),
                        macroMAE= c(baseline_acc$acc_1week_pred15_hist$macroMAE, baseline_acc$acc_1week_pred16_hist$macroMAE,
                                baseline_acc$acc_1week_pred17_hist$macroMAE, baseline_acc$acc_2week_pred15_hist$macroMAE,
                                baseline_acc$acc_2week_pred16_hist$macroMAE, baseline_acc$acc_2week_pred17_hist$macroMAE,
                                baseline_acc$acc_3week_pred15_hist$macroMAE, baseline_acc$acc_3week_pred16_hist$macroMAE,
                                baseline_acc$acc_3week_pred17_hist$macroMAE, baseline_acc$acc_4week_pred15_hist$macroMAE,
                                baseline_acc$acc_4week_pred16_hist$macroMAE, baseline_acc$acc_4week_pred17_hist$macroMAE,
                                baseline_acc$acc_1week_pred15_null$macroMAE, baseline_acc$acc_1week_pred16_null$macroMAE,
                                baseline_acc$acc_1week_pred17_null$macroMAE, baseline_acc$acc_2week_pred15_null$macroMAE,
                                baseline_acc$acc_2week_pred16_null$macroMAE, baseline_acc$acc_2week_pred17_null$macroMAE,
                                baseline_acc$acc_3week_pred15_null$macroMAE, baseline_acc$acc_3week_pred16_null$macroMAE,
                                baseline_acc$acc_3week_pred17_null$macroMAE, baseline_acc$acc_4week_pred15_null$macroMAE,
                                baseline_acc$acc_4week_pred16_null$macroMAE, baseline_acc$acc_4week_pred17_null$macroMAE)) %>%
  mutate(Year = factor(Year, levels = c("2015","2016", "2017")))

MAE_CI_basic_hist <- NULL
for (week in unique(indi_acc_baseline$Week_ahead)){
  for(year in unique(indi_acc_baseline$Year)){
    temp.CI.hist <- est.CI(indi_acc_baseline$macroMAE[which(indi_acc_baseline$Year==as.numeric(year)& 
                                                         indi_acc_baseline$Model=="hist"&
                                                         indi_acc_baseline$Week_ahead==as.numeric(week))]) 
    MAE_CI_basic_hist <- rbind(MAE_CI_basic_hist, temp.CI.hist)
  }
}

MAE_CI_basic_null <- NULL
for (week in unique(indi_acc_baseline$Week_ahead)){
  for(year in unique(indi_acc_baseline$Year)){
    temp.CI.null <- est.CI(indi_acc_baseline$macroMAE[which(indi_acc_baseline$Year==as.numeric(year)& 
                                                         indi_acc_baseline$Model=="null"&
                                                         indi_acc_baseline$Week_ahead==as.numeric(week))]) 
    MAE_CI_basic_null <- rbind(MAE_CI_basic_null, temp.CI.null)
  }
}

MAE_CI_basic <- rbind(MAE_CI_basic_hist, MAE_CI_basic_null)

MAE_basic <- cbind(MAE_basic, MAE_CI_basic) %>% 
  rename(lb = "1",
         ub = "2")

#' overall macro MAE XGBoost model
MAE_score <- data.frame(Model = c(rep("extending", 12), rep("fix", 12)),
                        Year = rep(c("2015","2016", "2017"), 8),
                        Week_ahead = rep(c(rep("1-week",3), rep("2-week", 3), rep("3-week", 3),
                                           rep("4-week", 3)), 2),
                        macroMAE = c(acc_WHO_roll_fix$acc_1week_pred15_rol$macroMAE, acc_WHO_roll_fix$acc_1week_pred16_rol$macroMAE,
                                acc_WHO_roll_fix$acc_1week_pred17_rol$macroMAE, acc_WHO_roll_fix$acc_2week_pred15_rol$macroMAE,
                                acc_WHO_roll_fix$acc_2week_pred16_rol$macroMAE, acc_WHO_roll_fix$acc_2week_pred17_rol$macroMAE,
                                acc_WHO_roll_fix$acc_3week_pred15_rol$macroMAE, acc_WHO_roll_fix$acc_3week_pred16_rol$macroMAE,
                                acc_WHO_roll_fix$acc_3week_pred17_rol$macroMAE, acc_WHO_roll_fix$acc_4week_pred15_rol$macroMAE,
                                acc_WHO_roll_fix$acc_4week_pred16_rol$macroMAE,acc_WHO_roll_fix$acc_4week_pred17_rol$macroMAE,
                                acc_WHO_roll_fix$acc_1week_pred15_fix$macroMAE, acc_WHO_roll_fix$acc_1week_pred16_fix$macroMAE,
                                acc_WHO_roll_fix$acc_1week_pred17_fix$macroMAE, acc_WHO_roll_fix$acc_2week_pred15_fix$macroMAE,
                                acc_WHO_roll_fix$acc_2week_pred16_fix$macroMAE, acc_WHO_roll_fix$acc_2week_pred17_fix$macroMAE,
                                acc_WHO_roll_fix$acc_3week_pred15_fix$macroMAE, acc_WHO_roll_fix$acc_3week_pred16_fix$macroMAE,
                                acc_WHO_roll_fix$acc_3week_pred17_fix$macroMAE, acc_WHO_roll_fix$acc_4week_pred15_fix$macroMAE,
                                acc_WHO_roll_fix$acc_4week_pred16_fix$macroMAE, acc_WHO_roll_fix$acc_4week_pred17_fix$macroMAE)) %>% 
  mutate(Year = factor(Year, levels = c("2015","2016", "2017")))

acc_roll_fix_byYears <- full_join(MZE_score,MAE_score, by = c("Model", "Year", "Week_ahead"))
write.csv(acc_roll_fix_byYears, "./saved_objects/acc_roll_fix_byYears.csv", row.names = FALSE)

#' macro MAE XBoost extending window
MAE_score_roll <- MAE_score %>%
  filter(Model == "extending")

MAE_CI_ext <- NULL
for (week in unique(indi_acc_roll_fix$Week_ahead)){
  for(year in unique(indi_acc_roll_fix$Year)){
    temp.CI <- est.CI(indi_acc_roll_fix$macroMAE[which(indi_acc_roll_fix$Year==as.numeric(year)& 
                                                    indi_acc_roll_fix$Model=="rolling"&
                                                    indi_acc_roll_fix$Week_ahead==as.numeric(week))]) 
    MAE_CI_ext <- rbind(MAE_CI_ext, temp.CI)
  }
}

MAE_score_roll <- cbind(MAE_score_roll, MAE_CI_ext) %>% 
  rename(lb = "1",
         ub = "2")


#' macro MAE XBoost fixed window
MAE_score_fix <- MAE_score %>%
  filter(Model == "fix")

MAE_CI_fix <- NULL
for (week in unique(indi_acc_roll_fix$Week_ahead)){
  for(year in unique(indi_acc_roll_fix$Year)){
    temp.CI <- est.CI(indi_acc_roll_fix$macroMAE[which(indi_acc_roll_fix$Year==as.numeric(year)& 
                                                         indi_acc_roll_fix$Model=="fix"&
                                                         indi_acc_roll_fix$Week_ahead==as.numeric(week))]) 
    MAE_CI_fix <- rbind(MAE_CI_fix, temp.CI)
  }
}

MAE_score_fix <- cbind(MAE_score_fix, MAE_CI_fix) %>% 
  rename(lb = "1",
         ub = "2")

######## rolling and fix XGBoost comparison #######
MZE_line_roll <- ggplot(MZE_score_roll, aes(x = Week_ahead,y = MZE, group = Year, color=Year))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0.15, 0.45))+
  # ylim(0.1,0.45)+
  labs(y = "Mean-Zero Error",
       x = "n-week ahead",
       title = "A. Extending window")+
  theme_bw()+
  theme(text = element_text(size = 18))

MZE_line_fix <- ggplot(MZE_score_fix, aes(x = Week_ahead,y = MZE, group=Year, color=Year))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0.15, 0.45))+
  # ylim(0.2,0.35)+
  labs(y = "Mean-Zero Error",
       x = "n-week ahead",
       title = "B. Fixed window")+
  theme_bw()+
  theme(text = element_text(size = 18))

ggarrange(MZE_line_roll, MZE_line_fix,
          common.legend = TRUE, legend = "right",
          # labels = c("A", "B"), 
          ncol = 2, nrow = 1)

ggsave(filename = "./Figures/MZE_roll_fix.pdf", width = 12, height = 8, dpi = 320, scale = 1)
ggsave(filename = "./Figures/MZE_roll_fix.png", width = 12, height = 8, dpi = 320, scale = 1)

#' macro-MAE
MAE_line_roll <- ggplot(MAE_score_roll, aes(x = Week_ahead,y = macroMAE, group=Year, color=Year))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0.3, 1.6))+
  # ylim(0.7,1.8)+
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead",
       title = "A. Extending window")+
  theme_bw()+
  theme(text = element_text(size = 18))

MAE_line_fix <- ggplot(MAE_score_fix, aes(x = Week_ahead,y = macroMAE, group=Year, color = Year))+
  geom_point(size = 3)+
  geom_line(size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2,
                position=position_dodge(0.05))+
  coord_cartesian(ylim = c(0.3, 1.6))+
  # ylim(1.6, 2.75)+
  labs(y = "macro-average Mean Absolute Error",
       x = "n-week ahead",
       title = "B. Fixed window")+
  theme_bw()+
  theme(text = element_text(size = 18))

ggarrange(MAE_line_roll, MAE_line_fix,
          common.legend = TRUE, legend = "right",
          # labels = c("A", "B"), 
          ncol = 2, nrow = 1)    

ggsave(filename = "./Figures/MAE_roll_fix.pdf", width = 12, height = 8, dpi = 320, scale = 1)
ggsave(filename = "./Figures/MAE_roll_fix.png", width = 12, height = 8, dpi = 320, scale = 1)

######### rolling XGBoost and baseline comparison ########

#' MZE plots
df_MZE_baseline_roll <- rbind(MZE_basic, MZE_score_roll) %>% 
  mutate(Model = case_when(Model == "extending" ~ "XGBoost \n(extending window)",
                           Model == "hist" ~ "Historical average",
                           Model == "null" ~ "Null")) %>% 
  mutate(Model = factor(Model, levels = c("XGBoost \n(extending window)", "Historical average", "Null")))

df_MZE_baseline_roll_15 <- df_MZE_baseline_roll %>% 
  filter(Year == 2015)

df_MZE_baseline_roll_16 <- df_MZE_baseline_roll %>% 
  filter(Year == 2016)

df_MZE_baseline_roll_17 <- df_MZE_baseline_roll %>% 
  filter(Year == 2017)

MZE_baseline_roll_15 <- ggplot(df_MZE_baseline_roll_15, aes(x = Week_ahead,y = MZE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,0.6))+
  labs(title = "A.year 2015",
       y = "Mean-Zero Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

MZE_baseline_roll_16 <- ggplot(df_MZE_baseline_roll_16, aes(x = Week_ahead,y = MZE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,0.6))+
  labs(title = "B.year 2016",
       y = "Mean-Zero Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

MZE_baseline_roll_17 <- ggplot(df_MZE_baseline_roll_17, aes(x = Week_ahead,y = MZE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,0.6))+
  labs(title = "C.year 2017",
       y = "Mean-Zero Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

ggarrange(MZE_baseline_roll_15, MZE_baseline_roll_16, MZE_baseline_roll_17,
          common.legend = TRUE, legend = "bottom", ncol = 3, nrow = 1)

ggsave(filename = "./Figures/MZE_baseline_roll.pdf", width = 14, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/MZE_baseline_roll.png", width = 14, height = 10, dpi = 320, scale = 1)

#' MAE plot

df_MAE_baseline_roll <- rbind(MAE_basic, MAE_score_roll) %>% 
  mutate(Model = case_when(Model == "extending" ~ "XGBoost \n(extending window)",
                           Model == "hist" ~ "Historical average",
                           Model == "null" ~ "Null")) %>% 
  mutate(Model = factor(Model, levels = c("XGBoost \n(extending window)", "Historical average", "Null")))

df_MAE_baseline_roll_15 <- df_MAE_baseline_roll %>% 
  filter(Year == 2015)

df_MAE_baseline_roll_16 <- df_MAE_baseline_roll %>% 
  filter(Year == 2016)

df_MAE_baseline_roll_17 <- df_MAE_baseline_roll %>% 
  filter(Year == 2017)

MAE_baseline_roll_15 <- ggplot(df_MAE_baseline_roll_15, aes(x = Week_ahead,y = macroMAE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,2.5))+
  labs(title = "A.year 2015",
       y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

MAE_baseline_roll_16 <- ggplot(df_MAE_baseline_roll_16, aes(x = Week_ahead,y = macroMAE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,2.5))+
  labs(title = "B.year 2016",
       y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

MAE_baseline_roll_17 <- ggplot(df_MAE_baseline_roll_17, aes(x = Week_ahead,y = macroMAE, group=Model))+
  geom_point(aes(color = Model, shape = Model),size = 3)+
  geom_line(aes(color = Model, linetype = Model), size = 1)+
  geom_errorbar(aes(ymin = lb, ymax = ub, color = Model, linetype = Model),
                position=position_dodge(0.05), size = 1)+
  coord_cartesian(ylim = c(0,2.5))+
  labs(title = "C.year 2017",
       y = "macro-average Mean Absolute Error",
       x = "n-week ahead")+
  scale_color_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 18))

ggarrange(MAE_baseline_roll_15, MAE_baseline_roll_16, MAE_baseline_roll_17,
          common.legend = TRUE, legend = "bottom", ncol = 3, nrow = 1)

ggsave(filename = "./Figures/MAE_baseline_roll.pdf", width = 14, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/MAE_baseline_roll.png", width = 14, height = 10, dpi = 320, scale = 1)

########### Heatmap for accuracy metrics #######
indi_acc_roll_fix <- indi_acc_roll_fix %>%
  rename(ISO3 = Country)

for (i in 1:nrow(indi_acc_roll_fix)){
  index <- which(countryISO$ISO3 %in% indi_acc_roll_fix$ISO3[i])
  indi_acc_roll_fix$Country[i] <- countryISO$Country[index]
}

indi_acc_roll_fix <- indi_acc_roll_fix[order(indi_acc_roll_fix$Country),]

#' dataframe for MZE 
df_heatmap_MZE_15 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2015) %>% 
  dplyr::select(-Accuracy, -macroMAE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

df_heatmap_MZE_16 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2016) %>% 
  dplyr::select(-Accuracy, -macroMAE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

df_heatmap_MZE_17 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2017) %>% 
  dplyr::select(-Accuracy, -macroMAE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

#' data frame for macro-MAE
df_heatmap_MAE_15 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2015) %>% 
  dplyr::select(-Accuracy, -MZE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

df_heatmap_MAE_16 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2016) %>% 
  dplyr::select(-Accuracy, -MZE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

df_heatmap_MAE_17 <- indi_acc_roll_fix %>% 
  filter(Model == "rolling") %>% 
  filter(Year == 2017) %>% 
  dplyr::select(-Accuracy, -MZE) %>% 
  mutate(Country = factor(Country, levels = unique(Country)))

heatmap_MZE_15 <- ggplot(data = df_heatmap_MZE_15, aes(x = Week_ahead, y = Country, fill = MZE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(MZE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Blues")+
  labs(# title = "A.year 2015",
       x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_MZE_16 <- ggplot(data = df_heatmap_MZE_16, aes(x = Week_ahead, y = Country, fill = MZE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(MZE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Blues")+
  labs(#title = "B.year 2016",
       x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_MZE_17 <- ggplot(data = df_heatmap_MZE_17, aes(x = Week_ahead, y = Country, fill = MZE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(MZE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Blues")+
  labs(#title = "C.year 2017",
       x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

ggarrange(heatmap_MZE_15, 
          heatmap_MZE_16 + theme(axis.text.y=element_blank(),
                                                 axis.title.y=element_blank()), 
          heatmap_MZE_17 + theme(axis.text.y=element_blank(),
                                 axis.title.y=element_blank()), 
          ncol = 3, nrow = 1, common.legend = TRUE,legend = "bottom",
          labels = c("A.2015", "B.2016", "C.2017"),
          align = "v")
ggsave(filename = "./Figures/heatmap_acc_MZE.pdf", width = 14, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_acc_MZE.png", width = 14, height = 10, dpi = 320, scale = 1)

heatmap_MAE_15 <- ggplot(data = df_heatmap_MAE_15, aes(x = Week_ahead, y = Country, fill = macroMAE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(macroMAE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Oranges")+
  labs(# title = " ",
       x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_MAE_16 <- ggplot(data = df_heatmap_MAE_16, aes(x = Week_ahead, y = Country, fill = macroMAE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(macroMAE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Oranges")+
  labs(# title = "A.year 2015",
    x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_MAE_17 <- ggplot(data = df_heatmap_MAE_17, aes(x = Week_ahead, y = Country, fill = macroMAE))+
  geom_tile(color= "white")+
  geom_text(aes(label = sprintf(macroMAE, fmt = '%#.3f')), color = "black", size = 4)+
  scale_fill_distiller(palette = "Oranges")+
  labs(# title = "A.year 2015",
    x = "n-week ahead")+
  theme_minimal()+
  theme(text = element_text(size = 18))

ggarrange(heatmap_MAE_15, 
          heatmap_MAE_16 + theme(axis.text.y=element_blank(),
                                 axis.title.y=element_blank()), 
          heatmap_MAE_16 + theme(axis.text.y=element_blank(),
                                 axis.title.y=element_blank()), 
          ncol = 3, nrow = 1, common.legend = TRUE,legend = "bottom",
          labels = c("A.2015", "B.2016", "C.2017"),
          align = "v")
ggsave(filename = "./Figures/heatmap_acc_MAE.pdf", width = 14, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_acc_MAE.png", width = 14, height = 10, dpi = 320, scale = 1)

# heatmap_acc_15 <- ggarrange(heatmap_MZE_15, heatmap_MAE_15, 
#           # labels = "A.year 2015",
#           # common.legend = TRUE, legend = "bottom", 
#                  ncol = 2, nrow = 1, align = "v")
# annotate_figure(heatmap_acc_15, text_grob("A.year 2015", face = "bold", size = 18))
# 
# ggsave(filename = "./Figures/heatmap_acc_15.pdf", width = 14, height = 10, dpi = 320, scale = 1)
# ggsave(filename = "./Figures/heatmap_acc_15.png", width = 14, height = 10, dpi = 320, scale = 1)

# heatmap_acc_16 <- ggarrange(heatmap_MZE_16, heatmap_MAE_16, 
#                             # labels = "A.year 2015",
#                             # common.legend = TRUE, legend = "bottom", 
#                             ncol = 2, nrow = 1, align = "v")
# annotate_figure(heatmap_acc_16, text_grob("B.year 2016", face = "bold", size = 18))
# 
# ggsave(filename = "./Figures/heatmap_acc_16.pdf", width = 14, height = 10, dpi = 320, scale = 1)
# ggsave(filename = "./Figures/heatmap_acc_16.png", width = 14, height = 10, dpi = 320, scale = 1)
# 
# heatmap_acc_17 <- ggarrange(heatmap_MZE_17, heatmap_MAE_17, 
#                             # labels = "A.year 2015",
#                             # common.legend = TRUE, legend = "bottom", 
#                             ncol = 2, nrow = 1, align = "v")
# annotate_figure(heatmap_acc_17, text_grob("C.year 2017", face = "bold", size = 18))
# 
# ggsave(filename = "./Figures/heatmap_acc_17.pdf", width = 14, height = 10, dpi = 320, scale = 1)
# ggsave(filename = "./Figures/heatmap_acc_17.png", width = 14, height = 10, dpi = 320, scale = 1)



########## Heatmap for categories #########
freq_table_1week_roll_15 <- acc_WHO_roll_fix$acc_1week_pred15_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_1week_roll_16 <- acc_WHO_roll_fix$acc_1week_pred16_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_1week_roll_17 <- acc_WHO_roll_fix$acc_1week_pred17_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_2week_roll_15 <- acc_WHO_roll_fix$acc_2week_pred15_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_2week_roll_16 <- acc_WHO_roll_fix$acc_2week_pred16_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_2week_roll_17 <- acc_WHO_roll_fix$acc_2week_pred17_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_3week_roll_15 <- acc_WHO_roll_fix$acc_3week_pred15_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_3week_roll_16 <- acc_WHO_roll_fix$acc_3week_pred16_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_3week_roll_17 <- acc_WHO_roll_fix$acc_3week_pred17_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_4week_roll_15 <- acc_WHO_roll_fix$acc_4week_pred15_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_4week_roll_16 <- acc_WHO_roll_fix$acc_4week_pred16_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)

freq_table_4week_roll_17 <- acc_WHO_roll_fix$acc_4week_pred17_rol$pred %>% 
  group_by(Observation, Prediction) %>%
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n),
         Observation = factor(Observation, levels = c(1:10)),
         Prediction = factor(Prediction, levels = c(1:10))) %>% 
  ungroup(Observation, Prediction) %>% 
  tidyr::complete(Observation, Prediction)


#' 1-week ahead
heatmap_1week_roll_15 <- ggplot(data = freq_table_1week_roll_15, aes(x = Observation, 
                                            y = Prediction,
                                            fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "A.year 2015")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_1week_roll_16 <- ggplot(data = freq_table_1week_roll_16, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")), color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "B.year 2016")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_1week_roll_17 <- ggplot(data = freq_table_1week_roll_17, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")), color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "C.year 2017")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_1week_roll <- ggarrange(heatmap_1week_roll_15, heatmap_1week_roll_16, heatmap_1week_roll_17,
          common.legend = TRUE, legend = "right", ncol = 3, nrow = 1)

annotate_figure(heatmap_1week_roll, top = text_grob("1-week ahead", face = "bold", size = 18))

ggsave(filename = "./Figures/heatmap_1week_roll.pdf", width = 16, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_1week_roll.png", width = 16, height = 10, dpi = 320, scale = 1)


#' 2-week ahead
heatmap_2week_roll_15 <- ggplot(data = freq_table_2week_roll_15, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "A.year 2015")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_2week_roll_16 <- ggplot(data = freq_table_2week_roll_16, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "B.year 2016")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_2week_roll_17 <- ggplot(data = freq_table_2week_roll_17, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "C.year 2017")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_2week_roll <- ggarrange(heatmap_2week_roll_15, heatmap_2week_roll_16, heatmap_2week_roll_17,
                                common.legend = TRUE, legend = "right", ncol = 3, nrow = 1)

annotate_figure(heatmap_2week_roll, top = text_grob("2-week ahead", face = "bold", size = 18))

ggsave(filename = "./Figures/heatmap_2week_roll.pdf", width = 16, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_2week_roll.png", width = 16, height = 10, dpi = 320, scale = 1)

#' 3-week ahead
heatmap_3week_roll_15 <- ggplot(data = freq_table_3week_roll_15, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "A.year 2015")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_3week_roll_16 <- ggplot(data = freq_table_3week_roll_16, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "B.year 2016")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_3week_roll_17 <- ggplot(data = freq_table_3week_roll_17, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "C.year 2017")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_3week_roll <- ggarrange(heatmap_3week_roll_15, heatmap_3week_roll_16, heatmap_3week_roll_17,
                                common.legend = TRUE, legend = "right", ncol = 3, nrow = 1)

annotate_figure(heatmap_3week_roll, top = text_grob("3-week ahead", face = "bold", size = 18))

ggsave(filename = "./Figures/heatmap_3week_roll.pdf", width = 16, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_3week_roll.png", width = 16, height = 10, dpi = 320, scale = 1)

#' 4-week ahead
heatmap_4week_roll_15 <- ggplot(data = freq_table_4week_roll_15, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "A.year 2015")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_4week_roll_16 <- ggplot(data = freq_table_4week_roll_16, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "B.year 2016")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_4week_roll_17 <- ggplot(data = freq_table_4week_roll_17, aes(x = Observation, 
                                                                     y = Prediction,
                                                                     fill = freq))+
  geom_tile(color= "white")+
  geom_text(aes(label=ifelse(!is.na(freq), round(freq, 2), "NA")),color = "black", size = 4)+
  # geom_text(aes(Observation, Prediction, label = round(freq, 2)), color = "black", size = 4)+
  scale_fill_gradient2(low="navy", high="red", 
                       midpoint= 0.3, limits=c(0,1),
                       na.value = "grey95",
                       name="Relative\nfreqency")+
  labs(title = "C.year 2017")+
  theme_minimal()+
  theme(text = element_text(size = 18))

heatmap_4week_roll <- ggarrange(heatmap_4week_roll_15, heatmap_4week_roll_16, heatmap_4week_roll_17,
                                common.legend = TRUE, legend = "right", ncol = 3, nrow = 1)

annotate_figure(heatmap_4week_roll, top = text_grob("4-week ahead", face = "bold", size = 18))

ggsave(filename = "./Figures/heatmap_4week_roll.pdf", width = 16, height = 10, dpi = 320, scale = 1)
ggsave(filename = "./Figures/heatmap_4week_roll.png", width = 16, height = 10, dpi = 320, scale = 1)

########### Time series plot ############

#' four countries - only XGBoost
TS_1week <- rbind(acc_WHO_roll_fix$acc_1week_pred15_rol$pred, acc_WHO_roll_fix$acc_1week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_1week_pred17_rol$pred)

TS_1week_MZE <- TS_1week %>% 
  filter(Country %in% c("MDA", "NLD", "USA", "AUT")) %>%
  mutate(Week_ahead = "1-week")

TS_2week <- rbind(acc_WHO_roll_fix$acc_2week_pred15_rol$pred, acc_WHO_roll_fix$acc_2week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_2week_pred17_rol$pred)

TS_2week_MZE <- TS_2week%>% 
  filter(Country %in% c("MDA", "NLD", "USA", "AUT"))%>%
  mutate(Week_ahead = "2-week")

TS_3week <- rbind(acc_WHO_roll_fix$acc_3week_pred15_rol$pred, acc_WHO_roll_fix$acc_3week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_3week_pred17_rol$pred)

TS_3week_MZE <- TS_3week %>% 
  filter(Country %in% c("MDA", "NLD", "USA", "AUT"))%>%
  mutate(Week_ahead = "3-week")

TS_4week <- rbind(acc_WHO_roll_fix$acc_4week_pred15_rol$pred, acc_WHO_roll_fix$acc_4week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_4week_pred17_rol$pred)

TS_4week_MZE <- TS_4week %>% 
  filter(Country %in% c("MDA", "NLD", "USA", "AUT"))%>%
  mutate(Week_ahead = "4-week")

TS_overall_MZE <- rbind(TS_1week_MZE, TS_2week_MZE) %>% 
  rbind(., TS_3week_MZE) %>% 
  rbind(., TS_4week_MZE) %>% 
  mutate(Week_ahead = factor(Week_ahead, levels = unique(Week_ahead)),
         Country = case_when(Country == "AUT" ~"Austria",
                             Country == "MDA" ~ "Moldova",
                             Country == "NLD" ~ "Netherlands",
                             Country == "USA" ~ "USA")) 

df_plot_TS_MZE <- TS_overall_MZE %>% 
  mutate(Year = substr(week_time,0,4),
         Week = substr(week_time,6,7))%>% 
  group_by(Country, Year, Week_ahead) %>%
  mutate(Week = as.numeric(Week)) %>% 
  tidyr::complete(Week = 1:53) %>% 
  ungroup(Country, Year, Week_ahead) %>%
  mutate(Week = sprintf("%02d", Week)) %>%
  dplyr::select(-week_time) %>% 
  mutate(Year_week = paste0(Year,"-", Week)) %>% 
  tidyr::pivot_longer(., 5:6, names_to = "stat", values_to = "value") %>% 
  rename(Category = stat,
         Incidence_level = value) %>% 
  mutate(Year_week = gsub("-", "-W", Year_week, fixed = TRUE),
         Incidence_level = as.numeric(Incidence_level),
         Category = factor(Category, levels = c("Observation", "Prediction"))) %>% 
  mutate(Year_week = week2date(Year_week)) %>% 
  mutate(Country = factor(Country, levels = c("Moldova","USA","Netherlands", "Austria")))
  
df_plot_TS_MZE <- df_plot_TS_MZE[-which(df_plot_TS_MZE$Year!=2015 & df_plot_TS_MZE$Week=="53"), ]

ggplot(df_plot_TS_MZE, aes(x = Year_week,y = Incidence_level, group = Category))+
  geom_point(aes(color = Category, shape = Category),size = 1.5)+
  geom_line(aes(color = Category), size = 0.7)+
  scale_y_continuous(breaks = c(1:10))+
  labs(y = "Incidence level",
       x = "Year")+
  facet_grid(Country ~ Week_ahead)+
  scale_colour_manual(values = c("#E08214", "#8073AC"))+
  scale_alpha_manual(values = c(1, 0.5))+
  theme_bw()+
  theme(text = element_text(size = 18))+
  theme(
    # strip.background = element_blank(),
    # strip.text.x = element_blank()
    text = element_text(size = 26),
    legend.position="bottom",
    axis.text.x = element_text(size=14),
    # axis.text.y = element_text(size=22)
    )
ggsave(filename = "./Figures/Selected_country_TS.pdf", width = 18, height = 12, dpi = 320, scale = 1)
ggsave(filename = "./Figures/Selected_country_TS.png", width = 18, height = 12, dpi = 320, scale = 1)


# MMAE
TS_1week_MMAE <- TS_1week %>% 
  filter(Country %in% c("MDA", "CHE", "EST", "HRV")) %>%
  mutate(Week_ahead = "1-week")

TS_2week_MMAE <- TS_2week%>% 
  filter(Country %in% c("MDA", "CHE", "EST", "HRV"))%>%
  mutate(Week_ahead = "2-week")

TS_3week_MMAE <- TS_3week %>% 
  filter(Country %in% c("MDA", "CHE", "EST", "HRV"))%>%
  mutate(Week_ahead = "3-week")

TS_4week_MMAE <- TS_4week %>% 
  filter(Country %in% c("MDA", "CHE", "EST", "HRV"))%>%
  mutate(Week_ahead = "4-week")

TS_overall_MMAE <- rbind(TS_1week_MMAE, TS_2week_MMAE) %>% 
  rbind(., TS_3week_MMAE) %>% 
  rbind(., TS_4week_MMAE) %>% 
  mutate(Week_ahead = factor(Week_ahead, levels = unique(Week_ahead)),
         Country = case_when(Country == "HRV" ~"Hungry",
                             Country == "MDA" ~ "Moldova",
                             Country == "EST" ~ "Estonia",
                             Country == "CHE" ~ "Switzerland")) 

df_plot_TS_MMAE <- TS_overall_MMAE %>% 
  mutate(Year = substr(week_time,0,4),
         Week = substr(week_time,6,7))%>% 
  group_by(Country, Year, Week_ahead) %>%
  mutate(Week = as.numeric(Week)) %>% 
  tidyr::complete(Week = 1:53) %>% 
  ungroup(Country, Year, Week_ahead) %>%
  mutate(Week = sprintf("%02d", Week)) %>%
  dplyr::select(-week_time) %>% 
  mutate(Year_week = paste0(Year,"-", Week)) %>% 
  tidyr::pivot_longer(., 5:6, names_to = "stat", values_to = "value") %>% 
  rename(Category = stat,
         Incidence_level = value) %>% 
  mutate(Year_week = gsub("-", "-W", Year_week, fixed = TRUE),
         Incidence_level = as.numeric(Incidence_level),
         Category = factor(Category, levels = c("Observation", "Prediction"))) %>% 
  mutate(Year_week = week2date(Year_week)) %>% 
  mutate(Country = factor(Country, levels = c("Moldova","Switzerland","Estonia", "Hungry")))

df_plot_TS_MMAE <- df_plot_TS_MMAE[-which(df_plot_TS_MMAE$Year!=2015 & df_plot_TS_MMAE$Week=="53"), ]

ggplot(df_plot_TS_MMAE, aes(x = Year_week,y = Incidence_level, group = Category))+
  geom_point(aes(color = Category, shape = Category),size = 1.5)+
  geom_line(aes(color = Category), size = 0.7)+
  scale_y_continuous(breaks = c(1:10))+
  labs(y = "Incidence level",
       x = "Year")+
  facet_grid(Country ~ Week_ahead)+
  scale_colour_manual(values = c("#E08214", "#8073AC"))+
  scale_alpha_manual(values = c(1, 0.5))+
  theme_bw()+
  theme(text = element_text(size = 18))+
  theme(
    # strip.background = element_blank(),
    # strip.text.x = element_blank()
    text = element_text(size = 26),
    legend.position="bottom",
    axis.text.x = element_text(size=14),
    # axis.text.y = element_text(size=22)
  )
ggsave(filename = "./Figures/Selected_country_TS_MMAE.pdf", width = 18, height = 12, dpi = 320, scale = 1)
ggsave(filename = "./Figures/Selected_country_TS_MMAE.png", width = 18, height = 12, dpi = 320, scale = 1)

########## TS plot(MZE) four countries - all models ##########
# 1 week
TS_rol_1week <- rbind(acc_WHO_roll_fix$acc_1week_pred15_rol$pred, acc_WHO_roll_fix$acc_1week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_1week_pred17_rol$pred) %>% 
  mutate(Model = "XGBoost(extending window)")

TS_fix_1week <- rbind(acc_WHO_roll_fix$acc_1week_pred15_fix$pred, acc_WHO_roll_fix$acc_1week_pred16_fix$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_1week_pred17_fix$pred) %>% 
  mutate(Model = "XGBoost(fix window)")

TS_hist_1week <- rbind(baseline_acc$acc_1week_pred15_hist$pred, baseline_acc$acc_1week_pred16_hist$pred) %>% 
  rbind(., baseline_acc$acc_1week_pred17_hist$pred) %>% 
  mutate(Model = "Hist")%>% 
  rename(Prediction = OneWeek_ahead,
         Accuracy = Accurate)

TS_null_1week <- rbind(baseline_acc$acc_1week_pred15_null$pred, baseline_acc$acc_1week_pred16_null$pred) %>% 
  rbind(., baseline_acc$acc_1week_pred17_null$pred) %>% 
  mutate(Model = "Null") %>% 
  rename(Prediction = OneWeek_ahead,
         Accuracy = Accurate)

TS_all_1week <- rbind(TS_rol_1week, TS_fix_1week) %>% 
  rbind(., TS_hist_1week) %>% 
  rbind(., TS_null_1week)%>% 
  mutate(Week_ahead = "1-week")

# 2 week
TS_rol_2week <- rbind(acc_WHO_roll_fix$acc_2week_pred15_rol$pred, acc_WHO_roll_fix$acc_2week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_2week_pred17_rol$pred) %>% 
  mutate(Model = "XGBoost(extending window)")

TS_fix_2week <- rbind(acc_WHO_roll_fix$acc_2week_pred15_fix$pred, acc_WHO_roll_fix$acc_2week_pred16_fix$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_2week_pred17_fix$pred) %>% 
  mutate(Model = "XGBoost(fix window)")

TS_hist_2week <- rbind(baseline_acc$acc_2week_pred15_hist$pred, baseline_acc$acc_2week_pred16_hist$pred) %>% 
  rbind(., baseline_acc$acc_2week_pred17_hist$pred) %>% 
  mutate(Model = "Hist")%>% 
  dplyr::select(-OneWeek_ahead) %>% 
  rename(Prediction = TwoWeek_ahead,
         Accuracy = Accurate)

TS_null_2week <- rbind(baseline_acc$acc_2week_pred15_null$pred, baseline_acc$acc_2week_pred16_null$pred) %>% 
  rbind(., baseline_acc$acc_2week_pred17_null$pred) %>% 
  mutate(Model = "Null") %>% 
  dplyr::select(-OneWeek_ahead) %>% 
  rename(Prediction = TwoWeek_ahead,
         Accuracy = Accurate)

TS_all_2week <- rbind(TS_rol_2week, TS_fix_2week) %>% 
  rbind(., TS_hist_2week) %>% 
  rbind(., TS_null_2week)%>% 
  mutate(Week_ahead = "2-week")

# 3 week
TS_rol_3week <- rbind(acc_WHO_roll_fix$acc_3week_pred15_rol$pred, acc_WHO_roll_fix$acc_3week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_3week_pred17_rol$pred) %>% 
  mutate(Model = "XGBoost(extending window)")

TS_fix_3week <- rbind(acc_WHO_roll_fix$acc_3week_pred15_fix$pred, acc_WHO_roll_fix$acc_3week_pred16_fix$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_3week_pred17_fix$pred) %>% 
  mutate(Model = "XGBoost(fix window)")

TS_hist_3week <- rbind(baseline_acc$acc_3week_pred15_hist$pred, baseline_acc$acc_3week_pred16_hist$pred) %>% 
  rbind(., baseline_acc$acc_3week_pred17_hist$pred) %>% 
  mutate(Model = "Hist")%>% 
  dplyr::select(-OneWeek_ahead, -TwoWeek_ahead) %>% 
  rename(Prediction = ThreeWeek_ahead,
         Accuracy = Accurate)

TS_null_3week <- rbind(baseline_acc$acc_3week_pred15_null$pred, baseline_acc$acc_3week_pred16_null$pred) %>% 
  rbind(., baseline_acc$acc_3week_pred17_null$pred) %>% 
  mutate(Model = "Null") %>% 
  dplyr::select(-OneWeek_ahead, -TwoWeek_ahead) %>% 
  rename(Prediction = ThreeWeek_ahead,
         Accuracy = Accurate)

TS_all_3week <- rbind(TS_rol_3week, TS_fix_3week) %>% 
  rbind(., TS_hist_3week) %>% 
  rbind(., TS_null_3week)%>% 
  mutate(Week_ahead = "3-week")

# 4 week
TS_rol_4week <- rbind(acc_WHO_roll_fix$acc_4week_pred15_rol$pred, acc_WHO_roll_fix$acc_4week_pred16_rol$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_4week_pred17_rol$pred) %>% 
  mutate(Model = "XGBoost(extending window)")

TS_fix_4week <- rbind(acc_WHO_roll_fix$acc_4week_pred15_fix$pred, acc_WHO_roll_fix$acc_4week_pred16_fix$pred) %>% 
  rbind(., acc_WHO_roll_fix$acc_4week_pred17_fix$pred) %>% 
  mutate(Model = "XGBoost(fix window)")

TS_hist_4week <- rbind(baseline_acc$acc_4week_pred15_hist$pred, baseline_acc$acc_4week_pred16_hist$pred) %>% 
  rbind(., baseline_acc$acc_4week_pred17_hist$pred) %>% 
  mutate(Model = "Hist")%>% 
  dplyr::select(-OneWeek_ahead, -TwoWeek_ahead, -ThreeWeek_ahead) %>%
  rename(Prediction = FourWeek_ahead,
         Accuracy = Accurate)

TS_null_4week <- rbind(baseline_acc$acc_4week_pred15_null$pred, baseline_acc$acc_4week_pred16_null$pred) %>% 
  rbind(., baseline_acc$acc_4week_pred17_null$pred) %>% 
  mutate(Model = "Null") %>% 
  dplyr::select(-OneWeek_ahead, -TwoWeek_ahead, -ThreeWeek_ahead) %>%
  rename(Prediction = FourWeek_ahead,
         Accuracy = Accurate)

TS_all_4week <- rbind(TS_rol_4week, TS_fix_4week) %>% 
  rbind(., TS_hist_4week) %>% 
  rbind(., TS_null_4week) %>% 
  mutate(Week_ahead = "4-week")

# combine together 
TS_all_model <- rbind(TS_all_1week, TS_all_2week) %>% 
  rbind(., TS_all_3week) %>% 
  rbind(., TS_all_4week)

TS_all_model_select <- TS_all_model %>% 
  filter(Country %in% c("MDA", "NLD", "USA", "AUT")) %>% 
  mutate(Week_ahead = factor(Week_ahead, levels = unique(Week_ahead)),
         Country = case_when(Country == "AUT" ~"Austria",
                             Country == "MDA" ~ "Moldova",
                             Country == "NLD" ~ "Netherlands",
                             Country == "USA" ~ "USA"))

obs <- TS_all_model_select %>%
  tidyr::pivot_longer(., 4, names_to = "stat", values_to = "value") %>% 
  dplyr::select(Country, week_time, Accuracy, Week_ahead, stat, value) %>% 
  rename(Model= stat, 
         Prediction = value) %>%
  relocate(Prediction, .after = 2) %>% 
  relocate(Model,  .before = 5)

TS_all_model_select <- TS_all_model_select %>% 
  dplyr::select(-Observation)

df_TS_all_model_select <- rbind(TS_all_model_select, obs) %>% 
  mutate(Year = substr(week_time,0,4),
         Week = substr(week_time,6,7))%>% 
  group_by(Country, Year, Week_ahead, Model) %>%
  mutate(Week = as.numeric(Week)) %>% 
  tidyr::complete(Week = 1:53) %>% 
  ungroup(Country, Year, Week_ahead, Model) %>%
  mutate(Week = sprintf("%02d", Week)) %>%
  dplyr::select(-week_time) %>% 
  mutate(Year_week = paste0(Year,"-", Week)) %>% 
  # tidyr::pivot_longer(., 6:7, names_to = "stat", values_to = "value") %>% 
  rename(
         Incidence_level = Prediction) %>%
  mutate(Year_week = gsub("-", "-W", Year_week, fixed = TRUE),
         Incidence_level = as.numeric(Incidence_level),
         # Category = factor(Category, levels = c("Observation", "Prediction"))
         ) %>% 
  mutate(Year_week = week2date(Year_week)) %>% 
  mutate(Country = factor(Country, levels = c("Moldova","USA","Netherlands", "Austria")),
         Model = factor(Model, levels = c("Observation", "XGBoost(extending window)", "XGBoost(fix window)",
                                          "Hist", "Null")))

ggplot(df_TS_all_model_select, aes(x = Year_week, y = Incidence_level,group = Model))+
  geom_point(aes(y = Incidence_level, color = Model, shape = Model),size = 1.5)+
  geom_line(aes(y = Incidence_level, color = Model), size = 0.7)+
  scale_y_continuous(breaks = c(1:10))+
  labs(y = "Incidence level",
       x = "Year")+
  facet_grid(Country ~ Week_ahead)+
  scale_colour_manual(values = c("#FC4E07", "#00AFBB", "#C3D7A4", "#E08214", "#8073AC"))+
  scale_alpha_manual(values = c(1, 0.5, 0.5, 0.5, 0.5))+
  # scale_size_manual(values=c(2,2,2,2,2))+
  theme_bw()+
  theme(text = element_text(size = 18))+
  theme(
    # strip.background = element_blank(),
    # strip.text.x = element_blank()
    text = element_text(size = 28),
    legend.position="bottom",
    axis.text.x = element_text(size=18)
    # axis.text.y = element_text(size=22)
  )+
  guides(linetype = guide_legend(override.aes = list(size = 3)))


ggsave(filename = "./Figures/Selected_country_allModel_TS.pdf", width = 24, height = 14, dpi = 320, scale = 1)
ggsave(filename = "./Figures/Selected_country_allModel_TS.png", width = 24, height = 14, dpi = 320, scale = 1)


########## TS plot(MMAE) four countries - all models ##########
TS_all_model_select_MMAE <- TS_all_model %>% 
  filter(Country %in% c("MDA", "EST", "CHE", "HRV")) %>% 
  mutate(Week_ahead = factor(Week_ahead, levels = unique(Week_ahead)),
         Country = case_when(Country == "HRV" ~"Hungry",
                             Country == "MDA" ~ "Moldova",
                             Country == "EST" ~ "Estonia",
                             Country == "CHE" ~ "Switzerland"))

obs <- TS_all_model_select_MMAE %>%
  tidyr::pivot_longer(., 4, names_to = "stat", values_to = "value") %>% 
  dplyr::select(Country, week_time, Accuracy, Week_ahead, stat, value) %>% 
  rename(Model= stat, 
         Prediction = value) %>%
  relocate(Prediction, .after = 2) %>% 
  relocate(Model,  .before = 5)

TS_all_model_select_MMAE <- TS_all_model_select_MMAE %>% 
  dplyr::select(-Observation)

df_TS_all_model_select_MMAE <- rbind(TS_all_model_select_MMAE, obs) %>% 
  mutate(Year = substr(week_time,0,4),
         Week = substr(week_time,6,7))%>% 
  group_by(Country, Year, Week_ahead, Model) %>%
  mutate(Week = as.numeric(Week)) %>% 
  tidyr::complete(Week = 1:53) %>% 
  ungroup(Country, Year, Week_ahead, Model) %>%
  mutate(Week = sprintf("%02d", Week)) %>%
  dplyr::select(-week_time) %>% 
  mutate(Year_week = paste0(Year,"-", Week)) %>% 
  # tidyr::pivot_longer(., 6:7, names_to = "stat", values_to = "value") %>% 
  rename(
    Incidence_level = Prediction) %>%
  mutate(Year_week = gsub("-", "-W", Year_week, fixed = TRUE),
         Incidence_level = as.numeric(Incidence_level),
         # Category = factor(Category, levels = c("Observation", "Prediction"))
  ) %>% 
  mutate(Year_week = week2date(Year_week)) %>% 
  mutate(Country = factor(Country, levels = c("Moldova","Switzerland","Estonia", "Hungry")),
         Model = factor(Model, levels = c("Observation", "XGBoost(extending window)", "XGBoost(fix window)",
                                          "Hist", "Null")))

ggplot(df_TS_all_model_select_MMAE, aes(x = Year_week, y = Incidence_level,group = Model))+
  geom_point(aes(y = Incidence_level, color = Model, shape = Model),size = 1.5)+
  geom_line(aes(y = Incidence_level, color = Model), size = 0.7)+
  scale_y_continuous(breaks = c(1:10))+
  labs(y = "Incidence level",
       x = "Year")+
  facet_grid(Country ~ Week_ahead)+
  scale_colour_manual(values = c("#FC4E07", "#00AFBB", "#C3D7A4", "#E08214", "#8073AC"))+
  scale_alpha_manual(values = c(1, 0.5, 0.5, 0.5, 0.5))+
  # scale_size_manual(values=c(2,2,2,2,2))+
  theme_bw()+
  theme(text = element_text(size = 18))+
  theme(
    # strip.background = element_blank(),
    # strip.text.x = element_blank()
    text = element_text(size = 28),
    legend.position="bottom",
    axis.text.x = element_text(size=18)
    # axis.text.y = element_text(size=22)
  )+
  guides(linetype = guide_legend(override.aes = list(size = 3)))


ggsave(filename = "./Figures/Selected_country_allModel_TS_MMAE.pdf", width = 24, height = 14, dpi = 320, scale = 1)
ggsave(filename = "./Figures/Selected_country_allModel_TS_MMAE.png", width = 24, height = 14, dpi = 320, scale = 1)
