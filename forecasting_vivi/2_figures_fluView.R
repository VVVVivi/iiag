#' Plots for FluView data analysis 

#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "cdcfluview")
load_package(pkgs)

#' Load rds for figures
accuracy_comparison <- readRDS("./saved_objects/accuracy_comparison.rds")

overall_accuracy_xgb_baseline <- accuracy_comparison[[1]]
overall_accuracy_xgb <- accuracy_comparison[[2]]
baseline_score <- accuracy_comparison[[3]]


######## Line plot ###########
accCompare <- ggplot(overall_accuracy_xgb_baseline, aes(x = nWeek_ahead,group=1))+
  geom_point(aes(y = year2018, color = "year2018"),size=3)+
  geom_line(aes(y = year2018, color = "year2018"),size=1)+
  geom_point(aes(y = year2019, color = "year2019"),size=3)+
  geom_line(aes(y = year2019, color = "year2019"),size=1)+
  geom_point(aes(y = year2020, color = "year2020"),size=3)+
  geom_line(aes(y = year2020, color = "year2020"),size=1)+
  geom_point(aes(y = year2021, color = "year2021"),size=3)+
  geom_line(aes(y = year2021, color = "year2021"),size=1)+
  geom_point(aes(y = Baseline_score, color = "Baseline_score"),size=3)+
  geom_line(aes(y = Baseline_score, color = "Baseline_score"),size=1)+
  ylim(0,0.85)+
  labs(y = "Forecast accuracy",
       x = "n-week ahead",
       colour = "Forecast year")+
  theme_bw(base_size = 18)

accCompare + theme(# legend.title = element_text(size = 18),
  # legend.text = element_text(size = 18),
  legend.position = c(0.85, 0.85))

ggsave(accCompare, filename = "accCompare.pdf",
       device = "pdf", width = 12, height = 10, scale = 1)
ggsave(accCompare, filename = "accCompare.png",
       device = "png", width = 12, height = 10, scale = 1)


########## Heat chart ############

