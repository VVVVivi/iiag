library('ggplot2') # transform integer numbers to categorical variable
library('xgboost') # boosted regression tree
library('formattable') # formatting on data frames
library('gtools') # permutations
library("lattice") # heat plot
library("RColorBrewer") # colour platter
library("raster") # colour platter
library("rasterVis") # colour platter
# library("purrr") # Partial dependence plots
library("tidyverse")
library("dplyr") # mutate
library("lubridate")
library("mlr")
library("Matrix")
library("e1071")
library("DiagrammeR") # tree plot
library("rsvg")
library("DiagrammeRsvg") # multi tree plot
library("data.table") # convert data frame to data table
library("aweek") # convert discrete time variable into contious variable
library("knitr")

rm(list = ls(all = TRUE))

# Load WHO FluID dataset
#' becuase idd package is not aviable for R version 3.6.1, I downloaded dataset fluIliCountryData.rda in the 
#' idd and loaded in the Rstudio directly.
load(file = "fluIliCountryData.rda")

countryISO <- read.csv("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/data_old/country_list_ISO.csv")