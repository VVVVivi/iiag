#' Make a note of when the report was generated.
Sys.time()
knitr.table.format = "markdown"

#' As always, remove all objects fromt the workspace before starting.
rm(list = ls(all = TRUE))

setwd("C:/Users/hw3616/Desktop/Imperial/Project1_Forecasting/Project_Coding/iiag/forecasting_vivi")

#' Pull in packages needed
source("./functions/load_packages.R")

#' Install US CDC package
# CRAN
# install.packages("cdcfluview")

# main branch
remotes::install_git("https://git.rud.is/hrbrmstr/cdcfluview.git")
remotes::install_git("https://sr.ht/~hrbrmstr/cdcfluview")
remotes::install_git("https://gitlab.com/hrbrmstr/cdcfluview")
remotes::install_github("hrbrmstr/cdcfluview")

pkgs <- c("xgboost", "stringr", "rasterVis", "cdcfluview", "hrbrthemes",
          "dplyr", "ggplot2", "cdcfluview")
load_package(pkgs)

#' Load self-written functions 
source("./fluView_funcs_vivi.R")
source("./vivi_funcs.R")
source("./gbm_complex_funcs.R")
source("./fluView_funcs_vivi.R")

#' Load survilliance ILI data by states 
#' 55 states in total
fview_ILINet <- ilinet(region = "state")
hhs_region <- ilinet(region = "hhs")
national <- ilinet(region = "national")

#' Extract incidence data by states.
# region <- unique(fview_ILINet$REGION)

#' Shape 
fview_incidence <- extract.incidence.fview(fview_ILINet,
                                           sel_states = unique(fview_ILINet$region),
                                           minYear = 2010,
                                           maxYear = 2021,
                                           c(2014,2020))
ncol(fview_incidence) # 55 states in total

minprop <- 0.5

#' None of satets contain entries between 2010-01 and 2010-39, so delete these rows before calculate the avaibility
#' of dataset
fview_incidence <- fview_incidence[-c(1:39),]

#' Florida and Commonwealth of the Northern Mariana Islands are excluded.
us_states <- names(which(colSums(is.na(fview_incidence))/dim(fview_incidence)[1]<minprop)) %>% 
  as.data.frame() # 53 states left
names(us_states) <- "States"
us_states$States <- as.character(us_states$States)

fview_incidence <- extract.incidence.fview(fview_ILINet,
                                           sel_states = us_states$States,
                                           minYear = 2010,
                                           maxYear = 2021,
                                           c(2014,2020))

#' Check if the data of remained states are eligible for xgboost
#' check the data availablity in each year
#' exclude states which do not have at least five consecutive weeks data in any year 
#' or do not have entries in certain years.
state_year <- duration.fview(fview_incidence,us_states$States, 4,2010,2021,c(2014,2020))

state_no21Or10 <- as.character(state_year$State)[which(state_year$`2021`=="No" | state_year$`2010`=="No")]

state_no10 <- as.character(state_year$State)[which(state_year$`2010`=="No")]

state_no21Or10 <- us_states[which(us_states$States %in% state_no21Or10),]
# Virgin Islands starts from 2011 and Puerto Rico starts from 2013.


#' Obtain the list of states will be maintained for xgboost analysis.
#' There are 51 states are maintained.
us_xgb <- us_states %>% 
  filter(States %in% state_no21Or10 == FALSE) %>% 
  as.matrix() # 51 states left


#' Check if left states have less than 5 weeks data in a year because I will do the 4-week ahead foreacast which
#' requires data of 5 consective weeks
#' Exclude states whose datasets are uneligible to do the 4-week ahead forecast
#' So far I (Vivi) did this manually, will write a function to check automatically.

# extract incidence of eligible states.
fview_incidence2 <- extract.incidence.fview(fview_ILINet,
                                            us_xgb,
                                            minYear = 2010,
                                            maxYear = 2021,
                                            c(2014,2020)) 
ncol(fview_incidence2)

#' check if data size is the same for each state
length1 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,1,c(2014,2020))
  tmp <- nrow(a)
  length1 <- append(length1,tmp)
  
}

length2 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,2,c(2014,2020))
  tmp <- nrow(a)
  length2 <- append(length2,tmp)
}

length3 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,3,c(2014,2020))
  tmp <- nrow(a)
  length3 <- append(length3,tmp)
}

length4 <- c()
for (i in 1:length(us_xgb)){
  a <- adjust.data.size.fview(fview_incidence2, us_xgb[i], 10,4,c(2014,2020))
  tmp <- nrow(a)
  length4 <- append(length4,tmp)
}

#' Save data set as rds for later analysis
fluView <- list(fview_incidence2,
                us_xgb)
saveRDS(fluView, "./saved_objects/fluView.rds")

