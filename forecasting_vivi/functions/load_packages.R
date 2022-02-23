load_package <- function(package.list){
  new.packages <- package.list[!(package.list %in% installed.packages()[, "Package"])]
  print(paste(length(new.packages), "packages require installation. Installing now"))
  if(length(new.packages)) install.packages(new.packages)
  print("Loading packages")
  lapply(package.list, require, character.only = TRUE)
}