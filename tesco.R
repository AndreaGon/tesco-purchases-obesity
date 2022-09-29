install.packages("ggplot2")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("rstudioapi")
install.packages("readr")
install.packages("data.table")
install.packages("GGally")
install.packages('parallel')

library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)
library(data.table)
library(parallel)
library(GGally)

library(rstudioapi) 

#Data Integration (Without parallel processing)
system.time({
  current_path <- dirname(getSourceEditorContext()$path)
  files <- list.files(path = paste(current_path, "/AreaLevel", sep=""),pattern = ".csv")
  file_list <- list()
  file_list <- lapply(files, function(filename){
    split_filename <- strsplit(filename[1], "_")[1]
    matrix_filename <- matrix(unlist(split_filename),ncol=3,byrow=T)
    month <- matrix_filename[1, 1]
    area <- matrix_filename[1, 2]
    
    ret <- read_csv(paste(current_path,"/AreaLevel/", filename, sep=""))
    ret$month <- month
    ret$area <- area
    return(ret)})
  df <- rbindlist(file_list)
  
  write.csv(df,paste(current_path, "/Warehouse/AreaLevel_month_combined.csv", sep=""), row.names = FALSE)
})

#With Parallel processing
system.time({
  current_path <- dirname(getSourceEditorContext()$path)
  files <- list.files(path = paste(current_path, "/AreaLevel", sep=""),pattern = ".csv")
  file_list <- list()
  
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, {
    library(ggplot2)
    library(ggpubr)
    library(dplyr)
    library(readr)
    library(data.table)
    library(parallel)
    library(GGally)
  })
  clusterExport(cl, list("current_path", "files", "file_list"), envir=environment())
  
  file_list <- parLapply(cl, files, function(filename){
    split_filename <- strsplit(filename[1], "_")[1]
    matrix_filename <- matrix(unlist(split_filename),ncol=3,byrow=T)
    month <- matrix_filename[1, 1]
    area <- matrix_filename[1, 2]
    
    ret <- read_csv(paste(current_path,"/AreaLevel/", filename, sep=""))
    ret$month <- month
    ret$area <- area
    return(ret)})
  df <- rbindlist(file_list)
  
  write.csv(df,paste(current_path, "/Warehouse/AreaLevel_month_combined.csv", sep=""), row.names = FALSE)
  stopCluster(cl)
})




