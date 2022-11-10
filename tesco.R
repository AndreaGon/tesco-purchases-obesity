install.packages("ggplot2")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("rstudioapi")
install.packages("readr")
install.packages("data.table")
install.packages("GGally")
install.packages('parallel')
install.packages("microbenchmark")

library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)
library(data.table)
library(parallel)
library(GGally)

library(rstudioapi)
library(microbenchmark)

current_path <- dirname(getSourceEditorContext()$path)
files <- list.files(path = paste(current_path, "/AreaLevel", sep=""), pattern = ".csv")
file_list <- list()

#Data Integration (Using Sequential Processing)
sequentialIntegration <- function(){
  file_list <- lapply(files, function(filename){
    split_filename <- strsplit(filename[1], "_")[1]
    matrix_filename <- matrix(unlist(split_filename), ncol=3, byrow=T)
    month <- matrix_filename[1,1]
    area <- matrix_filename[1, 2]
    
    ret <- read_csv(paste(current_path, "/AreaLevel/", filename, sep=""))
    ret$month <- month
    ret$area <- area
    return(ret)
  })
  
  df <- rbindlist(file_list)
  write.csv(df, paste(current_path, "/Warehouse/AreaLevel_month_combined_sequential.csv", sep=""), row.names =  FALSE)
}



#Data Integration (Using Parallel processing)
parallelIntegration <- function(){
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
}

#Timing Parallel Integration vs Sequential Integration using Microbenchmark
timing <- microbenchmark(sequentialIntegration(), parallelIntegration(), times=1)
ggplot(data=timing, aes(x=expr, y=time)) +
  geom_bar(stat="identity")



#Data Cleaning
area_level_combined <- read.csv(paste(current_path,"/Warehouse/AreaLevel_month_combined.csv", sep=""))
children_obesity <- read.csv(paste(current_path,"/ValidationData/child_obesity_london_borough_2015-2016.csv", sep=""))
adult_obesity <- read.csv(paste(current_path,"/ValidationData/london_obesity_borough_2012.csv", sep=""))
diabetes <- read.csv(paste(current_path,"/ValidationData/diabetes_estimates_osward_2016.csv", sep=""))

area_level_combined$representativeness_norm <- formatC(area_level_combined$representativeness_norm, digits = 2, format="f")

#----------Descriptive Analysis----------

#Borough population vs customers distribution
borough_area <- area_level_combined[area_level_combined$area=="borough",]

ggplot(borough_area, aes(x=population, y=..density..)) +  
  ggtitle("Distribution of Borough Residents") +
  geom_histogram(color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

ggplot(borough_area, aes(x=man_day, y=..density..)) +  
  ggtitle("Distribution of Borough Customers") +
  geom_histogram(color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


#Ward population vs customers distribution
ward_area <- area_level_combined[area_level_combined$area=="osward",]

ggplot(ward_area, aes(x=population, y=..density..)) +  
  ggtitle("Distribution of Ward Residents") +
  geom_histogram(color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(ward_area, aes(x=man_day, y=..density..)) +  
  ggtitle("Distribution of Ward Customers") +
  geom_histogram(color="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 



##Correlation between food consumption and obesity, overweight and diabetes

#Borough area, adult obesity
adult_obesity <- adult_obesity %>% rename("area_id" = "oslaua")

merged_adult_borough <- merge(area_level_combined, adult_obesity, by = "area_id")
ggpairs(merged_adult_borough, columns = c( "carb","fat", "sugar", "protein", "fibre", "f_obese"), title = "Prevalence of obese adult (Borough)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                                                               size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.05,
                                       size = 0.1)))


merged_children_borough <- merge(area_level_combined, children_obesity, by = "area_id")
ggpairs(merged_children_borough, columns = c( "carb","fat", "sugar", "protein", "fibre", "prevalence_obese_reception"), title = "Prevalence of obese children (Borough)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.05,
                                       size = 0.1)))

#Diabetes Prevalence (Ward level)
merged_diabetes <- merge(area_level_combined, diabetes, by = "area_id")
ggpairs(merged_diabetes, columns = c("carb","fat", "sugar", "protein", "fibre", "estimated_diabetes_prevalence"), title = "Diabetes Prevalence (Ward)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                            size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.05,
                                       size = 0.1)))


#----------Hypothesis Testing----------
#TEST NO. 1
#H0: Fats consumption do not have an effect on diabetes
#H1: Fats consumption have an effect on diabetes
fat_model <- lm(estimated_diabetes_prevalence ~ fat, data=merged_diabetes)
summary(fat_model)

#TEST NO. 2
#H0: Carbs consumption do not have an effect on diabetes
#H1: Carbs consumption have an effect on diabetes
carb_model <- lm(estimated_diabetes_prevalence ~ carb, data=merged_diabetes)
summary(carb_model)

#TEST NO. 3
#H0: Sugar consumption do not have an effect on diabetes
#H1: Sugar consumption have an effect on diabetes
sugar_model <- lm(estimated_diabetes_prevalence ~ sugar, data=merged_diabetes)
summary(sugar_model)

#TEST NO. 4
#Multiple Linear Regression
mlr_model <- lm(estimated_diabetes_prevalence ~ fat + sugar + carb, data=merged_diabetes)
summary(mlr_model)


