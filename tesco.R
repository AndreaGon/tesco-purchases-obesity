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

#Data Integration With Parallel processing
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

#Data Cleaning
area_level_combined <- read.csv(paste(current_path,"/Warehouse/AreaLevel_month_combined.csv", sep=""))
children_obesity <- read.csv(paste(current_path,"/ValidationData/child_obesity_london_borough_2015-2016.csv", sep=""))
adult_obesity <- read.csv(paste(current_path,"/ValidationData/london_obesity_borough_2012.csv", sep=""))
diabetes <- read.csv(paste(current_path,"/ValidationData/diabetes_estimates_osward_2016.csv", sep=""))

##Correlation between food consumption and obesity, overweight and diabetes

merged_children_borough <- merge(area_level_combined, children_obesity, by = "area_id")

#Borough area, children obesity
ggpairs(merged_children_borough, columns = c("carb", "fat", "saturate", "sugar", "protein", "fibre", "prevalence_obese_reception"), title = "Prevalence of Obese children (Borough reception)", upper = list(continuous = wrap("cor",
                                                                                                                                                                            size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.5,
                                       size = 0.1)))
children_obesity <- read.csv(paste(current_path,"/ValidationData/child_obesity_london_borough_2015-2016.csv", sep=""))

merged_children_borough <- merge(area_level_combined, children_obesity, by = "area_id")ggpairs(merged_children_borough, columns = c("carb", "fat", "saturate", "sugar", "protein", "fibre", "prevalence_obese_y6"), title = "Prevalence of Obese children (Borough Year 6)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                                                                                  size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.5,
                                       size = 0.1)))

#Borough area, adult obesity
adult_obesity <- adult_obesity %>% rename("area_id" = "oslaua")

merged_adult_borough <- merge(area_level_combined, adult_obesity, by = "area_id")
ggpairs(merged_adult_borough, columns = c("carb", "fat", "saturate", "sugar", "protein", "fibre", "f_obese"), title = "Prevalence of obese adult (Borough)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                                                               size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.5,
                                       size = 0.1)))

#Diabetes Prevalence (Ward level)
merged_diabetes <- merge(area_level_combined, diabetes, by = "area_id")
ggpairs(merged_diabetes, columns = c("carb", "fat", "saturate", "sugar", "protein", "fibre", "estimated_diabetes_prevalence"), title = "Diabetes Prevalence (Ward)", upper = list(continuous = wrap("cor",
                                                                                                                                                                                            size = 3)),
        lower = list(continuous = wrap("smooth",
                                       alpha = 0.05,
                                       size = 0.1)))






##Representativeness
area_level_combined$representativeness_norm <- format(round(area_level_combined$representativeness_norm, 2), nsmall = 2)

#LSOA population vs customers distribution
lsoa_area <- area_level_combined[area_level_combined$area=="lsoa",]

ggplot(lsoa_area, aes(x=population, y=..density..)) + 
  geom_histogram(color="black", fill="white")

ggplot(lsoa_area, aes(x=man_day, y=..density..)) + 
  geom_histogram(color="black", fill="white")

#MSOA population vs customers distribution
msoa_area <- area_level_combined[area_level_combined$area=="msoa",]

ggplot(msoa_area, aes(x=population, y=..density..)) + 
  geom_histogram(color="black", fill="white")

ggplot(msoa_area, aes(x=man_day, y=..density..)) + 
  geom_histogram(color="black", fill="white")

#Borough population vs customers distribution
borough_area <- area_level_combined[area_level_combined$area=="borough",]

ggplot(borough_area, aes(x=population, y=..density..)) + 
  geom_histogram(color="black", fill="white")

ggplot(borough_area, aes(x=man_day, y=..density..)) + 
  geom_histogram(color="black", fill="white")


#Ward population vs customers distribution
ward_area <- area_level_combined[area_level_combined$area=="osward",]

ggplot(ward_area, aes(x=population, y=..density..)) + 
  geom_histogram(color="black", fill="white")

ggplot(ward_area, aes(x=man_day, y=..density..)) + 
  geom_histogram(color="black", fill="white")


ggplot(ward_area, aes(x=representativeness_norm, y=area_sq_km)) + 
  geom_histogram(color="black", fill="white")


