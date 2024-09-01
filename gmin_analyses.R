### libraries
if (!require("tidyverse")) install.packages("tidyverse"); require("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr"); library("ggpubr")

### my functions
source("0_functions/function_vpsat.R")
source("0_functions/function_mfvpd.R")
source("0_functions/function_delta.R")
source("0_functions/function_gmin.R")
source("0_functions/function_gmin_routine.R")

### load data
db = read.table("1_data/Gmin_C_distishoclata_27_abr_2024.csv",
                  h=T, 
                  sep= ";",
                  dec = ","
                  )

### processing common info
db$atm = mean(db[["atmospheric_pressure"]], na.rm = T)

## sp name
sp = unique(db$specie)
individuals = unique(db$individual)

### loop
for(id in individuals){
  ## filter by id
  my_data1 = db %>% 
    filter(individual == id)
    ## how many leaves?
    leaves = unique(my_data1$leaf)
  for(leaf_num in leaves){
    ## filter by leaf
    my_data2 = my_data1 %>% 
      filter(leaf == leaf_num) %>% 
      mutate(area = mean(`area_moist_cm2`, na.rm = T))
    ## set name
    file_name = paste(sp,id,leaf_num, sep="_")
    ## gmin analyses
    gmin_routine(data = my_data2, file_name =  file_name)
    
  }
}



