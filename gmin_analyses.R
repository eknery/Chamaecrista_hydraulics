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
db = read.table("1_data/leaf_gmin_C_latistipula_02_set_2024.csv",
                  h=T, 
                  sep= ";",
                  dec = ","
                  )

############################### GMIN BY INDIVIDUAL ############################

## sp name
sp = unique(db$specie)
individuals = unique(db$individual)

### loop for each leaf 
for(id in individuals){
  ## filter by id
  my_data1 = db %>% 
    filter(individual == id)
    ## how many leaves?
    samples = unique(my_data1$sample)
  for(samp_num in samples){
    ## filter by leaf
    my_data2 = my_data1 %>% 
      filter(sample == samp_num) 
    ## set name
    file_name = paste(sp,id,samp_num, sep="_")
    ## gmin analyses
    gmin_routine(data = my_data2, file_name =  file_name)
    
  }
}

############################## GMIN BY SPECIES ################################

all_results = list.files(path=paste0(getwd(), "/2_gmin_results"),
                         pattern = ".csv")
