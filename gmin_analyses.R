### libraries
if (!require("tidyverse")) install.packages("tidyverse"); require("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr"); library("ggpubr")
if (!require("data.table")) install.packages("data.table"); require("data.table")

### needed functions
source("0_functions/function_vpsat.R")
source("0_functions/function_mfvpd.R")
source("0_functions/function_delta.R")
source("0_functions/function_gmin.R")
source("0_functions/function_gmin_routine.R")

### choose data 
data_path = "1_data/leaf_gmin_data_clean/leaf_gmin_C_flexuosa_03_set_2024.csv"

### load data
db = read.table(data_path,
                h=T, 
                sep= ",",
                dec = "."
                )

############################### GMIN BY INDIVIDUAL ############################

### data type
if(grepl("leaf", my_path) ){
  dtype = 'leaf'
}
if(grepl("stem", my_path) ){
  dtype = 'stem'
}

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
    file_name = paste(dtype, sp, id, samp_num, sep="_")
    ## gmin analyses
    gmin_routine(data = my_data2, file_name =  file_name)
    
  }
}

############################## GMIN BY SPECIES ################################

### all result files
all_files = list.files(path=paste0(getwd(), "/2_gmin_results"),
                       pattern = ".csv",
                       full.names= TRUE, 
                       recursive= FALSE
                       )

### choose data type
dtype = "leaf"
data_files = all_files[grepl(dtype, all_files)]

### species to consider
all_species = c("C_brachystachia",
                "C_latistipula", 
                "C_distishoclata",
                "C_flexuosa",
                "C_nictitans"
                )

### final vector with statistics
all_gmin = c()
all_stats = c()

for (one_species in all_species){
  ### results for one species
  sp_files = all_files[grepl(one_species, data_files)]
  
  ### reading results as list
  sp_resul_list = lapply(sp_files, read.csv)
  
  ### bindning into one dataframe
  sp_resul_df = rbindlist(sp_resul_list, fill = T)
  
  ### gmin value across individuals
  gmin_val = unique(sp_resul_df[["gmin_mean"]])
  
  ### gmin value per species
  sp_gmin = cbind(rep(one_species, length(gmin_val) ), gmin_val)
  
  ### add to final vector
  all_gmin = rbind(all_gmin, sp_gmin)
  
  ### gmin statistics for species
  sp_stats = 
    c(species = one_species,
      n = length(sp_files),
      gmin_avg = mean(gmin_val , na.rm = T),
      gmin_median = median(gmin_val , na.rm = T),
      gmin_sd = sd(gmin_val , na.rm = T),
      gmin_se = sd(gmin_val , na.rm = T)/sqrt(length(sp_files)),
      gmin_min = min(gmin_val, na.rm = T),
      gmin_max = max(gmin_val, na.rm = T) 
    )
  
  ### add to final vector
  all_stats = rbind(all_stats, sp_stats)
}

### convert to data frame
all_gmin_df = as.data.frame(all_gmin)
colnames(all_gmin_df) = c("species", "gmin_val")
all_stats_df = as.data.frame(all_stats, row.names = NA)

### plot
gmin_plot = ggplot(data = all_gmin_df,
       aes(x = species,
           y = as.numeric(gmin_val))) +
  
  geom_point(position = position_jitter(width = 0.10), 
             size = 2.5, 
             alpha = 0.5
  ) +
  
  geom_boxplot(weight = 0.5/length(unique(all_gmin_df$species)),
               linewidth = 0.3, 
               fill= "white",
               alpha = 0.50, 
               outlier.shape = NA
  )+
  
  xlab("species") +
  ylab("Gmin") +
  
  theme(panel.background=element_rect(fill="white"),
        panel.grid=element_line(colour=NULL),
        panel.border=element_rect(fill=NA,colour="black"),
        axis.title.x = element_text(size=12, face="bold"),
        axis.title.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(size= 10, angle = 0),
        axis.text.y = element_text(size= 10, angle = 0),
        legend.position = "none"
  )

### export
export_dir = paste0("2_gmin_results/0_",dtype,"_gmin_species")

### export tabble
write.table(all_stats_df, 
            paste0(export_dir, ".csv"),
            sep = ",",
            dec = ".",
            row.names = F
            )

## export plot
tiff(paste0(export_dir,".tiff"), 
     units="cm", 
     width=16, 
     height=12, 
     res=600)
print(gmin_plot)
dev.off()
