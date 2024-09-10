
gmin_routine = function(data, file_name){
  
  ### major parameters
  t = data[["time_minutes"]]
  w =  data[["weight_g"]]
  temp = data[["temp_C"]]
  humid = data[["humid_perc"]]
  atm = data[["atmospheric_pressure"]]
  
  ### checking available variables of area
  sum_moist = sum(data[["area_moist_cm2"]], na.rm = T)
  if(sum_moist != 0 ){
    data = data %>% 
      mutate(area = mean(`area_moist_cm2`, na.rm = T))
  } else {
    data = data %>% 
      mutate(area = mean(`area_dry_cm2`, na.rm = T))
  }
  ### setting area values
  area = data[["area"]]
  
  ### calcularte vpsat
  vpsat_val = vpsat(temp = temp)
  
  ### calculate mfvpd
  mfvpd_val = mfvpd(humid = humid,
                    vpsat = vpsat_val,
                    atm = atm
  )
  
  ### calculate delta weight and time
  dw_val = delta(x = w) 
  dt_val = delta(x = t) 
  
  ### calculate gmin
  gmin_val = gmin(dw = dw_val,
                  dt = dt_val, 
                  mfvpd = mfvpd_val[-1], 
                  area = area[-1] )
  
  ### gmin mean
  gmin_mean = mean(gmin_val)
  
  ### result table
  resul_table = cbind(t,vpsat_val, mfvpd_val, c(0,gmin_val), gmin_mean )
  colnames(resul_table) = c("time", "vpsat", "mfvpd" ,"gmin", "gmin_mean")
  
  ### linear model
  lm_fit = lm(w ~ t)
  lm_test = summary(lm_fit)
  intercept = round(lm_test$coefficients[[1]], 3)
  slope = round(lm_test$coefficients[[2]], 5)
  
  ### annotations
  r2_txt = round(lm_test$adj.r.squared, 5)
  intercept_txt = round(intercept, 3)
  if(slope > 0){
    slope_txt = paste0("+",slope)
  }else{
    slope_txt = slope
  }
  lm_formula = paste0("y = ", intercept_txt, slope_txt,"x")
  
  ### annotation coords
  x = t[length(t)-1 ]
  y1 = sort(w)[length(w)]
  y2 = sort(w)[length(w)-1 ]

  
  ### plot
  wt_plot = ggplot(data = data) +
    
    geom_point(aes(x = time_minutes, 
                   y = weight_g
                  ),
              size = 5,
              alpha = 0.5
    ) +
    
    geom_abline(
      intercept = intercept,
      slope = slope,
      linewidth = 1
    ) +
    
    annotate("text", x=x, y=y1, label= paste0("R2: ",r2_txt) ) + 
    annotate("text", x=x, y=y2, label= lm_formula ) +
    
    xlab("time (min)") +
    ylab("weight (g)") +
    
    theme(panel.background=element_rect(fill="white"),
          panel.grid=element_line(colour=NULL),
          panel.border=element_rect(fill=NA,colour="black"),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold"),
          axis.text.x = element_text(size= 10, angle = 0),
          axis.text.y = element_text(size= 10, angle = 0),
          legend.position = "none"
    )
  
  ################################### EXPORT ####################################
  
  ### export table
  write.table(resul_table, 
              paste0("2_gmin_results/",file_name, ".csv"), 
              sep=",", 
              quote = F, 
              row.names = F)
  
  
  ### export plot
  tiff(paste0("2_gmin_results/", file_name,"_wt_plot.tiff"), 
       units="cm", 
       width=14, 
       height=13, 
       res=600)
  print(wt_plot)
  dev.off()
  
  ### print
  print(paste0("Gmin analyses done: ", file_name) )
}


