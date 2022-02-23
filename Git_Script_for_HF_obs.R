#scripts for harvard forest observation data

setwd("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\")

#monthly climate
{
  #supplement nan data
  #knn interpolation
  library("mice")
  library("ggplot2")
  
  #preprocess climate data
  {
    clim_64_01 <- read.table("hf000-01-daily-m_for_R.csv",header = T, sep = ',')
    
    clim_df <- data.frame(airt = clim_64_01$airt,prec = clim_64_01$prec)
    
    out_interpo = mice(clim_df,m=5,maxit = 10,method = "pmm",seed=10)
    clim_complete = complete(out_interpo)
    #output to a file 
    write.csv(clim_complete,"hf000_daily_temp_prep.csv")
  }
  
  clim_input <- read.csv("hf000_daily_temp_prep.csv")
  
  #for HF-000 daily data
  d_step <- 1
  y_step <- 365
  
  #define the temp&prep array
  {temp_jan <- numeric(0)
    temp_feb <- numeric(0)
    temp_mar <- numeric(0)
    temp_apr <- numeric(0)
    temp_may <- numeric(0)
    temp_jun <- numeric(0)
    temp_jul <- numeric(0)
    temp_aug <- numeric(0)
    temp_sep <- numeric(0)
    temp_oct <- numeric(0)
    temp_nov <- numeric(0)
    temp_dec <- numeric(0)
    
    prep_jan <- numeric(0)
    prep_feb <- numeric(0)
    prep_mar <- numeric(0)
    prep_apr <- numeric(0)
    prep_may <- numeric(0)
    prep_jun <- numeric(0)
    prep_jul <- numeric(0)
    prep_aug <- numeric(0)
    prep_sep <- numeric(0)
    prep_oct <- numeric(0)
    prep_nov <- numeric(0)
    prep_dec <- numeric(0)
    temp_grow <- numeric(0)
    prep_grow <- numeric(0)
  }
  
  year = 0 
  for (i in 1964:2001){
    year = rep(i,12)
    year_sum = c(year_sum,year)
  }
  
  #1964-2001
  for (j in 1:38){    #year run 
    #current year
    jan_beg <- (j-1)*y_step + 1
    jan_end <- (j-1)*y_step+31*d_step
    feb_beg <- (j-1)*y_step+31*d_step+1
    feb_end <- (j-1)*y_step+59*d_step
    mar_beg <- (j-1)*y_step+59*d_step+1
    mar_end <- (j-1)*y_step+90*d_step
    apr_beg <- (j-1)*y_step+90*d_step+1
    apr_end <- (j-1)*y_step+120*d_step
    may_beg <- (j-1)*y_step+120*d_step+1
    may_end <- (j-1)*y_step+151*d_step
    jun_beg <- (j-1)*y_step+151*d_step+1
    jun_end <- (j-1)*y_step+181*d_step
    jul_beg <- (j-1)*y_step+181*d_step+1
    jul_end <- (j-1)*y_step+212*d_step
    aug_beg <- (j-1)*y_step+212*d_step+1
    aug_end <- (j-1)*y_step+243*d_step
    sep_beg <- (j-1)*y_step+243*d_step+1
    sep_end <- (j-1)*y_step+273*d_step
    oct_beg <- (j-1)*y_step+273*d_step+1
    oct_end <- (j-1)*y_step+304*d_step
    nov_beg <- (j-1)*y_step+304*d_step+1
    nov_end <- (j-1)*y_step+334*d_step
    dec_beg <- (j-1)*y_step+334*d_step+1
    dec_end <- (j-1)*y_step+365*d_step
    
    #current year prep
    temp_jan[j] <- mean(clim_input$airt[jan_beg:jan_end],na.rm=TRUE) 
    temp_feb[j] <- mean(clim_input$airt[feb_beg:feb_end],na.rm=TRUE) 
    temp_mar[j] <- mean(clim_input$airt[mar_beg:mar_end],na.rm=TRUE) 
    temp_apr[j] <- mean(clim_input$airt[apr_beg:apr_end],na.rm=TRUE) 
    temp_may[j] <- mean(clim_input$airt[may_beg:may_end],na.rm=TRUE)  
    temp_jun[j] <- mean(clim_input$airt[jun_beg:jun_end],na.rm=TRUE) 
    temp_jul[j] <- mean(clim_input$airt[jul_beg:jul_end],na.rm=TRUE) 
    temp_aug[j] <- mean(clim_input$airt[aug_beg:aug_end],na.rm=TRUE) 
    temp_sep[j] <- mean(clim_input$airt[sep_beg:sep_end],na.rm=TRUE) 
    temp_oct[j] <- mean(clim_input$airt[oct_beg:oct_end],na.rm=TRUE) 
    temp_nov[j] <- mean(clim_input$airt[nov_beg:nov_end],na.rm=TRUE) 
    temp_dec[j] <- mean(clim_input$airt[dec_beg:dec_end],na.rm=TRUE) 
    
    #current year prep
    prep_jan[j] <- mean(clim_input$prec[jan_beg:jan_end],na.rm=TRUE)
    prep_feb[j] <- mean(clim_input$prec[feb_beg:feb_end],na.rm=TRUE)
    prep_mar[j] <- mean(clim_input$prec[mar_beg:mar_end],na.rm=TRUE)
    prep_apr[j] <- mean(clim_input$prec[apr_beg:apr_end],na.rm=TRUE)
    prep_may[j] <- mean(clim_input$prec[may_beg:may_end],na.rm=TRUE)  
    prep_jun[j] <- mean(clim_input$prec[jun_beg:jun_end],na.rm=TRUE)
    prep_jul[j] <- mean(clim_input$prec[jul_beg:jul_end],na.rm=TRUE)
    prep_aug[j] <- mean(clim_input$prec[aug_beg:aug_end],na.rm=TRUE)
    prep_sep[j] <- mean(clim_input$prec[sep_beg:sep_end],na.rm=TRUE)
    prep_oct[j] <- mean(clim_input$prec[oct_beg:oct_end],na.rm=TRUE)
    prep_nov[j] <- mean(clim_input$prec[nov_beg:nov_end],na.rm=TRUE)
    prep_dec[j] <- mean(clim_input$prec[dec_beg:dec_end],na.rm=TRUE)  
    
    temp_grow[j] <- mean(c(temp_apr[j],temp_may[j],temp_jun[j],temp_jul[j],temp_aug[j],temp_sep[j]))
    prep_grow[j] <- mean(c(prep_apr[j],prep_may[j],prep_jun[j],prep_jul[j],prep_aug[j],prep_sep[j]))
    
    out_month_t <- c(temp_jan[j],temp_feb[j],temp_mar[j],temp_apr[j],temp_may[j],temp_jun[j],temp_jul[j],temp_aug[j],temp_sep[j],temp_oct[j],temp_nov[j],temp_dec[j])
    out_month_p <- c(prep_jan[j],prep_feb[j],prep_mar[j],prep_apr[j],prep_may[j],prep_jun[j],prep_jul[j],prep_aug[j],prep_sep[j],prep_oct[j],prep_nov[j],prep_dec[j])
    
    sum_month_t <- c(sum_month_t,out_month_t)
    sum_month_p <- c(sum_month_p,out_month_p)
  } 
  
  month_df = data.frame(year = year_sum,temp_m = sum_month_t,prep_m = sum_month_p)
  
  #1964-2019
  clim_64_19 <- read.csv("month_temp_prep_1964_2019.csv")
  clim_64_19_sub <- subset(clim_64_19,month >= 4 & month <= 8)
  clim_64_19_grow <- aggregate(clim_64_19_sub,by =list(clim_64_19_sub$year),FUN=mean)
  
  clim_64_19_sub <- subset(clim_64_19,month == 6)
  clim_64_19_grow <- aggregate(clim_64_19_sub,by =list(clim_64_19_sub$year),FUN=mean)
  
  clim_64_19_grow_t_scale <- scale(clim_64_19_grow$temp_m)
  clim_64_19_grow_p_scale <- scale(clim_64_19_grow$prep_m)
  
  clim_64_19_grow_scale_df <- data.frame(t_scale = clim_64_19_grow_t_scale[,1], p_scale = clim_64_19_grow_p_scale[,1])
  
  clim_64_19_grow = cbind(clim_64_19_grow,clim_64_19_grow_scale_df)
  
  #drought index pdsi 1980_2019
  {
    pdsi_HF <- read.csv("pdsi_HF1.csv")
    pdsi_sub <- subset(pdsi_HF,Month >= 4 & Month <=8)
    pdsi_sub_grow <- aggregate(pdsi_sub,by =list(pdsi_sub$Year),FUN=mean)
    
  }
  
  #monthly drought indicators 1958_2019
  #from https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE
  {
    pdsi_HF = read.csv("pdsi_HF_Monthly_4km.csv")
    vpd_HF = read.csv("vpd_HF_Monthly_4km.csv")
    tmmx_HF = read.csv("tmmx_HF_Monthly_4km.csv")
    
    
    pdsi_sub1 <- subset(pdsi_HF,Month >= 4 & Month <=6)
    pdsi_sub_grow1 <- aggregate(pdsi_sub1,by =list(pdsi_sub1$Year),FUN=mean)
    
    pdsi_sub2 <- subset(pdsi_HF,Month >= 7 & Month <=9)
    pdsi_sub_grow2 <- aggregate(pdsi_sub2,by =list(pdsi_sub2$Year),FUN=mean)
    
    pdsi_sub <- subset(pdsi_HF,Month >= 4 & Month <=9)
    pdsi_sub_grow <- aggregate(pdsi_sub,by =list(pdsi_sub$Year),FUN=mean)  
    
    vpd_sub1 <- subset(vpd_HF,Month >= 4 & Month <=6)
    vpd_sub_grow1 <- aggregate(vpd_sub1,by =list(vpd_sub1$Year),FUN=mean)
    
    vpd_sub2 <- subset(vpd_HF,Month >= 7 & Month <=9)
    vpd_sub_grow2 <- aggregate(vpd_sub2,by =list(vpd_sub2$Year),FUN=mean)
    
    vpd_sub <- subset(vpd_HF,Month >= 4 & Month <=9)
    vpd_sub_grow <- aggregate(vpd_sub,by =list(vpd_sub$Year),FUN=mean)  
    
    
    tmmx_sub1 <- subset(tmmx_HF,Month >= 4 & Month <=6)
    tmmx_sub_grow1 <- aggregate(tmmx_sub1,by =list(tmmx_sub1$Year),FUN=mean)
    
    tmmx_sub2 <- subset(tmmx_HF,Month >= 7 & Month <=9)
    tmmx_sub_grow2 <- aggregate(tmmx_sub2,by =list(tmmx_sub2$Year),FUN=mean)
    
    tmmx_sub <- subset(tmmx_HF,Month >= 4 & Month <=9)
    tmmx_sub_grow <- aggregate(tmmx_sub,by =list(tmmx_sub$Year),FUN=mean) 
    
    pdsi_sub_grow_58_14 = pdsi_sub_grow[1:57,]
    pdsi_sub_grow1_58_14 = pdsi_sub_grow1[1:57,]
    pdsi_sub_grow2_58_14 = pdsi_sub_grow2[1:57,]
  }
  
  #MODIS ET/PET
  {
    MODIS_ET_EMS = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_HF_EMS_01_19.csv")
    MODIS_ET_obs = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_HF_obs_01_19.csv")
    
    MODIS_PET_EMS = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_HF_EMS_01_19.csv") 
    MODIS_PET_obs = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_HF_obs_01_19.csv")
    
    #mice initialization
    library(mice)
    
    MODIS_PET_EMS_sub = MODIS_PET_EMS[ , -which(colnames(MODIS_PET_EMS) %in% c("nei1","nei2","nei3","nei4","nei5","nei6","nei7","nei8","Mean_nei","Mean"))]
    MODIS_ET_EMS_sub = MODIS_ET_EMS[ , -which(colnames(MODIS_ET_EMS) %in% c("nei1","nei2","nei3","nei4","nei5","nei6","nei7","nei8","Mean_nei","Mean"))]
    MODIS_PET_obs_sub = MODIS_PET_obs[ , -which(colnames(MODIS_PET_obs) %in% c("nei1","nei2","nei3","nei4","nei5","nei6","Mean_nei","Mean"))]
    MODIS_ET_obs_sub = MODIS_ET_obs[ , -which(colnames(MODIS_ET_obs) %in% c("nei1","nei2","nei3","nei4","nei5","nei6","Mean_nei","Mean"))]
    
    MODIS_PET_EMS_sub_fill1 = mice(MODIS_PET_EMS_sub, method="rf",m=5,seed = 1)
    MODIS_PET_EMS_sub_fill2 = complete(MODIS_PET_EMS_sub_fill1)
    
    MODIS_ET_EMS_sub_fill1 = mice(MODIS_ET_EMS_sub, method="rf",m=5,seed = 1)
    MODIS_ET_EMS_sub_fill2 = complete(MODIS_ET_EMS_sub_fill1)
    
    MODIS_PET_obs_sub_fill1 = mice(MODIS_PET_obs_sub, method="rf",m=5,seed = 1)
    MODIS_PET_obs_sub_fill2 = complete(MODIS_PET_obs_sub_fill1)
    
    MODIS_ET_obs_sub_fill1 = mice(MODIS_ET_obs_sub, method="rf",m=5,seed = 1)
    MODIS_ET_obs_sub_fill2 = complete(MODIS_ET_obs_sub_fill1)
    
    #write files out
    #   {
    #   write.csv(MODIS_ET_EMS_sub_fill2,"D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_EMS_fill2.csv")
    #   write.csv(MODIS_PET_EMS_sub_fill2,"D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_EMS_fill2.csv")
    #   write.csv(MODIS_ET_obs_sub_fill2,"D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_obs_fill2.csv")
    #   write.csv(MODIS_PET_obs_sub_fill2,"D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_obs_fill2.csv")
    # }
    
    #reread the combined files
    {
      MODIS_ET_EMS_filled = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_HF_EMS_01_19.csv")
      MODIS_ET_obs_filled = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_ET_HF_obs_01_19.csv")
      
      MODIS_PET_EMS_filled = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_HF_EMS_01_19.csv") 
      MODIS_PET_obs_filled = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\MODIS_ET_PET\\MODIS_PET_HF_obs_01_19.csv")  
    }
    
    #get the growing season
    {
      MODIS_PET_EMS_filled_grow_4_8 <- subset(MODIS_PET_EMS_filled,Month >= 4 & Month <= 8)
      MODIS_ET_EMS_filled_grow_4_8 <- subset(MODIS_ET_EMS_filled,Month >= 4 & Month <= 8)
      MODIS_PET_obs_filled_grow_4_8 <- subset(MODIS_PET_obs_filled,Month >= 4 & Month <= 8)
      MODIS_ET_obs_filled_grow_4_8 <- subset(MODIS_ET_obs_filled,Month >= 4 & Month <= 8)
      
      MODIS_PET_EMS_filled_grow_4_9 <- subset(MODIS_PET_EMS_filled,Month >= 4 & Month <= 9)
      MODIS_ET_EMS_filled_grow_4_9 <- subset(MODIS_ET_EMS_filled,Month >= 4 & Month <= 9)
      MODIS_PET_obs_filled_grow_4_9 <- subset(MODIS_PET_obs_filled,Month >= 4 & Month <= 9)
      MODIS_ET_obs_filled_grow_4_9 <- subset(MODIS_ET_obs_filled,Month >= 4 & Month <= 9)
    }
    
    #plot PET
    {
      MODIS_PET_EMS_obs_4_8 = data.frame(MODIS_PET_EMS_filled_grow_4_8$EMS_final,MODIS_PET_obs_filled_grow_4_8$obs_final)
      MODIS_PET_EMS_obs_4_9 = data.frame(MODIS_PET_EMS_filled_grow_4_9$EMS_final,MODIS_PET_obs_filled_grow_4_9$obs_final)
      
      MODIS_ET_EMS_obs_4_8 = data.frame(MODIS_ET_EMS_filled_grow_4_8$EMS_final,MODIS_ET_obs_filled_grow_4_8$obs_final)
      MODIS_ET_EMS_obs_4_9 = data.frame(MODIS_ET_EMS_filled_grow_4_9$EMS_final,MODIS_ET_obs_filled_grow_4_9$obs_final) 
      
      p_PET_4_9 = ggplot(MODIS_PET_EMS_obs_4_9,aes(MODIS_PET_EMS_filled_grow_4_9.EMS_final,MODIS_PET_obs_filled_grow_4_9.obs_final))+
        geom_point()+
        geom_smooth(method='lm', formula= y~x)+
        xlab("MODIS_PET_EMS kg/m^2/8day")+
        ylab("MODIS_PET_obs kg/m^2/8day")+
        xlim(100,700)+
        ylim(100,700)
      
      
      p_PET_obs_nei_4_9 = ggplot(MODIS_PET_obs_filled_grow_4_9,aes(obs,Mean_nei))+
        geom_point()+
        geom_smooth(method='lm', formula= y~x)+
        xlab("MODIS_PET_obs kg/m^2/8day")+
        ylab("MODIS_PET_obs_nei kg/m^2/8day")+
        xlim(100,700)+
        ylim(100,700)   
      
      p_PET_EMS_nei_4_9 = ggplot(MODIS_PET_EMS_filled_grow_4_9,aes(EMS,Mean_nei))+
        geom_point()+
        geom_smooth(method='lm', formula= y~x)+
        xlab("MODIS_PET_EMS kg/m^2/8day")+
        ylab("MODIS_PET_EMS_nei kg/m^2/8day")+
        xlim(100,700)+
        ylim(100,700) 
    }
    
  }
  
  #put PET info in clim df
  {
    clim_64_19_sub <- subset(clim_64_19,month >= 4 & month <= 9)
    clim_64_19_grow <- aggregate(clim_64_19_sub,by =list(clim_64_19_sub$year),FUN=mean)
    
    clim_64_19_sub <- subset(clim_64_19,month == 9)
    clim_64_19_grow <- aggregate(clim_64_19_sub,by =list(clim_64_19_sub$year),FUN=mean)
    
    clim_64_19_grow_t_scale <- scale(clim_64_19_grow$temp_m)
    clim_64_19_grow_p_scale <- scale(clim_64_19_grow$prep_m)
    
    clim_64_19_grow_scale_df <- data.frame(t_scale = clim_64_19_grow_t_scale[,1], p_scale = clim_64_19_grow_p_scale[,1])
    
    clim_64_19_grow_9 = cbind(clim_64_19_grow,clim_64_19_grow_scale_df)
    
    MODIS_PET_EMS_sub_filled = MODIS_PET_EMS_filled[ , -which(colnames(MODIS_PET_EMS_filled) %in% c("DY","EMS","nei1","nei2","nei3","nei4","nei5","nei6","nei7","nei8","Mean_nei","Mean","EMS_final"))]
    
    names(MODIS_PET_EMS_sub_filled) = c("year","month","EMS_final_filled")
    
    #for multiple months
    MODIS_PET_EMS_filled_grow_sub <- subset(MODIS_PET_EMS_sub_filled,month >= 4 & month <=8)
    #for each month
    MODIS_PET_EMS_filled_grow_sub <- subset(MODIS_PET_EMS_sub_filled,month == 9)
    
    MODIS_PET_EMS_filled_grow <- aggregate(MODIS_PET_EMS_filled_grow_sub,by =list(MODIS_PET_EMS_filled_grow_sub$year),FUN=mean)
    
    MODIS_PET_EMS_filled_grow_scale <- scale(MODIS_PET_EMS_filled_grow$EMS_final_filled)
    
    MODIS_PET_EMS_filled_grow_scale_df <- data.frame(PET_scale = MODIS_PET_EMS_filled_grow_scale[,1])
    
    MODIS_PET_EMS_filled_grow_9 = cbind(MODIS_PET_EMS_filled_grow,MODIS_PET_EMS_filled_grow_scale_df)
    
    #write files out
    {
      write.csv(clim_64_19_grow_4,"clim_64_19_grow_4.csv")
      write.csv(clim_64_19_grow_5,"clim_64_19_grow_5.csv")
      write.csv(clim_64_19_grow_6,"clim_64_19_grow_6.csv")
      write.csv(clim_64_19_grow_7,"clim_64_19_grow_7.csv")
      write.csv(clim_64_19_grow_8,"clim_64_19_grow_8.csv")
      write.csv(clim_64_19_grow_9,"clim_64_19_grow_9.csv")
      write.csv(clim_64_19_grow_4_8,"clim_64_19_grow_4_8.csv")
      write.csv(clim_64_19_grow_4_9,"clim_64_19_grow_4_9.csv")
      
      write.csv(MODIS_PET_EMS_filled_grow_4,"MODIS_PET_EMS_filled_grow_4.csv")
      write.csv(MODIS_PET_EMS_filled_grow_5,"MODIS_PET_EMS_filled_grow_5.csv")
      write.csv(MODIS_PET_EMS_filled_grow_6,"MODIS_PET_EMS_filled_grow_6.csv")
      write.csv(MODIS_PET_EMS_filled_grow_7,"MODIS_PET_EMS_filled_grow_7.csv")
      write.csv(MODIS_PET_EMS_filled_grow_8,"MODIS_PET_EMS_filled_grow_8.csv")
      write.csv(MODIS_PET_EMS_filled_grow_9,"MODIS_PET_EMS_filled_grow_9.csv")
      write.csv(MODIS_PET_EMS_filled_grow_4_8,"MODIS_PET_EMS_filled_grow_4_8.csv")
      write.csv(MODIS_PET_EMS_filled_grow_4_9,"MODIS_PET_EMS_filled_grow_4_9.csv")
      
    }
    
    #combined the dfs manually to get P/PET then read the new files
    {
      clim_pet_4_9_com = read.csv("clim_pet_64_19_grow_4_9.csv")
      clim_pet_4_8_com = read.csv("clim_pet_64_19_grow_4_8.csv")
      clim_pet_4_com = read.csv("clim_pet_64_19_grow_4.csv")
      clim_pet_5_com = read.csv("clim_pet_64_19_grow_5.csv")
      clim_pet_6_com = read.csv("clim_pet_64_19_grow_6.csv")
      clim_pet_7_com = read.csv("clim_pet_64_19_grow_7.csv")
      clim_pet_8_com = read.csv("clim_pet_64_19_grow_8.csv")
      clim_pet_9_com = read.csv("clim_pet_64_19_grow_9.csv")
    }
    
    ppet_scale <- scale(clim_pet_4_9_com$P_PET)
    
    ppet_scale_df <- data.frame(ppet_scale = ppet_scale[,1])


    clim_pet_ppet_4_8_com = cbind(clim_pet_4_8_com,ppet_scale_df)
    clim_pet_ppet_4_9_com = cbind(clim_pet_4_9_com,ppet_scale_df)
    clim_pet_ppet_4_com = cbind(clim_pet_4_com,ppet_scale_df)
    clim_pet_ppet_5_com = cbind(clim_pet_5_com,ppet_scale_df)
    clim_pet_ppet_6_com = cbind(clim_pet_6_com,ppet_scale_df)
    clim_pet_ppet_7_com = cbind(clim_pet_7_com,ppet_scale_df)        
    clim_pet_ppet_8_com = cbind(clim_pet_8_com,ppet_scale_df)
    clim_pet_ppet_9_com = cbind(clim_pet_9_com,ppet_scale_df)
    
    write.csv(clim_pet_ppet_4_9_com,"clim_pet_ppet_4_9_com.csv")
    write.csv(clim_pet_ppet_4_8_com,"clim_pet_ppet_4_8_com.csv")  
    write.csv(clim_pet_ppet_4_com,"clim_pet_ppet_4_com.csv")
    write.csv(clim_pet_ppet_5_com,"clim_pet_ppet_5_com.csv")
    write.csv(clim_pet_ppet_6_com,"clim_pet_ppet_6_com.csv")
    write.csv(clim_pet_ppet_7_com,"clim_pet_ppet_7_com.csv")
    write.csv(clim_pet_ppet_8_com,"clim_pet_ppet_8_com.csv")
    write.csv(clim_pet_ppet_9_com,"clim_pet_ppet_9_com.csv")
  }
  
  #monthly output

  p1 <- ggplot()+
    #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                ymax= max),fill = "green",alpha = 0.4)+
    #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                  ymax= max),fill = "pink",alpha = 0.4)+
    geom_line(data = pdsi_sub_grow,aes(x=Year,y=PDSI,color = "z"),linetype="solid",size = 1)+
    geom_line(data = pdsi_sub_grow1,aes(x=Year, 
                                        y=PDSI,color = "royalblue4"),linetype="solid",size =1)+
    geom_line(data = pdsi_sub_grow2,aes(x=Year,y=PDSI,color = "green4"),linetype="solid",size = 1)+ 
    #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("z-score")+
    xlab("Year")+
    xlim(1955,2020)+
    ylim(-7.5,7.5)+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("royalblue4"="#f37735","green4" = "#ffc425","z" = "#d11141"), labels = c('PDSI_4_6','PDSI_7_9','PDSI_4_9'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.8,0.18),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
#4_8
  {
  p_pdsi <- ggplot()+
    #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                ymax= max),fill = "green",alpha = 0.4)+
    #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                  ymax= max),fill = "pink",alpha = 0.4)+
    geom_line(data = clim_pet_ppet_4_8_com,aes(x=year,y= PET_scale,color = "z"),linetype="solid",size = 1,alpha=0.75)+
    geom_line(data = clim_pet_ppet_4_8_com,aes(x=year, 
                                               y=t_scale,color = "royalblue4"),linetype="solid",size =1)+
    geom_line(data = clim_pet_ppet_4_8_com,aes(x=year,y= p_scale,color = "green4"),linetype="solid",size = 1)+
    geom_line(data = clim_pet_ppet_4_8_com,aes(x=year,y= ppet_scale,color = "yellow4"),linetype="solid",size = 1)+  
    
    #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("z-score")+
    xlab("Year")+
    xlim(1955,2020)+
    ylim(-4,4)+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("royalblue4"="royalblue4","green4" = "green4","yellow4" = "#edae49","z" = "#F8766D"), labels = c('Prep_4_8','Temp_4_8','PPET_4_8','PET_4_8'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
}

#4_9
  {
    clim_pet_ppet_4_9_com = read.csv("clim_pet_ppet_4_9_com.csv")
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Figure_for_draft\\fig1_draft_updated_4_9_only_2022_2_24.pdf",width =10,height = 5)
    
    p_pdsi <- ggplot()+
      #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                                ymax= max),fill = "green",alpha = 0.4)+
      #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                                  ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = clim_pet_ppet_4_9_com,aes(x=year,y= PET_scale,color = "z"),linetype="solid",size = 1,alpha=0.75)+
      geom_line(data = clim_pet_ppet_4_9_com,aes(x=year, 
                                                 y=t_scale,color = "royalblue4"),linetype="solid",size =1)+
      geom_line(data = clim_pet_ppet_4_9_com,aes(x=year,y= p_scale,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = clim_pet_ppet_4_9_com,aes(x=year,y= ppet_scale,color = "yellow4"),linetype="solid",size = 1)+  
      
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("z-score")+
      xlab("Year")+
      xlim(1955,2020)+
      ylim(-4,4)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","yellow4" = "#edae49","z" = "#F8766D"), labels = c('Prep_4_9','Temp_4_9','PPET_4_9','PET_4_9'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 16))+
      theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
    
    p_pdsi
    dev.off()
  } 
      
}

#xylo data input
{
  library("CAVIAR")
  library("gdata")
  installXLSXsupport(perl = "D:\\software\\strawberryPerl\\perl\\bin\\perl.exe")
  
  ##data for HF
  HF2017_maple <- file.path("HF Xylo\\HF_Maple2017_BAND.xlsx")
  HF2017_pine <- file.path("HF Xylo\\HF_Pine2017_BAND.xlsx")
  HF2017_oak <- file.path("HF Xylo\\HF_Oak2017_BAND.xlsx")
  
  HF2018_maple <- file.path("HF Xylo\\HF_Maple2018_BAND.xlsx")
  HF2018_pine <- file.path("HF Xylo\\HF_Pine2018_BAND.xlsx")
  HF2018_oak <- file.path("HF Xylo\\HF_Oak2018_BAND.xlsx")
  
  HF2019_maple <- file.path("HF Xylo\\HF_Maple2019_BAND.xlsx")
  HF2019_pine <- file.path("HF Xylo\\HF_Pine2019_BAND.xlsx")
  HF2019_oak <- file.path("HF Xylo\\HF_Oak2019_BAND.xlsx")
  
  HF_test <- file.path("woodGrowthDataHarvardForest2017_2019.xlsx")
  
  HF2017_maple.raw.data <- readExcelCountTable(excel.file.name = HF2017_maple)
  HF2017_pine.raw.data <- readExcelCountTable(excel.file.name = HF2017_pine)
  HF2017_oak.raw.data <- readExcelCountTable(excel.file.name = HF2017_oak)
  
  HF2018_maple.raw.data <- readExcelCountTable(excel.file.name = HF2018_maple)
  HF2018_pine.raw.data <- readExcelCountTable(excel.file.name = HF2018_pine)
  HF2018_oak.raw.data <- readExcelCountTable(excel.file.name = HF2018_oak)
  
  HF2019_maple.raw.data <- readExcelCountTable(excel.file.name = HF2019_maple)
  HF2019_pine.raw.data <- readExcelCountTable(excel.file.name = HF2019_pine)
  HF2019_oak.raw.data <- readExcelCountTable(excel.file.name = HF2019_oak)
  
  HF_test.raw.data <- readExcelCountTable(excel.file.name = HF_test)
  
}

#Phenology
#leaf phenology O'keefe
{
  l_pheno = read.csv("hf003-06-spring-fall-mean-spp_QURU_ACRU_PIST.csv")
  
  l_pheno_QURU = subset(l_pheno,species == "QURU")
  l_pheno_ACRU = subset(l_pheno,species == "ACRU")
  l_pheno_PIST = subset(l_pheno,species == "PIST")
  
  l_pheno_QURU_17_19 = subset(l_pheno,species == "QURU" & year >= 2017 & year <= 2019)
  l_pheno_ACRU_17_19 = subset(l_pheno,species == "ACRU" & year >= 2017 & year <= 2019)
  
  l_pheno_QURU_17 = subset(l_pheno,species == "QURU" & year == 2017)
  l_pheno_QURU_18 = subset(l_pheno,species == "QURU" & year == 2018)
  l_pheno_QURU_19 = subset(l_pheno,species == "QURU" & year == 2019)
  
  l_pheno_ACRU_17 = subset(l_pheno,species == "ACRU" & year == 2017)
  l_pheno_ACRU_18 = subset(l_pheno,species == "ACRU" & year == 2018)
  l_pheno_ACRU_19 = subset(l_pheno,species == "ACRU" & year == 2019)
  
  
  l_pheno_QURU_mean = aggregate(l_pheno_QURU,by = list(l_pheno_QURU$species),FUN = mean,na.rm=TRUE, na.action=na.pass)
  l_pheno_ACRU_mean = aggregate(l_pheno_ACRU,by = list(l_pheno_ACRU$species),FUN = mean,na.rm=TRUE, na.action=na.pass)
  l_pheno_PIST_mean = aggregate(l_pheno_PIST,by = list(l_pheno_PIST$species),FUN = mean,na.rm=TRUE, na.action=na.pass)
  
  l_pheno_mean = rbind(l_pheno_QURU_mean,l_pheno_ACRU_mean,l_pheno_PIST_mean)
  l_pheno_17_19 = rbind(l_pheno_QURU_17_19,l_pheno_ACRU_17_19)
}

#leaf phenology Tim
{
  l_pheno_Tim = read.csv("leafPhenologySpringAndFallHavardForest2017-2019.csv")
  
  #2017:21 2018:20 2019:20
  for (i in 1:21){
    l_pheno_Tim$GDD4[i] = temp_gdd_dd_17_19$X.4_2017_acc[l_pheno_Tim$bb.doy[i]]
  }
  for (i in 22:41){
    l_pheno_Tim$GDD4[i] = temp_gdd_dd_17_19$X.4_2018_acc[l_pheno_Tim$bb.doy[i]]
  }
  for (i in 42:61){
    l_pheno_Tim$GDD4[i] = temp_gdd_dd_17_19$X.4_2019_acc[l_pheno_Tim$bb.doy[i]]
  }
  
  for (i in 1:21){
    l_pheno_Tim$CDD20[i] = temp_cdd_dd_17_19$X.20_2017_acc[l_pheno_Tim$lf.doy[i]]
  }
  for (i in 22:41){
    l_pheno_Tim$CDD20[i] = temp_cdd_dd_17_19$X.20_2018_acc[l_pheno_Tim$lf.doy[i]]
  }
  for (i in 42:61){
    l_pheno_Tim$CDD20[i] = temp_cdd_dd_17_19$X.20_2019_acc[l_pheno_Tim$lf.doy[i]]
  }
  
  
  
  
  l_pheno_QURU_Tim = subset(l_pheno_Tim,species == "Quercus rubra")
  l_pheno_ACRU_Tim = subset(l_pheno_Tim,species == "Acer rubrum")
  l_pheno_PIST_Tim = subset(l_pheno_Tim,species == "Pinus strobus")
  
  l_pheno_QURU_17_19_Tim = subset(l_pheno_Tim,species == "Quercus rubra" & year >= 2017 & year <= 2019)
  l_pheno_ACRU_17_19_Tim = subset(l_pheno_Tim,species == "Acer rubrum" & year >= 2017 & year <= 2019)
  l_pheno_PIST_17_19_Tim = subset(l_pheno_Tim,species == "Pinus strobus" & year >= 2017 & year <= 2019)
  
  l_pheno_QURU_17_Tim = subset(l_pheno_Tim,species == "Quercus rubra" & year == 2017)
  l_pheno_QURU_18_Tim = subset(l_pheno_Tim,species == "Quercus rubra" & year == 2018)
  l_pheno_QURU_19_Tim = subset(l_pheno_Tim,species == "Quercus rubra" & year == 2019)
  
  l_pheno_ACRU_17_Tim = subset(l_pheno_Tim,species == "Acer rubrum" & year == 2017)
  l_pheno_ACRU_18_Tim = subset(l_pheno_Tim,species == "Acer rubrum" & year == 2018)
  l_pheno_ACRU_19_Tim = subset(l_pheno_Tim,species == "Acer rubrum" & year == 2019)
  
  l_pheno_PIST_17_Tim = subset(l_pheno_Tim,species == "Pinus strobus" & year == 2017)
  l_pheno_PIST_18_Tim = subset(l_pheno_Tim,species == "Pinus strobus" & year == 2018)
  l_pheno_PIST_19_Tim = subset(l_pheno_Tim,species == "Pinus strobus" & year == 2019)
  
  
  l_pheno_QURU_mean_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_17_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_QURU_mean_17_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_QURU_sd_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_17_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_QURU_sd_17_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_QURU_17_Tim_df = merge(l_pheno_QURU_mean_17_Tim,l_pheno_QURU_sd_17_Tim)
  
  l_pheno_QURU_mean_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_18_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_QURU_mean_18_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_QURU_sd_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_18_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_QURU_sd_18_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_QURU_18_Tim_df = merge(l_pheno_QURU_mean_18_Tim,l_pheno_QURU_sd_18_Tim)
  
  l_pheno_QURU_mean_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_19_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_QURU_mean_19_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_QURU_sd_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_QURU_19_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_QURU_sd_19_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_QURU_19_Tim_df = merge(l_pheno_QURU_mean_19_Tim,l_pheno_QURU_sd_19_Tim)
  
  l_pheno_ACRU_mean_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_17_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_ACRU_mean_17_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_ACRU_sd_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_17_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_ACRU_sd_17_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_ACRU_17_Tim_df = merge(l_pheno_ACRU_mean_17_Tim,l_pheno_ACRU_sd_17_Tim)
  
  l_pheno_ACRU_mean_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_18_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_ACRU_mean_18_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_ACRU_sd_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_18_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_ACRU_sd_18_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_ACRU_18_Tim_df = merge(l_pheno_ACRU_mean_18_Tim,l_pheno_ACRU_sd_18_Tim)
  
  l_pheno_ACRU_mean_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_19_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_ACRU_mean_19_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_ACRU_sd_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_ACRU_19_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_ACRU_sd_19_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_ACRU_19_Tim_df = merge(l_pheno_ACRU_mean_19_Tim,l_pheno_ACRU_sd_19_Tim)
  
  l_pheno_PIST_mean_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_17_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_PIST_mean_17_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_PIST_sd_17_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_17_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_PIST_sd_17_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_PIST_17_Tim_df = merge(l_pheno_PIST_mean_17_Tim,l_pheno_PIST_sd_17_Tim)
  
  l_pheno_PIST_mean_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_18_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_PIST_mean_18_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_PIST_sd_18_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_18_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_PIST_sd_18_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_PIST_18_Tim_df = merge(l_pheno_PIST_mean_18_Tim,l_pheno_PIST_sd_18_Tim)
  
  l_pheno_PIST_mean_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_19_Tim,FUN=mean,na.rm=TRUE)
  names(l_pheno_PIST_mean_19_Tim) = c("species","bb.doy_mean","lc.doy_mean","lf.doy_mean")
  l_pheno_PIST_sd_19_Tim <- aggregate(cbind(bb.doy,lc.doy,lf.doy)~species,data = l_pheno_PIST_19_Tim,FUN=sd,na.rm=TRUE)
  names(l_pheno_PIST_sd_19_Tim) = c("species","bb.doy_sd","lc.doy_sd","lf.doy_sd")
  l_pheno_PIST_19_Tim_df = merge(l_pheno_PIST_mean_19_Tim,l_pheno_PIST_sd_19_Tim)
  
}

#Cell counts 
{
  #remove data of A30 from HF2017_maple.raw.data the extra tree than the other two years
  HF2017_maple.raw.data1 = HF2017_maple.raw.data[-(which(HF2017_maple.raw.data$Tree == "A30")),]
  
  ## Computing the mean for raw data
  HF2017_pine.mean <- aggregateRadialFiles(data=HF2017_pine.raw.data, stat="mean")
  HF2018_pine.mean <- aggregateRadialFiles(data=HF2018_pine.raw.data, stat="mean")
  HF2019_pine.mean <- aggregateRadialFiles(data=HF2019_pine.raw.data, stat="mean")
  
  HF2017_maple.mean <- aggregateRadialFiles(data=HF2017_maple.raw.data1, stat="mean")
  HF2018_maple.mean <- aggregateRadialFiles(data=HF2018_maple.raw.data, stat="mean")
  HF2019_maple.mean <- aggregateRadialFiles(data=HF2019_maple.raw.data, stat="mean")
  
  HF2017_oak.mean <- aggregateRadialFiles(data=HF2017_oak.raw.data, stat="mean")
  HF2018_oak.mean <- aggregateRadialFiles(data=HF2018_oak.raw.data, stat="mean")
  HF2019_oak.mean <- aggregateRadialFiles(data=HF2019_oak.raw.data, stat="mean")
  
  ## Standardising averaged data
  HF2017_pine_mean.sdd <- standardiseCellCounts(HF2017_pine.mean)
  HF2018_pine_mean.sdd <- standardiseCellCounts(HF2018_pine.mean)
  HF2019_pine_mean.sdd <- standardiseCellCounts(HF2019_pine.mean)
  
  HF2017_maple_mean.sdd <- standardiseCellCounts(HF2017_maple.mean)
  HF2018_maple_mean.sdd <- standardiseCellCounts(HF2018_maple.mean)
  HF2019_maple_mean.sdd <- standardiseCellCounts(HF2019_maple.mean)
  
  HF2017_oak_mean.sdd <- standardiseCellCounts(HF2017_oak.mean)
  HF2018_oak_mean.sdd <- standardiseCellCounts(HF2018_oak.mean)
  HF2019_oak_mean.sdd <- standardiseCellCounts(HF2019_oak.mean)
  
  #ring width calculated from the past year
  RW2016_maple <- mean(HF2017_maple.raw.data1$PR,na.rm = T)
  RW2017_maple <- mean(HF2018_maple.raw.data$PR,na.rm = T)
  RW2018_maple <- mean(HF2019_maple.raw.data$PR,na.rm = T)
  
  RW2016_oak <- mean(HF2017_oak.raw.data$PR,na.rm = T)
  RW2017_oak <- mean(HF2018_oak.raw.data$PR,na.rm = T)
  RW2018_oak <- mean(HF2019_oak.raw.data$PR,na.rm = T)
  
  RW2016_pine <- mean(HF2017_pine.raw.data$PR,na.rm = T)
  RW2017_pine <- mean(HF2018_pine.raw.data$PR,na.rm = T)
  RW2018_pine <- mean(HF2019_pine.raw.data$PR,na.rm = T)
  
  #check the completeness of the data
  library("VIM")
  {
    matrixplot(HF2017_maple_mean.sdd)
  }
  
  #fill the data using mice
  library("mice")
  {
    HF2017_pine_mean.sdd_filled = mice(HF2017_pine_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2018_pine_mean.sdd_filled = mice(HF2018_pine_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2019_pine_mean.sdd_filled = mice(HF2019_pine_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    
    HF2017_oak_mean.sdd_filled = mice(HF2017_oak_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2018_oak_mean.sdd_filled = mice(HF2018_oak_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2019_oak_mean.sdd_filled = mice(HF2019_oak_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    
    HF2017_maple_mean.sdd_filled = mice(HF2017_maple_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2018_maple_mean.sdd_filled = mice(HF2018_maple_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    HF2019_maple_mean.sdd_filled = mice(HF2019_maple_mean.sdd,m=5,method="pmm",maxit = 100,seed=1)
    
    HF2017_pine_mean.sdd_filled1 = complete(HF2017_pine_mean.sdd_filled)
    HF2018_pine_mean.sdd_filled1 = complete(HF2018_pine_mean.sdd_filled)
    HF2019_pine_mean.sdd_filled1 = complete(HF2019_pine_mean.sdd_filled)
    HF2017_maple_mean.sdd_filled1 = complete(HF2017_maple_mean.sdd_filled)
    HF2018_maple_mean.sdd_filled1 = complete(HF2018_maple_mean.sdd_filled)
    HF2019_maple_mean.sdd_filled1 = complete(HF2019_maple_mean.sdd_filled)
    HF2017_oak_mean.sdd_filled1 = complete(HF2017_oak_mean.sdd_filled)
    HF2018_oak_mean.sdd_filled1 = complete(HF2018_oak_mean.sdd_filled)
    HF2019_oak_mean.sdd_filled1 = complete(HF2019_oak_mean.sdd_filled)
  }
  
  #recheck the completeness of the data
  {
    matrixplot(HF2017_maple_mean.sdd_filled1)
    matrixplot(HF2018_maple_mean.sdd_filled1)
    matrixplot(HF2019_maple_mean.sdd_filled1)
    matrixplot(HF2017_oak_mean.sdd_filled1)
    matrixplot(HF2018_oak_mean.sdd_filled1)
    matrixplot(HF2019_oak_mean.sdd_filled1)
    matrixplot(HF2017_pine_mean.sdd_filled1)
    matrixplot(HF2018_pine_mean.sdd_filled1)
    matrixplot(HF2019_pine_mean.sdd_filled1)
  }
  
  #recalculate EWMZ
  {
    HF2017_pine_mean.sdd_filled1$EWMZ = HF2017_pine_mean.sdd_filled1$EZ+HF2017_pine_mean.sdd_filled1$WZ+HF2017_pine_mean.sdd_filled1$MZ
    HF2018_pine_mean.sdd_filled1$EWMZ = HF2018_pine_mean.sdd_filled1$EZ+HF2018_pine_mean.sdd_filled1$WZ+HF2018_pine_mean.sdd_filled1$MZ
    HF2019_pine_mean.sdd_filled1$EWMZ = HF2019_pine_mean.sdd_filled1$EZ+HF2019_pine_mean.sdd_filled1$WZ+HF2019_pine_mean.sdd_filled1$MZ
    
    HF2017_maple_mean.sdd_filled1$EWMZ = HF2017_maple_mean.sdd_filled1$EZ+HF2017_maple_mean.sdd_filled1$WZ+HF2017_maple_mean.sdd_filled1$MZ
    HF2018_maple_mean.sdd_filled1$EWMZ = HF2018_maple_mean.sdd_filled1$EZ+HF2018_maple_mean.sdd_filled1$WZ+HF2018_maple_mean.sdd_filled1$MZ
    HF2019_maple_mean.sdd_filled1$EWMZ = HF2019_maple_mean.sdd_filled1$EZ+HF2019_maple_mean.sdd_filled1$WZ+HF2019_maple_mean.sdd_filled1$MZ
    
    HF2017_oak_mean.sdd_filled1$EWMZ = HF2017_oak_mean.sdd_filled1$EZ+HF2017_oak_mean.sdd_filled1$WZ+HF2017_oak_mean.sdd_filled1$MZ
    HF2018_oak_mean.sdd_filled1$EWMZ = HF2018_oak_mean.sdd_filled1$EZ+HF2018_oak_mean.sdd_filled1$WZ+HF2018_oak_mean.sdd_filled1$MZ
    HF2019_oak_mean.sdd_filled1$EWMZ = HF2019_oak_mean.sdd_filled1$EZ+HF2019_oak_mean.sdd_filled1$WZ+HF2019_oak_mean.sdd_filled1$MZ
    
  }
  
  #3-year data cut to the same end date with 2019
  {
    #PIST
    {
      HF2017_pine_mean.sdd_filled1_P16 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P16" & as.numeric(DY) <= 289)
      HF2017_pine_mean.sdd_filled1_P20 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P20" & as.numeric(DY) <= 289)
      HF2017_pine_mean.sdd_filled1_P21 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P21" & as.numeric(DY) <= 289)
      HF2017_pine_mean.sdd_filled1_P25 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P25" & as.numeric(DY) <= 289)
      HF2017_pine_mean.sdd_filled1_P26 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P26" & as.numeric(DY) <= 289)
      HF2017_pine_mean.sdd_filled1_P7 = subset(HF2017_pine_mean.sdd_filled1,Tree == "P7" & as.numeric(DY) <= 289)
      
      HF2018_pine_mean.sdd_filled1_P16 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P16" & as.numeric(DY) <= 289)
      HF2018_pine_mean.sdd_filled1_P20 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P20" & as.numeric(DY) <= 289)
      HF2018_pine_mean.sdd_filled1_P21 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P21" & as.numeric(DY) <= 289)
      HF2018_pine_mean.sdd_filled1_P25 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P25" & as.numeric(DY) <= 289)
      HF2018_pine_mean.sdd_filled1_P26 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P26" & as.numeric(DY) <= 289)
      HF2018_pine_mean.sdd_filled1_P7 = subset(HF2018_pine_mean.sdd_filled1,Tree == "P7" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P16 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P16" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P20 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P20" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P21 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P21" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P25 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P25" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P26 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P26" & as.numeric(DY) <= 289)
      HF2019_pine_mean.sdd_filled1_P7 = subset(HF2019_pine_mean.sdd_filled1,Tree == "P7" & as.numeric(DY) <= 289)
    }
    #ACRU
    {
      HF2017_maple_mean.sdd_filled1_A18 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A18" & as.numeric(DY) <= 240)
      HF2017_maple_mean.sdd_filled1_A19 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A19" & as.numeric(DY) <= 240)
      HF2017_maple_mean.sdd_filled1_A23 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A23" & as.numeric(DY) <= 240)
      HF2017_maple_mean.sdd_filled1_A28 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A28" & as.numeric(DY) <= 240)
      HF2017_maple_mean.sdd_filled1_A29 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A29" & as.numeric(DY) <= 240)
      HF2017_maple_mean.sdd_filled1_A8 = subset(HF2017_maple_mean.sdd_filled1,Tree == "A8" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A18 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A18" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A19 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A19" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A23 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A23" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A28 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A28" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A29 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A29" & as.numeric(DY) <= 240)
      HF2018_maple_mean.sdd_filled1_A8 = subset(HF2018_maple_mean.sdd_filled1,Tree == "A8" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A18 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A18" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A19 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A19" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A23 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A23" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A28 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A28" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A29 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A29" & as.numeric(DY) <= 240)
      HF2019_maple_mean.sdd_filled1_A8 = subset(HF2019_maple_mean.sdd_filled1,Tree == "A8" & as.numeric(DY) <= 240)
    }
    #QURU
    {
      HF2017_oak_mean.sdd_filled1_Q05 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q05" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q06 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q06" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q09 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q09" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q10 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q10" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q11 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q11" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q12 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q12" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q13 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q13" & as.numeric(DY) <= 269)
      HF2017_oak_mean.sdd_filled1_Q15 = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q15" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q05 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q05" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q06 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q06" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q09 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q09" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q10 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q10" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q11 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q11" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q12 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q12" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q13 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q13" & as.numeric(DY) <= 269)
      HF2018_oak_mean.sdd_filled1_Q15 = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q15" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q05 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q05" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q06 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q06" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q09 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q09" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q10 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q10" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q11 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q11" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q12 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q12" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q13 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q13" & as.numeric(DY) <= 269)
      HF2019_oak_mean.sdd_filled1_Q15 = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q15" & as.numeric(DY) <= 269)
    }
  }
  
  #no cut
  #_com
  {
    #PIST
    {
      HF2017_pine_mean.sdd_filled1_P16_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P16")
      HF2017_pine_mean.sdd_filled1_P20_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P20")
      HF2017_pine_mean.sdd_filled1_P21_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P21")
      HF2017_pine_mean.sdd_filled1_P25_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P25")
      HF2017_pine_mean.sdd_filled1_P26_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P26")
      HF2017_pine_mean.sdd_filled1_P7_com = subset(HF2017_pine_mean.sdd_filled1,Tree == "P7")
      
      HF2018_pine_mean.sdd_filled1_P16_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P16")
      HF2018_pine_mean.sdd_filled1_P20_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P20")
      HF2018_pine_mean.sdd_filled1_P21_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P21")
      HF2018_pine_mean.sdd_filled1_P25_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P25")
      HF2018_pine_mean.sdd_filled1_P26_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P26")
      HF2018_pine_mean.sdd_filled1_P7_com = subset(HF2018_pine_mean.sdd_filled1,Tree == "P7")
      
      HF2019_pine_mean.sdd_filled1_P16_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P16")
      HF2019_pine_mean.sdd_filled1_P20_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P20")
      HF2019_pine_mean.sdd_filled1_P21_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P21")
      HF2019_pine_mean.sdd_filled1_P25_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P25")
      HF2019_pine_mean.sdd_filled1_P26_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P26")
      HF2019_pine_mean.sdd_filled1_P7_com = subset(HF2019_pine_mean.sdd_filled1,Tree == "P7")
    }
    #ACRU
    {
      HF2017_maple_mean.sdd_filled1_A18_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A18")
      HF2017_maple_mean.sdd_filled1_A19_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A19")
      HF2017_maple_mean.sdd_filled1_A23_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A23")
      HF2017_maple_mean.sdd_filled1_A28_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A28")
      HF2017_maple_mean.sdd_filled1_A29_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A29")
      HF2017_maple_mean.sdd_filled1_A8_com = subset(HF2017_maple_mean.sdd_filled1,Tree == "A8")
      HF2018_maple_mean.sdd_filled1_A18_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A18")
      HF2018_maple_mean.sdd_filled1_A19_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A19")
      HF2018_maple_mean.sdd_filled1_A23_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A23")
      HF2018_maple_mean.sdd_filled1_A28_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A28")
      HF2018_maple_mean.sdd_filled1_A29_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A29")
      HF2018_maple_mean.sdd_filled1_A8_com = subset(HF2018_maple_mean.sdd_filled1,Tree == "A8")
      HF2019_maple_mean.sdd_filled1_A18_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A18")
      HF2019_maple_mean.sdd_filled1_A19_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A19")
      HF2019_maple_mean.sdd_filled1_A23_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A23")
      HF2019_maple_mean.sdd_filled1_A28_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A28")
      HF2019_maple_mean.sdd_filled1_A29_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A29")
      HF2019_maple_mean.sdd_filled1_A8_com = subset(HF2019_maple_mean.sdd_filled1,Tree == "A8")
    }
    #QURU
    {
      HF2017_oak_mean.sdd_filled1_Q05_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q05" )
      HF2017_oak_mean.sdd_filled1_Q06_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q06" )
      HF2017_oak_mean.sdd_filled1_Q09_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q09" )
      HF2017_oak_mean.sdd_filled1_Q10_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q10" )
      HF2017_oak_mean.sdd_filled1_Q11_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q11" )
      HF2017_oak_mean.sdd_filled1_Q12_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q12" )
      HF2017_oak_mean.sdd_filled1_Q13_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q13" )
      HF2017_oak_mean.sdd_filled1_Q15_com = subset(HF2017_oak_mean.sdd_filled1,Tree == "Q15" )
      HF2018_oak_mean.sdd_filled1_Q05_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q05" )
      HF2018_oak_mean.sdd_filled1_Q06_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q06" )
      HF2018_oak_mean.sdd_filled1_Q09_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q09" )
      HF2018_oak_mean.sdd_filled1_Q10_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q10" )
      HF2018_oak_mean.sdd_filled1_Q11_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q11" )
      HF2018_oak_mean.sdd_filled1_Q12_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q12" )
      HF2018_oak_mean.sdd_filled1_Q13_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q13" )
      HF2018_oak_mean.sdd_filled1_Q15_com = subset(HF2018_oak_mean.sdd_filled1,Tree == "Q15" )
      HF2019_oak_mean.sdd_filled1_Q05_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q05" )
      HF2019_oak_mean.sdd_filled1_Q06_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q06" )
      HF2019_oak_mean.sdd_filled1_Q09_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q09" )
      HF2019_oak_mean.sdd_filled1_Q10_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q10" )
      HF2019_oak_mean.sdd_filled1_Q11_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q11" )
      HF2019_oak_mean.sdd_filled1_Q12_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q12" )
      HF2019_oak_mean.sdd_filled1_Q13_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q13" )
      HF2019_oak_mean.sdd_filled1_Q15_com = subset(HF2019_oak_mean.sdd_filled1,Tree == "Q15" )
    }
  }
  
  #gam for each individual tree
  library("mgcv")
  #cut
  {
    #PIST
    {
      #2017
      #CZ
      {
        gam_pine_2017_CZ_P16 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16)
        gam_pine_2017_CZ_P20 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20)
        gam_pine_2017_CZ_P21 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21)
        gam_pine_2017_CZ_P25 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25)
        gam_pine_2017_CZ_P26 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26)
        gam_pine_2017_CZ_P7 <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7)
        HF2017_pine_mean.sdd_filled1_P16$gam_CZ = gam_pine_2017_CZ_P16$fitted.values
        HF2017_pine_mean.sdd_filled1_P20$gam_CZ = gam_pine_2017_CZ_P20$fitted.values
        HF2017_pine_mean.sdd_filled1_P21$gam_CZ = gam_pine_2017_CZ_P21$fitted.values
        HF2017_pine_mean.sdd_filled1_P25$gam_CZ = gam_pine_2017_CZ_P25$fitted.values
        HF2017_pine_mean.sdd_filled1_P26$gam_CZ = gam_pine_2017_CZ_P26$fitted.values
        HF2017_pine_mean.sdd_filled1_P7$gam_CZ = gam_pine_2017_CZ_P7$fitted.values
      }
      #EZ
      {
        gam_pine_2017_EZ_P16 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16)
        gam_pine_2017_EZ_P20 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20)
        gam_pine_2017_EZ_P21 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21)
        gam_pine_2017_EZ_P25 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25)
        gam_pine_2017_EZ_P26 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26)
        gam_pine_2017_EZ_P7 <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7)
        HF2017_pine_mean.sdd_filled1_P16$gam_EZ = gam_pine_2017_EZ_P16$fitted.values
        HF2017_pine_mean.sdd_filled1_P20$gam_EZ = gam_pine_2017_EZ_P20$fitted.values
        HF2017_pine_mean.sdd_filled1_P21$gam_EZ = gam_pine_2017_EZ_P21$fitted.values
        HF2017_pine_mean.sdd_filled1_P25$gam_EZ = gam_pine_2017_EZ_P25$fitted.values
        HF2017_pine_mean.sdd_filled1_P26$gam_EZ = gam_pine_2017_EZ_P26$fitted.values
        HF2017_pine_mean.sdd_filled1_P7$gam_EZ = gam_pine_2017_EZ_P7$fitted.values
        
        HF2017_pine_mean.sdd_filled1_P16[HF2017_pine_mean.sdd_filled1_P16$gam_EZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P20[HF2017_pine_mean.sdd_filled1_P20$gam_EZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P21[HF2017_pine_mean.sdd_filled1_P21$gam_EZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P25[HF2017_pine_mean.sdd_filled1_P25$gam_EZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P26[HF2017_pine_mean.sdd_filled1_P26$gam_EZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P7[HF2017_pine_mean.sdd_filled1_P7$gam_EZ <0,] = 0
        
      }
      #WZ
      {
        gam_pine_2017_WZ_P16 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16)
        gam_pine_2017_WZ_P20 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20)
        gam_pine_2017_WZ_P21 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21)
        gam_pine_2017_WZ_P25 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25)
        gam_pine_2017_WZ_P26 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26)
        gam_pine_2017_WZ_P7 <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7)
        HF2017_pine_mean.sdd_filled1_P16$gam_WZ = gam_pine_2017_WZ_P16$fitted.values
        HF2017_pine_mean.sdd_filled1_P20$gam_WZ = gam_pine_2017_WZ_P20$fitted.values
        HF2017_pine_mean.sdd_filled1_P21$gam_WZ = gam_pine_2017_WZ_P21$fitted.values
        HF2017_pine_mean.sdd_filled1_P25$gam_WZ = gam_pine_2017_WZ_P25$fitted.values
        HF2017_pine_mean.sdd_filled1_P26$gam_WZ = gam_pine_2017_WZ_P26$fitted.values
        HF2017_pine_mean.sdd_filled1_P7$gam_WZ = gam_pine_2017_WZ_P7$fitted.values
        
        HF2017_pine_mean.sdd_filled1_P16[HF2017_pine_mean.sdd_filled1_P16$gam_WZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P20[HF2017_pine_mean.sdd_filled1_P20$gam_WZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P21[HF2017_pine_mean.sdd_filled1_P21$gam_WZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P25[HF2017_pine_mean.sdd_filled1_P25$gam_WZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P26[HF2017_pine_mean.sdd_filled1_P26$gam_WZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P7[HF2017_pine_mean.sdd_filled1_P7$gam_WZ <0,] = 0
      }
      #MZ
      {
        gam_pine_2017_MZ_P16 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16)
        gam_pine_2017_MZ_P20 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20)
        gam_pine_2017_MZ_P21 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21)
        gam_pine_2017_MZ_P25 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25)
        gam_pine_2017_MZ_P26 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26)
        gam_pine_2017_MZ_P7 <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7)
        HF2017_pine_mean.sdd_filled1_P16$gam_MZ = gam_pine_2017_MZ_P16$fitted.values
        HF2017_pine_mean.sdd_filled1_P20$gam_MZ = gam_pine_2017_MZ_P20$fitted.values
        HF2017_pine_mean.sdd_filled1_P21$gam_MZ = gam_pine_2017_MZ_P21$fitted.values
        HF2017_pine_mean.sdd_filled1_P25$gam_MZ = gam_pine_2017_MZ_P25$fitted.values
        HF2017_pine_mean.sdd_filled1_P26$gam_MZ = gam_pine_2017_MZ_P26$fitted.values
        HF2017_pine_mean.sdd_filled1_P7$gam_MZ = gam_pine_2017_MZ_P7$fitted.values
        
        HF2017_pine_mean.sdd_filled1_P16[HF2017_pine_mean.sdd_filled1_P16$gam_MZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P20[HF2017_pine_mean.sdd_filled1_P20$gam_MZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P21[HF2017_pine_mean.sdd_filled1_P21$gam_MZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P25[HF2017_pine_mean.sdd_filled1_P25$gam_MZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P26[HF2017_pine_mean.sdd_filled1_P26$gam_MZ <0,] = 0
        HF2017_pine_mean.sdd_filled1_P7[HF2017_pine_mean.sdd_filled1_P7$gam_MZ <0,] = 0
      }
      #WMZ
      {
        HF2017_pine_mean.sdd_filled1_P16$gam_WMZ = HF2017_pine_mean.sdd_filled1_P16$gam_WZ+HF2017_pine_mean.sdd_filled1_P16$gam_MZ
        HF2017_pine_mean.sdd_filled1_P20$gam_WMZ = HF2017_pine_mean.sdd_filled1_P20$gam_WZ+HF2017_pine_mean.sdd_filled1_P20$gam_MZ
        HF2017_pine_mean.sdd_filled1_P21$gam_WMZ = HF2017_pine_mean.sdd_filled1_P21$gam_WZ+HF2017_pine_mean.sdd_filled1_P21$gam_MZ
        HF2017_pine_mean.sdd_filled1_P25$gam_WMZ = HF2017_pine_mean.sdd_filled1_P25$gam_WZ+HF2017_pine_mean.sdd_filled1_P25$gam_MZ
        HF2017_pine_mean.sdd_filled1_P26$gam_WMZ = HF2017_pine_mean.sdd_filled1_P26$gam_WZ+HF2017_pine_mean.sdd_filled1_P26$gam_MZ
        HF2017_pine_mean.sdd_filled1_P7$gam_WMZ = HF2017_pine_mean.sdd_filled1_P7$gam_WZ+HF2017_pine_mean.sdd_filled1_P7$gam_MZ
      }
      #EWMZ
      {
        HF2017_pine_mean.sdd_filled1_P16$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P16$gam_EZ+HF2017_pine_mean.sdd_filled1_P16$gam_WZ+HF2017_pine_mean.sdd_filled1_P16$gam_MZ
        HF2017_pine_mean.sdd_filled1_P20$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P20$gam_EZ+HF2017_pine_mean.sdd_filled1_P20$gam_WZ+HF2017_pine_mean.sdd_filled1_P20$gam_MZ
        HF2017_pine_mean.sdd_filled1_P21$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P21$gam_EZ+HF2017_pine_mean.sdd_filled1_P21$gam_WZ+HF2017_pine_mean.sdd_filled1_P21$gam_MZ
        HF2017_pine_mean.sdd_filled1_P25$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P25$gam_EZ+HF2017_pine_mean.sdd_filled1_P25$gam_WZ+HF2017_pine_mean.sdd_filled1_P25$gam_MZ
        HF2017_pine_mean.sdd_filled1_P26$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P26$gam_EZ+HF2017_pine_mean.sdd_filled1_P26$gam_WZ+HF2017_pine_mean.sdd_filled1_P26$gam_MZ
        HF2017_pine_mean.sdd_filled1_P7$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P7$gam_EZ+HF2017_pine_mean.sdd_filled1_P7$gam_WZ+HF2017_pine_mean.sdd_filled1_P7$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_pine_2018_CZ_P16 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16)
        gam_pine_2018_CZ_P20 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20)
        gam_pine_2018_CZ_P21 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21)
        gam_pine_2018_CZ_P25 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25)
        gam_pine_2018_CZ_P26 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26)
        gam_pine_2018_CZ_P7 <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7)
        HF2018_pine_mean.sdd_filled1_P16$gam_CZ = gam_pine_2018_CZ_P16$fitted.values
        HF2018_pine_mean.sdd_filled1_P20$gam_CZ = gam_pine_2018_CZ_P20$fitted.values
        HF2018_pine_mean.sdd_filled1_P21$gam_CZ = gam_pine_2018_CZ_P21$fitted.values
        HF2018_pine_mean.sdd_filled1_P25$gam_CZ = gam_pine_2018_CZ_P25$fitted.values
        HF2018_pine_mean.sdd_filled1_P26$gam_CZ = gam_pine_2018_CZ_P26$fitted.values
        HF2018_pine_mean.sdd_filled1_P7$gam_CZ = gam_pine_2018_CZ_P7$fitted.values
      }
      #EZ
      {
        gam_pine_2018_EZ_P16 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16)
        gam_pine_2018_EZ_P20 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20)
        gam_pine_2018_EZ_P21 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21)
        gam_pine_2018_EZ_P25 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25)
        gam_pine_2018_EZ_P26 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26)
        gam_pine_2018_EZ_P7 <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7)
        HF2018_pine_mean.sdd_filled1_P16$gam_EZ = gam_pine_2018_EZ_P16$fitted.values
        HF2018_pine_mean.sdd_filled1_P20$gam_EZ = gam_pine_2018_EZ_P20$fitted.values
        HF2018_pine_mean.sdd_filled1_P21$gam_EZ = gam_pine_2018_EZ_P21$fitted.values
        HF2018_pine_mean.sdd_filled1_P25$gam_EZ = gam_pine_2018_EZ_P25$fitted.values
        HF2018_pine_mean.sdd_filled1_P26$gam_EZ = gam_pine_2018_EZ_P26$fitted.values
        HF2018_pine_mean.sdd_filled1_P7$gam_EZ = gam_pine_2018_EZ_P7$fitted.values
      }
      #WZ
      {
        gam_pine_2018_WZ_P16 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16)
        gam_pine_2018_WZ_P20 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20)
        gam_pine_2018_WZ_P21 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21)
        gam_pine_2018_WZ_P25 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25)
        gam_pine_2018_WZ_P26 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26)
        gam_pine_2018_WZ_P7 <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7)
        HF2018_pine_mean.sdd_filled1_P16$gam_WZ = gam_pine_2018_WZ_P16$fitted.values
        HF2018_pine_mean.sdd_filled1_P20$gam_WZ = gam_pine_2018_WZ_P20$fitted.values
        HF2018_pine_mean.sdd_filled1_P21$gam_WZ = gam_pine_2018_WZ_P21$fitted.values
        HF2018_pine_mean.sdd_filled1_P25$gam_WZ = gam_pine_2018_WZ_P25$fitted.values
        HF2018_pine_mean.sdd_filled1_P26$gam_WZ = gam_pine_2018_WZ_P26$fitted.values
        HF2018_pine_mean.sdd_filled1_P7$gam_WZ = gam_pine_2018_WZ_P7$fitted.values
      }
      #MZ
      {
        gam_pine_2018_MZ_P16 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16)
        gam_pine_2018_MZ_P20 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20)
        gam_pine_2018_MZ_P21 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21)
        gam_pine_2018_MZ_P25 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25)
        gam_pine_2018_MZ_P26 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26)
        gam_pine_2018_MZ_P7 <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7)
        HF2018_pine_mean.sdd_filled1_P16$gam_MZ = gam_pine_2018_MZ_P16$fitted.values
        HF2018_pine_mean.sdd_filled1_P20$gam_MZ = gam_pine_2018_MZ_P20$fitted.values
        HF2018_pine_mean.sdd_filled1_P21$gam_MZ = gam_pine_2018_MZ_P21$fitted.values
        HF2018_pine_mean.sdd_filled1_P25$gam_MZ = gam_pine_2018_MZ_P25$fitted.values
        HF2018_pine_mean.sdd_filled1_P26$gam_MZ = gam_pine_2018_MZ_P26$fitted.values
        HF2018_pine_mean.sdd_filled1_P7$gam_MZ = gam_pine_2018_MZ_P7$fitted.values
      }
      #WMZ
      {
        
        HF2018_pine_mean.sdd_filled1_P16$gam_WMZ = HF2018_pine_mean.sdd_filled1_P16$gam_WZ+HF2018_pine_mean.sdd_filled1_P16$gam_MZ
        HF2018_pine_mean.sdd_filled1_P20$gam_WMZ = HF2018_pine_mean.sdd_filled1_P20$gam_WZ+HF2018_pine_mean.sdd_filled1_P20$gam_MZ
        HF2018_pine_mean.sdd_filled1_P21$gam_WMZ = HF2018_pine_mean.sdd_filled1_P21$gam_WZ+HF2018_pine_mean.sdd_filled1_P21$gam_MZ
        HF2018_pine_mean.sdd_filled1_P25$gam_WMZ = HF2018_pine_mean.sdd_filled1_P25$gam_WZ+HF2018_pine_mean.sdd_filled1_P25$gam_MZ
        HF2018_pine_mean.sdd_filled1_P26$gam_WMZ = HF2018_pine_mean.sdd_filled1_P26$gam_WZ+HF2018_pine_mean.sdd_filled1_P26$gam_MZ
        HF2018_pine_mean.sdd_filled1_P7$gam_WMZ = HF2018_pine_mean.sdd_filled1_P7$gam_WZ+HF2018_pine_mean.sdd_filled1_P7$gam_MZ
      }
      #EWMZ
      {
        HF2018_pine_mean.sdd_filled1_P16$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P16$gam_EZ+HF2018_pine_mean.sdd_filled1_P16$gam_WZ+HF2018_pine_mean.sdd_filled1_P16$gam_MZ
        HF2018_pine_mean.sdd_filled1_P20$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P20$gam_EZ+HF2018_pine_mean.sdd_filled1_P20$gam_WZ+HF2018_pine_mean.sdd_filled1_P20$gam_MZ
        HF2018_pine_mean.sdd_filled1_P21$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P21$gam_EZ+HF2018_pine_mean.sdd_filled1_P21$gam_WZ+HF2018_pine_mean.sdd_filled1_P21$gam_MZ
        HF2018_pine_mean.sdd_filled1_P25$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P25$gam_EZ+HF2018_pine_mean.sdd_filled1_P25$gam_WZ+HF2018_pine_mean.sdd_filled1_P25$gam_MZ
        HF2018_pine_mean.sdd_filled1_P26$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P26$gam_EZ+HF2018_pine_mean.sdd_filled1_P26$gam_WZ+HF2018_pine_mean.sdd_filled1_P26$gam_MZ
        HF2018_pine_mean.sdd_filled1_P7$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P7$gam_EZ+HF2018_pine_mean.sdd_filled1_P7$gam_WZ+HF2018_pine_mean.sdd_filled1_P7$gam_MZ
      }
      
      #2019
      #CZ
      {
        gam_pine_2019_CZ_P16 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16)
        gam_pine_2019_CZ_P20 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20)
        gam_pine_2019_CZ_P21 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21)
        gam_pine_2019_CZ_P25 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25)
        gam_pine_2019_CZ_P26 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26)
        gam_pine_2019_CZ_P7 <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7)
        HF2019_pine_mean.sdd_filled1_P16$gam_CZ = gam_pine_2019_CZ_P16$fitted.values
        HF2019_pine_mean.sdd_filled1_P20$gam_CZ = gam_pine_2019_CZ_P20$fitted.values
        HF2019_pine_mean.sdd_filled1_P21$gam_CZ = gam_pine_2019_CZ_P21$fitted.values
        HF2019_pine_mean.sdd_filled1_P25$gam_CZ = gam_pine_2019_CZ_P25$fitted.values
        HF2019_pine_mean.sdd_filled1_P26$gam_CZ = gam_pine_2019_CZ_P26$fitted.values
        HF2019_pine_mean.sdd_filled1_P7$gam_CZ = gam_pine_2019_CZ_P7$fitted.values
      }
      #EZ
      {
        gam_pine_2019_EZ_P16 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16)
        gam_pine_2019_EZ_P20 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20)
        gam_pine_2019_EZ_P21 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21)
        gam_pine_2019_EZ_P25 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25)
        gam_pine_2019_EZ_P26 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26)
        gam_pine_2019_EZ_P7 <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7)
        HF2019_pine_mean.sdd_filled1_P16$gam_EZ = gam_pine_2019_EZ_P16$fitted.values
        HF2019_pine_mean.sdd_filled1_P20$gam_EZ = gam_pine_2019_EZ_P20$fitted.values
        HF2019_pine_mean.sdd_filled1_P21$gam_EZ = gam_pine_2019_EZ_P21$fitted.values
        HF2019_pine_mean.sdd_filled1_P25$gam_EZ = gam_pine_2019_EZ_P25$fitted.values
        HF2019_pine_mean.sdd_filled1_P26$gam_EZ = gam_pine_2019_EZ_P26$fitted.values
        HF2019_pine_mean.sdd_filled1_P7$gam_EZ = gam_pine_2019_EZ_P7$fitted.values
      }
      #WZ
      {
        gam_pine_2019_WZ_P16 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16)
        gam_pine_2019_WZ_P20 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20)
        gam_pine_2019_WZ_P21 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21)
        gam_pine_2019_WZ_P25 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25)
        gam_pine_2019_WZ_P26 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26)
        gam_pine_2019_WZ_P7 <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7)
        HF2019_pine_mean.sdd_filled1_P16$gam_WZ = gam_pine_2019_WZ_P16$fitted.values
        HF2019_pine_mean.sdd_filled1_P20$gam_WZ = gam_pine_2019_WZ_P20$fitted.values
        HF2019_pine_mean.sdd_filled1_P21$gam_WZ = gam_pine_2019_WZ_P21$fitted.values
        HF2019_pine_mean.sdd_filled1_P25$gam_WZ = gam_pine_2019_WZ_P25$fitted.values
        HF2019_pine_mean.sdd_filled1_P26$gam_WZ = gam_pine_2019_WZ_P26$fitted.values
        HF2019_pine_mean.sdd_filled1_P7$gam_WZ = gam_pine_2019_WZ_P7$fitted.values
      }
      #MZ
      {
        gam_pine_2019_MZ_P16 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16)
        gam_pine_2019_MZ_P20 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20)
        gam_pine_2019_MZ_P21 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21)
        gam_pine_2019_MZ_P25 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25)
        gam_pine_2019_MZ_P26 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26)
        gam_pine_2019_MZ_P7 <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7)
        HF2019_pine_mean.sdd_filled1_P16$gam_MZ = gam_pine_2019_MZ_P16$fitted.values
        HF2019_pine_mean.sdd_filled1_P20$gam_MZ = gam_pine_2019_MZ_P20$fitted.values
        HF2019_pine_mean.sdd_filled1_P21$gam_MZ = gam_pine_2019_MZ_P21$fitted.values
        HF2019_pine_mean.sdd_filled1_P25$gam_MZ = gam_pine_2019_MZ_P25$fitted.values
        HF2019_pine_mean.sdd_filled1_P26$gam_MZ = gam_pine_2019_MZ_P26$fitted.values
        HF2019_pine_mean.sdd_filled1_P7$gam_MZ = gam_pine_2019_MZ_P7$fitted.values
      }
      #WMZ
      {
        
        HF2019_pine_mean.sdd_filled1_P16$gam_WMZ = HF2019_pine_mean.sdd_filled1_P16$gam_WZ+HF2019_pine_mean.sdd_filled1_P16$gam_MZ
        HF2019_pine_mean.sdd_filled1_P20$gam_WMZ = HF2019_pine_mean.sdd_filled1_P20$gam_WZ+HF2019_pine_mean.sdd_filled1_P20$gam_MZ
        HF2019_pine_mean.sdd_filled1_P21$gam_WMZ = HF2019_pine_mean.sdd_filled1_P21$gam_WZ+HF2019_pine_mean.sdd_filled1_P21$gam_MZ
        HF2019_pine_mean.sdd_filled1_P25$gam_WMZ = HF2019_pine_mean.sdd_filled1_P25$gam_WZ+HF2019_pine_mean.sdd_filled1_P25$gam_MZ
        HF2019_pine_mean.sdd_filled1_P26$gam_WMZ = HF2019_pine_mean.sdd_filled1_P26$gam_WZ+HF2019_pine_mean.sdd_filled1_P26$gam_MZ
        HF2019_pine_mean.sdd_filled1_P7$gam_WMZ = HF2019_pine_mean.sdd_filled1_P7$gam_WZ+HF2019_pine_mean.sdd_filled1_P7$gam_MZ
      }
      #EWMZ
      {
        HF2019_pine_mean.sdd_filled1_P16$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P16$gam_EZ+HF2019_pine_mean.sdd_filled1_P16$gam_WZ+HF2019_pine_mean.sdd_filled1_P16$gam_MZ
        HF2019_pine_mean.sdd_filled1_P20$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P20$gam_EZ+HF2019_pine_mean.sdd_filled1_P20$gam_WZ+HF2019_pine_mean.sdd_filled1_P20$gam_MZ
        HF2019_pine_mean.sdd_filled1_P21$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P21$gam_EZ+HF2019_pine_mean.sdd_filled1_P21$gam_WZ+HF2019_pine_mean.sdd_filled1_P21$gam_MZ
        HF2019_pine_mean.sdd_filled1_P25$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P25$gam_EZ+HF2019_pine_mean.sdd_filled1_P25$gam_WZ+HF2019_pine_mean.sdd_filled1_P25$gam_MZ
        HF2019_pine_mean.sdd_filled1_P26$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P26$gam_EZ+HF2019_pine_mean.sdd_filled1_P26$gam_WZ+HF2019_pine_mean.sdd_filled1_P26$gam_MZ
        HF2019_pine_mean.sdd_filled1_P7$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P7$gam_EZ+HF2019_pine_mean.sdd_filled1_P7$gam_WZ+HF2019_pine_mean.sdd_filled1_P7$gam_MZ
      }
      
      #combine the tree dfs to one for each year
      {
        HF2017_pine_mean.sdd_filled2 = rbind(HF2017_pine_mean.sdd_filled1_P16,HF2017_pine_mean.sdd_filled1_P20,HF2017_pine_mean.sdd_filled1_P21,HF2017_pine_mean.sdd_filled1_P25,HF2017_pine_mean.sdd_filled1_P26,HF2017_pine_mean.sdd_filled1_P7)
        HF2018_pine_mean.sdd_filled2 = rbind(HF2018_pine_mean.sdd_filled1_P16,HF2018_pine_mean.sdd_filled1_P20,HF2018_pine_mean.sdd_filled1_P21,HF2018_pine_mean.sdd_filled1_P25,HF2018_pine_mean.sdd_filled1_P26,HF2018_pine_mean.sdd_filled1_P7)
        HF2019_pine_mean.sdd_filled2 = rbind(HF2019_pine_mean.sdd_filled1_P16,HF2019_pine_mean.sdd_filled1_P20,HF2019_pine_mean.sdd_filled1_P21,HF2019_pine_mean.sdd_filled1_P25,HF2019_pine_mean.sdd_filled1_P26,HF2019_pine_mean.sdd_filled1_P7)
      }
    }
    #ACRU
    {
      #2017
      #CZ
      {
        gam_maple_2017_CZ_A18 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18)
        gam_maple_2017_CZ_A19 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19)
        gam_maple_2017_CZ_A23 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23)
        gam_maple_2017_CZ_A28 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28)
        gam_maple_2017_CZ_A29 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29)
        gam_maple_2017_CZ_A8 <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8)
        HF2017_maple_mean.sdd_filled1_A18$gam_CZ = gam_maple_2017_CZ_A18$fitted.values
        HF2017_maple_mean.sdd_filled1_A19$gam_CZ = gam_maple_2017_CZ_A19$fitted.values
        HF2017_maple_mean.sdd_filled1_A23$gam_CZ = gam_maple_2017_CZ_A23$fitted.values
        HF2017_maple_mean.sdd_filled1_A28$gam_CZ = gam_maple_2017_CZ_A28$fitted.values
        HF2017_maple_mean.sdd_filled1_A29$gam_CZ = gam_maple_2017_CZ_A29$fitted.values
        HF2017_maple_mean.sdd_filled1_A8$gam_CZ = gam_maple_2017_CZ_A8$fitted.values
      }
      #EZ
      {
        gam_maple_2017_EZ_A18 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18)
        gam_maple_2017_EZ_A19 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19)
        gam_maple_2017_EZ_A23 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23)
        gam_maple_2017_EZ_A28 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28)
        gam_maple_2017_EZ_A29 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29)
        gam_maple_2017_EZ_A8 <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8)
        HF2017_maple_mean.sdd_filled1_A18$gam_EZ = gam_maple_2017_EZ_A18$fitted.values
        HF2017_maple_mean.sdd_filled1_A19$gam_EZ = gam_maple_2017_EZ_A19$fitted.values
        HF2017_maple_mean.sdd_filled1_A23$gam_EZ = gam_maple_2017_EZ_A23$fitted.values
        HF2017_maple_mean.sdd_filled1_A28$gam_EZ = gam_maple_2017_EZ_A28$fitted.values
        HF2017_maple_mean.sdd_filled1_A29$gam_EZ = gam_maple_2017_EZ_A29$fitted.values
        HF2017_maple_mean.sdd_filled1_A8$gam_EZ = gam_maple_2017_EZ_A8$fitted.values
      }
      #WZ
      {
        gam_maple_2017_WZ_A18 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18)
        gam_maple_2017_WZ_A19 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19)
        gam_maple_2017_WZ_A23 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23)
        gam_maple_2017_WZ_A28 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28)
        gam_maple_2017_WZ_A29 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29)
        gam_maple_2017_WZ_A8 <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8)
        HF2017_maple_mean.sdd_filled1_A18$gam_WZ = gam_maple_2017_WZ_A18$fitted.values
        HF2017_maple_mean.sdd_filled1_A19$gam_WZ = gam_maple_2017_WZ_A19$fitted.values
        HF2017_maple_mean.sdd_filled1_A23$gam_WZ = gam_maple_2017_WZ_A23$fitted.values
        HF2017_maple_mean.sdd_filled1_A28$gam_WZ = gam_maple_2017_WZ_A28$fitted.values
        HF2017_maple_mean.sdd_filled1_A29$gam_WZ = gam_maple_2017_WZ_A29$fitted.values
        HF2017_maple_mean.sdd_filled1_A8$gam_WZ = gam_maple_2017_WZ_A8$fitted.values
      }
      #MZ
      {
        gam_maple_2017_MZ_A18 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18)
        gam_maple_2017_MZ_A19 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19)
        gam_maple_2017_MZ_A23 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23)
        gam_maple_2017_MZ_A28 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28)
        gam_maple_2017_MZ_A29 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29)
        gam_maple_2017_MZ_A8 <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8)
        HF2017_maple_mean.sdd_filled1_A18$gam_MZ = gam_maple_2017_MZ_A18$fitted.values
        HF2017_maple_mean.sdd_filled1_A19$gam_MZ = gam_maple_2017_MZ_A19$fitted.values
        HF2017_maple_mean.sdd_filled1_A23$gam_MZ = gam_maple_2017_MZ_A23$fitted.values
        HF2017_maple_mean.sdd_filled1_A28$gam_MZ = gam_maple_2017_MZ_A28$fitted.values
        HF2017_maple_mean.sdd_filled1_A29$gam_MZ = gam_maple_2017_MZ_A29$fitted.values
        HF2017_maple_mean.sdd_filled1_A8$gam_MZ = gam_maple_2017_MZ_A8$fitted.values
      }
      #WMZ
      {
        HF2017_maple_mean.sdd_filled1_A18$gam_WMZ = HF2017_maple_mean.sdd_filled1_A18$gam_WZ + HF2017_maple_mean.sdd_filled1_A18$gam_MZ
        HF2017_maple_mean.sdd_filled1_A19$gam_WMZ = HF2017_maple_mean.sdd_filled1_A19$gam_WZ + HF2017_maple_mean.sdd_filled1_A19$gam_MZ
        HF2017_maple_mean.sdd_filled1_A23$gam_WMZ = HF2017_maple_mean.sdd_filled1_A23$gam_WZ + HF2017_maple_mean.sdd_filled1_A23$gam_MZ
        HF2017_maple_mean.sdd_filled1_A28$gam_WMZ = HF2017_maple_mean.sdd_filled1_A28$gam_WZ + HF2017_maple_mean.sdd_filled1_A28$gam_MZ
        HF2017_maple_mean.sdd_filled1_A29$gam_WMZ = HF2017_maple_mean.sdd_filled1_A29$gam_WZ + HF2017_maple_mean.sdd_filled1_A29$gam_MZ
        HF2017_maple_mean.sdd_filled1_A8$gam_WMZ = HF2017_maple_mean.sdd_filled1_A8$gam_WZ + HF2017_maple_mean.sdd_filled1_A8$gam_MZ
      }
      #EWMZ
      {
        HF2017_maple_mean.sdd_filled1_A18$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A18$gam_WZ + HF2017_maple_mean.sdd_filled1_A18$gam_MZ
        HF2017_maple_mean.sdd_filled1_A19$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A19$gam_WZ + HF2017_maple_mean.sdd_filled1_A19$gam_MZ
        HF2017_maple_mean.sdd_filled1_A23$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A23$gam_WZ + HF2017_maple_mean.sdd_filled1_A23$gam_MZ
        HF2017_maple_mean.sdd_filled1_A28$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A28$gam_WZ + HF2017_maple_mean.sdd_filled1_A28$gam_MZ
        HF2017_maple_mean.sdd_filled1_A29$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A29$gam_WZ + HF2017_maple_mean.sdd_filled1_A29$gam_MZ
        HF2017_maple_mean.sdd_filled1_A8$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18$gam_EZ +HF2017_maple_mean.sdd_filled1_A8$gam_WZ + HF2017_maple_mean.sdd_filled1_A8$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_maple_2018_CZ_A18 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18)
        gam_maple_2018_CZ_A19 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19)
        gam_maple_2018_CZ_A23 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23)
        gam_maple_2018_CZ_A28 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28)
        gam_maple_2018_CZ_A29 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29)
        gam_maple_2018_CZ_A8 <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8)
        HF2018_maple_mean.sdd_filled1_A18$gam_CZ = gam_maple_2018_CZ_A18$fitted.values
        HF2018_maple_mean.sdd_filled1_A19$gam_CZ = gam_maple_2018_CZ_A19$fitted.values
        HF2018_maple_mean.sdd_filled1_A23$gam_CZ = gam_maple_2018_CZ_A23$fitted.values
        HF2018_maple_mean.sdd_filled1_A28$gam_CZ = gam_maple_2018_CZ_A28$fitted.values
        HF2018_maple_mean.sdd_filled1_A29$gam_CZ = gam_maple_2018_CZ_A29$fitted.values
        HF2018_maple_mean.sdd_filled1_A8$gam_CZ = gam_maple_2018_CZ_A8$fitted.values
      }
      #EZ
      {
        gam_maple_2018_EZ_A18 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18)
        gam_maple_2018_EZ_A19 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19)
        gam_maple_2018_EZ_A23 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23)
        gam_maple_2018_EZ_A28 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28)
        gam_maple_2018_EZ_A29 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29)
        gam_maple_2018_EZ_A8 <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8)
        HF2018_maple_mean.sdd_filled1_A18$gam_EZ = gam_maple_2018_EZ_A18$fitted.values
        HF2018_maple_mean.sdd_filled1_A19$gam_EZ = gam_maple_2018_EZ_A19$fitted.values
        HF2018_maple_mean.sdd_filled1_A23$gam_EZ = gam_maple_2018_EZ_A23$fitted.values
        HF2018_maple_mean.sdd_filled1_A28$gam_EZ = gam_maple_2018_EZ_A28$fitted.values
        HF2018_maple_mean.sdd_filled1_A29$gam_EZ = gam_maple_2018_EZ_A29$fitted.values
        HF2018_maple_mean.sdd_filled1_A8$gam_EZ = gam_maple_2018_EZ_A8$fitted.values
      }
      #WZ
      {
        gam_maple_2018_WZ_A18 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18)
        gam_maple_2018_WZ_A19 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19)
        gam_maple_2018_WZ_A23 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23)
        gam_maple_2018_WZ_A28 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28)
        gam_maple_2018_WZ_A29 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29)
        gam_maple_2018_WZ_A8 <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8)
        HF2018_maple_mean.sdd_filled1_A18$gam_WZ = gam_maple_2018_WZ_A18$fitted.values
        HF2018_maple_mean.sdd_filled1_A19$gam_WZ = gam_maple_2018_WZ_A19$fitted.values
        HF2018_maple_mean.sdd_filled1_A23$gam_WZ = gam_maple_2018_WZ_A23$fitted.values
        HF2018_maple_mean.sdd_filled1_A28$gam_WZ = gam_maple_2018_WZ_A28$fitted.values
        HF2018_maple_mean.sdd_filled1_A29$gam_WZ = gam_maple_2018_WZ_A29$fitted.values
        HF2018_maple_mean.sdd_filled1_A8$gam_WZ = gam_maple_2018_WZ_A8$fitted.values
      }
      #MZ
      {
        gam_maple_2018_MZ_A18 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18)
        gam_maple_2018_MZ_A19 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19)
        gam_maple_2018_MZ_A23 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23)
        gam_maple_2018_MZ_A28 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28)
        gam_maple_2018_MZ_A29 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29)
        gam_maple_2018_MZ_A8 <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8)
        HF2018_maple_mean.sdd_filled1_A18$gam_MZ = gam_maple_2018_MZ_A18$fitted.values
        HF2018_maple_mean.sdd_filled1_A19$gam_MZ = gam_maple_2018_MZ_A19$fitted.values
        HF2018_maple_mean.sdd_filled1_A23$gam_MZ = gam_maple_2018_MZ_A23$fitted.values
        HF2018_maple_mean.sdd_filled1_A28$gam_MZ = gam_maple_2018_MZ_A28$fitted.values
        HF2018_maple_mean.sdd_filled1_A29$gam_MZ = gam_maple_2018_MZ_A29$fitted.values
        HF2018_maple_mean.sdd_filled1_A8$gam_MZ = gam_maple_2018_MZ_A8$fitted.values
      }
      #WMZ
      {
        HF2018_maple_mean.sdd_filled1_A18$gam_WMZ = HF2018_maple_mean.sdd_filled1_A18$gam_WZ + HF2018_maple_mean.sdd_filled1_A18$gam_MZ
        HF2018_maple_mean.sdd_filled1_A19$gam_WMZ = HF2018_maple_mean.sdd_filled1_A19$gam_WZ + HF2018_maple_mean.sdd_filled1_A19$gam_MZ
        HF2018_maple_mean.sdd_filled1_A23$gam_WMZ = HF2018_maple_mean.sdd_filled1_A23$gam_WZ + HF2018_maple_mean.sdd_filled1_A23$gam_MZ
        HF2018_maple_mean.sdd_filled1_A28$gam_WMZ = HF2018_maple_mean.sdd_filled1_A28$gam_WZ + HF2018_maple_mean.sdd_filled1_A28$gam_MZ
        HF2018_maple_mean.sdd_filled1_A29$gam_WMZ = HF2018_maple_mean.sdd_filled1_A29$gam_WZ + HF2018_maple_mean.sdd_filled1_A29$gam_MZ
        HF2018_maple_mean.sdd_filled1_A8$gam_WMZ = HF2018_maple_mean.sdd_filled1_A8$gam_WZ + HF2018_maple_mean.sdd_filled1_A8$gam_MZ
      }
      #EWMZ
      {
        HF2018_maple_mean.sdd_filled1_A18$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A18$gam_EZ +HF2018_maple_mean.sdd_filled1_A18$gam_WZ + HF2018_maple_mean.sdd_filled1_A18$gam_MZ
        HF2018_maple_mean.sdd_filled1_A19$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A19$gam_EZ +HF2018_maple_mean.sdd_filled1_A19$gam_WZ + HF2018_maple_mean.sdd_filled1_A19$gam_MZ
        HF2018_maple_mean.sdd_filled1_A23$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A23$gam_EZ +HF2018_maple_mean.sdd_filled1_A23$gam_WZ + HF2018_maple_mean.sdd_filled1_A23$gam_MZ
        HF2018_maple_mean.sdd_filled1_A28$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A28$gam_EZ +HF2018_maple_mean.sdd_filled1_A28$gam_WZ + HF2018_maple_mean.sdd_filled1_A28$gam_MZ
        HF2018_maple_mean.sdd_filled1_A29$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A29$gam_EZ +HF2018_maple_mean.sdd_filled1_A29$gam_WZ + HF2018_maple_mean.sdd_filled1_A29$gam_MZ
        HF2018_maple_mean.sdd_filled1_A8$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A8$gam_EZ +HF2018_maple_mean.sdd_filled1_A8$gam_WZ + HF2018_maple_mean.sdd_filled1_A8$gam_MZ
      }
      
      #2019
      #CZ
      {
        gam_maple_2019_CZ_A18 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18)
        gam_maple_2019_CZ_A19 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19)
        gam_maple_2019_CZ_A23 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23)
        gam_maple_2019_CZ_A28 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28)
        gam_maple_2019_CZ_A29 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29)
        gam_maple_2019_CZ_A8 <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8)
        HF2019_maple_mean.sdd_filled1_A18$gam_CZ = gam_maple_2019_CZ_A18$fitted.values
        HF2019_maple_mean.sdd_filled1_A19$gam_CZ = gam_maple_2019_CZ_A19$fitted.values
        HF2019_maple_mean.sdd_filled1_A23$gam_CZ = gam_maple_2019_CZ_A23$fitted.values
        HF2019_maple_mean.sdd_filled1_A28$gam_CZ = gam_maple_2019_CZ_A28$fitted.values
        HF2019_maple_mean.sdd_filled1_A29$gam_CZ = gam_maple_2019_CZ_A29$fitted.values
        HF2019_maple_mean.sdd_filled1_A8$gam_CZ = gam_maple_2019_CZ_A8$fitted.values
      }
      #EZ
      {
        gam_maple_2019_EZ_A18 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18)
        gam_maple_2019_EZ_A19 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19)
        gam_maple_2019_EZ_A23 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23)
        gam_maple_2019_EZ_A28 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28)
        gam_maple_2019_EZ_A29 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29)
        gam_maple_2019_EZ_A8 <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8)
        HF2019_maple_mean.sdd_filled1_A18$gam_EZ = gam_maple_2019_EZ_A18$fitted.values
        HF2019_maple_mean.sdd_filled1_A19$gam_EZ = gam_maple_2019_EZ_A19$fitted.values
        HF2019_maple_mean.sdd_filled1_A23$gam_EZ = gam_maple_2019_EZ_A23$fitted.values
        HF2019_maple_mean.sdd_filled1_A28$gam_EZ = gam_maple_2019_EZ_A28$fitted.values
        HF2019_maple_mean.sdd_filled1_A29$gam_EZ = gam_maple_2019_EZ_A29$fitted.values
        HF2019_maple_mean.sdd_filled1_A8$gam_EZ = gam_maple_2019_EZ_A8$fitted.values
      }
      #WZ
      {
        gam_maple_2019_WZ_A18 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18)
        gam_maple_2019_WZ_A19 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19)
        gam_maple_2019_WZ_A23 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23)
        gam_maple_2019_WZ_A28 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28)
        gam_maple_2019_WZ_A29 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29)
        gam_maple_2019_WZ_A8 <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8)
        HF2019_maple_mean.sdd_filled1_A18$gam_WZ = gam_maple_2019_WZ_A18$fitted.values
        HF2019_maple_mean.sdd_filled1_A19$gam_WZ = gam_maple_2019_WZ_A19$fitted.values
        HF2019_maple_mean.sdd_filled1_A23$gam_WZ = gam_maple_2019_WZ_A23$fitted.values
        HF2019_maple_mean.sdd_filled1_A28$gam_WZ = gam_maple_2019_WZ_A28$fitted.values
        HF2019_maple_mean.sdd_filled1_A29$gam_WZ = gam_maple_2019_WZ_A29$fitted.values
        HF2019_maple_mean.sdd_filled1_A8$gam_WZ = gam_maple_2019_WZ_A8$fitted.values
      }
      #MZ
      {
        gam_maple_2019_MZ_A18 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18)
        gam_maple_2019_MZ_A19 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19)
        gam_maple_2019_MZ_A23 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23)
        gam_maple_2019_MZ_A28 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28)
        gam_maple_2019_MZ_A29 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29)
        gam_maple_2019_MZ_A8 <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8)
        HF2019_maple_mean.sdd_filled1_A18$gam_MZ = gam_maple_2019_MZ_A18$fitted.values
        HF2019_maple_mean.sdd_filled1_A19$gam_MZ = gam_maple_2019_MZ_A19$fitted.values
        HF2019_maple_mean.sdd_filled1_A23$gam_MZ = gam_maple_2019_MZ_A23$fitted.values
        HF2019_maple_mean.sdd_filled1_A28$gam_MZ = gam_maple_2019_MZ_A28$fitted.values
        HF2019_maple_mean.sdd_filled1_A29$gam_MZ = gam_maple_2019_MZ_A29$fitted.values
        HF2019_maple_mean.sdd_filled1_A8$gam_MZ = gam_maple_2019_MZ_A8$fitted.values
      }
      #WMZ
      {
        HF2019_maple_mean.sdd_filled1_A18$gam_WMZ = HF2019_maple_mean.sdd_filled1_A18$gam_WZ + HF2019_maple_mean.sdd_filled1_A18$gam_MZ
        HF2019_maple_mean.sdd_filled1_A19$gam_WMZ = HF2019_maple_mean.sdd_filled1_A19$gam_WZ + HF2019_maple_mean.sdd_filled1_A19$gam_MZ
        HF2019_maple_mean.sdd_filled1_A23$gam_WMZ = HF2019_maple_mean.sdd_filled1_A23$gam_WZ + HF2019_maple_mean.sdd_filled1_A23$gam_MZ
        HF2019_maple_mean.sdd_filled1_A28$gam_WMZ = HF2019_maple_mean.sdd_filled1_A28$gam_WZ + HF2019_maple_mean.sdd_filled1_A28$gam_MZ
        HF2019_maple_mean.sdd_filled1_A29$gam_WMZ = HF2019_maple_mean.sdd_filled1_A29$gam_WZ + HF2019_maple_mean.sdd_filled1_A29$gam_MZ
        HF2019_maple_mean.sdd_filled1_A8$gam_WMZ = HF2019_maple_mean.sdd_filled1_A8$gam_WZ + HF2019_maple_mean.sdd_filled1_A8$gam_MZ
      }
      #EWMZ
      {
        HF2019_maple_mean.sdd_filled1_A18$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A18$gam_EZ +HF2019_maple_mean.sdd_filled1_A18$gam_WZ + HF2019_maple_mean.sdd_filled1_A18$gam_MZ
        HF2019_maple_mean.sdd_filled1_A19$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A19$gam_EZ +HF2019_maple_mean.sdd_filled1_A19$gam_WZ + HF2019_maple_mean.sdd_filled1_A19$gam_MZ
        HF2019_maple_mean.sdd_filled1_A23$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A23$gam_EZ +HF2019_maple_mean.sdd_filled1_A23$gam_WZ + HF2019_maple_mean.sdd_filled1_A23$gam_MZ
        HF2019_maple_mean.sdd_filled1_A28$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A28$gam_EZ +HF2019_maple_mean.sdd_filled1_A28$gam_WZ + HF2019_maple_mean.sdd_filled1_A28$gam_MZ
        HF2019_maple_mean.sdd_filled1_A29$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A29$gam_EZ +HF2019_maple_mean.sdd_filled1_A29$gam_WZ + HF2019_maple_mean.sdd_filled1_A29$gam_MZ
        HF2019_maple_mean.sdd_filled1_A8$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A8$gam_EZ +HF2019_maple_mean.sdd_filled1_A8$gam_WZ + HF2019_maple_mean.sdd_filled1_A8$gam_MZ
      }
      
      #combine the tree dfs to one for each year
      {
        HF2017_maple_mean.sdd_filled2 = rbind(HF2017_maple_mean.sdd_filled1_A18,HF2017_maple_mean.sdd_filled1_A19,HF2017_maple_mean.sdd_filled1_A23,HF2017_maple_mean.sdd_filled1_A28,HF2017_maple_mean.sdd_filled1_A29,HF2017_maple_mean.sdd_filled1_A8)
        HF2018_maple_mean.sdd_filled2 = rbind(HF2018_maple_mean.sdd_filled1_A18,HF2018_maple_mean.sdd_filled1_A19,HF2018_maple_mean.sdd_filled1_A23,HF2018_maple_mean.sdd_filled1_A28,HF2018_maple_mean.sdd_filled1_A29,HF2018_maple_mean.sdd_filled1_A8)
        HF2019_maple_mean.sdd_filled2 = rbind(HF2019_maple_mean.sdd_filled1_A18,HF2019_maple_mean.sdd_filled1_A19,HF2019_maple_mean.sdd_filled1_A23,HF2019_maple_mean.sdd_filled1_A28,HF2019_maple_mean.sdd_filled1_A29,HF2019_maple_mean.sdd_filled1_A8)
      }
    }
    #QURU
    {
      #CZ
      {
        gam_oak_2017_CZ_Q05 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05)
        gam_oak_2017_CZ_Q06 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06)
        gam_oak_2017_CZ_Q09 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09)
        gam_oak_2017_CZ_Q10 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10)
        gam_oak_2017_CZ_Q11 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11)
        gam_oak_2017_CZ_Q12 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12)
        gam_oak_2017_CZ_Q13 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13)
        gam_oak_2017_CZ_Q15 <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15)
        HF2017_oak_mean.sdd_filled1_Q05$gam_CZ = gam_oak_2017_CZ_Q05$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06$gam_CZ = gam_oak_2017_CZ_Q06$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09$gam_CZ = gam_oak_2017_CZ_Q09$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10$gam_CZ = gam_oak_2017_CZ_Q10$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11$gam_CZ = gam_oak_2017_CZ_Q11$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12$gam_CZ = gam_oak_2017_CZ_Q12$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13$gam_CZ = gam_oak_2017_CZ_Q13$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15$gam_CZ = gam_oak_2017_CZ_Q15$fitted.values
      }
      #EZ
      {
        gam_oak_2017_EZ_Q05 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05)
        gam_oak_2017_EZ_Q06 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06)
        gam_oak_2017_EZ_Q09 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09)
        gam_oak_2017_EZ_Q10 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10)
        gam_oak_2017_EZ_Q11 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11)
        gam_oak_2017_EZ_Q12 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12)
        gam_oak_2017_EZ_Q13 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13)
        gam_oak_2017_EZ_Q15 <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15)
        HF2017_oak_mean.sdd_filled1_Q05$gam_EZ = gam_oak_2017_EZ_Q05$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06$gam_EZ = gam_oak_2017_EZ_Q06$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09$gam_EZ = gam_oak_2017_EZ_Q09$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10$gam_EZ = gam_oak_2017_EZ_Q10$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11$gam_EZ = gam_oak_2017_EZ_Q11$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12$gam_EZ = gam_oak_2017_EZ_Q12$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13$gam_EZ = gam_oak_2017_EZ_Q13$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15$gam_EZ = gam_oak_2017_EZ_Q15$fitted.values
      }
      #WZ
      {
        gam_oak_2017_WZ_Q05 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05)
        gam_oak_2017_WZ_Q06 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06)
        gam_oak_2017_WZ_Q09 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09)
        gam_oak_2017_WZ_Q10 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10)
        gam_oak_2017_WZ_Q11 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11)
        gam_oak_2017_WZ_Q12 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12)
        gam_oak_2017_WZ_Q13 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13)
        gam_oak_2017_WZ_Q15 <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15)
        HF2017_oak_mean.sdd_filled1_Q05$gam_WZ = gam_oak_2017_WZ_Q05$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06$gam_WZ = gam_oak_2017_WZ_Q06$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09$gam_WZ = gam_oak_2017_WZ_Q09$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10$gam_WZ = gam_oak_2017_WZ_Q10$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11$gam_WZ = gam_oak_2017_WZ_Q11$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12$gam_WZ = gam_oak_2017_WZ_Q12$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13$gam_WZ = gam_oak_2017_WZ_Q13$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15$gam_WZ = gam_oak_2017_WZ_Q15$fitted.values
      }
      #MZ
      {
        gam_oak_2017_MZ_Q05 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05)
        gam_oak_2017_MZ_Q06 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06)
        gam_oak_2017_MZ_Q09 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09)
        gam_oak_2017_MZ_Q10 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10)
        gam_oak_2017_MZ_Q11 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11)
        gam_oak_2017_MZ_Q12 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12)
        gam_oak_2017_MZ_Q13 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13)
        gam_oak_2017_MZ_Q15 <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15)
        HF2017_oak_mean.sdd_filled1_Q05$gam_MZ = gam_oak_2017_MZ_Q05$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06$gam_MZ = gam_oak_2017_MZ_Q06$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09$gam_MZ = gam_oak_2017_MZ_Q09$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10$gam_MZ = gam_oak_2017_MZ_Q10$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11$gam_MZ = gam_oak_2017_MZ_Q11$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12$gam_MZ = gam_oak_2017_MZ_Q12$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13$gam_MZ = gam_oak_2017_MZ_Q13$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15$gam_MZ = gam_oak_2017_MZ_Q15$fitted.values
      }
      #WMZ
      {
        HF2017_oak_mean.sdd_filled1_Q05$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q05$gam_WZ +HF2017_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q06$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q06$gam_WZ +HF2017_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q09$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q09$gam_WZ +HF2017_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q10$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q10$gam_WZ +HF2017_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q11$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q11$gam_WZ +HF2017_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q12$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q12$gam_WZ +HF2017_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q13$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q13$gam_WZ +HF2017_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q15$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q15$gam_WZ +HF2017_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      #EWMZ
      {
        HF2017_oak_mean.sdd_filled1_Q05$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q05$gam_EZ +HF2017_oak_mean.sdd_filled1_Q05$gam_WZ +HF2017_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q06$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q06$gam_EZ +HF2017_oak_mean.sdd_filled1_Q06$gam_WZ +HF2017_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q09$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q09$gam_EZ +HF2017_oak_mean.sdd_filled1_Q09$gam_WZ +HF2017_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q10$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q10$gam_EZ +HF2017_oak_mean.sdd_filled1_Q10$gam_WZ +HF2017_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q11$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q11$gam_EZ +HF2017_oak_mean.sdd_filled1_Q11$gam_WZ +HF2017_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q12$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q12$gam_EZ +HF2017_oak_mean.sdd_filled1_Q12$gam_WZ +HF2017_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q13$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q13$gam_EZ +HF2017_oak_mean.sdd_filled1_Q13$gam_WZ +HF2017_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q15$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q15$gam_EZ +HF2017_oak_mean.sdd_filled1_Q15$gam_WZ +HF2017_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_oak_2018_CZ_Q05 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05)
        gam_oak_2018_CZ_Q06 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06)
        gam_oak_2018_CZ_Q09 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09)
        gam_oak_2018_CZ_Q10 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10)
        gam_oak_2018_CZ_Q11 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11)
        gam_oak_2018_CZ_Q12 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12)
        gam_oak_2018_CZ_Q13 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13)
        gam_oak_2018_CZ_Q15 <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15)
        HF2018_oak_mean.sdd_filled1_Q05$gam_CZ = gam_oak_2018_CZ_Q05$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06$gam_CZ = gam_oak_2018_CZ_Q06$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09$gam_CZ = gam_oak_2018_CZ_Q09$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10$gam_CZ = gam_oak_2018_CZ_Q10$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11$gam_CZ = gam_oak_2018_CZ_Q11$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12$gam_CZ = gam_oak_2018_CZ_Q12$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13$gam_CZ = gam_oak_2018_CZ_Q13$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15$gam_CZ = gam_oak_2018_CZ_Q15$fitted.values
      }
      #EZ
      {
        gam_oak_2018_EZ_Q05 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05)
        gam_oak_2018_EZ_Q06 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06)
        gam_oak_2018_EZ_Q09 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09)
        gam_oak_2018_EZ_Q10 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10)
        gam_oak_2018_EZ_Q11 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11)
        gam_oak_2018_EZ_Q12 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12)
        gam_oak_2018_EZ_Q13 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13)
        gam_oak_2018_EZ_Q15 <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15)
        HF2018_oak_mean.sdd_filled1_Q05$gam_EZ = gam_oak_2018_EZ_Q05$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06$gam_EZ = gam_oak_2018_EZ_Q06$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09$gam_EZ = gam_oak_2018_EZ_Q09$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10$gam_EZ = gam_oak_2018_EZ_Q10$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11$gam_EZ = gam_oak_2018_EZ_Q11$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12$gam_EZ = gam_oak_2018_EZ_Q12$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13$gam_EZ = gam_oak_2018_EZ_Q13$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15$gam_EZ = gam_oak_2018_EZ_Q15$fitted.values
      }
      #WZ
      {
        gam_oak_2018_WZ_Q05 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05)
        gam_oak_2018_WZ_Q06 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06)
        gam_oak_2018_WZ_Q09 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09)
        gam_oak_2018_WZ_Q10 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10)
        gam_oak_2018_WZ_Q11 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11)
        gam_oak_2018_WZ_Q12 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12)
        gam_oak_2018_WZ_Q13 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13)
        gam_oak_2018_WZ_Q15 <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15)
        HF2018_oak_mean.sdd_filled1_Q05$gam_WZ = gam_oak_2018_WZ_Q05$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06$gam_WZ = gam_oak_2018_WZ_Q06$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09$gam_WZ = gam_oak_2018_WZ_Q09$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10$gam_WZ = gam_oak_2018_WZ_Q10$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11$gam_WZ = gam_oak_2018_WZ_Q11$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12$gam_WZ = gam_oak_2018_WZ_Q12$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13$gam_WZ = gam_oak_2018_WZ_Q13$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15$gam_WZ = gam_oak_2018_WZ_Q15$fitted.values
      }
      #MZ
      {
        gam_oak_2018_MZ_Q05 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05)
        gam_oak_2018_MZ_Q06 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06)
        gam_oak_2018_MZ_Q09 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09)
        gam_oak_2018_MZ_Q10 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10)
        gam_oak_2018_MZ_Q11 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11)
        gam_oak_2018_MZ_Q12 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12)
        gam_oak_2018_MZ_Q13 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13)
        gam_oak_2018_MZ_Q15 <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15)
        HF2018_oak_mean.sdd_filled1_Q05$gam_MZ = gam_oak_2018_MZ_Q05$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06$gam_MZ = gam_oak_2018_MZ_Q06$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09$gam_MZ = gam_oak_2018_MZ_Q09$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10$gam_MZ = gam_oak_2018_MZ_Q10$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11$gam_MZ = gam_oak_2018_MZ_Q11$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12$gam_MZ = gam_oak_2018_MZ_Q12$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13$gam_MZ = gam_oak_2018_MZ_Q13$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15$gam_MZ = gam_oak_2018_MZ_Q15$fitted.values
      }
      #WMZ
      {
        HF2018_oak_mean.sdd_filled1_Q05$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q05$gam_WZ +HF2018_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q06$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q06$gam_WZ +HF2018_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q09$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q09$gam_WZ +HF2018_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q10$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q10$gam_WZ +HF2018_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q11$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q11$gam_WZ +HF2018_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q12$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q12$gam_WZ +HF2018_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q13$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q13$gam_WZ +HF2018_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q15$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q15$gam_WZ +HF2018_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      #EWMZ
      {
        HF2018_oak_mean.sdd_filled1_Q05$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q05$gam_EZ +HF2018_oak_mean.sdd_filled1_Q05$gam_WZ +HF2018_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q06$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q06$gam_EZ +HF2018_oak_mean.sdd_filled1_Q06$gam_WZ +HF2018_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q09$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q09$gam_EZ +HF2018_oak_mean.sdd_filled1_Q09$gam_WZ +HF2018_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q10$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q10$gam_EZ +HF2018_oak_mean.sdd_filled1_Q10$gam_WZ +HF2018_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q11$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q11$gam_EZ +HF2018_oak_mean.sdd_filled1_Q11$gam_WZ +HF2018_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q12$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q12$gam_EZ +HF2018_oak_mean.sdd_filled1_Q12$gam_WZ +HF2018_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q13$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q13$gam_EZ +HF2018_oak_mean.sdd_filled1_Q13$gam_WZ +HF2018_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q15$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q15$gam_EZ +HF2018_oak_mean.sdd_filled1_Q15$gam_WZ +HF2018_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      #2019
      #CZ
      {
        gam_oak_2019_CZ_Q05 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05)
        gam_oak_2019_CZ_Q06 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06)
        gam_oak_2019_CZ_Q09 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09)
        gam_oak_2019_CZ_Q10 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10)
        gam_oak_2019_CZ_Q11 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11)
        gam_oak_2019_CZ_Q12 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12)
        gam_oak_2019_CZ_Q13 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13)
        gam_oak_2019_CZ_Q15 <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15)
        HF2019_oak_mean.sdd_filled1_Q05$gam_CZ = gam_oak_2019_CZ_Q05$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06$gam_CZ = gam_oak_2019_CZ_Q06$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09$gam_CZ = gam_oak_2019_CZ_Q09$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10$gam_CZ = gam_oak_2019_CZ_Q10$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11$gam_CZ = gam_oak_2019_CZ_Q11$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12$gam_CZ = gam_oak_2019_CZ_Q12$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13$gam_CZ = gam_oak_2019_CZ_Q13$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15$gam_CZ = gam_oak_2019_CZ_Q15$fitted.values
      }
      #EZ
      {
        gam_oak_2019_EZ_Q05 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05)
        gam_oak_2019_EZ_Q06 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06)
        gam_oak_2019_EZ_Q09 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09)
        gam_oak_2019_EZ_Q10 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10)
        gam_oak_2019_EZ_Q11 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11)
        gam_oak_2019_EZ_Q12 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12)
        gam_oak_2019_EZ_Q13 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13)
        gam_oak_2019_EZ_Q15 <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15)
        HF2019_oak_mean.sdd_filled1_Q05$gam_EZ = gam_oak_2019_EZ_Q05$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06$gam_EZ = gam_oak_2019_EZ_Q06$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09$gam_EZ = gam_oak_2019_EZ_Q09$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10$gam_EZ = gam_oak_2019_EZ_Q10$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11$gam_EZ = gam_oak_2019_EZ_Q11$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12$gam_EZ = gam_oak_2019_EZ_Q12$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13$gam_EZ = gam_oak_2019_EZ_Q13$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15$gam_EZ = gam_oak_2019_EZ_Q15$fitted.values
      }
      #WZ
      {
        gam_oak_2019_WZ_Q05 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05)
        gam_oak_2019_WZ_Q06 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06)
        gam_oak_2019_WZ_Q09 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09)
        gam_oak_2019_WZ_Q10 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10)
        gam_oak_2019_WZ_Q11 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11)
        gam_oak_2019_WZ_Q12 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12)
        gam_oak_2019_WZ_Q13 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13)
        gam_oak_2019_WZ_Q15 <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15)
        HF2019_oak_mean.sdd_filled1_Q05$gam_WZ = gam_oak_2019_WZ_Q05$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06$gam_WZ = gam_oak_2019_WZ_Q06$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09$gam_WZ = gam_oak_2019_WZ_Q09$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10$gam_WZ = gam_oak_2019_WZ_Q10$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11$gam_WZ = gam_oak_2019_WZ_Q11$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12$gam_WZ = gam_oak_2019_WZ_Q12$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13$gam_WZ = gam_oak_2019_WZ_Q13$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15$gam_WZ = gam_oak_2019_WZ_Q15$fitted.values
      }
      #MZ
      {
        gam_oak_2019_MZ_Q05 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05)
        gam_oak_2019_MZ_Q06 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06)
        gam_oak_2019_MZ_Q09 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09)
        gam_oak_2019_MZ_Q10 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10)
        gam_oak_2019_MZ_Q11 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11)
        gam_oak_2019_MZ_Q12 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12)
        gam_oak_2019_MZ_Q13 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13)
        gam_oak_2019_MZ_Q15 <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15)
        HF2019_oak_mean.sdd_filled1_Q05$gam_MZ = gam_oak_2019_MZ_Q05$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06$gam_MZ = gam_oak_2019_MZ_Q06$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09$gam_MZ = gam_oak_2019_MZ_Q09$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10$gam_MZ = gam_oak_2019_MZ_Q10$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11$gam_MZ = gam_oak_2019_MZ_Q11$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12$gam_MZ = gam_oak_2019_MZ_Q12$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13$gam_MZ = gam_oak_2019_MZ_Q13$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15$gam_MZ = gam_oak_2019_MZ_Q15$fitted.values
      }
      #WMZ
      {
        HF2019_oak_mean.sdd_filled1_Q05$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q05$gam_WZ +HF2019_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q06$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q06$gam_WZ +HF2019_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q09$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q09$gam_WZ +HF2019_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q10$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q10$gam_WZ +HF2019_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q11$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q11$gam_WZ +HF2019_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q12$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q12$gam_WZ +HF2019_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q13$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q13$gam_WZ +HF2019_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q15$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q15$gam_WZ +HF2019_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      #EWMZ
      {
        HF2019_oak_mean.sdd_filled1_Q05$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q05$gam_EZ +HF2019_oak_mean.sdd_filled1_Q05$gam_WZ +HF2019_oak_mean.sdd_filled1_Q05$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q06$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q06$gam_EZ +HF2019_oak_mean.sdd_filled1_Q06$gam_WZ +HF2019_oak_mean.sdd_filled1_Q06$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q09$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q09$gam_EZ +HF2019_oak_mean.sdd_filled1_Q09$gam_WZ +HF2019_oak_mean.sdd_filled1_Q09$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q10$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q10$gam_EZ +HF2019_oak_mean.sdd_filled1_Q10$gam_WZ +HF2019_oak_mean.sdd_filled1_Q10$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q11$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q11$gam_EZ +HF2019_oak_mean.sdd_filled1_Q11$gam_WZ +HF2019_oak_mean.sdd_filled1_Q11$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q12$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q12$gam_EZ +HF2019_oak_mean.sdd_filled1_Q12$gam_WZ +HF2019_oak_mean.sdd_filled1_Q12$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q13$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q13$gam_EZ +HF2019_oak_mean.sdd_filled1_Q13$gam_WZ +HF2019_oak_mean.sdd_filled1_Q13$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q15$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q15$gam_EZ +HF2019_oak_mean.sdd_filled1_Q15$gam_WZ +HF2019_oak_mean.sdd_filled1_Q15$gam_MZ
      }
      
      #combine the tree dfs to one
      {
        HF2017_oak_mean.sdd_filled2 = rbind(HF2017_oak_mean.sdd_filled1_Q05,HF2017_oak_mean.sdd_filled1_Q06,HF2017_oak_mean.sdd_filled1_Q09,HF2017_oak_mean.sdd_filled1_Q10,HF2017_oak_mean.sdd_filled1_Q11,HF2017_oak_mean.sdd_filled1_Q12,HF2017_oak_mean.sdd_filled1_Q13,HF2017_oak_mean.sdd_filled1_Q15)
        HF2018_oak_mean.sdd_filled2 = rbind(HF2018_oak_mean.sdd_filled1_Q05,HF2018_oak_mean.sdd_filled1_Q06,HF2018_oak_mean.sdd_filled1_Q09,HF2018_oak_mean.sdd_filled1_Q10,HF2018_oak_mean.sdd_filled1_Q11,HF2018_oak_mean.sdd_filled1_Q12,HF2018_oak_mean.sdd_filled1_Q13,HF2018_oak_mean.sdd_filled1_Q15)
        HF2019_oak_mean.sdd_filled2 = rbind(HF2019_oak_mean.sdd_filled1_Q05,HF2019_oak_mean.sdd_filled1_Q06,HF2019_oak_mean.sdd_filled1_Q09,HF2019_oak_mean.sdd_filled1_Q10,HF2019_oak_mean.sdd_filled1_Q11,HF2019_oak_mean.sdd_filled1_Q12,HF2019_oak_mean.sdd_filled1_Q13,HF2019_oak_mean.sdd_filled1_Q15)
      }
    }
  }
  #no cut
  {
    #PIST
    {
      #2017
      #CZ not very good
      {
        gam_pine_2017_CZ_P16_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16_com)
        gam_pine_2017_CZ_P20_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20_com)
        gam_pine_2017_CZ_P21_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21_com)
        gam_pine_2017_CZ_P25_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25_com)
        gam_pine_2017_CZ_P26_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26_com)
        gam_pine_2017_CZ_P7_com <- gam(CZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7_com)
        HF2017_pine_mean.sdd_filled1_P16_com$gam_CZ = gam_pine_2017_CZ_P16_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P20_com$gam_CZ = gam_pine_2017_CZ_P20_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P21_com$gam_CZ = gam_pine_2017_CZ_P21_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P25_com$gam_CZ = gam_pine_2017_CZ_P25_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P26_com$gam_CZ = gam_pine_2017_CZ_P26_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P7_com$gam_CZ = gam_pine_2017_CZ_P7_com$fitted.values
      }
      #EZ
      {
        gam_pine_2017_EZ_P16_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16_com)
        gam_pine_2017_EZ_P20_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20_com)
        gam_pine_2017_EZ_P21_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21_com)
        gam_pine_2017_EZ_P25_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25_com)
        gam_pine_2017_EZ_P26_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26_com)
        gam_pine_2017_EZ_P7_com <- gam(EZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7_com)
        HF2017_pine_mean.sdd_filled1_P16_com$gam_EZ = gam_pine_2017_EZ_P16_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P20_com$gam_EZ = gam_pine_2017_EZ_P20_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P21_com$gam_EZ = gam_pine_2017_EZ_P21_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P25_com$gam_EZ = gam_pine_2017_EZ_P25_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P26_com$gam_EZ = gam_pine_2017_EZ_P26_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P7_com$gam_EZ = gam_pine_2017_EZ_P7_com$fitted.values
      }
      
      #WZ
      {
        gam_pine_2017_WZ_P16_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16_com)
        gam_pine_2017_WZ_P20_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20_com)
        gam_pine_2017_WZ_P21_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21_com)
        gam_pine_2017_WZ_P25_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25_com)
        gam_pine_2017_WZ_P26_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26_com)
        gam_pine_2017_WZ_P7_com <- gam(WZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7_com)
        HF2017_pine_mean.sdd_filled1_P16_com$gam_WZ = gam_pine_2017_WZ_P16_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P20_com$gam_WZ = gam_pine_2017_WZ_P20_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P21_com$gam_WZ = gam_pine_2017_WZ_P21_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P25_com$gam_WZ = gam_pine_2017_WZ_P25_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P26_com$gam_WZ = gam_pine_2017_WZ_P26_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P7_com$gam_WZ = gam_pine_2017_WZ_P7_com$fitted.values
      }
      #MZ
      {
        gam_pine_2017_MZ_P16_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P16_com)
        gam_pine_2017_MZ_P20_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P20_com)
        gam_pine_2017_MZ_P21_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P21_com)
        gam_pine_2017_MZ_P25_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P25_com)
        gam_pine_2017_MZ_P26_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P26_com)
        gam_pine_2017_MZ_P7_com <- gam(MZ ~ s(DY), data=HF2017_pine_mean.sdd_filled1_P7_com)
        HF2017_pine_mean.sdd_filled1_P16_com$gam_MZ = gam_pine_2017_MZ_P16_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P20_com$gam_MZ = gam_pine_2017_MZ_P20_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P21_com$gam_MZ = gam_pine_2017_MZ_P21_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P25_com$gam_MZ = gam_pine_2017_MZ_P25_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P26_com$gam_MZ = gam_pine_2017_MZ_P26_com$fitted.values
        HF2017_pine_mean.sdd_filled1_P7_com$gam_MZ = gam_pine_2017_MZ_P7_com$fitted.values
      }
      #WMZ
      {
        HF2017_pine_mean.sdd_filled1_P16_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P20_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P21_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P25_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P26_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P7_com$gam_WMZ = HF2017_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      #EWMZ
      {
        HF2017_pine_mean.sdd_filled1_P16_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P16_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P20_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P20_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P21_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P21_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P25_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P25_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P26_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P26_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2017_pine_mean.sdd_filled1_P7_com$gam_EWMZ = HF2017_pine_mean.sdd_filled1_P7_com$gam_EZ+HF2017_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2017_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_pine_2018_CZ_P16_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16_com)
        gam_pine_2018_CZ_P20_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20_com)
        gam_pine_2018_CZ_P21_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21_com)
        gam_pine_2018_CZ_P25_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25_com)
        gam_pine_2018_CZ_P26_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26_com)
        gam_pine_2018_CZ_P7_com <- gam(CZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7_com)
        HF2018_pine_mean.sdd_filled1_P16_com$gam_CZ = gam_pine_2018_CZ_P16_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P20_com$gam_CZ = gam_pine_2018_CZ_P20_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P21_com$gam_CZ = gam_pine_2018_CZ_P21_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P25_com$gam_CZ = gam_pine_2018_CZ_P25_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P26_com$gam_CZ = gam_pine_2018_CZ_P26_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P7_com$gam_CZ = gam_pine_2018_CZ_P7_com$fitted.values
      }
      #EZ
      {
        gam_pine_2018_EZ_P16_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16_com)
        gam_pine_2018_EZ_P20_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20_com)
        gam_pine_2018_EZ_P21_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21_com)
        gam_pine_2018_EZ_P25_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25_com)
        gam_pine_2018_EZ_P26_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26_com)
        gam_pine_2018_EZ_P7_com <- gam(EZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7_com)
        HF2018_pine_mean.sdd_filled1_P16_com$gam_EZ = gam_pine_2018_EZ_P16_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P20_com$gam_EZ = gam_pine_2018_EZ_P20_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P21_com$gam_EZ = gam_pine_2018_EZ_P21_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P25_com$gam_EZ = gam_pine_2018_EZ_P25_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P26_com$gam_EZ = gam_pine_2018_EZ_P26_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P7_com$gam_EZ = gam_pine_2018_EZ_P7_com$fitted.values
      }
      #WZ
      {
        gam_pine_2018_WZ_P16_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16_com)
        gam_pine_2018_WZ_P20_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20_com)
        gam_pine_2018_WZ_P21_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21_com)
        gam_pine_2018_WZ_P25_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25_com)
        gam_pine_2018_WZ_P26_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26_com)
        gam_pine_2018_WZ_P7_com <- gam(WZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7_com)
        HF2018_pine_mean.sdd_filled1_P16_com$gam_WZ = gam_pine_2018_WZ_P16_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P20_com$gam_WZ = gam_pine_2018_WZ_P20_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P21_com$gam_WZ = gam_pine_2018_WZ_P21_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P25_com$gam_WZ = gam_pine_2018_WZ_P25_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P26_com$gam_WZ = gam_pine_2018_WZ_P26_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P7_com$gam_WZ = gam_pine_2018_WZ_P7_com$fitted.values
      }
      #MZ
      {
        gam_pine_2018_MZ_P16_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P16_com)
        gam_pine_2018_MZ_P20_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P20_com)
        gam_pine_2018_MZ_P21_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P21_com)
        gam_pine_2018_MZ_P25_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P25_com)
        gam_pine_2018_MZ_P26_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P26_com)
        gam_pine_2018_MZ_P7_com <- gam(MZ ~ s(DY), data=HF2018_pine_mean.sdd_filled1_P7_com)
        HF2018_pine_mean.sdd_filled1_P16_com$gam_MZ = gam_pine_2018_MZ_P16_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P20_com$gam_MZ = gam_pine_2018_MZ_P20_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P21_com$gam_MZ = gam_pine_2018_MZ_P21_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P25_com$gam_MZ = gam_pine_2018_MZ_P25_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P26_com$gam_MZ = gam_pine_2018_MZ_P26_com$fitted.values
        HF2018_pine_mean.sdd_filled1_P7_com$gam_MZ = gam_pine_2018_MZ_P7_com$fitted.values
      }
      #WMZ
      {
        
        HF2018_pine_mean.sdd_filled1_P16_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P20_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P21_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P25_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P26_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P7_com$gam_WMZ = HF2018_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      #EWMZ
      {
        HF2018_pine_mean.sdd_filled1_P16_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P16_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P20_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P20_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P21_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P21_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P25_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P25_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P26_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P26_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2018_pine_mean.sdd_filled1_P7_com$gam_EWMZ = HF2018_pine_mean.sdd_filled1_P7_com$gam_EZ+HF2018_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2018_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      
      #2019
      #CZ
      {
        gam_pine_2019_CZ_P16_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16_com)
        gam_pine_2019_CZ_P20_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20_com)
        gam_pine_2019_CZ_P21_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21_com)
        gam_pine_2019_CZ_P25_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25_com)
        gam_pine_2019_CZ_P26_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26_com)
        gam_pine_2019_CZ_P7_com <- gam(CZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7_com)
        HF2019_pine_mean.sdd_filled1_P16_com$gam_CZ = gam_pine_2019_CZ_P16_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P20_com$gam_CZ = gam_pine_2019_CZ_P20_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P21_com$gam_CZ = gam_pine_2019_CZ_P21_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P25_com$gam_CZ = gam_pine_2019_CZ_P25_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P26_com$gam_CZ = gam_pine_2019_CZ_P26_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P7_com$gam_CZ = gam_pine_2019_CZ_P7_com$fitted.values
      }
      #EZ
      {
        gam_pine_2019_EZ_P16_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16_com)
        gam_pine_2019_EZ_P20_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20_com)
        gam_pine_2019_EZ_P21_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21_com)
        gam_pine_2019_EZ_P25_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25_com)
        gam_pine_2019_EZ_P26_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26_com)
        gam_pine_2019_EZ_P7_com <- gam(EZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7_com)
        HF2019_pine_mean.sdd_filled1_P16_com$gam_EZ = gam_pine_2019_EZ_P16_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P20_com$gam_EZ = gam_pine_2019_EZ_P20_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P21_com$gam_EZ = gam_pine_2019_EZ_P21_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P25_com$gam_EZ = gam_pine_2019_EZ_P25_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P26_com$gam_EZ = gam_pine_2019_EZ_P26_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P7_com$gam_EZ = gam_pine_2019_EZ_P7_com$fitted.values
      }
      #WZ
      {
        gam_pine_2019_WZ_P16_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16_com)
        gam_pine_2019_WZ_P20_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20_com)
        gam_pine_2019_WZ_P21_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21_com)
        gam_pine_2019_WZ_P25_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25_com)
        gam_pine_2019_WZ_P26_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26_com)
        gam_pine_2019_WZ_P7_com <- gam(WZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7_com)
        HF2019_pine_mean.sdd_filled1_P16_com$gam_WZ = gam_pine_2019_WZ_P16_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P20_com$gam_WZ = gam_pine_2019_WZ_P20_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P21_com$gam_WZ = gam_pine_2019_WZ_P21_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P25_com$gam_WZ = gam_pine_2019_WZ_P25_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P26_com$gam_WZ = gam_pine_2019_WZ_P26_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P7_com$gam_WZ = gam_pine_2019_WZ_P7_com$fitted.values
      }
      #MZ
      {
        gam_pine_2019_MZ_P16_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P16_com)
        gam_pine_2019_MZ_P20_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P20_com)
        gam_pine_2019_MZ_P21_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P21_com)
        gam_pine_2019_MZ_P25_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P25_com)
        gam_pine_2019_MZ_P26_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P26_com)
        gam_pine_2019_MZ_P7_com <- gam(MZ ~ s(DY), data=HF2019_pine_mean.sdd_filled1_P7_com)
        HF2019_pine_mean.sdd_filled1_P16_com$gam_MZ = gam_pine_2019_MZ_P16_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P20_com$gam_MZ = gam_pine_2019_MZ_P20_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P21_com$gam_MZ = gam_pine_2019_MZ_P21_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P25_com$gam_MZ = gam_pine_2019_MZ_P25_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P26_com$gam_MZ = gam_pine_2019_MZ_P26_com$fitted.values
        HF2019_pine_mean.sdd_filled1_P7_com$gam_MZ = gam_pine_2019_MZ_P7_com$fitted.values
      }
      #WMZ
      {
        
        HF2019_pine_mean.sdd_filled1_P16_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P20_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P21_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P25_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P26_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P7_com$gam_WMZ = HF2019_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      #EWMZ
      {
        HF2019_pine_mean.sdd_filled1_P16_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P16_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P16_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P16_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P20_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P20_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P20_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P20_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P21_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P21_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P21_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P21_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P25_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P25_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P25_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P25_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P26_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P26_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P26_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P26_com$gam_MZ
        HF2019_pine_mean.sdd_filled1_P7_com$gam_EWMZ = HF2019_pine_mean.sdd_filled1_P7_com$gam_EZ+HF2019_pine_mean.sdd_filled1_P7_com$gam_WZ+HF2019_pine_mean.sdd_filled1_P7_com$gam_MZ
      }
      
    }
    #ACRU
    {
      #2017
      #CZ
      {
        gam_maple_2017_CZ_A18_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18_com)
        gam_maple_2017_CZ_A19_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19_com)
        gam_maple_2017_CZ_A23_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23_com)
        gam_maple_2017_CZ_A28_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28_com)
        gam_maple_2017_CZ_A29_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29_com)
        gam_maple_2017_CZ_A8_com <- gam(CZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8_com)
        HF2017_maple_mean.sdd_filled1_A18_com$gam_CZ = gam_maple_2017_CZ_A18_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A19_com$gam_CZ = gam_maple_2017_CZ_A19_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A23_com$gam_CZ = gam_maple_2017_CZ_A23_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A28_com$gam_CZ = gam_maple_2017_CZ_A28_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A29_com$gam_CZ = gam_maple_2017_CZ_A29_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A8_com$gam_CZ = gam_maple_2017_CZ_A8_com$fitted.values
      }
      #EZ
      {
        gam_maple_2017_EZ_A18_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18_com)
        gam_maple_2017_EZ_A19_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19_com)
        gam_maple_2017_EZ_A23_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23_com)
        gam_maple_2017_EZ_A28_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28_com)
        gam_maple_2017_EZ_A29_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29_com)
        gam_maple_2017_EZ_A8_com <- gam(EZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8_com)
        HF2017_maple_mean.sdd_filled1_A18_com$gam_EZ = gam_maple_2017_EZ_A18_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A19_com$gam_EZ = gam_maple_2017_EZ_A19_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A23_com$gam_EZ = gam_maple_2017_EZ_A23_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A28_com$gam_EZ = gam_maple_2017_EZ_A28_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A29_com$gam_EZ = gam_maple_2017_EZ_A29_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A8_com$gam_EZ = gam_maple_2017_EZ_A8_com$fitted.values
      }
      #WZ
      {
        gam_maple_2017_WZ_A18_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18_com)
        gam_maple_2017_WZ_A19_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19_com)
        gam_maple_2017_WZ_A23_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23_com)
        gam_maple_2017_WZ_A28_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28_com)
        gam_maple_2017_WZ_A29_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29_com)
        gam_maple_2017_WZ_A8_com <- gam(WZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8_com)
        HF2017_maple_mean.sdd_filled1_A18_com$gam_WZ = gam_maple_2017_WZ_A18_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A19_com$gam_WZ = gam_maple_2017_WZ_A19_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A23_com$gam_WZ = gam_maple_2017_WZ_A23_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A28_com$gam_WZ = gam_maple_2017_WZ_A28_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A29_com$gam_WZ = gam_maple_2017_WZ_A29_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A8_com$gam_WZ = gam_maple_2017_WZ_A8_com$fitted.values
      }
      #MZ
      {
        gam_maple_2017_MZ_A18_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A18_com)
        gam_maple_2017_MZ_A19_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A19_com)
        gam_maple_2017_MZ_A23_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A23_com)
        gam_maple_2017_MZ_A28_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A28_com)
        gam_maple_2017_MZ_A29_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A29_com)
        gam_maple_2017_MZ_A8_com <- gam(MZ ~ s(DY), data=HF2017_maple_mean.sdd_filled1_A8_com)
        HF2017_maple_mean.sdd_filled1_A18_com$gam_MZ = gam_maple_2017_MZ_A18_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A19_com$gam_MZ = gam_maple_2017_MZ_A19_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A23_com$gam_MZ = gam_maple_2017_MZ_A23_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A28_com$gam_MZ = gam_maple_2017_MZ_A28_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A29_com$gam_MZ = gam_maple_2017_MZ_A29_com$fitted.values
        HF2017_maple_mean.sdd_filled1_A8_com$gam_MZ = gam_maple_2017_MZ_A8_com$fitted.values
      }
      #WMZ
      {
        HF2017_maple_mean.sdd_filled1_A18_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A19_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A23_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A28_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A29_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A8_com$gam_WMZ = HF2017_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      #EWMZ
      {
        HF2017_maple_mean.sdd_filled1_A18_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A18_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A19_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A19_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A23_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A23_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A28_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A28_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A29_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A29_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2017_maple_mean.sdd_filled1_A8_com$gam_EWMZ = HF2017_maple_mean.sdd_filled1_A8_com$gam_EZ +HF2017_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2017_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_maple_2018_CZ_A18_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18_com)
        gam_maple_2018_CZ_A19_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19_com)
        gam_maple_2018_CZ_A23_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23_com)
        gam_maple_2018_CZ_A28_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28_com)
        gam_maple_2018_CZ_A29_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29_com)
        gam_maple_2018_CZ_A8_com <- gam(CZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8_com)
        HF2018_maple_mean.sdd_filled1_A18_com$gam_CZ = gam_maple_2018_CZ_A18_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A19_com$gam_CZ = gam_maple_2018_CZ_A19_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A23_com$gam_CZ = gam_maple_2018_CZ_A23_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A28_com$gam_CZ = gam_maple_2018_CZ_A28_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A29_com$gam_CZ = gam_maple_2018_CZ_A29_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A8_com$gam_CZ = gam_maple_2018_CZ_A8_com$fitted.values
      }
      #EZ
      {
        gam_maple_2018_EZ_A18_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18_com)
        gam_maple_2018_EZ_A19_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19_com)
        gam_maple_2018_EZ_A23_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23_com)
        gam_maple_2018_EZ_A28_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28_com)
        gam_maple_2018_EZ_A29_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29_com)
        gam_maple_2018_EZ_A8_com <- gam(EZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8_com)
        HF2018_maple_mean.sdd_filled1_A18_com$gam_EZ = gam_maple_2018_EZ_A18_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A19_com$gam_EZ = gam_maple_2018_EZ_A19_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A23_com$gam_EZ = gam_maple_2018_EZ_A23_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A28_com$gam_EZ = gam_maple_2018_EZ_A28_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A29_com$gam_EZ = gam_maple_2018_EZ_A29_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A8_com$gam_EZ = gam_maple_2018_EZ_A8_com$fitted.values
      }
      #WZ
      {
        gam_maple_2018_WZ_A18_com <- gam(log(WZ) ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18_com,subse = WZ >0)
        
        gam_maple_2018_WZ_A19_com <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19_com)
        gam_maple_2018_WZ_A23_com <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23_com)
        gam_maple_2018_WZ_A28_com <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28_com)
        gam_maple_2018_WZ_A29_com <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29_com)
        gam_maple_2018_WZ_A8_com <- gam(WZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8_com)
        HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ = gam_maple_2018_WZ_A18_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A19_com$gam_WZ = gam_maple_2018_WZ_A19_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A23_com$gam_WZ = gam_maple_2018_WZ_A23_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A28_com$gam_WZ = gam_maple_2018_WZ_A28_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A29_com$gam_WZ = gam_maple_2018_WZ_A29_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A8_com$gam_WZ = gam_maple_2018_WZ_A8_com$fitted.values
        #get output from gam_maple_2018_WZ_A18_com as only values >0 were used to fit gam
        HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ = c(0,0,0,0,0,2.718**2.792908,2.718**3.740206,2.718**4.599893,2.718**5.054938,2.718**5.034167,2.718**4.729764,0,2.718**4.243162,2.718**4.325822,2.718**4.833647,2.718**5.011378,0,0,0,0,0,0,0,0)
      }
      #MZ
      {
        gam_maple_2018_MZ_A18_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A18_com)
        gam_maple_2018_MZ_A19_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A19_com)
        gam_maple_2018_MZ_A23_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A23_com)
        gam_maple_2018_MZ_A28_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A28_com)
        gam_maple_2018_MZ_A29_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A29_com)
        gam_maple_2018_MZ_A8_com <- gam(MZ ~ s(DY), data=HF2018_maple_mean.sdd_filled1_A8_com)
        HF2018_maple_mean.sdd_filled1_A18_com$gam_MZ = gam_maple_2018_MZ_A18_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A19_com$gam_MZ = gam_maple_2018_MZ_A19_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A23_com$gam_MZ = gam_maple_2018_MZ_A23_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A28_com$gam_MZ = gam_maple_2018_MZ_A28_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A29_com$gam_MZ = gam_maple_2018_MZ_A29_com$fitted.values
        HF2018_maple_mean.sdd_filled1_A8_com$gam_MZ = gam_maple_2018_MZ_A8_com$fitted.values
      }
      #WMZ
      {
        HF2018_maple_mean.sdd_filled1_A18_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A19_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A23_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A28_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A29_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A8_com$gam_WMZ = HF2018_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      #EWMZ
      {
        HF2018_maple_mean.sdd_filled1_A18_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A18_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A19_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A19_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A23_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A23_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A28_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A28_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A29_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A29_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2018_maple_mean.sdd_filled1_A8_com$gam_EWMZ = HF2018_maple_mean.sdd_filled1_A8_com$gam_EZ +HF2018_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2018_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      
      #2019
      #CZ
      {
        gam_maple_2019_CZ_A18_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18_com)
        gam_maple_2019_CZ_A19_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19_com)
        gam_maple_2019_CZ_A23_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23_com)
        gam_maple_2019_CZ_A28_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28_com)
        gam_maple_2019_CZ_A29_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29_com)
        gam_maple_2019_CZ_A8_com <- gam(CZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8_com)
        HF2019_maple_mean.sdd_filled1_A18_com$gam_CZ = gam_maple_2019_CZ_A18_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A19_com$gam_CZ = gam_maple_2019_CZ_A19_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A23_com$gam_CZ = gam_maple_2019_CZ_A23_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A28_com$gam_CZ = gam_maple_2019_CZ_A28_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A29_com$gam_CZ = gam_maple_2019_CZ_A29_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A8_com$gam_CZ = gam_maple_2019_CZ_A8_com$fitted.values
      }
      #EZ
      {
        gam_maple_2019_EZ_A18_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18_com)
        gam_maple_2019_EZ_A19_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19_com)
        gam_maple_2019_EZ_A23_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23_com)
        gam_maple_2019_EZ_A28_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28_com)
        gam_maple_2019_EZ_A29_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29_com)
        gam_maple_2019_EZ_A8_com <- gam(EZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8_com)
        HF2019_maple_mean.sdd_filled1_A18_com$gam_EZ = gam_maple_2019_EZ_A18_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A19_com$gam_EZ = gam_maple_2019_EZ_A19_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A23_com$gam_EZ = gam_maple_2019_EZ_A23_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A28_com$gam_EZ = gam_maple_2019_EZ_A28_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A29_com$gam_EZ = gam_maple_2019_EZ_A29_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A8_com$gam_EZ = gam_maple_2019_EZ_A8_com$fitted.values
      }
      #WZ
      {
        gam_maple_2019_WZ_A18_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18_com)
        gam_maple_2019_WZ_A19_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19_com)
        gam_maple_2019_WZ_A23_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23_com)
        gam_maple_2019_WZ_A28_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28_com)
        gam_maple_2019_WZ_A29_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29_com)
        gam_maple_2019_WZ_A8_com <- gam(WZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8_com)
        HF2019_maple_mean.sdd_filled1_A18_com$gam_WZ = gam_maple_2019_WZ_A18_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A19_com$gam_WZ = gam_maple_2019_WZ_A19_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A23_com$gam_WZ = gam_maple_2019_WZ_A23_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A28_com$gam_WZ = gam_maple_2019_WZ_A28_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A29_com$gam_WZ = gam_maple_2019_WZ_A29_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A8_com$gam_WZ = gam_maple_2019_WZ_A8_com$fitted.values
      }
      #MZ
      {
        gam_maple_2019_MZ_A18_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A18_com)
        gam_maple_2019_MZ_A19_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A19_com)
        gam_maple_2019_MZ_A23_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A23_com)
        gam_maple_2019_MZ_A28_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A28_com)
        gam_maple_2019_MZ_A29_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A29_com)
        gam_maple_2019_MZ_A8_com <- gam(MZ ~ s(DY), data=HF2019_maple_mean.sdd_filled1_A8_com)
        HF2019_maple_mean.sdd_filled1_A18_com$gam_MZ = gam_maple_2019_MZ_A18_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A19_com$gam_MZ = gam_maple_2019_MZ_A19_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A23_com$gam_MZ = gam_maple_2019_MZ_A23_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A28_com$gam_MZ = gam_maple_2019_MZ_A28_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A29_com$gam_MZ = gam_maple_2019_MZ_A29_com$fitted.values
        HF2019_maple_mean.sdd_filled1_A8_com$gam_MZ = gam_maple_2019_MZ_A8_com$fitted.values
      }
      #WMZ
      {
        HF2019_maple_mean.sdd_filled1_A18_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A19_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A23_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A28_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A29_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A8_com$gam_WMZ = HF2019_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      #EWMZ
      {
        HF2019_maple_mean.sdd_filled1_A18_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A18_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A18_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A18_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A19_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A19_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A19_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A19_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A23_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A23_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A23_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A23_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A28_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A28_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A28_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A28_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A29_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A29_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A29_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A29_com$gam_MZ
        HF2019_maple_mean.sdd_filled1_A8_com$gam_EWMZ = HF2019_maple_mean.sdd_filled1_A8_com$gam_EZ +HF2019_maple_mean.sdd_filled1_A8_com$gam_WZ + HF2019_maple_mean.sdd_filled1_A8_com$gam_MZ
      }
      
      
    }
    #QURU
    {
      #CZ
      {
        gam_oak_2017_CZ_Q05_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2017_CZ_Q06_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2017_CZ_Q09_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2017_CZ_Q10_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2017_CZ_Q11_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2017_CZ_Q12_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2017_CZ_Q13_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2017_CZ_Q15_com <- gam(CZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15_com)
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_CZ = gam_oak_2017_CZ_Q05_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_CZ = gam_oak_2017_CZ_Q06_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_CZ = gam_oak_2017_CZ_Q09_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_CZ = gam_oak_2017_CZ_Q10_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_CZ = gam_oak_2017_CZ_Q11_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_CZ = gam_oak_2017_CZ_Q12_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_CZ = gam_oak_2017_CZ_Q13_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_CZ = gam_oak_2017_CZ_Q15_com$fitted.values
      }
      #EZ
      {
        gam_oak_2017_EZ_Q05_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2017_EZ_Q06_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2017_EZ_Q09_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2017_EZ_Q10_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2017_EZ_Q11_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2017_EZ_Q12_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2017_EZ_Q13_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2017_EZ_Q15_com <- gam(EZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15_com)
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_EZ = gam_oak_2017_EZ_Q05_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_EZ = gam_oak_2017_EZ_Q06_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_EZ = gam_oak_2017_EZ_Q09_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_EZ = gam_oak_2017_EZ_Q10_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_EZ = gam_oak_2017_EZ_Q11_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_EZ = gam_oak_2017_EZ_Q12_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_EZ = gam_oak_2017_EZ_Q13_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_EZ = gam_oak_2017_EZ_Q15_com$fitted.values
      }
      #WZ
      {
        gam_oak_2017_WZ_Q05_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2017_WZ_Q06_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2017_WZ_Q09_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2017_WZ_Q10_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2017_WZ_Q11_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2017_WZ_Q12_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2017_WZ_Q13_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2017_WZ_Q15_com <- gam(WZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15_com)
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_WZ = gam_oak_2017_WZ_Q05_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_WZ = gam_oak_2017_WZ_Q06_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_WZ = gam_oak_2017_WZ_Q09_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_WZ = gam_oak_2017_WZ_Q10_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_WZ = gam_oak_2017_WZ_Q11_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_WZ = gam_oak_2017_WZ_Q12_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_WZ = gam_oak_2017_WZ_Q13_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_WZ = gam_oak_2017_WZ_Q15_com$fitted.values
      }
      #MZ
      {
        gam_oak_2017_MZ_Q05_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2017_MZ_Q06_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2017_MZ_Q09_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2017_MZ_Q10_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2017_MZ_Q11_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2017_MZ_Q12_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2017_MZ_Q13_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2017_MZ_Q15_com <- gam(MZ ~ s(DY), data=HF2017_oak_mean.sdd_filled1_Q15_com)
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_MZ = gam_oak_2017_MZ_Q05_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_MZ = gam_oak_2017_MZ_Q06_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_MZ = gam_oak_2017_MZ_Q09_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_MZ = gam_oak_2017_MZ_Q10_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_MZ = gam_oak_2017_MZ_Q11_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_MZ = gam_oak_2017_MZ_Q12_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_MZ = gam_oak_2017_MZ_Q13_com$fitted.values
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_MZ = gam_oak_2017_MZ_Q15_com$fitted.values
      }
      #WMZ
      {
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_WMZ = HF2017_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      #EWMZ
      {
        HF2017_oak_mean.sdd_filled1_Q05_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q05_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q06_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q06_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q09_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q09_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q10_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q10_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q11_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q11_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q12_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q12_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q13_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q13_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2017_oak_mean.sdd_filled1_Q15_com$gam_EWMZ = HF2017_oak_mean.sdd_filled1_Q15_com$gam_EZ +HF2017_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2017_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      
      #2018
      #CZ
      {
        gam_oak_2018_CZ_Q05_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2018_CZ_Q06_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2018_CZ_Q09_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2018_CZ_Q10_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2018_CZ_Q11_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2018_CZ_Q12_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2018_CZ_Q13_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2018_CZ_Q15_com <- gam(CZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15_com)
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_CZ = gam_oak_2018_CZ_Q05_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_CZ = gam_oak_2018_CZ_Q06_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_CZ = gam_oak_2018_CZ_Q09_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_CZ = gam_oak_2018_CZ_Q10_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_CZ = gam_oak_2018_CZ_Q11_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_CZ = gam_oak_2018_CZ_Q12_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_CZ = gam_oak_2018_CZ_Q13_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_CZ = gam_oak_2018_CZ_Q15_com$fitted.values
      }
      #EZ
      {
        gam_oak_2018_EZ_Q05_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2018_EZ_Q06_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2018_EZ_Q09_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2018_EZ_Q10_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2018_EZ_Q11_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2018_EZ_Q12_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2018_EZ_Q13_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2018_EZ_Q15_com <- gam(EZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15_com)
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_EZ = gam_oak_2018_EZ_Q05_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_EZ = gam_oak_2018_EZ_Q06_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_EZ = gam_oak_2018_EZ_Q09_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_EZ = gam_oak_2018_EZ_Q10_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_EZ = gam_oak_2018_EZ_Q11_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_EZ = gam_oak_2018_EZ_Q12_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_EZ = gam_oak_2018_EZ_Q13_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_EZ = gam_oak_2018_EZ_Q15_com$fitted.values
      }
      #WZ
      {
        gam_oak_2018_WZ_Q05_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2018_WZ_Q06_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2018_WZ_Q09_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2018_WZ_Q10_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2018_WZ_Q11_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2018_WZ_Q12_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2018_WZ_Q13_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2018_WZ_Q15_com <- gam(WZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15_com)
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_WZ = gam_oak_2018_WZ_Q05_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_WZ = gam_oak_2018_WZ_Q06_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_WZ = gam_oak_2018_WZ_Q09_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_WZ = gam_oak_2018_WZ_Q10_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_WZ = gam_oak_2018_WZ_Q11_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_WZ = gam_oak_2018_WZ_Q12_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_WZ = gam_oak_2018_WZ_Q13_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_WZ = gam_oak_2018_WZ_Q15_com$fitted.values
      }
      #MZ
      {
        gam_oak_2018_MZ_Q05_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2018_MZ_Q06_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2018_MZ_Q09_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2018_MZ_Q10_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2018_MZ_Q11_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2018_MZ_Q12_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2018_MZ_Q13_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2018_MZ_Q15_com <- gam(MZ ~ s(DY), data=HF2018_oak_mean.sdd_filled1_Q15_com)
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_MZ = gam_oak_2018_MZ_Q05_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_MZ = gam_oak_2018_MZ_Q06_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_MZ = gam_oak_2018_MZ_Q09_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_MZ = gam_oak_2018_MZ_Q10_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_MZ = gam_oak_2018_MZ_Q11_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_MZ = gam_oak_2018_MZ_Q12_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_MZ = gam_oak_2018_MZ_Q13_com$fitted.values
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_MZ = gam_oak_2018_MZ_Q15_com$fitted.values
      }
      #WMZ
      {
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_WMZ = HF2018_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      #EWMZ
      {
        HF2018_oak_mean.sdd_filled1_Q05_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q05_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q06_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q06_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q09_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q09_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q10_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q10_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q11_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q11_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q12_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q12_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q13_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q13_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2018_oak_mean.sdd_filled1_Q15_com$gam_EWMZ = HF2018_oak_mean.sdd_filled1_Q15_com$gam_EZ +HF2018_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2018_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      #2019
      #CZ
      {
        gam_oak_2019_CZ_Q05_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2019_CZ_Q06_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2019_CZ_Q09_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2019_CZ_Q10_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2019_CZ_Q11_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2019_CZ_Q12_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2019_CZ_Q13_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2019_CZ_Q15_com <- gam(CZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15_com)
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_CZ = gam_oak_2019_CZ_Q05_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_CZ = gam_oak_2019_CZ_Q06_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_CZ = gam_oak_2019_CZ_Q09_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_CZ = gam_oak_2019_CZ_Q10_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_CZ = gam_oak_2019_CZ_Q11_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_CZ = gam_oak_2019_CZ_Q12_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_CZ = gam_oak_2019_CZ_Q13_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_CZ = gam_oak_2019_CZ_Q15_com$fitted.values
      }
      #EZ
      {
        gam_oak_2019_EZ_Q05_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2019_EZ_Q06_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2019_EZ_Q09_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2019_EZ_Q10_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2019_EZ_Q11_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2019_EZ_Q12_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2019_EZ_Q13_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2019_EZ_Q15_com <- gam(EZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15_com)
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_EZ = gam_oak_2019_EZ_Q05_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_EZ = gam_oak_2019_EZ_Q06_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_EZ = gam_oak_2019_EZ_Q09_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_EZ = gam_oak_2019_EZ_Q10_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_EZ = gam_oak_2019_EZ_Q11_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_EZ = gam_oak_2019_EZ_Q12_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_EZ = gam_oak_2019_EZ_Q13_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_EZ = gam_oak_2019_EZ_Q15_com$fitted.values
      }
      #WZ
      {
        gam_oak_2019_WZ_Q05_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2019_WZ_Q06_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2019_WZ_Q09_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2019_WZ_Q10_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2019_WZ_Q11_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2019_WZ_Q12_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2019_WZ_Q13_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2019_WZ_Q15_com <- gam(WZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15_com)
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_WZ = gam_oak_2019_WZ_Q05_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_WZ = gam_oak_2019_WZ_Q06_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_WZ = gam_oak_2019_WZ_Q09_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_WZ = gam_oak_2019_WZ_Q10_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_WZ = gam_oak_2019_WZ_Q11_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_WZ = gam_oak_2019_WZ_Q12_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_WZ = gam_oak_2019_WZ_Q13_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_WZ = gam_oak_2019_WZ_Q15_com$fitted.values
      }
      #MZ
      {
        gam_oak_2019_MZ_Q05_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q05_com)
        gam_oak_2019_MZ_Q06_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q06_com)
        gam_oak_2019_MZ_Q09_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q09_com)
        gam_oak_2019_MZ_Q10_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q10_com)
        gam_oak_2019_MZ_Q11_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q11_com)
        gam_oak_2019_MZ_Q12_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q12_com)
        gam_oak_2019_MZ_Q13_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q13_com)
        gam_oak_2019_MZ_Q15_com <- gam(MZ ~ s(DY), data=HF2019_oak_mean.sdd_filled1_Q15_com)
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_MZ = gam_oak_2019_MZ_Q05_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_MZ = gam_oak_2019_MZ_Q06_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_MZ = gam_oak_2019_MZ_Q09_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_MZ = gam_oak_2019_MZ_Q10_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_MZ = gam_oak_2019_MZ_Q11_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_MZ = gam_oak_2019_MZ_Q12_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_MZ = gam_oak_2019_MZ_Q13_com$fitted.values
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_MZ = gam_oak_2019_MZ_Q15_com$fitted.values
      }
      #WMZ
      {
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_WMZ = HF2019_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      #EWMZ
      {
        HF2019_oak_mean.sdd_filled1_Q05_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q05_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q05_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q05_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q06_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q06_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q06_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q06_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q09_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q09_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q09_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q09_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q10_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q10_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q10_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q10_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q11_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q11_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q11_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q11_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q12_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q12_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q12_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q12_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q13_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q13_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q13_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q13_com$gam_MZ
        HF2019_oak_mean.sdd_filled1_Q15_com$gam_EWMZ = HF2019_oak_mean.sdd_filled1_Q15_com$gam_EZ +HF2019_oak_mean.sdd_filled1_Q15_com$gam_WZ +HF2019_oak_mean.sdd_filled1_Q15_com$gam_MZ
      }
      
    }  
  }
  
  #calculate the dcell for each tree
  #here only complete data is calculated
  {
    #no cut
    {
      #pine 2017
      {
        #P16
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P16_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P16_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P16_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P16_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P16_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P16_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P16_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P16_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P16_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P16_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P16_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P16_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P16_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P16_com$dgam_MZ_mean = dgam_MZ
        }
        #P20
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P20_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P20_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P20_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P20_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P20_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P20_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P20_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P20_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P20_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P20_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P20_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P20_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P20_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P20_com$dgam_MZ_mean = dgam_MZ
        }
        #P21
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P21_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P21_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P21_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P21_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P21_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P21_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P21_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P21_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P21_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P21_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P21_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P21_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P21_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P21_com$dgam_MZ_mean = dgam_MZ
        }
        #P25
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P25_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P25_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P25_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P25_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P25_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P25_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P25_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P25_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P25_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P25_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P25_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P25_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P25_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P25_com$dgam_MZ_mean = dgam_MZ
        }
        #P26
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P26_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P26_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P26_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P26_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P26_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P26_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P26_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P26_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P26_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P26_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P26_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P26_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P26_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P26_com$dgam_MZ_mean = dgam_MZ
        }
        #P7
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_pine_mean.sdd_filled1_P7_com$DY)-1)){
            dcell[i+1] = HF2017_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i+1]-HF2017_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_pine_mean.sdd_filled1_P7_com$gam_CZ[i+1]-HF2017_pine_mean.sdd_filled1_P7_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_pine_mean.sdd_filled1_P7_com$gam_EZ[i+1]-HF2017_pine_mean.sdd_filled1_P7_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_pine_mean.sdd_filled1_P7_com$gam_WZ[i+1]-HF2017_pine_mean.sdd_filled1_P7_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_pine_mean.sdd_filled1_P7_com$gam_MZ[i+1]-HF2017_pine_mean.sdd_filled1_P7_com$gam_MZ[i]
          }
          
          HF2017_pine_mean.sdd_filled1_P7_com$dcell = dcell
          HF2017_pine_mean.sdd_filled1_P7_com$dgam_CZ_mean = dgam_CZ
          HF2017_pine_mean.sdd_filled1_P7_com$dgam_EZ_mean = dgam_EZ
          HF2017_pine_mean.sdd_filled1_P7_com$dgam_WZ_mean = dgam_WZ
          HF2017_pine_mean.sdd_filled1_P7_com$dgam_MZ_mean = dgam_MZ
        }       
      }
      
      #pine 2018
      {
        #P16
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P16_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P16_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P16_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P16_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P16_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P16_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P16_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P16_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P16_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P16_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P16_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P16_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P16_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P16_com$dgam_MZ_mean = dgam_MZ
        }
        #P20
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P20_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P20_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P20_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P20_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P20_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P20_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P20_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P20_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P20_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P20_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P20_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P20_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P20_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P20_com$dgam_MZ_mean = dgam_MZ
        }
        #P21
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P21_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P21_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P21_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P21_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P21_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P21_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P21_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P21_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P21_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P21_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P21_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P21_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P21_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P21_com$dgam_MZ_mean = dgam_MZ
        }
        #P25
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P25_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P25_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P25_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P25_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P25_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P25_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P25_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P25_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P25_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P25_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P25_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P25_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P25_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P25_com$dgam_MZ_mean = dgam_MZ
        }
        #P26
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P26_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P26_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P26_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P26_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P26_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P26_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P26_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P26_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P26_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P26_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P26_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P26_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P26_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P26_com$dgam_MZ_mean = dgam_MZ
        }
        #P7
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_pine_mean.sdd_filled1_P7_com$DY)-1)){
            dcell[i+1] = HF2018_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i+1]-HF2018_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_pine_mean.sdd_filled1_P7_com$gam_CZ[i+1]-HF2018_pine_mean.sdd_filled1_P7_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_pine_mean.sdd_filled1_P7_com$gam_EZ[i+1]-HF2018_pine_mean.sdd_filled1_P7_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_pine_mean.sdd_filled1_P7_com$gam_WZ[i+1]-HF2018_pine_mean.sdd_filled1_P7_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_pine_mean.sdd_filled1_P7_com$gam_MZ[i+1]-HF2018_pine_mean.sdd_filled1_P7_com$gam_MZ[i]
          }
          
          HF2018_pine_mean.sdd_filled1_P7_com$dcell = dcell
          HF2018_pine_mean.sdd_filled1_P7_com$dgam_CZ_mean = dgam_CZ
          HF2018_pine_mean.sdd_filled1_P7_com$dgam_EZ_mean = dgam_EZ
          HF2018_pine_mean.sdd_filled1_P7_com$dgam_WZ_mean = dgam_WZ
          HF2018_pine_mean.sdd_filled1_P7_com$dgam_MZ_mean = dgam_MZ
        }        
      }
      
      #pine 2019
      {
        #P16
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P16_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P16_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P16_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P16_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P16_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P16_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P16_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P16_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P16_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P16_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P16_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P16_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P16_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P16_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P16_com$dgam_MZ_mean = dgam_MZ
        }
        #P20
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P20_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P20_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P20_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P20_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P20_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P20_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P20_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P20_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P20_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P20_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P20_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P20_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P20_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P20_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P20_com$dgam_MZ_mean = dgam_MZ
        }
        #P21
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P21_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P21_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P21_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P21_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P21_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P21_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P21_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P21_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P21_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P21_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P21_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P21_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P21_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P21_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P21_com$dgam_MZ_mean = dgam_MZ
        }
        #P25
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P25_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P25_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P25_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P25_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P25_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P25_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P25_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P25_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P25_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P25_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P25_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P25_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P25_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P25_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P25_com$dgam_MZ_mean = dgam_MZ
        }
        #P26
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P26_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P26_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P26_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P26_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P26_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P26_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P26_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P26_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P26_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P26_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P26_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P26_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P26_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P26_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P26_com$dgam_MZ_mean = dgam_MZ
        }
        #P7
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_pine_mean.sdd_filled1_P7_com$DY)-1)){
            dcell[i+1] = HF2019_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i+1]-HF2019_pine_mean.sdd_filled1_P7_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_pine_mean.sdd_filled1_P7_com$gam_CZ[i+1]-HF2019_pine_mean.sdd_filled1_P7_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_pine_mean.sdd_filled1_P7_com$gam_EZ[i+1]-HF2019_pine_mean.sdd_filled1_P7_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_pine_mean.sdd_filled1_P7_com$gam_WZ[i+1]-HF2019_pine_mean.sdd_filled1_P7_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_pine_mean.sdd_filled1_P7_com$gam_MZ[i+1]-HF2019_pine_mean.sdd_filled1_P7_com$gam_MZ[i]
          }
          
          HF2019_pine_mean.sdd_filled1_P7_com$dcell = dcell
          HF2019_pine_mean.sdd_filled1_P7_com$dgam_CZ_mean = dgam_CZ
          HF2019_pine_mean.sdd_filled1_P7_com$dgam_EZ_mean = dgam_EZ
          HF2019_pine_mean.sdd_filled1_P7_com$dgam_WZ_mean = dgam_WZ
          HF2019_pine_mean.sdd_filled1_P7_com$dgam_MZ_mean = dgam_MZ
        }        
      }
      
      #maple 2017
      {
        #A18
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A18_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A18_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A18_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A18_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A18_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A18_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A18_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A18_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A18_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A18_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A18_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A18_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A18_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A18_com$dgam_MZ = dgam_MZ
        }
        #A19
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A19_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A19_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A19_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A19_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A19_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A19_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A19_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A19_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A19_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A19_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A19_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A19_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A19_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A19_com$dgam_MZ = dgam_MZ
        }      
        #A23
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A23_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A23_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A23_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A23_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A23_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A23_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A23_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A23_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A23_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A23_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A23_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A23_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A23_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A23_com$dgam_MZ = dgam_MZ
        } 
        #A28
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A28_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A28_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A28_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A28_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A28_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A28_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A28_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A28_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A28_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A28_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A28_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A28_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A28_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A28_com$dgam_MZ = dgam_MZ
        }
        #A29
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A29_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A29_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A29_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A29_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A29_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A29_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A29_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A29_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A29_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A29_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A29_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A29_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A29_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A29_com$dgam_MZ = dgam_MZ
        } 
        #A8
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_maple_mean.sdd_filled1_A8_com$DY)-1)){
            dcell[i+1] = HF2017_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i+1]-HF2017_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_maple_mean.sdd_filled1_A8_com$gam_CZ[i+1]-HF2017_maple_mean.sdd_filled1_A8_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_maple_mean.sdd_filled1_A8_com$gam_EZ[i+1]-HF2017_maple_mean.sdd_filled1_A8_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_maple_mean.sdd_filled1_A8_com$gam_WZ[i+1]-HF2017_maple_mean.sdd_filled1_A8_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_maple_mean.sdd_filled1_A8_com$gam_MZ[i+1]-HF2017_maple_mean.sdd_filled1_A8_com$gam_MZ[i]
          }
          
          HF2017_maple_mean.sdd_filled1_A8_com$dcell = dcell
          HF2017_maple_mean.sdd_filled1_A8_com$dgam_CZ = dgam_CZ
          HF2017_maple_mean.sdd_filled1_A8_com$dgam_EZ = dgam_EZ
          HF2017_maple_mean.sdd_filled1_A8_com$dgam_WZ = dgam_WZ
          HF2017_maple_mean.sdd_filled1_A8_com$dgam_MZ = dgam_MZ
        } 
      }
      
      #maple 2018
      {
        #A18
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A18_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A18_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A18_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A18_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A18_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A18_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A18_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A18_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A18_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A18_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A18_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A18_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A18_com$dgam_MZ = dgam_MZ
        }
        #A19
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A19_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A19_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A19_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A19_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A19_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A19_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A19_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A19_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A19_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A19_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A19_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A19_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A19_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A19_com$dgam_MZ = dgam_MZ
        }      
        #A23
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A23_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A23_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A23_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A23_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A23_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A23_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A23_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A23_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A23_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A23_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A23_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A23_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A23_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A23_com$dgam_MZ = dgam_MZ
        } 
        #A28
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A28_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A28_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A28_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A28_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A28_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A28_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A28_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A28_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A28_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A28_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A28_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A28_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A28_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A28_com$dgam_MZ = dgam_MZ
        }
        #A29
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A29_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A29_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A29_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A29_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A29_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A29_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A29_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A29_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A29_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A29_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A29_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A29_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A29_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A29_com$dgam_MZ = dgam_MZ
        } 
        #A8
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_maple_mean.sdd_filled1_A8_com$DY)-1)){
            dcell[i+1] = HF2018_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i+1]-HF2018_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_maple_mean.sdd_filled1_A8_com$gam_CZ[i+1]-HF2018_maple_mean.sdd_filled1_A8_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_maple_mean.sdd_filled1_A8_com$gam_EZ[i+1]-HF2018_maple_mean.sdd_filled1_A8_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_maple_mean.sdd_filled1_A8_com$gam_WZ[i+1]-HF2018_maple_mean.sdd_filled1_A8_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_maple_mean.sdd_filled1_A8_com$gam_MZ[i+1]-HF2018_maple_mean.sdd_filled1_A8_com$gam_MZ[i]
          }
          
          HF2018_maple_mean.sdd_filled1_A8_com$dcell = dcell
          HF2018_maple_mean.sdd_filled1_A8_com$dgam_CZ = dgam_CZ
          HF2018_maple_mean.sdd_filled1_A8_com$dgam_EZ = dgam_EZ
          HF2018_maple_mean.sdd_filled1_A8_com$dgam_WZ = dgam_WZ
          HF2018_maple_mean.sdd_filled1_A8_com$dgam_MZ = dgam_MZ
        }
      }
      
      #maple 2019
      {
        #A18
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A18_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A18_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A18_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A18_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A18_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A18_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A18_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A18_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A18_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A18_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A18_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A18_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A18_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A18_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A18_com$dgam_MZ = dgam_MZ
        }
        #A19
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A19_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A19_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A19_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A19_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A19_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A19_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A19_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A19_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A19_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A19_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A19_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A19_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A19_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A19_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A19_com$dgam_MZ = dgam_MZ
        }      
        #A23
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A23_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A23_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A23_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A23_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A23_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A23_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A23_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A23_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A23_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A23_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A23_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A23_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A23_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A23_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A23_com$dgam_MZ = dgam_MZ
        } 
        #A28
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A28_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A28_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A28_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A28_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A28_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A28_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A28_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A28_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A28_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A28_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A28_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A28_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A28_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A28_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A28_com$dgam_MZ = dgam_MZ
        }
        #A29
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A29_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A29_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A29_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A29_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A29_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A29_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A29_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A29_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A29_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A29_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A29_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A29_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A29_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A29_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A29_com$dgam_MZ = dgam_MZ
        } 
        #A8
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_maple_mean.sdd_filled1_A8_com$DY)-1)){
            dcell[i+1] = HF2019_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i+1]-HF2019_maple_mean.sdd_filled1_A8_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_maple_mean.sdd_filled1_A8_com$gam_CZ[i+1]-HF2019_maple_mean.sdd_filled1_A8_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_maple_mean.sdd_filled1_A8_com$gam_EZ[i+1]-HF2019_maple_mean.sdd_filled1_A8_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_maple_mean.sdd_filled1_A8_com$gam_WZ[i+1]-HF2019_maple_mean.sdd_filled1_A8_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_maple_mean.sdd_filled1_A8_com$gam_MZ[i+1]-HF2019_maple_mean.sdd_filled1_A8_com$gam_MZ[i]
          }
          
          HF2019_maple_mean.sdd_filled1_A8_com$dcell = dcell
          HF2019_maple_mean.sdd_filled1_A8_com$dgam_CZ = dgam_CZ
          HF2019_maple_mean.sdd_filled1_A8_com$dgam_EZ = dgam_EZ
          HF2019_maple_mean.sdd_filled1_A8_com$dgam_WZ = dgam_WZ
          HF2019_maple_mean.sdd_filled1_A8_com$dgam_MZ = dgam_MZ
        }
      } 
      
      #oak 2017
      {
        #Q05
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q05_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q05_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q05_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q05_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q05_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q05_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q05_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q05_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q05_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q05_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q05_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q05_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q05_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q05_com$dgam_MZ = dgam_MZ
        }
        #Q06
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q06_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q06_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q06_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q06_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q06_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q06_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q06_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q06_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q06_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q06_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q06_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q06_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q06_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q06_com$dgam_MZ = dgam_MZ
        } 
        #Q09
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q09_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q09_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q09_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q09_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q09_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q09_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q09_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q09_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q09_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q09_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q09_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q09_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q09_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q09_com$dgam_MZ = dgam_MZ
        }
        #Q10
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q10_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q10_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q10_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q10_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q10_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q10_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q10_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q10_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q10_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q10_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q10_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q10_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q10_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q10_com$dgam_MZ = dgam_MZ
        }
        #Q11
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q11_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q11_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q11_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q11_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q11_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q11_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q11_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q11_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q11_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q11_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q11_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q11_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q11_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q11_com$dgam_MZ = dgam_MZ
        }
        #Q12
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q12_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q12_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q12_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q12_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q12_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q12_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q12_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q12_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q12_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q12_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q12_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q12_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q12_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q12_com$dgam_MZ = dgam_MZ
        }
        #Q13
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q13_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q13_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q13_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q13_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q13_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q13_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q13_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q13_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q13_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q13_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q13_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q13_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q13_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q13_com$dgam_MZ = dgam_MZ
        }
        #Q15
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2017_oak_mean.sdd_filled1_Q15_com$DY)-1)){
            dcell[i+1] = HF2017_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i+1]-HF2017_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2017_oak_mean.sdd_filled1_Q15_com$gam_CZ[i+1]-HF2017_oak_mean.sdd_filled1_Q15_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2017_oak_mean.sdd_filled1_Q15_com$gam_EZ[i+1]-HF2017_oak_mean.sdd_filled1_Q15_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2017_oak_mean.sdd_filled1_Q15_com$gam_WZ[i+1]-HF2017_oak_mean.sdd_filled1_Q15_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2017_oak_mean.sdd_filled1_Q15_com$gam_MZ[i+1]-HF2017_oak_mean.sdd_filled1_Q15_com$gam_MZ[i]
          }
          
          HF2017_oak_mean.sdd_filled1_Q15_com$dcell = dcell
          HF2017_oak_mean.sdd_filled1_Q15_com$dgam_CZ = dgam_CZ
          HF2017_oak_mean.sdd_filled1_Q15_com$dgam_EZ = dgam_EZ
          HF2017_oak_mean.sdd_filled1_Q15_com$dgam_WZ = dgam_WZ
          HF2017_oak_mean.sdd_filled1_Q15_com$dgam_MZ = dgam_MZ
        }
      }
      
      #oak 2018
      {
        #Q05
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q05_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q05_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q05_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q05_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q05_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q05_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q05_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q05_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q05_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q05_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q05_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q05_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q05_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q05_com$dgam_MZ = dgam_MZ
        }
        #Q06
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q06_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q06_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q06_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q06_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q06_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q06_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q06_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q06_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q06_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q06_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q06_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q06_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q06_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q06_com$dgam_MZ = dgam_MZ
        } 
        #Q09
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q09_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q09_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q09_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q09_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q09_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q09_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q09_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q09_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q09_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q09_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q09_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q09_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q09_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q09_com$dgam_MZ = dgam_MZ
        }
        #Q10
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q10_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q10_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q10_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q10_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q10_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q10_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q10_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q10_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q10_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q10_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q10_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q10_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q10_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q10_com$dgam_MZ = dgam_MZ
        }
        #Q11
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q11_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q11_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q11_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q11_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q11_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q11_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q11_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q11_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q11_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q11_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q11_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q11_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q11_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q11_com$dgam_MZ = dgam_MZ
        }
        #Q12
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q12_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q12_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q12_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q12_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q12_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q12_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q12_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q12_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q12_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q12_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q12_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q12_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q12_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q12_com$dgam_MZ = dgam_MZ
        }
        #Q13
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q13_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q13_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q13_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q13_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q13_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q13_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q13_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q13_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q13_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q13_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q13_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q13_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q13_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q13_com$dgam_MZ = dgam_MZ
        }
        #Q15
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2018_oak_mean.sdd_filled1_Q15_com$DY)-1)){
            dcell[i+1] = HF2018_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i+1]-HF2018_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2018_oak_mean.sdd_filled1_Q15_com$gam_CZ[i+1]-HF2018_oak_mean.sdd_filled1_Q15_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2018_oak_mean.sdd_filled1_Q15_com$gam_EZ[i+1]-HF2018_oak_mean.sdd_filled1_Q15_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2018_oak_mean.sdd_filled1_Q15_com$gam_WZ[i+1]-HF2018_oak_mean.sdd_filled1_Q15_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2018_oak_mean.sdd_filled1_Q15_com$gam_MZ[i+1]-HF2018_oak_mean.sdd_filled1_Q15_com$gam_MZ[i]
          }
          
          HF2018_oak_mean.sdd_filled1_Q15_com$dcell = dcell
          HF2018_oak_mean.sdd_filled1_Q15_com$dgam_CZ = dgam_CZ
          HF2018_oak_mean.sdd_filled1_Q15_com$dgam_EZ = dgam_EZ
          HF2018_oak_mean.sdd_filled1_Q15_com$dgam_WZ = dgam_WZ
          HF2018_oak_mean.sdd_filled1_Q15_com$dgam_MZ = dgam_MZ
        }
      }
      
      #oak 2019
      {
        #Q05
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q05_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q05_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q05_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q05_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q05_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q05_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q05_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q05_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q05_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q05_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q05_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q05_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q05_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q05_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q05_com$dgam_MZ = dgam_MZ
        }
        #Q06
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q06_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q06_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q06_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q06_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q06_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q06_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q06_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q06_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q06_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q06_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q06_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q06_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q06_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q06_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q06_com$dgam_MZ = dgam_MZ
        } 
        #Q09
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q09_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q09_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q09_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q09_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q09_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q09_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q09_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q09_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q09_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q09_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q09_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q09_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q09_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q09_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q09_com$dgam_MZ = dgam_MZ
        }
        #Q10
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q10_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q10_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q10_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q10_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q10_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q10_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q10_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q10_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q10_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q10_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q10_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q10_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q10_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q10_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q10_com$dgam_MZ = dgam_MZ
        }
        #Q11
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q11_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q11_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q11_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q11_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q11_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q11_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q11_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q11_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q11_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q11_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q11_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q11_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q11_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q11_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q11_com$dgam_MZ = dgam_MZ
        }
        #Q12
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q12_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q12_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q12_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q12_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q12_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q12_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q12_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q12_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q12_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q12_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q12_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q12_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q12_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q12_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q12_com$dgam_MZ = dgam_MZ
        }
        #Q13
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q13_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q13_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q13_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q13_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q13_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q13_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q13_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q13_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q13_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q13_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q13_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q13_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q13_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q13_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q13_com$dgam_MZ = dgam_MZ
        }
        #Q15
        {
          #define variables
          {
            dcell <- numeric(0)
            dgam_CZ <- numeric(0)
            dgam_EZ <- numeric(0)
            dgam_WZ <- numeric(0)
            dgam_MZ <- numeric(0)
          }
          
          for (i in 1:(length(HF2019_oak_mean.sdd_filled1_Q15_com$DY)-1)){
            dcell[i+1] = HF2019_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i+1]-HF2019_oak_mean.sdd_filled1_Q15_com$gam_EWMZ[i]
            dgam_CZ[i+1] = HF2019_oak_mean.sdd_filled1_Q15_com$gam_CZ[i+1]-HF2019_oak_mean.sdd_filled1_Q15_com$gam_CZ[i]
            dgam_EZ[i+1] = HF2019_oak_mean.sdd_filled1_Q15_com$gam_EZ[i+1]-HF2019_oak_mean.sdd_filled1_Q15_com$gam_EZ[i]
            dgam_WZ[i+1] = HF2019_oak_mean.sdd_filled1_Q15_com$gam_WZ[i+1]-HF2019_oak_mean.sdd_filled1_Q15_com$gam_WZ[i]
            dgam_MZ[i+1] = HF2019_oak_mean.sdd_filled1_Q15_com$gam_MZ[i+1]-HF2019_oak_mean.sdd_filled1_Q15_com$gam_MZ[i]
          }
          
          HF2019_oak_mean.sdd_filled1_Q15_com$dcell = dcell
          HF2019_oak_mean.sdd_filled1_Q15_com$dgam_CZ = dgam_CZ
          HF2019_oak_mean.sdd_filled1_Q15_com$dgam_EZ = dgam_EZ
          HF2019_oak_mean.sdd_filled1_Q15_com$dgam_WZ = dgam_WZ
          HF2019_oak_mean.sdd_filled1_Q15_com$dgam_MZ = dgam_MZ
        }
      }
    }
    
    #remove negative values of dcell
    {
      #pine
      {
        HF2017_pine_mean.sdd_filled1_P7_com[HF2017_pine_mean.sdd_filled1_P7_com$dcell[2:length(HF2017_pine_mean.sdd_filled1_P7_com$dcell)] <0,] = 0
        HF2017_pine_mean.sdd_filled1_P16_com[HF2017_pine_mean.sdd_filled1_P16_com$dcell <0,] = 0
        HF2017_pine_mean.sdd_filled1_P20_com[HF2017_pine_mean.sdd_filled1_P20_com$dcell <0,] = 0
        HF2017_pine_mean.sdd_filled1_P21_com[HF2017_pine_mean.sdd_filled1_P21_com$dcell <0,] = 0
        HF2017_pine_mean.sdd_filled1_P25_com[HF2017_pine_mean.sdd_filled1_P25_com$dcell <0,] = 0
        HF2017_pine_mean.sdd_filled1_P26_com[HF2017_pine_mean.sdd_filled1_P26_com$dcell <0,] = 0
        
        HF2018_pine_mean.sdd_filled1_P7_com[HF2018_pine_mean.sdd_filled1_P7_com$dcell <0,] = 0
        HF2018_pine_mean.sdd_filled1_P16_com[HF2018_pine_mean.sdd_filled1_P16_com$dcell <0,] = 0
        HF2018_pine_mean.sdd_filled1_P20_com[HF2018_pine_mean.sdd_filled1_P20_com$dcell <0,] = 0
        HF2018_pine_mean.sdd_filled1_P21_com[HF2018_pine_mean.sdd_filled1_P21_com$dcell <0,] = 0
        HF2018_pine_mean.sdd_filled1_P25_com[HF2018_pine_mean.sdd_filled1_P25_com$dcell <0,] = 0
        HF2018_pine_mean.sdd_filled1_P26_com[HF2018_pine_mean.sdd_filled1_P26_com$dcell <0,] = 0
        
        HF2019_pine_mean.sdd_filled1_P7_com[HF2019_pine_mean.sdd_filled1_P7_com$dcell <0,] = 0
        HF2019_pine_mean.sdd_filled1_P16_com[HF2019_pine_mean.sdd_filled1_P16_com$dcell <0,] = 0
        HF2019_pine_mean.sdd_filled1_P20_com[HF2019_pine_mean.sdd_filled1_P20_com$dcell <0,] = 0
        HF2019_pine_mean.sdd_filled1_P21_com[HF2019_pine_mean.sdd_filled1_P21_com$dcell <0,] = 0
        HF2019_pine_mean.sdd_filled1_P25_com[HF2019_pine_mean.sdd_filled1_P25_com$dcell <0,] = 0
        HF2019_pine_mean.sdd_filled1_P26_com[HF2019_pine_mean.sdd_filled1_P26_com$dcell <0,] = 0
      }
      
      #oak
      {}
      
      
      #maple
      {
        HF2017_maple_mean.sdd_filled1_A8_com[HF2017_maple_mean.sdd_filled1_A8_com$dcell <0,] = 0
        HF2017_maple_mean.sdd_filled1_A18_com[HF2017_maple_mean.sdd_filled1_A18_com$dcell <0,] = 0
        HF2017_maple_mean.sdd_filled1_A19_com[HF2017_maple_mean.sdd_filled1_A19_com$dcell <0,] = 0
        HF2017_maple_mean.sdd_filled1_A23_com[HF2017_maple_mean.sdd_filled1_A23_com$dcell <0,] = 0
        HF2017_maple_mean.sdd_filled1_A28_com[HF2017_maple_mean.sdd_filled1_A28_com$dcell <0,] = 0
        HF2017_maple_mean.sdd_filled1_A29_com[HF2017_maple_mean.sdd_filled1_A29_com$dcell <0,] = 0
        
        HF2018_maple_mean.sdd_filled1_A8_com[HF2018_maple_mean.sdd_filled1_A8_com$dcell <0,] = 0
        HF2018_maple_mean.sdd_filled1_A18_com[HF2018_maple_mean.sdd_filled1_A18_com$dcell <0,] = 0
        HF2018_maple_mean.sdd_filled1_A19_com[HF2018_maple_mean.sdd_filled1_A19_com$dcell <0,] = 0
        HF2018_maple_mean.sdd_filled1_A23_com[HF2018_maple_mean.sdd_filled1_A23_com$dcell <0,] = 0
        HF2018_maple_mean.sdd_filled1_A28_com[HF2018_maple_mean.sdd_filled1_A28_com$dcell <0,] = 0
        HF2018_maple_mean.sdd_filled1_A29_com[HF2018_maple_mean.sdd_filled1_A29_com$dcell <0,] = 0
        
        HF2019_maple_mean.sdd_filled1_A8_com[HF2019_maple_mean.sdd_filled1_A8_com$dcell <0,] = 0
        HF2019_maple_mean.sdd_filled1_A18_com[HF2019_maple_mean.sdd_filled1_A18_com$dcell <0,] = 0
        HF2019_maple_mean.sdd_filled1_A19_com[HF2019_maple_mean.sdd_filled1_A19_com$dcell <0,] = 0
        HF2019_maple_mean.sdd_filled1_A23_com[HF2019_maple_mean.sdd_filled1_A23_com$dcell <0,] = 0
        HF2019_maple_mean.sdd_filled1_A28_com[HF2019_maple_mean.sdd_filled1_A28_com$dcell <0,] = 0
        HF2019_maple_mean.sdd_filled1_A29_com[HF2019_maple_mean.sdd_filled1_A29_com$dcell <0,] = 0
      }
    }
    
  }
  
  #combine the tree dfs to one for each year
  #PIST
  {
    HF2017_pine_mean.sdd_filled2_com = rbind(HF2017_pine_mean.sdd_filled1_P16_com,HF2017_pine_mean.sdd_filled1_P20_com,HF2017_pine_mean.sdd_filled1_P21_com,HF2017_pine_mean.sdd_filled1_P25_com,HF2017_pine_mean.sdd_filled1_P26_com,HF2017_pine_mean.sdd_filled1_P7_com)
    HF2018_pine_mean.sdd_filled2_com = rbind(HF2018_pine_mean.sdd_filled1_P16_com,HF2018_pine_mean.sdd_filled1_P20_com,HF2018_pine_mean.sdd_filled1_P21_com,HF2018_pine_mean.sdd_filled1_P25_com,HF2018_pine_mean.sdd_filled1_P26_com,HF2018_pine_mean.sdd_filled1_P7_com)
    HF2019_pine_mean.sdd_filled2_com = rbind(HF2019_pine_mean.sdd_filled1_P16_com,HF2019_pine_mean.sdd_filled1_P20_com,HF2019_pine_mean.sdd_filled1_P21_com,HF2019_pine_mean.sdd_filled1_P25_com,HF2019_pine_mean.sdd_filled1_P26_com,HF2019_pine_mean.sdd_filled1_P7_com)
    
    #rename the wrong column names
    {
      names(HF2017_pine_mean.sdd_filled1_P16_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2017_pine_mean.sdd_filled1_P20_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2017_pine_mean.sdd_filled1_P21_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2017_pine_mean.sdd_filled1_P25_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",      "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2017_pine_mean.sdd_filled1_P26_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2017_pine_mean.sdd_filled1_P7_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      
      names(HF2018_pine_mean.sdd_filled1_P16_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2018_pine_mean.sdd_filled1_P20_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2018_pine_mean.sdd_filled1_P21_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2018_pine_mean.sdd_filled1_P25_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2018_pine_mean.sdd_filled1_P26_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2018_pine_mean.sdd_filled1_P7_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P16_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P20_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P21_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P25_com) = c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P26_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
      names(HF2019_pine_mean.sdd_filled1_P7_com) =c("Site","Year","Tree","Species","Sample","DY","Ratio","CZ","EZ",       "WZ","MZ","WMZ","EWMZ","gam_CZ","gam_EZ","gam_WZ","gam_MZ","gam_WMZ","gam_EWMZ","dcell","dgam_CZ","dgam_EZ","dgam_WZ",  "dgam_MZ")
    }
  }
  #ACRU
  {
    HF2017_maple_mean.sdd_filled2_com = rbind(HF2017_maple_mean.sdd_filled1_A18_com,HF2017_maple_mean.sdd_filled1_A19_com,HF2017_maple_mean.sdd_filled1_A23_com,HF2017_maple_mean.sdd_filled1_A28_com,HF2017_maple_mean.sdd_filled1_A29_com,HF2017_maple_mean.sdd_filled1_A8_com)
    HF2018_maple_mean.sdd_filled2_com = rbind(HF2018_maple_mean.sdd_filled1_A18_com,HF2018_maple_mean.sdd_filled1_A19_com,HF2018_maple_mean.sdd_filled1_A23_com,HF2018_maple_mean.sdd_filled1_A28_com,HF2018_maple_mean.sdd_filled1_A29_com,HF2018_maple_mean.sdd_filled1_A8_com)
    HF2019_maple_mean.sdd_filled2_com = rbind(HF2019_maple_mean.sdd_filled1_A18_com,HF2019_maple_mean.sdd_filled1_A19_com,HF2019_maple_mean.sdd_filled1_A23_com,HF2019_maple_mean.sdd_filled1_A28_com,HF2019_maple_mean.sdd_filled1_A29_com,HF2019_maple_mean.sdd_filled1_A8_com)
  }
  #QURU
  {
    HF2017_oak_mean.sdd_filled2_com = rbind(HF2017_oak_mean.sdd_filled1_Q05_com,HF2017_oak_mean.sdd_filled1_Q06_com,HF2017_oak_mean.sdd_filled1_Q09_com,HF2017_oak_mean.sdd_filled1_Q10_com,HF2017_oak_mean.sdd_filled1_Q11_com,HF2017_oak_mean.sdd_filled1_Q12_com,HF2017_oak_mean.sdd_filled1_Q13_com,HF2017_oak_mean.sdd_filled1_Q15_com)
    HF2018_oak_mean.sdd_filled2_com = rbind(HF2018_oak_mean.sdd_filled1_Q05_com,HF2018_oak_mean.sdd_filled1_Q06_com,HF2018_oak_mean.sdd_filled1_Q09_com,HF2018_oak_mean.sdd_filled1_Q10_com,HF2018_oak_mean.sdd_filled1_Q11_com,HF2018_oak_mean.sdd_filled1_Q12_com,HF2018_oak_mean.sdd_filled1_Q13_com,HF2018_oak_mean.sdd_filled1_Q15_com)
    HF2019_oak_mean.sdd_filled2_com = rbind(HF2019_oak_mean.sdd_filled1_Q05_com,HF2019_oak_mean.sdd_filled1_Q06_com,HF2019_oak_mean.sdd_filled1_Q09_com,HF2019_oak_mean.sdd_filled1_Q10_com,HF2019_oak_mean.sdd_filled1_Q11_com,HF2019_oak_mean.sdd_filled1_Q12_com,HF2019_oak_mean.sdd_filled1_Q13_com,HF2019_oak_mean.sdd_filled1_Q15_com)
  }
  
  
  #aggregate to site
  #raw
  {
    HF2017_pine_mean_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_pine_mean.sdd_filled1,FUN=mean)
    HF2018_pine_mean_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_pine_mean.sdd_filled1,FUN=mean)    
    HF2019_pine_mean_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_pine_mean.sdd_filled1,FUN=mean)
    HF2017_pine_sd_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_pine_mean.sdd_filled1,FUN=sd)
    HF2018_pine_sd_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_pine_mean.sdd_filled1,FUN=sd)
    HF2019_pine_sd_site <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_pine_mean.sdd_filled1,FUN=sd)
    
    HF2017_maple_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2_com,FUN=mean)
    HF2018_maple_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2_com,FUN=mean)
    HF2019_maple_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2_com,FUN=mean)
    HF2017_maple_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2_com,FUN=sd)
    HF2018_maple_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2_com,FUN=sd)
    HF2019_maple_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2_com,FUN=sd)
    
    HF2017_oak_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2_com,FUN=mean)
    HF2018_oak_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2_com,FUN=mean)
    HF2019_oak_mean_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2_com,FUN=mean)
    HF2017_oak_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2_com,FUN=sd)
    HF2018_oak_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2_com,FUN=sd)
    HF2019_oak_sd_site_com <- aggregate(cbind(CZ,EZ,WZ,MZ,WMZ,EWMZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2_com,FUN=sd)
  }
  
  #gam
  #cut
  {
    HF2017_pine_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_pine_mean.sdd_filled2,FUN=mean)
    HF2018_pine_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_pine_mean.sdd_filled2,FUN=mean)    
    HF2019_pine_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_pine_mean.sdd_filled2,FUN=mean)
    HF2017_pine_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_pine_mean.sdd_filled2,FUN=sd)
    HF2018_pine_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_pine_mean.sdd_filled2,FUN=sd)
    HF2019_pine_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_pine_mean.sdd_filled2,FUN=sd)
    
    HF2017_maple_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2,FUN=mean)
    HF2018_maple_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2,FUN=mean)
    HF2019_maple_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2,FUN=mean)
    HF2017_maple_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2,FUN=sd)
    HF2018_maple_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2,FUN=sd)
    HF2019_maple_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2,FUN=sd)
    
    HF2017_oak_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2,FUN=mean)
    HF2018_oak_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2,FUN=mean)
    HF2019_oak_gam_mean_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2,FUN=mean)
    HF2017_oak_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2,FUN=sd)
    HF2018_oak_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2,FUN=sd)
    HF2019_oak_gam_sd_site <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2,FUN=sd)
  }
  #no cut
  {
    HF2017_pine_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2017_pine_mean.sdd_filled2_com,FUN=mean)
    HF2018_pine_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2018_pine_mean.sdd_filled2_com,FUN=mean)    
    HF2019_pine_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2019_pine_mean.sdd_filled2_com,FUN=mean)
    HF2017_pine_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2017_pine_mean.sdd_filled2_com,FUN=sd)
    HF2018_pine_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2018_pine_mean.sdd_filled2_com,FUN=sd)
    HF2019_pine_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ_mean,dgam_EZ_mean,dgam_WZ_mean,dgam_MZ_mean)~Species + DY,data = HF2019_pine_mean.sdd_filled2_com,FUN=sd)
    
    HF2017_maple_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2_com,FUN=mean)
    HF2018_maple_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2_com,FUN=mean)
    HF2019_maple_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2_com,FUN=mean)
    HF2017_maple_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2017_maple_mean.sdd_filled2_com,FUN=sd)
    HF2018_maple_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2018_maple_mean.sdd_filled2_com,FUN=sd)
    HF2019_maple_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2019_maple_mean.sdd_filled2_com,FUN=sd)
    
    HF2017_oak_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2_com,FUN=mean)
    HF2018_oak_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2_com,FUN=mean)
    HF2019_oak_gam_mean_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2_com,FUN=mean)
    HF2017_oak_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2017_oak_mean.sdd_filled2_com,FUN=sd)
    HF2018_oak_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2018_oak_mean.sdd_filled2_com,FUN=sd)
    HF2019_oak_gam_sd_site_com <- aggregate(cbind(gam_CZ,gam_EZ,gam_WZ,gam_MZ,gam_WMZ,gam_EWMZ,dcell,dgam_CZ,dgam_EZ,dgam_WZ,dgam_MZ)~Species + DY,data = HF2019_oak_mean.sdd_filled2_com,FUN=sd)
  }
  
  #rename the columns of the df
  #cut
  {
    #pine
    {
      names(HF2017_pine_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2018_pine_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2019_pine_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      
      names(HF2017_pine_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2018_pine_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2019_pine_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
    }
    
    #maple
    {
      names(HF2017_maple_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2018_maple_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2019_maple_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      
      names(HF2017_maple_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2018_maple_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2019_maple_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd") 
    }
    
    #oak
    {
      names(HF2017_oak_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2018_oak_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      names(HF2019_oak_gam_mean_site) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean")
      
      names(HF2017_oak_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2018_oak_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd")
      names(HF2019_oak_gam_sd_site) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd") 
    } 
  }
  
  #no cut
  {
    #pine
    {
      names(HF2017_pine_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2018_pine_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2019_pine_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      
      names(HF2017_pine_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2018_pine_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2019_pine_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
    }
    
    #maple
    {
      names(HF2017_maple_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2018_maple_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2019_maple_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      
      names(HF2017_maple_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2018_maple_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2019_maple_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
    }
    
    #oak
    {
      names(HF2017_oak_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2018_oak_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      names(HF2019_oak_gam_mean_site_com) = c("Species","DY","gam_CZ_mean","gam_EZ_mean","gam_WZ_mean","gam_MZ_mean","gam_WMZ_mean","gam_EWMZ_mean","gam_dcell_mean","dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean")
      
      names(HF2017_oak_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2018_oak_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
      names(HF2019_oak_gam_sd_site_com) = c("Species","DY","gam_CZ_sd","gam_EZ_sd","gam_WZ_sd","gam_MZ_sd","gam_WMZ_sd","gam_EWMZ_sd","gam_dcell_sd","dgam_CZ_sd","dgam_EZ_sd","dgam_WZ_sd","dgam_MZ_sd")
    }  
  }
  
  #quantify the weekly difference of xylogenesis
  #raw
  {
    #pine 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_pine_mean_site$DY)-1)){
        dcell[i+1] = HF2017_pine_mean_site$EWMZ[i+1]-HF2017_pine_mean_site$EWMZ[i]
        dCZ[i+1] = HF2017_pine_mean_site$CZ[i+1]-HF2017_pine_mean_site$CZ[i]
        dEZ[i+1] = HF2017_pine_mean_site$EZ[i+1]-HF2017_pine_mean_site$EZ[i]
        dWZ[i+1] = HF2017_pine_mean_site$WZ[i+1]-HF2017_pine_mean_site$WZ[i]
        dMZ[i+1] = HF2017_pine_mean_site$MZ[i+1]-HF2017_pine_mean_site$MZ[i]
      }
      
      HF2017_pine_mean_site$dcell = dcell
      HF2017_pine_mean_site$dCZ = dCZ
      HF2017_pine_mean_site$dEZ = dEZ
      HF2017_pine_mean_site$dWZ = dWZ
      HF2017_pine_mean_site$dMZ = dMZ
    }
    
    #pine 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      for (i in 1:(length(HF2018_pine_mean_site$DY)-1)){
        dcell[i+1] = HF2018_pine_mean_site$EWMZ[i+1]-HF2018_pine_mean_site$EWMZ[i]
        dCZ[i+1] = HF2018_pine_mean_site$CZ[i+1]-HF2018_pine_mean_site$CZ[i]
        dEZ[i+1] = HF2018_pine_mean_site$EZ[i+1]-HF2018_pine_mean_site$EZ[i]
        dWZ[i+1] = HF2018_pine_mean_site$WZ[i+1]-HF2018_pine_mean_site$WZ[i]
        dMZ[i+1] = HF2018_pine_mean_site$MZ[i+1]-HF2018_pine_mean_site$MZ[i]
      }
      
      HF2018_pine_mean_site$dcell = dcell
      HF2018_pine_mean_site$dCZ = dCZ
      HF2018_pine_mean_site$dEZ = dEZ
      HF2018_pine_mean_site$dWZ = dWZ
      HF2018_pine_mean_site$dMZ = dMZ
    }
    
    #pine 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_pine_mean_site$DY)-1)){
        dcell[i+1] = HF2019_pine_mean_site$EWMZ[i+1]-HF2019_pine_mean_site$EWMZ[i]
        dCZ[i+1] = HF2019_pine_mean_site$CZ[i+1]-HF2019_pine_mean_site$CZ[i]
        dEZ[i+1] = HF2019_pine_mean_site$EZ[i+1]-HF2019_pine_mean_site$EZ[i]
        dWZ[i+1] = HF2019_pine_mean_site$WZ[i+1]-HF2019_pine_mean_site$WZ[i]
        dMZ[i+1] = HF2019_pine_mean_site$MZ[i+1]-HF2019_pine_mean_site$MZ[i]
      }
      
      HF2019_pine_mean_site$dcell = dcell
      HF2019_pine_mean_site$dCZ = dCZ
      HF2019_pine_mean_site$dEZ = dEZ
      HF2019_pine_mean_site$dWZ = dWZ
      HF2019_pine_mean_site$dMZ = dMZ
    }
    
    #maple 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_maple_mean_site$DY)-1)){
        dcell[i+1] = HF2017_maple_mean_site$EWMZ[i+1]-HF2017_maple_mean_site$EWMZ[i]
        dCZ[i+1] = HF2017_maple_mean_site$CZ[i+1]-HF2017_maple_mean_site$CZ[i]
        dEZ[i+1] = HF2017_maple_mean_site$EZ[i+1]-HF2017_maple_mean_site$EZ[i]
        dWZ[i+1] = HF2017_maple_mean_site$WZ[i+1]-HF2017_maple_mean_site$WZ[i]
        dMZ[i+1] = HF2017_maple_mean_site$MZ[i+1]-HF2017_maple_mean_site$MZ[i]
      }
      
      HF2017_maple_mean_site$dcell = dcell
      HF2017_maple_mean_site$dCZ = dCZ
      HF2017_maple_mean_site$dEZ = dEZ
      HF2017_maple_mean_site$dWZ = dWZ
      HF2017_maple_mean_site$dMZ = dMZ
    }
    
    #maple 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      for (i in 1:(length(HF2018_maple_mean_site$DY)-1)){
        dcell[i+1] = HF2018_maple_mean_site$EWMZ[i+1]-HF2018_maple_mean_site$EWMZ[i]
        dCZ[i+1] = HF2018_maple_mean_site$CZ[i+1]-HF2018_maple_mean_site$CZ[i]
        dEZ[i+1] = HF2018_maple_mean_site$EZ[i+1]-HF2018_maple_mean_site$EZ[i]
        dWZ[i+1] = HF2018_maple_mean_site$WZ[i+1]-HF2018_maple_mean_site$WZ[i]
        dMZ[i+1] = HF2018_maple_mean_site$MZ[i+1]-HF2018_maple_mean_site$MZ[i]
      }
      
      HF2018_maple_mean_site$dcell = dcell
      HF2018_maple_mean_site$dCZ = dCZ
      HF2018_maple_mean_site$dEZ = dEZ
      HF2018_maple_mean_site$dWZ = dWZ
      HF2018_maple_mean_site$dMZ = dMZ
    }
    
    #maple 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_maple_mean_site$DY)-1)){
        dcell[i+1] = HF2019_maple_mean_site$EWMZ[i+1]-HF2019_maple_mean_site$EWMZ[i]
        dCZ[i+1] = HF2019_maple_mean_site$CZ[i+1]-HF2019_maple_mean_site$CZ[i]
        dEZ[i+1] = HF2019_maple_mean_site$EZ[i+1]-HF2019_maple_mean_site$EZ[i]
        dWZ[i+1] = HF2019_maple_mean_site$WZ[i+1]-HF2019_maple_mean_site$WZ[i]
        dMZ[i+1] = HF2019_maple_mean_site$MZ[i+1]-HF2019_maple_mean_site$MZ[i]
      }
      
      HF2019_maple_mean_site$dcell = dcell
      HF2019_maple_mean_site$dCZ = dCZ
      HF2019_maple_mean_site$dEZ = dEZ
      HF2019_maple_mean_site$dWZ = dWZ
      HF2019_maple_mean_site$dMZ = dMZ
    }  
    
    #oak 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_oak_mean_site$DY)-1)){
        dcell[i+1] = HF2017_oak_mean_site$EWMZ[i+1]-HF2017_oak_mean_site$EWMZ[i]
        dCZ[i+1] = HF2017_oak_mean_site$CZ[i+1]-HF2017_oak_mean_site$CZ[i]
        dEZ[i+1] = HF2017_oak_mean_site$EZ[i+1]-HF2017_oak_mean_site$EZ[i]
        dWZ[i+1] = HF2017_oak_mean_site$WZ[i+1]-HF2017_oak_mean_site$WZ[i]
        dMZ[i+1] = HF2017_oak_mean_site$MZ[i+1]-HF2017_oak_mean_site$MZ[i]
      }
      
      HF2017_oak_mean_site$dcell = dcell
      HF2017_oak_mean_site$dCZ = dCZ
      HF2017_oak_mean_site$dEZ = dEZ
      HF2017_oak_mean_site$dWZ = dWZ
      HF2017_oak_mean_site$dMZ = dMZ
    }
    
    #oak 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      for (i in 1:(length(HF2018_oak_mean_site$DY)-1)){
        dcell[i+1] = HF2018_oak_mean_site$EWMZ[i+1]-HF2018_oak_mean_site$EWMZ[i]
        dCZ[i+1] = HF2018_oak_mean_site$CZ[i+1]-HF2018_oak_mean_site$CZ[i]
        dEZ[i+1] = HF2018_oak_mean_site$EZ[i+1]-HF2018_oak_mean_site$EZ[i]
        dWZ[i+1] = HF2018_oak_mean_site$WZ[i+1]-HF2018_oak_mean_site$WZ[i]
        dMZ[i+1] = HF2018_oak_mean_site$MZ[i+1]-HF2018_oak_mean_site$MZ[i]
      }
      
      HF2018_oak_mean_site$dcell = dcell
      HF2018_oak_mean_site$dCZ = dCZ
      HF2018_oak_mean_site$dEZ = dEZ
      HF2018_oak_mean_site$dWZ = dWZ
      HF2018_oak_mean_site$dMZ = dMZ
    }
    
    #oak 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dCZ <- numeric(0)
        dEZ <- numeric(0)
        dWZ <- numeric(0)
        dMZ <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_oak_mean_site$DY)-1)){
        dcell[i+1] = HF2019_oak_mean_site$EWMZ[i+1]-HF2019_oak_mean_site$EWMZ[i]
        dCZ[i+1] = HF2019_oak_mean_site$CZ[i+1]-HF2019_oak_mean_site$CZ[i]
        dEZ[i+1] = HF2019_oak_mean_site$EZ[i+1]-HF2019_oak_mean_site$EZ[i]
        dWZ[i+1] = HF2019_oak_mean_site$WZ[i+1]-HF2019_oak_mean_site$WZ[i]
        dMZ[i+1] = HF2019_oak_mean_site$MZ[i+1]-HF2019_oak_mean_site$MZ[i]
      }
      
      HF2019_oak_mean_site$dcell = dcell
      HF2019_oak_mean_site$dCZ = dCZ
      HF2019_oak_mean_site$dEZ = dEZ
      HF2019_oak_mean_site$dWZ = dWZ
      HF2019_oak_mean_site$dMZ = dMZ
    }
  }
  
  #cut
  {
    #pine 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_pine_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2017_pine_gam_mean_site$gam_EWMZ_mean[i+1]-HF2017_pine_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_pine_gam_mean_site$gam_CZ_mean[i+1]-HF2017_pine_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_pine_gam_mean_site$gam_EZ_mean[i+1]-HF2017_pine_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_pine_gam_mean_site$gam_WZ_mean[i+1]-HF2017_pine_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_pine_gam_mean_site$gam_MZ_mean[i+1]-HF2017_pine_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2017_pine_gam_mean_site$dcell = dcell
      HF2017_pine_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2017_pine_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2017_pine_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2017_pine_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #pine 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_pine_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2018_pine_gam_mean_site$gam_EWMZ_mean[i+1]-HF2018_pine_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_pine_gam_mean_site$gam_CZ_mean[i+1]-HF2018_pine_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_pine_gam_mean_site$gam_EZ_mean[i+1]-HF2018_pine_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_pine_gam_mean_site$gam_WZ_mean[i+1]-HF2018_pine_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_pine_gam_mean_site$gam_MZ_mean[i+1]-HF2018_pine_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2018_pine_gam_mean_site$dcell = dcell
      HF2018_pine_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2018_pine_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2018_pine_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2018_pine_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #pine 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_pine_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2019_pine_gam_mean_site$gam_EWMZ_mean[i+1]-HF2019_pine_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_pine_gam_mean_site$gam_CZ_mean[i+1]-HF2019_pine_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_pine_gam_mean_site$gam_EZ_mean[i+1]-HF2019_pine_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_pine_gam_mean_site$gam_WZ_mean[i+1]-HF2019_pine_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_pine_gam_mean_site$gam_MZ_mean[i+1]-HF2019_pine_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2019_pine_gam_mean_site$dcell = dcell
      HF2019_pine_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2019_pine_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2019_pine_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2019_pine_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_maple_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2017_maple_gam_mean_site$gam_EWMZ_mean[i+1]-HF2017_maple_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_maple_gam_mean_site$gam_CZ_mean[i+1]-HF2017_maple_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_maple_gam_mean_site$gam_EZ_mean[i+1]-HF2017_maple_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_maple_gam_mean_site$gam_WZ_mean[i+1]-HF2017_maple_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_maple_gam_mean_site$gam_MZ_mean[i+1]-HF2017_maple_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2017_maple_gam_mean_site$dcell = dcell
      HF2017_maple_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2017_maple_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2017_maple_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2017_maple_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_maple_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2018_maple_gam_mean_site$gam_EWMZ_mean[i+1]-HF2018_maple_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_maple_gam_mean_site$gam_CZ_mean[i+1]-HF2018_maple_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_maple_gam_mean_site$gam_EZ_mean[i+1]-HF2018_maple_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_maple_gam_mean_site$gam_WZ_mean[i+1]-HF2018_maple_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_maple_gam_mean_site$gam_MZ_mean[i+1]-HF2018_maple_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2018_maple_gam_mean_site$dcell = dcell
      HF2018_maple_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2018_maple_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2018_maple_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2018_maple_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_maple_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2019_maple_gam_mean_site$gam_EWMZ_mean[i+1]-HF2019_maple_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_maple_gam_mean_site$gam_CZ_mean[i+1]-HF2019_maple_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_maple_gam_mean_site$gam_EZ_mean[i+1]-HF2019_maple_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_maple_gam_mean_site$gam_WZ_mean[i+1]-HF2019_maple_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_maple_gam_mean_site$gam_MZ_mean[i+1]-HF2019_maple_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2019_maple_gam_mean_site$dcell = dcell
      HF2019_maple_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2019_maple_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2019_maple_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2019_maple_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }  
    
    #oak 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_oak_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2017_oak_gam_mean_site$gam_EWMZ_mean[i+1]-HF2017_oak_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_oak_gam_mean_site$gam_CZ_mean[i+1]-HF2017_oak_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_oak_gam_mean_site$gam_EZ_mean[i+1]-HF2017_oak_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_oak_gam_mean_site$gam_WZ_mean[i+1]-HF2017_oak_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_oak_gam_mean_site$gam_MZ_mean[i+1]-HF2017_oak_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2017_oak_gam_mean_site$dcell = dcell
      HF2017_oak_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2017_oak_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2017_oak_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2017_oak_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #oak 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_oak_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2018_oak_gam_mean_site$gam_EWMZ_mean[i+1]-HF2018_oak_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_oak_gam_mean_site$gam_CZ_mean[i+1]-HF2018_oak_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_oak_gam_mean_site$gam_EZ_mean[i+1]-HF2018_oak_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_oak_gam_mean_site$gam_WZ_mean[i+1]-HF2018_oak_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_oak_gam_mean_site$gam_MZ_mean[i+1]-HF2018_oak_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2018_oak_gam_mean_site$dcell = dcell
      HF2018_oak_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2018_oak_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2018_oak_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2018_oak_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #oak 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_oak_gam_mean_site$DY)-1)){
        dcell[i+1] = HF2019_oak_gam_mean_site$gam_EWMZ_mean[i+1]-HF2019_oak_gam_mean_site$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_oak_gam_mean_site$gam_CZ_mean[i+1]-HF2019_oak_gam_mean_site$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_oak_gam_mean_site$gam_EZ_mean[i+1]-HF2019_oak_gam_mean_site$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_oak_gam_mean_site$gam_WZ_mean[i+1]-HF2019_oak_gam_mean_site$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_oak_gam_mean_site$gam_MZ_mean[i+1]-HF2019_oak_gam_mean_site$gam_MZ_mean[i]
      }
      
      HF2019_oak_gam_mean_site$dcell = dcell
      HF2019_oak_gam_mean_site$dgam_CZ_mean = dgam_CZ_mean
      HF2019_oak_gam_mean_site$dgam_EZ_mean = dgam_EZ_mean
      HF2019_oak_gam_mean_site$dgam_WZ_mean = dgam_WZ_mean
      HF2019_oak_gam_mean_site$dgam_MZ_mean = dgam_MZ_mean
    }
  }  
  
  #no cut
  {
    #pine 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_pine_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2017_pine_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2017_pine_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_pine_gam_mean_site_com$gam_CZ_mean[i+1]-HF2017_pine_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_pine_gam_mean_site_com$gam_EZ_mean[i+1]-HF2017_pine_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_pine_gam_mean_site_com$gam_WZ_mean[i+1]-HF2017_pine_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_pine_gam_mean_site_com$gam_MZ_mean[i+1]-HF2017_pine_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2017_pine_gam_mean_site_com$dcell = dcell
      HF2017_pine_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2017_pine_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2017_pine_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2017_pine_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #pine 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_pine_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2018_pine_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2018_pine_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_pine_gam_mean_site_com$gam_CZ_mean[i+1]-HF2018_pine_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_pine_gam_mean_site_com$gam_EZ_mean[i+1]-HF2018_pine_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_pine_gam_mean_site_com$gam_WZ_mean[i+1]-HF2018_pine_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_pine_gam_mean_site_com$gam_MZ_mean[i+1]-HF2018_pine_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2018_pine_gam_mean_site_com$dcell = dcell
      HF2018_pine_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2018_pine_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2018_pine_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2018_pine_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #pine 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_pine_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2019_pine_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2019_pine_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_pine_gam_mean_site_com$gam_CZ_mean[i+1]-HF2019_pine_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_pine_gam_mean_site_com$gam_EZ_mean[i+1]-HF2019_pine_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_pine_gam_mean_site_com$gam_WZ_mean[i+1]-HF2019_pine_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_pine_gam_mean_site_com$gam_MZ_mean[i+1]-HF2019_pine_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2019_pine_gam_mean_site_com$dcell = dcell
      HF2019_pine_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2019_pine_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2019_pine_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2019_pine_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_maple_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2017_maple_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2017_maple_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_maple_gam_mean_site_com$gam_CZ_mean[i+1]-HF2017_maple_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_maple_gam_mean_site_com$gam_EZ_mean[i+1]-HF2017_maple_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_maple_gam_mean_site_com$gam_WZ_mean[i+1]-HF2017_maple_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_maple_gam_mean_site_com$gam_MZ_mean[i+1]-HF2017_maple_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2017_maple_gam_mean_site_com$dcell = dcell
      HF2017_maple_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2017_maple_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2017_maple_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2017_maple_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_maple_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2018_maple_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2018_maple_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_maple_gam_mean_site_com$gam_CZ_mean[i+1]-HF2018_maple_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_maple_gam_mean_site_com$gam_EZ_mean[i+1]-HF2018_maple_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_maple_gam_mean_site_com$gam_WZ_mean[i+1]-HF2018_maple_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_maple_gam_mean_site_com$gam_MZ_mean[i+1]-HF2018_maple_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2018_maple_gam_mean_site_com$dcell = dcell
      HF2018_maple_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2018_maple_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2018_maple_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2018_maple_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #maple 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_maple_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2019_maple_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2019_maple_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_maple_gam_mean_site_com$gam_CZ_mean[i+1]-HF2019_maple_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_maple_gam_mean_site_com$gam_EZ_mean[i+1]-HF2019_maple_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_maple_gam_mean_site_com$gam_WZ_mean[i+1]-HF2019_maple_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_maple_gam_mean_site_com$gam_MZ_mean[i+1]-HF2019_maple_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2019_maple_gam_mean_site_com$dcell = dcell
      HF2019_maple_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2019_maple_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2019_maple_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2019_maple_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }  
    
    #oak 2017
    {
      #define variables
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2017_oak_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2017_oak_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2017_oak_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2017_oak_gam_mean_site_com$gam_CZ_mean[i+1]-HF2017_oak_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2017_oak_gam_mean_site_com$gam_EZ_mean[i+1]-HF2017_oak_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2017_oak_gam_mean_site_com$gam_WZ_mean[i+1]-HF2017_oak_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2017_oak_gam_mean_site_com$gam_MZ_mean[i+1]-HF2017_oak_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2017_oak_gam_mean_site_com$dcell = dcell
      HF2017_oak_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2017_oak_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2017_oak_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2017_oak_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #oak 2018
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      for (i in 1:(length(HF2018_oak_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2018_oak_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2018_oak_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2018_oak_gam_mean_site_com$gam_CZ_mean[i+1]-HF2018_oak_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2018_oak_gam_mean_site_com$gam_EZ_mean[i+1]-HF2018_oak_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2018_oak_gam_mean_site_com$gam_WZ_mean[i+1]-HF2018_oak_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2018_oak_gam_mean_site_com$gam_MZ_mean[i+1]-HF2018_oak_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2018_oak_gam_mean_site_com$dcell = dcell
      HF2018_oak_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2018_oak_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2018_oak_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2018_oak_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
    
    #oak 2019
    {
      #define variables   
      {
        dcell <- numeric(0)
        dgam_CZ_mean <- numeric(0)
        dgam_EZ_mean <- numeric(0)
        dgam_WZ_mean <- numeric(0)
        dgam_MZ_mean <- numeric(0)
      }
      
      for (i in 1:(length(HF2019_oak_gam_mean_site_com$DY)-1)){
        dcell[i+1] = HF2019_oak_gam_mean_site_com$gam_EWMZ_mean[i+1]-HF2019_oak_gam_mean_site_com$gam_EWMZ_mean[i]
        dgam_CZ_mean[i+1] = HF2019_oak_gam_mean_site_com$gam_CZ_mean[i+1]-HF2019_oak_gam_mean_site_com$gam_CZ_mean[i]
        dgam_EZ_mean[i+1] = HF2019_oak_gam_mean_site_com$gam_EZ_mean[i+1]-HF2019_oak_gam_mean_site_com$gam_EZ_mean[i]
        dgam_WZ_mean[i+1] = HF2019_oak_gam_mean_site_com$gam_WZ_mean[i+1]-HF2019_oak_gam_mean_site_com$gam_WZ_mean[i]
        dgam_MZ_mean[i+1] = HF2019_oak_gam_mean_site_com$gam_MZ_mean[i+1]-HF2019_oak_gam_mean_site_com$gam_MZ_mean[i]
      }
      
      HF2019_oak_gam_mean_site_com$dcell = dcell
      HF2019_oak_gam_mean_site_com$dgam_CZ_mean = dgam_CZ_mean
      HF2019_oak_gam_mean_site_com$dgam_EZ_mean = dgam_EZ_mean
      HF2019_oak_gam_mean_site_com$dgam_WZ_mean = dgam_WZ_mean
      HF2019_oak_gam_mean_site_com$dgam_MZ_mean = dgam_MZ_mean
    }
  }
  
  
  #combine mean and sd dfs
  HF2017_pine_gam_mean_sd_com = merge(HF2017_pine_gam_mean_site_com,HF2017_pine_gam_sd_site_com)
  HF2018_pine_gam_mean_sd_com = merge(HF2018_pine_gam_mean_site_com,HF2018_pine_gam_sd_site_com)
  HF2019_pine_gam_mean_sd_com = merge(HF2019_pine_gam_mean_site_com,HF2019_pine_gam_sd_site_com)
  
  HF2017_oak_gam_mean_sd_com = merge(HF2017_oak_gam_mean_site_com,HF2017_oak_gam_sd_site_com)
  HF2018_oak_gam_mean_sd_com = merge(HF2018_oak_gam_mean_site_com,HF2018_oak_gam_sd_site_com)
  HF2019_oak_gam_mean_sd_com = merge(HF2019_oak_gam_mean_site_com,HF2019_oak_gam_sd_site_com)
  
  HF2017_maple_gam_mean_sd_com = merge(HF2017_maple_gam_mean_site_com,HF2017_maple_gam_sd_site_com)
  HF2018_maple_gam_mean_sd_com = merge(HF2018_maple_gam_mean_site_com,HF2018_maple_gam_sd_site_com)
  HF2019_maple_gam_mean_sd_com = merge(HF2019_maple_gam_mean_site_com,HF2019_maple_gam_sd_site_com) 
  
}

#wood phenology
{
  
  Pheno_PINE_2017 = computeCriticalDates(HF2017_pine_mean.sdd_filled1, plot=TRUE)
  Pheno_PINE_2018 = computeCriticalDates(HF2018_pine_mean.sdd, plot=TRUE)
  Pheno_PINE_2019 = computeCriticalDates(HF2019_pine_mean.sdd, plot=TRUE)
  
  plotWoodFormationCalendar(Pheno_PINE_2017, level = "Group.Dates")
  plotWoodFormationCalendar(Pheno_PINE_2018, level = "Group.Dates")
  plotWoodFormationCalendar(Pheno_PINE_2019, level = "Group.Dates")
  
  Pheno_MAPLE_2017 = computeCriticalDates(HF2017_maple_mean.sdd, plot=TRUE)
  Pheno_MAPLE_2018 = computeCriticalDates(HF2018_maple_mean.sdd, plot=TRUE)
  Pheno_MAPLE_2019 = computeCriticalDates(HF2019_maple_mean.sdd, plot=TRUE)
  
  plotWoodFormationCalendar(Pheno_MAPLE_2017, level = "Group.Dates")  
  plotWoodFormationCalendar(Pheno_MAPLE_2018, level = "Group.Dates")
  plotWoodFormationCalendar(Pheno_MAPLE_2019, level = "Group.Dates")
  
  Pheno_OAK_2017 = computeCriticalDates(HF2017_oak_mean.sdd, plot=TRUE)
  Pheno_OAK_2018 = computeCriticalDates(HF2018_oak_mean.sdd, plot=TRUE)
  Pheno_OAK_2019 = computeCriticalDates(HF2019_oak_mean.sdd, plot=TRUE)
  
  plotWoodFormationCalendar(Pheno_OAK_2017, level = "Group.Dates") 
  plotWoodFormationCalendar(Pheno_OAK_2018, level = "Group.Dates")
  plotWoodFormationCalendar(Pheno_OAK_2019, level = "Group.Dates")
  
  Pheno_PINE = rbind(Pheno_PINE_2017,Pheno_PINE_2018,Pheno_PINE_2019)
  Pheno_MAPLE = rbind(Pheno_MAPLE_2017,Pheno_MAPLE_2018,Pheno_MAPLE_2019)
  Pheno_OAK = rbind(Pheno_OAK_2017,Pheno_OAK_2018,Pheno_OAK_2019)
  
  Spec_PINE_df = data.frame(Species = rep("PIST",length(Pheno_PINE$Tree)))
  Spec_MAPLE_df = data.frame(Species = rep("ACRU",length(Pheno_MAPLE$Tree)))
  Spec_OAK_df = data.frame(Species = rep("QURU",length(Pheno_OAK$Tree))) 
  
  Year_PINE_df = data.frame(Year = c(rep(2017,length(Pheno_PINE_2017$Tree)),rep(2018,length(Pheno_PINE_2018$Tree)),rep(2019,length(Pheno_PINE_2019$Tree))))
  Year_MAPLE_df = data.frame(Year = c(rep(2017,length(Pheno_MAPLE_2017$Tree)),rep(2018,length(Pheno_MAPLE_2018$Tree)),rep(2019,length(Pheno_MAPLE_2019$Tree))))
  Year_OAK_df = data.frame(Year = c(rep(2017,length(Pheno_OAK_2017$Tree)),rep(2018,length(Pheno_OAK_2018$Tree)),rep(2019,length(Pheno_OAK_2019$Tree))))
  
  Pheno_PINE_df = cbind(Year_PINE_df,Spec_PINE_df,Pheno_PINE)
  Pheno_MAPLE_df = cbind(Year_MAPLE_df,Spec_MAPLE_df,Pheno_MAPLE)
  Pheno_OAK_df = cbind(Year_OAK_df,Spec_OAK_df,Pheno_OAK)
  
  Pheno_wood_df = rbind(Pheno_PINE_df,Pheno_MAPLE_df,Pheno_OAK_df)
  
  
  
  #terms:bE:begin of the enlargement cE: cessation of the enlargement
  #dE: duration of ... dX: total duration
  Pheno_PINE_df_mean <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_PINE_df,FUN=mean)
  names(Pheno_PINE_df_mean) <- c("Species","Year","bE_mean","bW_mean","bM_mean","cE_mean","cW_mean","dE_mean","dW_mean","dX_mean")
  Pheno_PINE_df_sd <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_PINE_df,FUN=sd)
  Pheno_PINE_df_sd = Pheno_PINE_df_sd[,-1]
  Pheno_PINE_df_sd = Pheno_PINE_df_sd[,-1]
  names(Pheno_PINE_df_sd) <- c("bE_sd","bW_sd","bM_sd","cE_sd","cW_sd","dE_sd","dW_sd","dX_sd")
  Pheno_PINE_df_comb <- cbind(Pheno_PINE_df_mean,Pheno_PINE_df_sd)
  
  Pheno_MAPLE_df_mean <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_MAPLE_df,FUN=mean)
  names(Pheno_MAPLE_df_mean) <- c("Species","Year","bE_mean","bW_mean","bM_mean","cE_mean","cW_mean","dE_mean","dW_mean","dX_mean")
  Pheno_MAPLE_df_sd <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_MAPLE_df,FUN=sd) 
  Pheno_MAPLE_df_sd = Pheno_MAPLE_df_sd[,-1]
  Pheno_MAPLE_df_sd = Pheno_MAPLE_df_sd[,-1]
  names(Pheno_MAPLE_df_sd) <- c("bE_sd","bW_sd","bM_sd","cE_sd","cW_sd","dE_sd","dW_sd","dX_sd")
  Pheno_MAPLE_df_comb <- cbind(Pheno_MAPLE_df_mean,Pheno_MAPLE_df_sd) 
  
  Pheno_OAK_df_mean <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_OAK_df,FUN=mean)
  names(Pheno_OAK_df_mean) <- c("Species","Year","bE_mean","bW_mean","bM_mean","cE_mean","cW_mean","dE_mean","dW_mean","dX_mean")
  Pheno_OAK_df_sd <- aggregate(cbind(bE,bW,bM,cE,cW,dE,dW,dX)~Species + Year,data = Pheno_OAK_df,FUN=sd) 
  Pheno_OAK_df_sd = Pheno_OAK_df_sd[,-1]
  Pheno_OAK_df_sd = Pheno_OAK_df_sd[,-1]
  names(Pheno_OAK_df_sd) <- c("bE_sd","bW_sd","bM_sd","cE_sd","cW_sd","dE_sd","dW_sd","dX_sd")
  Pheno_OAK_df_comb <- cbind(Pheno_OAK_df_mean,Pheno_OAK_df_sd)
  
  
  #yearly output
  Pheno_OAK_2017_df <- subset(Pheno_OAK_df_comb,Year == 2017)
  Pheno_OAK_2018_df <- subset(Pheno_OAK_df_comb,Year == 2018)
  Pheno_OAK_2019_df <- subset(Pheno_OAK_df_comb,Year == 2019)
  
  Pheno_MAPLE_2017_df <- subset(Pheno_MAPLE_df_comb,Year == 2017)
  Pheno_MAPLE_2018_df <- subset(Pheno_MAPLE_df_comb,Year == 2018)
  Pheno_MAPLE_2019_df <- subset(Pheno_MAPLE_df_comb,Year == 2019)
  
  Pheno_PINE_2017_df <- subset(Pheno_PINE_df_comb,Year == 2017)
  Pheno_PINE_2018_df <- subset(Pheno_PINE_df_comb,Year == 2018)
  Pheno_PINE_2019_df <- subset(Pheno_PINE_df_comb,Year == 2019)
  
}


#combine leaf & wood & GDD & CDD
{
  
  
}

#daily fluxes
{
  flux <- read.csv("hf004-02-filled_updated.csv")
  flux <- flux[,-1]
  #gee unit: gross ecosystem exchange derived and filled (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
  
  gee_yy_mean <- aggregate(cbind(gee)~year,data = flux,FUN = mean)
  gee_dd_mean <- aggregate(cbind(gee)~doy + year,data = flux,FUN=mean)
  gee_yy_mean$gpp_yy = -(gee_yy_mean$gee)
  gpp_dd = -(gee_dd_mean$gee)
  gee_dd_mean$gpp_dd = -(gee_dd_mean$gee)  
  
  gee_dd_mean_2017 = subset(gee_dd_mean,year == "2017")
  gee_dd_mean_2018 = subset(gee_dd_mean,year == "2018")
  gee_dd_mean_2019 = subset(gee_dd_mean,year == "2019")
  
  names(gee_dd_mean_2017) = c("DY","year","gee","gpp_dd")
  names(gee_dd_mean_2018) = c("DY","year","gee","gpp_dd")
  names(gee_dd_mean_2019) = c("DY","year","gee","gpp_dd")
  
  #clim_dd 17-19
  library("dplyr")
  clim_dd <- read.csv("hf001-06-daily-prep_ta_par_for_R.csv")
  clim_dd <- clim_dd[,-1]
  
  clim_dd_2017 <- subset(clim_dd,year == 2017)
  clim_dd_2018 <- subset(clim_dd,year == 2018)
  clim_dd_2019 <- subset(clim_dd,year == 2019)
  
  clim_ww_mean_2017 <- aggregate(clim_dd_2017,by =list(clim_dd_2017$wk),FUN=mean,na.rm=TRUE, na.action=NULL)
  clim_ww_mean_2018 <- aggregate(clim_dd_2018,by =list(clim_dd_2018$wk),FUN=mean,na.rm=TRUE, na.action=NULL)
  clim_ww_mean_2019 <- aggregate(clim_dd_2019,by =list(clim_dd_2019$wk),FUN=mean,na.rm=TRUE, na.action=NULL)
  
  clim_ww_mean <- aggregate(clim_dd,by =list(clim_dd$wk),FUN=mean,na.rm=TRUE, na.action=NULL)
  clim_ww_yr_mean <- aggregate(clim_dd,by =list(clim_dd$wk,clim_dd$year),FUN=mean,na.rm=TRUE, na.action=NULL)
  clim_ww_sd <- aggregate(clim_ww_yr_mean,by =list(clim_ww_yr_mean$wk),FUN=sd,na.rm=TRUE)
  
  names(clim_ww_mean) = c("week","year","month","wk","jd","prec_mean","part_mean","ta_mean")
  names(clim_ww_sd) = c("week","Group.1","Group.2","year","month","wk","jd","prec_sd","part_sd","ta_sd")
  
  write.csv(clim_ww_mean,"clim_ww_mean.csv")
  write.csv(clim_ww_sd,"clim_ww_sd.csv")
  
  clim_ww_comb <- read.csv("clim_ww_comb.csv")
  
  #prep
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Prep_ww_2017_2021_6_14.pdf",width =10,height = 4)
    
    p_ww_prep_2017 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= prec_mean - prec_sd, 
                                          ymax= prec_mean + prec_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=prec_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2017,aes(x=jd,y= prec,color = "2017"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Prep mm")+
      xlab("DY")+
      ylim(-5,40)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2017" = "#00aedb"), labels = c('Prep_2017','Prep_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_ww_prep_2017
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Prep_ww_2018_2021_6_14.pdf",width =10,height = 4)
    p_ww_prep_2018 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= prec_mean - prec_sd, 
                                          ymax= prec_mean + prec_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=prec_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2018,aes(x=jd,y= prec,color = "2018"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Prep mm")+
      xlab("DY")+
      ylim(-5,40)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2018" = "#00aedb"), labels = c('Prep_2018','Prep_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_ww_prep_2018
    dev.off()
    
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Prep_ww_2019_2021_6_14.pdf",width =10,height = 4)
    
    p_ww_prep_2019 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= prec_mean - prec_sd, 
                                          ymax= prec_mean + prec_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=prec_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2019,aes(x=jd,y= prec,color = "2019"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Prep mm")+
      xlab("DY")+
      ylim(-5,40)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2019" = "#00aedb"), labels = c('Prep_2019','Prep_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_ww_prep_2019
    dev.off()
  } 
  
  #par
  {
    p_ww_part_2017 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= part_mean - part_sd, 
                                          ymax= part_mean + part_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=part_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2017,aes(x=jd,y= part,color = "2017"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("PAR mol/m2")+
      xlab("DOY")+
      ylim(0,60)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2017" = "#ffc425"), labels = c('PAR_2017','PAR_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_ww_part_2018 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= part_mean - part_sd, 
                                          ymax= part_mean + part_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=part_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2018,aes(x=jd,y= part,color = "2018"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("PAR mol/m2")+
      xlab("DOY")+
      ylim(0,60)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2018" = "#ffc425"), labels = c('PAR_2018','PAR_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_ww_part_2019 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= part_mean - part_sd, 
                                          ymax= part_mean + part_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=part_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2019,aes(x=jd,y= part,color = "2019"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("PAR mol/m2")+
      xlab("DOY")+
      ylim(0,60)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2019" = "#ffc425"), labels = c('PAR_2019','PAR_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  }  
  
  #temp
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Temp_ww_2017_2021_6_14.pdf",width =10,height = 4) 
    p_ww_ta_2017 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= ta_mean - ta_sd, 
                                          ymax= ta_mean + ta_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=ta_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2017,aes(x=jd,y= ta,color = "2017"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Temp °C")+
      xlab("DY")+
      ylim(-15,25)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM" = "#cccccc","2017" = "#d11141"), labels = c('Temp_2017','Temp_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_ww_ta_2017
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Temp_ww_2018_2021_6_14.pdf",width =10,height = 4) 
    p_ww_ta_2018 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= ta_mean - ta_sd, 
                                          ymax= ta_mean + ta_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=ta_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2018,aes(x=jd,y= ta,color = "2018"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Temp °C")+
      xlab("DY")+
      ylim(-15,25)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2018" = "#d11141"), labels = c('Temp_2018','Temp_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_ww_ta_2018
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Temp_ww_2019_2021_6_14.pdf",width =10,height = 4) 
    p_ww_ta_2019 <- ggplot()+
      geom_ribbon(data = clim_ww_comb,aes(x=jd,ymin= ta_mean - ta_sd, 
                                          ymax= ta_mean + ta_sd),fill = "lightgreen",alpha = 0.6)+
      geom_line(data =clim_ww_comb,aes(x=jd, 
                                       y=ta_mean,color = "MYM"),linetype="solid",size =1)+
      geom_line(data = clim_ww_mean_2019,aes(x=jd,y= ta,color = "2019"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Temp °C")+
      xlab("DY")+
      ylim(-15,25)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("MYM"="#cccccc","2019" = "#d11141"), labels = c('Temp_2019','Temp_MYM'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_ww_ta_2019
    dev.off()
  }
}

#Tree ring data
{
  library("dplR")
  library("dplyr")
  
  input_ACRU<-read.tucson('LF_ACRU.rwl')
  input_QURU<-read.tucson('LF_QURU.rwl')
  input_PIST<-read.tucson('HF_PIST_All.rwl')
  
  #ACRU
  start_ACRU<-1861
  end_ACRU<-2014
  #QURU
  start_QURU<-1870
  end_QURU<-2014
  #PIST
  start_PIST<-1863
  end_PIST<-2014
  
  # get period
  TRW_ACRU<-data.frame(year=seq(start_ACRU,end_ACRU,1))
  TRW_QURU<-data.frame(year=seq(start_QURU,end_QURU,1))
  TRW_PIST<-data.frame(year=seq(start_PIST,end_PIST,1))
  #dataframe for mean values
  TRW_ACRU_MEAN<-data.frame(year=seq(start_ACRU,end_ACRU,1))
  TRW_QURU_MEAN<-data.frame(year=seq(start_QURU,end_QURU,1))
  TRW_PIST_MEAN<-data.frame(year=seq(start_PIST,end_PIST,1))
  
  TRW_ACRU_DE_MEAN<-data.frame(year=seq(start_ACRU,end_ACRU,1))
  TRW_QURU_DE_MEAN<-data.frame(year=seq(start_QURU,end_QURU,1))
  TRW_PIST_DE_MEAN<-data.frame(year=seq(start_PIST,end_PIST,1))
  
  #dataframe for standard residuals
  TRW_ACRU_DE_std_res<-data.frame(year=seq(start_ACRU,end_ACRU,1)) 
  TRW_QURU_DE_std_res<-data.frame(year=seq(start_QURU,end_QURU,1)) 
  TRW_PIST_DE_std_res<-data.frame(year=seq(start_PIST,end_PIST,1)) 
  
  TRW_QURU_DE_MEAN_test<-data.frame(year=seq(start_QURU,end_QURU,1))
  
  #get the col names
  all_ACRU<-names(input_ACRU)
  all_QURU<-names(input_QURU)
  all_PIST<-names(input_PIST)
  
  trees_ACRU<-input_ACRU[names(input_ACRU)==all_ACRU,]
  trees_QURU<-input_QURU[names(input_QURU)==all_QURU,][1:145,]
  trees_PIST<-input_PIST[names(input_PIST)==all_PIST,][1:152,]
  
  # put together new dataframe TRW and the trees from the input
  TRW_ACRU<-cbind(TRW_ACRU,trees_ACRU)
  TRW_QURU<-cbind(TRW_QURU,trees_QURU)
  TRW_PIST<-cbind(TRW_PIST,trees_PIST)
  #create new dataframes to update
  TRW_ACRU_DE <- TRW_ACRU
  TRW_QURU_DE <- TRW_QURU
  TRW_PIST_DE <- TRW_PIST
  
  TRW_QURU_mean <- rowMeans(TRW_QURU[,2:145],na.rm=TRUE)
  TRW_ACRU_mean <- rowMeans(TRW_ACRU[,2:96],na.rm=TRUE)
  TRW_PIST_mean <- rowMeans(TRW_PIST[,2:31],na.rm=TRUE)
  
  TRW_QURU_MEAN <- cbind(TRW_QURU_MEAN,TRW_QURU_mean)  
  TRW_ACRU_MEAN <- cbind(TRW_ACRU_MEAN,TRW_ACRU_mean)
  TRW_PIST_MEAN <- cbind(TRW_PIST_MEAN,TRW_PIST_mean)
  
  chron_ACRU <- chron(input_ACRU)
  chron_QURU <- chron(input_QURU)
  chron_PIST <- chron(input_PIST)
  
  #chron with white noise
  chron_ACRU_wt <- chron(input_ACRU,prewhiten = TRUE)
  chron_QURU_wt <- chron(input_QURU,prewhiten = TRUE)
  chron_PIST_wt <- chron(input_PIST,prewhiten = TRUE)
  
  #chron with only red noise
  chron_ACRU_rt <- data.frame(rt = chron_ACRU_wt$xxxstd - chron_ACRU_wt$xxxres,samp.depth = chron_ACRU_wt$samp.depth)
  chron_QURU_rt <- data.frame(rt = chron_QURU_wt$xxxstd - chron_QURU_wt$xxxres,samp.depth = chron_QURU_wt$samp.depth)
  chron_PIST_rt <- data.frame(rt = chron_PIST_wt$xxxstd - chron_PIST_wt$xxxres,samp.depth = chron_PIST_wt$samp.depth)
  
  row.names(chron_ACRU_rt) = row.names(chron_ACRU_wt)
  row.names(chron_QURU_rt) = row.names(chron_QURU_wt)
  row.names(chron_PIST_rt) = row.names(chron_PIST_wt)
  
  sea_ACRU = sea(chron_ACRU,years)
  sea_QURU = sea(chron_QURU,years)
  sea_PIST = sea(chron_PIST,years)
  
  sea_ACRU_rt = sea(chron_ACRU_rt,years)
  sea_QURU_rt = sea(chron_QURU_rt,years)
  sea_PIST_rt = sea(chron_PIST_rt,years)
  #chron_ACRU_rt_DE = detrend(chron_ACRU_rt[1],method = c("Spline"))
}

years = c(1963,1964,1965,1966,1981,1985,1995,2002)

sea_ACRU_rt = sea(chron_ACRU_rt_58_14,years)  
sea_QURU_rt = sea(chron_QURU_rt_58_14,years)
sea_PIST_rt = sea(chron_PIST_rt_58_14,years)

Chron_58_14 = data.frame(Years = c(1958:2014),CR_ACRU = chron_ACRU$xxxstd[98:154],CR_QURU = chron_QURU$xxxstd[89:145],CR_PIST = chron_PIST$xxxstd[96:152])

p_Chron <- ggplot()+
  #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
  #                                                ymax= max),fill = "green",alpha = 0.4)+
  #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
  #                                                  ymax= max),fill = "pink",alpha = 0.4)+
  geom_line(data = Chron_58_14,aes(x=Years,y=CR_ACRU,color = "z"),linetype="solid",size = 1)+
  geom_line(data = Chron_58_14,aes(x=Years, 
                                   y=CR_QURU,color = "royalblue4"),linetype="solid",size =1)+
  geom_line(data = Chron_58_14,aes(x=Years,y=CR_PIST,color = "green4"),linetype="solid",size = 1)+ 
  #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
  ylab("RWI")+
  xlab("Year")+
  ylim(0,6)+
  #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
  scale_color_manual(name = '', 
                     values =c("green4" = "green4","royalblue4"="royalblue4","z" = "orange"), labels = c('RWI_PIST','RWI_QURU','RWI_ACRU'))+
  theme_set(theme_bw())+
  theme(panel.grid.major=element_line(colour=NA))+
  theme(legend.position = c(0.8,0.75),legend.text = element_text(size = 12))+
  theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))

}

#plot FLUX GPP vs TRW
{
  gpp_yy_QURU = read.csv("gpp_yy_QURU_TRW.csv")
  gpp_yy_ACRU = read.csv("gpp_yy_ACRU_TRW.csv")
  gpp_yy_PIST = read.csv("gpp_yy_PIST_TRW.csv") 
  
  gpp_yy_scaled = scale(gpp_yy_QURU$gpp_yy)
  TRW_QURU_scaled = scale(gpp_yy_QURU$QURU) 
  TRW_ACRU_scaled = scale(gpp_yy_ACRU$ACRU)
  TRW_PIST_scaled = scale(gpp_yy_PIST$PIST)
  
  yy_gpp_TRW = c(1992:2019)
  
  gpp_yy_TRW_scaled = data.frame(year = yy_gpp_TRW,gpp_scaled = gpp_yy_scaled,QURU_scaled = TRW_QURU_scaled,ACRU_scaled = TRW_ACRU_scaled,PIST_scaled = TRW_PIST_scaled)
  
  library("ggplot2")
  p1 <- ggplot()+
    #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                ymax= max),fill = "green",alpha = 0.4)+
    #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                  ymax= max),fill = "pink",alpha = 0.4)+
    geom_line(data = gpp_yy_TRW_scaled,aes(x=year, 
                                           y=gpp_scaled,color = "green4"),linetype="solid",size =1)+
    geom_line(data = gpp_yy_TRW_scaled,aes(x=year,y= PIST_scaled,color = "royalblue4"),linetype="solid",size = 1)+
    #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("z-score")+
    xlab("Year")+
    ylim(-4,4)+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("royalblue4"="orange","green4" = "black"), labels = c('GPP','TRW_PIST'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  
}

#daylength
{
  library("geosphere")
  #Lat of Harvard Forest: 42.537755
  dl_HF = daylength(42.537755,1:365)
  dl_df = data.frame(DY = gee_dd_mean_2017$DY,DL=dl_HF)
}

#combine cell count/length and GPP together
{
  gee_growth_pine_2017 = merge(gee_dd_mean_2017,HF2017_pine_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_pine_2017 = merge(gee_growth_pine_2017,dl_df,by = "DY",all = TRUE)
  gee_growth_pine_2018 = merge(gee_dd_mean_2018,HF2018_pine_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_pine_2018 = merge(gee_growth_pine_2018,dl_df,by = "DY",all = TRUE)  
  gee_growth_pine_2019 = merge(gee_dd_mean_2019,HF2019_pine_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_pine_2019 = merge(gee_growth_pine_2019,dl_df,by = "DY",all = TRUE)
  
  
  gee_growth_maple_2017 = merge(gee_dd_mean_2017,HF2017_maple_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_maple_2017 = merge(gee_growth_maple_2017,dl_df,by = "DY",all = TRUE)
  gee_growth_maple_2018 = merge(gee_dd_mean_2018,HF2018_maple_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_maple_2018 = merge(gee_growth_maple_2018,dl_df,by = "DY",all = TRUE)  
  gee_growth_maple_2019 = merge(gee_dd_mean_2019,HF2019_maple_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_maple_2019 = merge(gee_growth_maple_2019,dl_df,by = "DY",all = TRUE)
  
  gee_growth_oak_2017 = merge(gee_dd_mean_2017,HF2017_oak_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_oak_2017 = merge(gee_growth_oak_2017,dl_df,by = "DY",all = TRUE)
  gee_growth_oak_2018 = merge(gee_dd_mean_2018,HF2018_oak_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_oak_2018 = merge(gee_growth_oak_2018,dl_df,by = "DY",all = TRUE)  
  gee_growth_oak_2019 = merge(gee_dd_mean_2019,HF2019_oak_gam_mean_sd_com,by = "DY",all = TRUE)
  gee_growth_oak_2019 = merge(gee_growth_oak_2019,dl_df,by = "DY",all = TRUE)
}
#combine... tree level
{
  #pine
  {
    gee_growth_pine_tree_P16_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P16_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P20_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P20_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P21_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P21_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P25_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P25_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P26_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P26_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P7_2017 = merge(gee_dd_mean_2017,HF2017_pine_mean.sdd_filled1_P7_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P16_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P16_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P20_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P20_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P21_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P21_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P25_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P25_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P26_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P26_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P7_2018 = merge(gee_dd_mean_2018,HF2018_pine_mean.sdd_filled1_P7_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P16_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P16_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P20_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P20_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P21_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P21_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P25_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P25_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P26_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P26_com,by = "DY",all = TRUE)
    gee_growth_pine_tree_P7_2019 = merge(gee_dd_mean_2019,HF2019_pine_mean.sdd_filled1_P7_com,by = "DY",all = TRUE)
  }
  #oak
  {
    gee_growth_oak_tree_Q05_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q05_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q06_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q06_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q09_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q09_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q10_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q10_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q11_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q11_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q12_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q12_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q13_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q13_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q15_2017 = merge(gee_dd_mean_2017,HF2017_oak_mean.sdd_filled1_Q15_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q05_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q05_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q06_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q06_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q09_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q09_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q10_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q10_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q11_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q11_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q12_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q12_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q13_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q13_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q15_2018 = merge(gee_dd_mean_2018,HF2018_oak_mean.sdd_filled1_Q15_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q05_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q05_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q06_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q06_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q09_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q09_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q10_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q10_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q11_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q11_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q12_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q12_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q13_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q13_com,by = "DY",all = TRUE)
    gee_growth_oak_tree_Q15_2019 = merge(gee_dd_mean_2019,HF2019_oak_mean.sdd_filled1_Q15_com,by = "DY",all = TRUE)
    
    
  }
  #maple
  {
    gee_growth_maple_tree_A18_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A18_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A19_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A19_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A23_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A23_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A28_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A28_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A29_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A29_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A8_2017 = merge(gee_dd_mean_2017,HF2017_maple_mean.sdd_filled1_A8_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A18_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A18_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A19_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A19_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A23_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A23_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A28_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A28_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A29_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A29_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A8_2018 = merge(gee_dd_mean_2018,HF2018_maple_mean.sdd_filled1_A8_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A18_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A18_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A19_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A19_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A23_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A23_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A28_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A28_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A29_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A29_com,by = "DY",all = TRUE)
    gee_growth_maple_tree_A8_2019 = merge(gee_dd_mean_2019,HF2019_maple_mean.sdd_filled1_A8_com,by = "DY",all = TRUE)
    
  }
}

write.csv(gee_growth_pine_2017,"gee_pine_2017_ori2.csv")
write.csv(gee_growth_pine_2018,"gee_pine_2018_ori2.csv")
write.csv(gee_growth_pine_2019,"gee_pine_2019_ori2.csv")

write.csv(gee_growth_maple_2017,"gee_maple_2017_ori2.csv")
write.csv(gee_growth_maple_2018,"gee_maple_2018_ori2.csv")
write.csv(gee_growth_maple_2019,"gee_maple_2019_ori2.csv")

write.csv(gee_growth_oak_2017,"gee_oak_2017_ori2.csv")
write.csv(gee_growth_oak_2018,"gee_oak_2018_ori2.csv")
write.csv(gee_growth_oak_2019,"gee_oak_2019_ori2.csv")

#write tree level output
{
  write.csv(gee_growth_pine_tree_P16_2017,"gee_growth_pine_tree_P16_2017.csv")
  write.csv(gee_growth_pine_tree_P20_2017,"gee_growth_pine_tree_P20_2017.csv")
  write.csv(gee_growth_pine_tree_P21_2017,"gee_growth_pine_tree_P21_2017.csv")
  write.csv(gee_growth_pine_tree_P25_2017,"gee_growth_pine_tree_P25_2017.csv")
  write.csv(gee_growth_pine_tree_P26_2017,"gee_growth_pine_tree_P26_2017.csv")
  write.csv(gee_growth_pine_tree_P7_2017,"gee_growth_pine_tree_P7_2017.csv")
  write.csv(gee_growth_pine_tree_P16_2018,"gee_growth_pine_tree_P16_2018.csv")
  write.csv(gee_growth_pine_tree_P20_2018,"gee_growth_pine_tree_P20_2018.csv")
  write.csv(gee_growth_pine_tree_P21_2018,"gee_growth_pine_tree_P21_2018.csv")
  write.csv(gee_growth_pine_tree_P25_2018,"gee_growth_pine_tree_P25_2018.csv")
  write.csv(gee_growth_pine_tree_P26_2018,"gee_growth_pine_tree_P26_2018.csv")
  write.csv(gee_growth_pine_tree_P7_2018,"gee_growth_pine_tree_P7_2018.csv")
  write.csv(gee_growth_pine_tree_P16_2019,"gee_growth_pine_tree_P16_2019.csv")
  write.csv(gee_growth_pine_tree_P20_2019,"gee_growth_pine_tree_P20_2019.csv")
  write.csv(gee_growth_pine_tree_P21_2019,"gee_growth_pine_tree_P21_2019.csv")
  write.csv(gee_growth_pine_tree_P25_2019,"gee_growth_pine_tree_P25_2019.csv")
  write.csv(gee_growth_pine_tree_P26_2019,"gee_growth_pine_tree_P26_2019.csv")
  write.csv(gee_growth_pine_tree_P7_2019,"gee_growth_pine_tree_P7_2019.csv")
  
  write.csv(gee_growth_maple_tree_A18_2017,"gee_growth_maple_tree_A18_2017.csv")
  write.csv(gee_growth_maple_tree_A19_2017,"gee_growth_maple_tree_A19_2017.csv")
  write.csv(gee_growth_maple_tree_A23_2017,"gee_growth_maple_tree_A23_2017.csv")
  write.csv(gee_growth_maple_tree_A28_2017,"gee_growth_maple_tree_A28_2017.csv")
  write.csv(gee_growth_maple_tree_A29_2017,"gee_growth_maple_tree_A29_2017.csv")
  write.csv(gee_growth_maple_tree_A8_2017,"gee_growth_maple_tree_A8_2017.csv")
  write.csv(gee_growth_maple_tree_A18_2018,"gee_growth_maple_tree_A18_2018.csv")
  write.csv(gee_growth_maple_tree_A19_2018,"gee_growth_maple_tree_A19_2018.csv")
  write.csv(gee_growth_maple_tree_A23_2018,"gee_growth_maple_tree_A23_2018.csv")
  write.csv(gee_growth_maple_tree_A28_2018,"gee_growth_maple_tree_A28_2018.csv")
  write.csv(gee_growth_maple_tree_A29_2018,"gee_growth_maple_tree_A29_2018.csv")
  write.csv(gee_growth_maple_tree_A8_2018,"gee_growth_maple_tree_A8_2018.csv")
  write.csv(gee_growth_maple_tree_A18_2019,"gee_growth_maple_tree_A18_2019.csv")
  write.csv(gee_growth_maple_tree_A19_2019,"gee_growth_maple_tree_A19_2019.csv")
  write.csv(gee_growth_maple_tree_A23_2019,"gee_growth_maple_tree_A23_2019.csv")
  write.csv(gee_growth_maple_tree_A28_2019,"gee_growth_maple_tree_A28_2019.csv")
  write.csv(gee_growth_maple_tree_A29_2019,"gee_growth_maple_tree_A29_2019.csv")
  write.csv(gee_growth_maple_tree_A8_2019,"gee_growth_maple_tree_A8_2019.csv")
  
  write.csv(gee_growth_oak_tree_Q05_2017,"gee_growth_oak_tree_Q05_2017.csv")
  write.csv(gee_growth_oak_tree_Q06_2017,"gee_growth_oak_tree_Q06_2017.csv")
  write.csv(gee_growth_oak_tree_Q09_2017,"gee_growth_oak_tree_Q09_2017.csv")
  write.csv(gee_growth_oak_tree_Q10_2017,"gee_growth_oak_tree_Q10_2017.csv")
  write.csv(gee_growth_oak_tree_Q11_2017,"gee_growth_oak_tree_Q11_2017.csv")
  write.csv(gee_growth_oak_tree_Q12_2017,"gee_growth_oak_tree_Q12_2017.csv")
  write.csv(gee_growth_oak_tree_Q13_2017,"gee_growth_oak_tree_Q13_2017.csv")
  write.csv(gee_growth_oak_tree_Q15_2017,"gee_growth_oak_tree_Q15_2017.csv")
  write.csv(gee_growth_oak_tree_Q05_2018,"gee_growth_oak_tree_Q05_2018.csv")
  write.csv(gee_growth_oak_tree_Q06_2018,"gee_growth_oak_tree_Q06_2018.csv")
  write.csv(gee_growth_oak_tree_Q09_2018,"gee_growth_oak_tree_Q09_2018.csv")
  write.csv(gee_growth_oak_tree_Q10_2018,"gee_growth_oak_tree_Q10_2018.csv")
  write.csv(gee_growth_oak_tree_Q11_2018,"gee_growth_oak_tree_Q11_2018.csv")
  write.csv(gee_growth_oak_tree_Q12_2018,"gee_growth_oak_tree_Q12_2018.csv")
  write.csv(gee_growth_oak_tree_Q13_2018,"gee_growth_oak_tree_Q13_2018.csv")
  write.csv(gee_growth_oak_tree_Q15_2018,"gee_growth_oak_tree_Q15_2018.csv")
  write.csv(gee_growth_oak_tree_Q05_2019,"gee_growth_oak_tree_Q05_2019.csv")
  write.csv(gee_growth_oak_tree_Q06_2019,"gee_growth_oak_tree_Q06_2019.csv")
  write.csv(gee_growth_oak_tree_Q09_2019,"gee_growth_oak_tree_Q09_2019.csv")
  write.csv(gee_growth_oak_tree_Q10_2019,"gee_growth_oak_tree_Q10_2019.csv")
  write.csv(gee_growth_oak_tree_Q11_2019,"gee_growth_oak_tree_Q11_2019.csv")
  write.csv(gee_growth_oak_tree_Q12_2019,"gee_growth_oak_tree_Q12_2019.csv")
  write.csv(gee_growth_oak_tree_Q13_2019,"gee_growth_oak_tree_Q13_2019.csv")
  write.csv(gee_growth_oak_tree_Q15_2019,"gee_growth_oak_tree_Q15_2019.csv")
  
  
}


#read data with ww outputs
{
  gpp_oak_2017 = read.csv("gee_oak_2017_ori2.csv")
  gpp_oak_2018 = read.csv("gee_oak_2018_ori2.csv")  
  gpp_oak_2019 = read.csv("gee_oak_2019_ori2.csv")
  
  gpp_maple_2017 = read.csv("gee_maple_2017_ori2.csv")
  gpp_maple_2018 = read.csv("gee_maple_2018_ori2.csv")
  gpp_maple_2019 = read.csv("gee_maple_2019_ori2.csv")
  
  gpp_pine_2017 = read.csv("gee_pine_2017_ori2.csv")
  gpp_pine_2018 = read.csv("gee_pine_2018_ori2.csv")
  gpp_pine_2019 = read.csv("gee_pine_2019_ori2.csv")
  
  #read tree level data
  {
    gpp_growth_pine_tree_P16_2017=read.csv("gee_growth_pine_tree_P16_2017.csv")
    gpp_growth_pine_tree_P20_2017=read.csv("gee_growth_pine_tree_P20_2017.csv")
    gpp_growth_pine_tree_P21_2017=read.csv("gee_growth_pine_tree_P21_2017.csv")
    gpp_growth_pine_tree_P25_2017=read.csv("gee_growth_pine_tree_P25_2017.csv")
    gpp_growth_pine_tree_P26_2017=read.csv("gee_growth_pine_tree_P26_2017.csv")
    gpp_growth_pine_tree_P7_2017=read.csv("gee_growth_pine_tree_P7_2017.csv")
    gpp_growth_pine_tree_P16_2018=read.csv("gee_growth_pine_tree_P16_2018.csv")
    gpp_growth_pine_tree_P20_2018=read.csv("gee_growth_pine_tree_P20_2018.csv")
    gpp_growth_pine_tree_P21_2018=read.csv("gee_growth_pine_tree_P21_2018.csv")
    gpp_growth_pine_tree_P25_2018=read.csv("gee_growth_pine_tree_P25_2018.csv")
    gpp_growth_pine_tree_P26_2018=read.csv("gee_growth_pine_tree_P26_2018.csv")
    gpp_growth_pine_tree_P7_2018=read.csv("gee_growth_pine_tree_P7_2018.csv")
    gpp_growth_pine_tree_P16_2019=read.csv("gee_growth_pine_tree_P16_2019.csv")
    gpp_growth_pine_tree_P20_2019=read.csv("gee_growth_pine_tree_P20_2019.csv")
    gpp_growth_pine_tree_P21_2019=read.csv("gee_growth_pine_tree_P21_2019.csv")
    gpp_growth_pine_tree_P25_2019=read.csv("gee_growth_pine_tree_P25_2019.csv")
    gpp_growth_pine_tree_P26_2019=read.csv("gee_growth_pine_tree_P26_2019.csv")
    gpp_growth_pine_tree_P7_2019=read.csv("gee_growth_pine_tree_P7_2019.csv")
    
    gpp_growth_maple_tree_A18_2017=read.csv("gee_growth_maple_tree_A18_2017.csv")
    gpp_growth_maple_tree_A19_2017=read.csv("gee_growth_maple_tree_A19_2017.csv")
    gpp_growth_maple_tree_A23_2017=read.csv("gee_growth_maple_tree_A23_2017.csv")
    gpp_growth_maple_tree_A28_2017=read.csv("gee_growth_maple_tree_A28_2017.csv")
    gpp_growth_maple_tree_A29_2017=read.csv("gee_growth_maple_tree_A29_2017.csv")
    gpp_growth_maple_tree_A8_2017=read.csv("gee_growth_maple_tree_A8_2017.csv")
    gpp_growth_maple_tree_A18_2018=read.csv("gee_growth_maple_tree_A18_2018.csv")
    gpp_growth_maple_tree_A19_2018=read.csv("gee_growth_maple_tree_A19_2018.csv")
    gpp_growth_maple_tree_A23_2018=read.csv("gee_growth_maple_tree_A23_2018.csv")
    gpp_growth_maple_tree_A28_2018=read.csv("gee_growth_maple_tree_A28_2018.csv")
    gpp_growth_maple_tree_A29_2018=read.csv("gee_growth_maple_tree_A29_2018.csv")
    gpp_growth_maple_tree_A8_2018=read.csv("gee_growth_maple_tree_A8_2018.csv")
    gpp_growth_maple_tree_A18_2019=read.csv("gee_growth_maple_tree_A18_2019.csv")
    gpp_growth_maple_tree_A19_2019=read.csv("gee_growth_maple_tree_A19_2019.csv")
    gpp_growth_maple_tree_A23_2019=read.csv("gee_growth_maple_tree_A23_2019.csv")
    gpp_growth_maple_tree_A28_2019=read.csv("gee_growth_maple_tree_A28_2019.csv")
    gpp_growth_maple_tree_A29_2019=read.csv("gee_growth_maple_tree_A29_2019.csv")
    gpp_growth_maple_tree_A8_2019=read.csv("gee_growth_maple_tree_A8_2019.csv")
    
    gpp_growth_oak_tree_Q05_2017=read.csv("gee_growth_oak_tree_Q05_2017.csv")
    gpp_growth_oak_tree_Q06_2017=read.csv("gee_growth_oak_tree_Q06_2017.csv")
    gpp_growth_oak_tree_Q09_2017=read.csv("gee_growth_oak_tree_Q09_2017.csv")
    gpp_growth_oak_tree_Q10_2017=read.csv("gee_growth_oak_tree_Q10_2017.csv")
    gpp_growth_oak_tree_Q11_2017=read.csv("gee_growth_oak_tree_Q11_2017.csv")
    gpp_growth_oak_tree_Q12_2017=read.csv("gee_growth_oak_tree_Q12_2017.csv")
    gpp_growth_oak_tree_Q13_2017=read.csv("gee_growth_oak_tree_Q13_2017.csv")
    gpp_growth_oak_tree_Q15_2017=read.csv("gee_growth_oak_tree_Q15_2017.csv")
    gpp_growth_oak_tree_Q05_2018=read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Tree_level_files\\gee_growth_oak_tree_Q05_2018.csv")
    gpp_growth_oak_tree_Q06_2018=read.csv("gee_growth_oak_tree_Q06_2018.csv")
    gpp_growth_oak_tree_Q09_2018=read.csv("gee_growth_oak_tree_Q09_2018.csv")
    gpp_growth_oak_tree_Q10_2018=read.csv("gee_growth_oak_tree_Q10_2018.csv")
    gpp_growth_oak_tree_Q11_2018=read.csv("gee_growth_oak_tree_Q11_2018.csv")
    gpp_growth_oak_tree_Q12_2018=read.csv("gee_growth_oak_tree_Q12_2018.csv")
    gpp_growth_oak_tree_Q13_2018=read.csv("gee_growth_oak_tree_Q13_2018.csv")
    gpp_growth_oak_tree_Q15_2018=read.csv("gee_growth_oak_tree_Q15_2018.csv")
    gpp_growth_oak_tree_Q05_2019=read.csv("gee_growth_oak_tree_Q05_2019.csv")
    gpp_growth_oak_tree_Q06_2019=read.csv("gee_growth_oak_tree_Q06_2019.csv")
    gpp_growth_oak_tree_Q09_2019=read.csv("gee_growth_oak_tree_Q09_2019.csv")
    gpp_growth_oak_tree_Q10_2019=read.csv("gee_growth_oak_tree_Q10_2019.csv")
    gpp_growth_oak_tree_Q11_2019=read.csv("gee_growth_oak_tree_Q11_2019.csv")
    gpp_growth_oak_tree_Q12_2019=read.csv("gee_growth_oak_tree_Q12_2019.csv")
    gpp_growth_oak_tree_Q13_2019=read.csv("gee_growth_oak_tree_Q13_2019.csv")
    gpp_growth_oak_tree_Q15_2019=read.csv("gee_growth_oak_tree_Q15_2019.csv")
    
  }
}


library("ggplot2")

#multi-year mean
{
  p<- ggplot() + 
    geom_point(data = l_pheno_mean, aes(x=Group.1, y=bb.doy,color=Group.1,shape = Group.1))+
    geom_errorbar(data = l_pheno_mean, aes(x=Group.1,ymin=bb.doy-sd.bb.doy, ymax=bb.doy+sd.bb.doy,color = Group.1), width=.2,
                  position=position_dodge(0.05))+
    geom_point(data = l_pheno_mean, aes(x=Group.1, y=lc.doy,color=Group.1,shape = Group.1))+
    geom_errorbar(data = l_pheno_mean, aes(x=Group.1,ymin=lc.doy-sd.lc.doy, ymax=lc.doy+sd.lc.doy,color = Group.1), width=.2,
                  position=position_dodge(0.05))+
    geom_point(data = l_pheno_mean, aes(x=Group.1, y=lf.doy,color=Group.1,shape = Group.1))+
    geom_errorbar(data = l_pheno_mean, aes(x=Group.1,ymin=lf.doy-sd.lf.doy, ymax=lf.doy+sd.lf.doy,color = Group.1), width=.2,
                  position=position_dodge(0.05))+
    scale_colour_manual(values = c("red", "blue", "green"))+
    theme(legend.title=element_blank())+
    ylab("DOY")+
    xlab("Species")
}
#three year pattern
{
  p<- ggplot() + 
    geom_line(data = l_pheno_ACRU_17_19,aes(x=year, y=bb.doy))+
    geom_point(data = l_pheno_ACRU_17_19,aes(x=year, y=bb.doy))+
    geom_errorbar(data = l_pheno_ACRU_17_19,aes(x=year,ymin=bb.doy-sd.bb.doy, ymax=bb.doy+sd.bb.doy), width=.2,position=position_dodge(0.05))+
    geom_line(data = l_pheno_ACRU_17_19,aes(x=year, y=bb.doy))+
    geom_point(data = l_pheno_ACRU_17_19, aes(x=year, y=lc.doy))+
    geom_errorbar(data = l_pheno_ACRU_17_19, aes(x=year,ymin=lc.doy-sd.lc.doy, ymax=lc.doy+sd.lc.doy), width=.2,position=position_dodge(0.05))+
    geom_line(data = l_pheno_QURU_17_19,aes(x=year, y=bb.doy))+
    geom_point(data = l_pheno_QURU_17_19, aes(x=year, y=lc.doy))+
    geom_errorbar(data = l_pheno_QURU_17_19, aes(x=year,ymin=lc.doy-sd.lc.doy, ymax=lc.doy+sd.lc.doy), width=.2,position=position_dodge(0.05))+
    geom_line(data = l_pheno_QURU_17_19,aes(x=year, y=bb.doy))+
    geom_point(data = l_pheno_QURU_17_19, aes(x=year, y=lc.doy))+
    geom_errorbar(data = l_pheno_QURU_17_19, aes(x=year,ymin=lc.doy-sd.lc.doy, ymax=lc.doy+sd.lc.doy), width=.2,position=position_dodge(0.05))+
    theme(legend.title=element_blank())+
    ylab("DOY")+
    xlab("Species")  
}

#get the pheno & growth & gpp together

library("ggplot2")


#cumulative values
{
  #pine
  {
    #EWMZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EWMZ_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)  
      
      p1 <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500), width=.2,color = "darkred")+
        xlim(80,350)+
        ylim(-500,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EWMZ_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)
      
      p2 <- ggplot() + 
        
        geom_ribbon(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        #geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="d"),size = 1)+
        #geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="e"),size = 1)+
        #geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="f"),size = 1)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = l_pheno_QURU_17,aes(x=bb.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_QURU_18,aes(x=bb.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_QURU_18,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_QURU_19,aes(x=bb.doy,y=-950),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_QURU_19,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-950), width=.2,color = "#F8766D")+
        geom_point(data = l_pheno_QURU_17,aes(x=lf.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lf.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_QURU_18,aes(x=lf.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_QURU_18,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lf.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_QURU_19,aes(x=lf.doy,y=-950),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_QURU_19,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lf.doy,y=-950), width=.2,color = "#F8766D")+
        xlim(80,350)+
        ylim(-950,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p2
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EWMZ_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)
      p3 <- ggplot() + 
        
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = l_pheno_ACRU_17,aes(x=bb.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_ACRU_18,aes(x=bb.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_ACRU_18,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_ACRU_19,aes(x=bb.doy,y=-999),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_ACRU_19,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-999), width=.2,color = "#F8766D")+
        geom_point(data = l_pheno_ACRU_17,aes(x=lf.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_ACRU_17,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lf.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_ACRU_18,aes(x=lf.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_ACRU_18,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lc.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_ACRU_19,aes(x=lf.doy,y=-999),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_ACRU_19,aes(xmin=lf.doy-sd.lf.doy, xmax=lf.doy+sd.lc.doy,y=-999), width=.2,color = "#F8766D")+
        xlim(80,350)+
        ylim(-999,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      
      p3
      dev.off()
    }
    
    #EWMZ no phenology
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EWMZ_3Yrs_renew_2021_5_9_no_phenology.pdf",width =10,height = 5)  
      
      p1 <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EWMZ_3Yrs_renew_2021_5_9_no_phenology.pdf",width =10,height = 5)
      
      p2 <- ggplot() + 
        
        geom_ribbon(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        #geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="d"),size = 1)+
        #geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="e"),size = 1)+
        #geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="f"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p2
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EWMZ_3Yrs_renew_2021_5_9_no_phenology.pdf",width =10,height = 5)
      p3 <- ggplot() + 
        
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      
      p3
      dev.off()
    }  
    
    #EWMZ no phenology new color code
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EWMZ_3Yrs_renew_2021_6_12_no_phenology.pdf",width =10,height = 5)  
      
      p1 <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#FEB24C",alpha = 0.2)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#FC4E2A",alpha = 0.2)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                           ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#B10026",alpha = 0.2)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#B10026","b" = "#FC4E2A","c" = "#FEB24C"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EWMZ_3Yrs_renew_2021_6_12_no_phenology.pdf",width =10,height = 5)
      
      p2 <- ggplot() + 
        
        geom_ribbon(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill =  "#9EC9E2",alpha = 0.2)+
        
        geom_ribbon(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#3C93C2",alpha = 0.2)+
        geom_ribbon(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                                          ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#0D4A70",alpha = 0.2)+
        geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        #geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="d"),size = 1)+
        #geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="e"),size = 1)+
        #geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="f"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#0D4A70","b" = "#3C93C2","c" = "#9EC9E2"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p2
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EWMZ_3Yrs_renew_2021_6_12_no_phenology.pdf",width =10,height = 5)
      p3 <- ggplot() + 
        
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#9CCEA7",alpha = 0.2)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#40AD5A",alpha = 0.2)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "#06592A",alpha = 0.2)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        xlim(80,320)+
        ylim(-600,4000)+
        ylab("Width of the forming annual ring/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#06592A","b" = "#40AD5A","c" = "#9CCEA7"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      
      p3
      dev.off()
    }
    
    
    #CZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_CZ_3Yrs_updated_2021_3_24.pdf",width =6,height = 5)
      
      p1_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                           ymax= gam_CZ_mean + gam_CZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                           ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                           ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="c"),size = 1)+
        xlim(100,300)+
        ylim(0,200)+
        ylab("Zone width/μm")+
        #scale_y_continuous(breaks = pretty(c(0,200)),limits = range(pretty(c(0,200))))+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1_CZ
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_CZ_3Yrs_updated_2021_3_24.pdf",width =6,height = 5)
      
      p2_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                          ymax= gam_CZ_mean + gam_CZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                          ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                          ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="c"),size = 1)+
        xlim(100,300)+
        ylim(30,100)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_CZ
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_CZ_3Yrs_updated_2021_3_24.pdf",width =6,height = 5)
      
      p3_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                            ymax= gam_CZ_mean + gam_CZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                            ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_CZ_mean - gam_CZ_sd, 
                                                            ymax= gam_CZ_mean + gam_CZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_CZ_mean,color="c"),size = 1)+
        xlim(100,300)+
        ylim(0,150)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))  
      
      p3_CZ
      dev.off()
    }
    
    #EZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      
      p1_EZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      p2_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      p3_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))  
      p3_EZ
      dev.off()
    }
    
    #EZ new color codes  
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EZ_3Yrs_2021_6_12.pdf",width =6,height = 5)
      
      p1_EZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "#FEB24C",alpha = 0.2)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "#FC4E2A",alpha = 0.2)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "#B10026",alpha = 0.2)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#B10026","b" = "#FC4E2A","c" = "#FEB24C"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.85,0.85),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EZ_3Yrs_2021_6_12.pdf",width =6,height = 5)
      p2_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "#9EC9E2",alpha = 0.2)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "#3C93C2",alpha = 0.2)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "#0D4A70",alpha = 0.2)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#0D4A70","b" = "#3C93C2","c" = "#9EC9E2"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.85,0.85),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EZ_3Yrs_2021_6_12.pdf",width =6,height = 5)
      p3_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "#9CCEA7",alpha = 0.2)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "#40AD5A",alpha = 0.2)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "#06592A",alpha = 0.2)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
      # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-30,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="#06592A","b" = "#40AD5A","c" = "#9CCEA7"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.85,0.85),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))  
      p3_EZ
      dev.off()
    } 
    
    #ring width plot three years
    RW_3yrs_df = data.frame("Year" = c(2017,2018,2019),"QURU_mean" = c(1437.075,2415.425,2572.7125),"ACRU_mean" = c(1066,2409.2667,2143.55),"PIST_mean" = c(1253.7033,1447.1333,1296.35),"QURU_sd" = c(305.9351,504.2537,866.9513),"ACRU_sd" = c(440.7488,1700.8485,633.4869),"PIST_sd" = c(383.1682,794.2405,616.8066))
    {
      p1_RW <- ggplot() + 
        
        geom_ribbon(data = RW_3yrs_df,aes(x=Year,ymin= PIST_mean - PIST_sd, 
                                          ymax= PIST_mean + PIST_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = RW_3yrs_df,aes(x=Year,ymin= ACRU_mean - ACRU_sd, 
                                          ymax= ACRU_mean + ACRU_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = RW_3yrs_df,aes(x=Year,ymin= QURU_mean - QURU_sd, 
                                          ymax= QURU_mean + QURU_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = RW_3yrs_df,aes(x=Year, y=QURU_mean,color="a"),size = 1)+
        geom_line(data = RW_3yrs_df,aes(x=Year, y=ACRU_mean,color="b"),size = 1)+
        geom_line(data = RW_3yrs_df,aes(x=Year, y=PIST_mean,color="c"),size = 1)+
        #xlim(98,300)+
        ylim(0,4500)+
        ylab("Annual ring width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_RW
      
      dev.off()
      
    }
    
    
    #WZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_WZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      
      p1_WZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        # geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
      # geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-120,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_WZ
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_WZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      
      p2_WZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        # geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
      # geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-120,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_WZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_WZ_3Yrs_2021_5_9.pdf",width =6,height = 5)    
      
      p3_WZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        # geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        # geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        # geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        # geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        # geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
      # geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-120,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      p3_WZ
      dev.off()
    } 
    
    #MZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_MZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      
      p1_MZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                           ymax= gam_MZ_mean + gam_MZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                           ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                           ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="c"),size = 1)+
        #geom_point(data = Pheno_PINE_2017_df,aes(x=bM_mean,y=-120),color = "blue")+
        #geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-120), width=.2,color = "blue")+
        #geom_point(data = Pheno_PINE_2018_df,aes(x=bM_mean,y=-160),color = "darkgreen")+
        #geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-160), width=.2,color = "darkgreen")+
        #geom_point(data = Pheno_PINE_2019_df,aes(x=bM_mean,y=-200),color = "darkred")+
        #geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-200), width=.2,color = "darkred")+
        xlim(98,300)+
        ylim(-600,4000)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_MZ
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_MZ_3Yrs_2021_5_9.pdf",width =6,height = 5)
      
      p2_MZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                          ymax= gam_MZ_mean + gam_MZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                          ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                          ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="c"),size = 1)+
        #geom_point(data = Pheno_OAK_2017_df,aes(x=bM_mean,y=-120),color = "blue")+
        #geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-120), width=.2,color = "blue")+
        #geom_point(data = Pheno_OAK_2018_df,aes(x=bM_mean,y=-160),color = "darkgreen")+
        #geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-160), width=.2,color = "darkgreen")+
        #geom_point(data = Pheno_OAK_2019_df,aes(x=bM_mean,y=-200),color = "darkred")+
        #geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-200), width=.2,color = "darkred")+
        #geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        #geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        #geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        #geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        #geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
      #geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-600,4000)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_MZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_MZ_3Yrs_2021_5_9.pdf",width =6,height = 5)    
      
      p3_MZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                            ymax= gam_MZ_mean + gam_MZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                            ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_MZ_mean - gam_MZ_sd, 
                                                            ymax= gam_MZ_mean + gam_MZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_MZ_mean,color="c"),size = 1)+
        #geom_point(data = Pheno_MAPLE_2017_df,aes(x=bM_mean,y=-120),color = "blue")+
        #geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-120), width=.2,color = "blue")+
        #geom_point(data = Pheno_MAPLE_2018_df,aes(x=bM_mean,y=-160),color = "darkgreen")+
        #geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-160), width=.2,color = "darkgreen")+
        #geom_point(data = Pheno_MAPLE_2019_df,aes(x=bM_mean,y=-200),color = "darkred")+
        #geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bM_mean-bM_sd, xmax=bM_mean+bM_sd,y=-200), width=.2,color = "darkred")+
        #geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        #geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        #geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        #geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        #geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
      #geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
      xlim(98,300)+
        ylim(-600,4000)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      p3_MZ
      dev.off()    
      
      
    }
    
    #phenology only
    #original color code
    {
      p_p1 <- ggplot()+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_QURU_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("a"="#2e4057","b" = "#66a182","c" = "#d1495b","d" = "#00B9E3","e" = "#00BA38","f" = "#F8766D"), labels = c('2017_w','2018_w',"2019_w","2017_l","2018_l","2019_l"))+
        scale_y_continuous(breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      
      p_p2 <- ggplot()+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_ACRU_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("a"="#2e4057","b" = "#66a182","c" = "#d1495b","d" = "#00B9E3","e" = "#00BA38","f" = "#F8766D"), labels = c('2017_w','2018_w',"2019_w","2017_l","2018_l","2019_l"))+
        scale_y_continuous(breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p_p3 <- ggplot()+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-300,color = "a"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-400,color = "b"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-500,color = "c"),shape = 15,size = 5)+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "d"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "e"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("a"="#2e4057","b" = "#66a182","c" = "#d1495b","d" = "#00B9E3","e" = "#00BA38","f" = "#F8766D"), labels = c('2017_w','2018_w',"2019_w","2017_l","2018_l","2019_l"))+
        scale_y_continuous(breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }
    #new color code
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_Pheno_renew_2021_6_4.pdf",width =12,height = 4)
      
      p_pheno_QURU <- ggplot()+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-300,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-400,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-500,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-300,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-400,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-500,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-300,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-400,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-500,color = "a"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cE_mean,y=-290,color = "a"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-290,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cE_mean,y=-390,color = "a"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-390,color = "a"), width=3,height = 10)+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cE_mean,y=-490,color = "a"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-490,color = "a"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "d"),shape = 17,size = 4)+
        geom_point(data = l_pheno_QURU_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "d"), width=3,height = 10)+
        geom_point(data = l_pheno_QURU_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "d"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_QURU_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "d"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("a"="#2e4057","d" = "#00CCFF"))+
        scale_y_continuous(limits = c(-540,-250),breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none")+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p_pheno_QURU
      
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_Pheno_renew_2021_6_4.pdf",width =12,height = 4)
      
      p_pheno_ACRU <- ggplot()+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-300,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-400,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-500,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-300,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-400,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-500,color = "b"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cE_mean,y=-290,color = "b"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-290,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cE_mean,y=-390,color = "b"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-390,color = "b"), width=3,height = 10)+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cE_mean,y=-490,color = "b"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-490,color = "b"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "e"), width=3,height = 10)+
        geom_point(data = l_pheno_ACRU_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "e"),shape = 17,size = 4)+
        geom_errorbarh(data = l_pheno_ACRU_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "e"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("b" = "darkgreen","e" = "#00BA38"))+
        scale_y_continuous(limits = c(-540,-250),breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none")+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p_pheno_ACRU
      
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_Pheno_renew_2021_6_4.pdf",width =12,height = 4)
      
      p_pheno_PIST <- ggplot()+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-300,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-400,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-500,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-300,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-400,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-500,color = "c"),shape = 15,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cE_mean,y=-290,color = "c"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-290,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cE_mean,y=-390,color = "c"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-390,color = "c"), width=3,height = 10)+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cE_mean,y=-490,color = "c"),shape = 5,size = 4)+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-490,color = "c"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_17_Tim_df,aes(x=bb.doy_mean,y=-325,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_17_Tim_df,aes(xmin=bb.doy_mean - bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-325,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_18_Tim_df,aes(x=bb.doy_mean,y=-425,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_18_Tim_df,aes(xmin=bb.doy_mean- bb.doy_sd, xmax=bb.doy_mean + bb.doy_sd,y=-425,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_19_Tim_df,aes(x=bb.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_19_Tim_df,aes(xmin=bb.doy_mean-bb.doy_sd, xmax=bb.doy_mean+bb.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_17_Tim_df,aes(x=lf.doy_mean,y=-325,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_17_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-325,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_18_Tim_df,aes(x=lf.doy_mean,y=-425,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_18_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-425,color = "f"), width=3,height = 10)+
        geom_point(data = l_pheno_PIST_19_Tim_df,aes(x=lf.doy_mean,y=-525,color = "f"),shape = 17,size = 5)+
        geom_errorbarh(data = l_pheno_PIST_19_Tim_df,aes(xmin=lf.doy_mean-lf.doy_sd, xmax=lf.doy_mean+lf.doy_sd,y=-525,color = "f"), width=3,height = 10)+
        xlim(80,320)+
        ylab("Year")+
        xlab("DY")+
        scale_color_manual(name = '', 
                           values =c("c" = "darkred","f" = "#F8766D"))+
        scale_y_continuous(limits = c(-540,-250),breaks = round(seq(-300,-500, by = -100),1),labels = c("2017","2018","2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "none")+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p_pheno_PIST
      
      dev.off()
      
      
    }  
    #new color code with linerange
    {
      
      #QURU
      {
        pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_Pheno_renew_2021_10_18.pdf",width =12,height = 4)
        p_pheno_QURU_range = ggplot()+
          #wood phenology enlargement
          geom_linerange(data = Pheno_OAK_2017_df, aes(x = 300,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2017_df,aes(x = 300, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_OAK_2017_df,aes(x = 300, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_OAK_2018_df, aes(x = 200,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2018_df,aes(x = 200, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_OAK_2018_df,aes(x = 200, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_OAK_2019_df, aes(x = 100,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2019_df,aes(x = 100, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_OAK_2019_df,aes(x = 100, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          #wood phenology wallthickening
          geom_linerange(data = Pheno_OAK_2017_df, aes(x = 280,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2017_df,aes(x = 280, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_OAK_2017_df,aes(x = 280, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_OAK_2018_df, aes(x = 180,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2018_df,aes(x = 180, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_OAK_2018_df,aes(x = 180, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_OAK_2019_df, aes(x = 80,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_OAK_2019_df,aes(x = 80, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_OAK_2019_df,aes(x = 80, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          #leaf phenology
          geom_linerange(data = l_pheno_QURU_17_Tim_df, aes(x = 260,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_QURU_17_Tim_df,aes(x = 260, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_QURU_17_Tim_df,aes(x = 260, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_QURU_18_Tim_df, aes(x = 160,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_QURU_18_Tim_df,aes(x = 160, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_QURU_18_Tim_df,aes(x = 160, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_QURU_19_Tim_df, aes(x = 60,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_QURU_19_Tim_df,aes(x = 60, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_QURU_19_Tim_df,aes(x = 60, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          ylab("DY")+
          xlab("Year")+
          ylim(90,330)+
          scale_color_manual(name = '', labels = c("W_e","W_w","L"),
                             values =c("a"="#000066","b" = "#0000CC","c" = "#99CCFF"))+
          scale_x_continuous(limits = c(50,320),breaks = round(seq(80,280, by = 100),1),labels = c("2019","2018","2017"))+
          theme_set(theme_bw())+
          theme(legend.position = c(0.95,0.5),legend.text = element_text(size = 8))+
          theme(panel.grid.major=element_line(colour=NA))+
          theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =16),axis.title.x=element_text(size=12),axis.title.y=element_text(size=16))+
          coord_flip()
        p_pheno_QURU_range
        
        dev.off()
      }
      #ACRU
      {
        pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_Pheno_renew_2021_10_18.pdf",width =12,height = 4)
        p_pheno_ACRU_range = ggplot()+
          #wood phenology enlargement
          geom_linerange(data = Pheno_MAPLE_2017_df, aes(x = 300,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2017_df,aes(x = 300, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2017_df,aes(x = 300, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_MAPLE_2018_df, aes(x = 200,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2018_df,aes(x = 200, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2018_df,aes(x = 200, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_MAPLE_2019_df, aes(x = 100,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2019_df,aes(x = 100, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2019_df,aes(x = 100, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          #wood phenology wallthickening
          geom_linerange(data = Pheno_MAPLE_2017_df, aes(x = 280,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2017_df,aes(x = 280, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2017_df,aes(x = 280, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_MAPLE_2018_df, aes(x = 180,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2018_df,aes(x = 180, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2018_df,aes(x = 180, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_MAPLE_2019_df, aes(x = 80,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_MAPLE_2019_df,aes(x = 80, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_MAPLE_2019_df,aes(x = 80, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          #leaf phenology
          geom_linerange(data = l_pheno_ACRU_17_Tim_df, aes(x = 260,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_ACRU_17_Tim_df,aes(x = 260, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_ACRU_17_Tim_df,aes(x = 260, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_ACRU_18_Tim_df, aes(x = 160,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_ACRU_18_Tim_df,aes(x = 160, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_ACRU_18_Tim_df,aes(x = 160, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_ACRU_19_Tim_df, aes(x = 60,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_ACRU_19_Tim_df,aes(x = 60, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_ACRU_19_Tim_df,aes(x = 60, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          ylab("DY")+
          xlab("Year")+
          ylim(90,330)+
          scale_color_manual(name = '', labels = c("W_e","W_w","L"),
                             values =c("a"="#003300","b" = "#339966","c" = "#99CC00"))+
          scale_x_continuous(limits = c(50,320),breaks = round(seq(80,280, by = 100),1),labels = c("2019","2018","2017"))+
          theme_set(theme_bw())+
          theme(legend.position = c(0.95,0.5),legend.text = element_text(size = 8))+
          theme(panel.grid.major=element_line(colour=NA))+
          theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =16),axis.title.x=element_text(size=12),axis.title.y=element_text(size=16))+
          coord_flip()
        p_pheno_ACRU_range
        
        dev.off()
      }    
      #PIST
      {
        pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_Pheno_renew_2021_10_18.pdf",width =12,height = 4)
        p_pheno_PIST_range = ggplot()+
          #wood phenology enlargement
          geom_linerange(data = Pheno_PINE_2017_df, aes(x = 300,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2017_df,aes(x = 300, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_PINE_2017_df,aes(x = 300, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_PINE_2018_df, aes(x = 200,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2018_df,aes(x = 200, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_PINE_2018_df,aes(x = 200, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          geom_linerange(data = Pheno_PINE_2019_df, aes(x = 100,ymin = bE_mean, ymax = cE_mean,color = "a"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2019_df,aes(x = 100, ymin=bE_mean-bE_sd, ymax=bE_mean+bE_sd,color = "a", width=.1))+
          geom_errorbar(data = Pheno_PINE_2019_df,aes(x = 100, ymin=cE_mean-cE_sd, ymax=cE_mean+cE_sd,color = "a", width=.1))+
          #wood phenology wallthickening
          geom_linerange(data = Pheno_PINE_2017_df, aes(x = 280,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2017_df,aes(x = 280, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_PINE_2017_df,aes(x = 280, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_PINE_2018_df, aes(x = 180,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2018_df,aes(x = 180, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_PINE_2018_df,aes(x = 180, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          geom_linerange(data = Pheno_PINE_2019_df, aes(x = 80,ymin = bW_mean, ymax = cW_mean,color = "b"),size = 6)+
          geom_errorbar(data = Pheno_PINE_2019_df,aes(x = 80, ymin=bW_mean-bW_sd, ymax=bW_mean+bW_sd,color = "b", width=.1))+
          geom_errorbar(data = Pheno_PINE_2019_df,aes(x = 80, ymin=cW_mean-cW_sd, ymax=cW_mean+cW_sd,color = "b", width=.1))+
          #leaf phenology
          geom_linerange(data = l_pheno_PIST_17_Tim_df, aes(x = 260,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_PIST_17_Tim_df,aes(x = 260, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_PIST_17_Tim_df,aes(x = 260, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_PIST_18_Tim_df, aes(x = 160,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_PIST_18_Tim_df,aes(x = 160, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_PIST_18_Tim_df,aes(x = 160, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          geom_linerange(data = l_pheno_PIST_19_Tim_df, aes(x = 60,ymin = bb.doy_mean, ymax = lf.doy_mean,color = "c"),size = 6)+
          geom_errorbar(data = l_pheno_PIST_19_Tim_df,aes(x = 60, ymin=bb.doy_mean-bb.doy_sd, ymax=bb.doy_mean+bb.doy_sd,color = "c", width=.1))+
          geom_errorbar(data = l_pheno_PIST_19_Tim_df,aes(x = 60, ymin=lf.doy_mean-lf.doy_sd, ymax=lf.doy_mean+lf.doy_sd,color = "c", width=.1))+
          ylab("DY")+
          xlab("Year")+
          ylim(90,330)+
          scale_color_manual(name = '', labels = c("W_e","W_w","L"),
                             values =c("a"="#660000","b" = "#CC0000","c" = "#FF9999"))+
          scale_x_continuous(limits = c(50,320),breaks = round(seq(80,280, by = 100),1),labels = c("2019","2018","2017"))+
          theme_set(theme_bw())+
          theme(legend.position = c(0.95,0.5),legend.text = element_text(size = 8))+
          theme(panel.grid.major=element_line(colour=NA))+
          theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =16),axis.title.x=element_text(size=12),axis.title.y=element_text(size=16))+
          coord_flip()
        p_pheno_PIST_range
        
        dev.off()
      }   
      
      
    }  
  }
  
  #maple
  {
    gpp_maple_2017_na_omit = na.omit(gpp_maple_2017)
    gpp_maple_2018_na_omit = na.omit(gpp_maple_2018)
    gpp_maple_2019_na_omit = na.omit(gpp_maple_2019)
    
    #gam
    gam_maple_2017 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)
    gam_maple_2019 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2019_na_omit)
    
    gam_maple_2017_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2019_na_omit)  
    
    gam_maple_2017_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2019_na_omit) 
    
    gam_maple_2017_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2019_na_omit) 
    
    gpp_maple_2017_na_omit$gam_maple_2017 = gam_maple_2017$fitted.values
    gpp_maple_2018_na_omit$gam_maple_2018 = gam_maple_2018$fitted.values
    gpp_maple_2019_na_omit$gam_maple_2019 = gam_maple_2019$fitted.values
    
    
    p4 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2017_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2017_na_omit,aes(x=DY, y=gam_maple_2017),color="black",size = 2)+
      geom_line(data = gpp_maple_2017_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p5 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2018_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2018_na_omit,aes(x=DY, y=gam_maple_2018),color="black",size = 2)+
      geom_line(data = gpp_maple_2018_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p6 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2019_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2019_na_omit,aes(x=DY, y=gam_maple_2019),color="black",size = 2)+
      geom_line(data = gpp_maple_2019_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
  }
  
  #oak
  {
    gpp_oak_2017_na_omit = na.omit(gpp_oak_2017)
    gpp_oak_2018_na_omit = na.omit(gpp_oak_2018)
    gpp_oak_2019_na_omit = na.omit(gpp_oak_2019)
    
    #gam
    gam_oak_2017 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)
    gam_oak_2019 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2019_na_omit)
    
    gam_oak_2017_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2019_na_omit)  
    
    gam_oak_2017_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2019_na_omit) 
    
    gam_oak_2017_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2019_na_omit) 
    
    gpp_oak_2017_na_omit$gam_oak_2017 = gam_oak_2017$fitted.values
    gpp_oak_2018_na_omit$gam_oak_2018 = gam_oak_2018$fitted.values
    gpp_oak_2019_na_omit$gam_oak_2019 = gam_oak_2019$fitted.values
    
    
    p7 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2017_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2017_na_omit,aes(x=DY, y=gam_oak_2017),color="black",size = 2)+
      geom_line(data = gpp_oak_2017_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p8 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2018_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2018_na_omit,aes(x=DY, y=gam_oak_2018),color="black",size = 2)+
      geom_line(data = gpp_oak_2018_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p9 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2019_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2019_na_omit,aes(x=DY, y=gam_oak_2019),color="black",size = 2)+
      geom_line(data = gpp_oak_2019_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
  }
}

#growth rates
{
  #pine
  {
    #EWMZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_dcell_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)  
      
      p1 <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_dcell_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_dcell_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_dcell_mean,color="c"),size = 1)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-50), width=.2,color = "darkred")+
        xlim(100,320)+
        ylim(-200,400)+
        ylab("Ring length increment rate weekly/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_dcell_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)
      
      p2 <- ggplot() + 
        
        geom_ribbon(data = subset(gpp_oak_2019,!is.na(gam_dcell_mean)),aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = subset(gpp_oak_2018,!is.na(gam_dcell_mean)),aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = subset(gpp_oak_2017,!is.na(gam_dcell_mean)),aes(x=DY,ymin= gam_dcell_mean - gam_dcell_sd, 
                                                                           ymax= gam_dcell_mean + gam_dcell_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = subset(gpp_oak_2017,!is.na(gam_dcell_mean)),aes(x=DY, y=gam_dcell_mean,color="a"),size = 1)+
        geom_line(data = subset(gpp_oak_2018,!is.na(gam_dcell_mean)),aes(x=DY, y=gam_dcell_mean,color="b"),size = 1)+
        geom_line(data = subset(gpp_oak_2019,!is.na(gam_dcell_mean)),aes(x=DY, y=gam_dcell_mean,color="c"),size = 1)+
        #geom_line(data = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="d"),size = 1)+
        #geom_line(data = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="e"),size = 1)+
        #geom_line(data = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)),aes(x=DY, y=gpp_ww_acc,color="f"),size = 1)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = l_pheno_QURU_17,aes(x=bb.doy,y=-80),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-80), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_QURU_18,aes(x=bb.doy,y=-90),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_QURU_18,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-90), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_QURU_19,aes(x=bb.doy,y=-95),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_QURU_19,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-95), width=.2,color = "#F8766D")+
        geom_point(data = l_pheno_QURU_17,aes(x=lc.doy,y=-80),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-80), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_QURU_18,aes(x=lc.doy,y=-90),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_QURU_18,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-90), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_QURU_19,aes(x=lc.doy,y=-95),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_QURU_19,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-95), width=.2,color = "#F8766D")+
        xlim(100,320)+
        ylim(-95,400)+
        ylab("Cumulated zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p2
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EWMZ_3Yrs_renew_2021_1_25.pdf",width =6,height = 4)
      p3 <- ggplot() + 
        
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EWMZ_mean - gam_EWMZ_sd, 
                                                            ymax= gam_EWMZ_mean + gam_EWMZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EWMZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-300),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-300), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-400),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-400), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-500),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-500), width=.2,color = "darkred")+
        geom_point(data = l_pheno_ACRU_17,aes(x=bb.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_QURU_17,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_ACRU_18,aes(x=bb.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_ACRU_18,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_ACRU_19,aes(x=bb.doy,y=-999),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_ACRU_19,aes(xmin=bb.doy-sd.bb.doy, xmax=bb.doy+sd.bb.doy,y=-999), width=.2,color = "#F8766D")+
        geom_point(data = l_pheno_ACRU_17,aes(x=lc.doy,y=-800),color = "#00B9E3")+
        geom_errorbarh(data = l_pheno_ACRU_17,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-800), width=.2,color = "#00B9E3")+
        geom_point(data = l_pheno_ACRU_18,aes(x=lc.doy,y=-900),color = "#00BA38")+
        geom_errorbarh(data = l_pheno_ACRU_18,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-900), width=.2,color = "#00BA38")+
        geom_point(data = l_pheno_ACRU_19,aes(x=lc.doy,y=-999),color = "#F8766D")+
        geom_errorbarh(data = l_pheno_ACRU_19,aes(xmin=lc.doy-sd.lc.doy, xmax=lc.doy+sd.lc.doy,y=-999), width=.2,color = "#F8766D")+
        xlim(100,320)+
        ylim(-999,4000)+
        ylab("Cumulated zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.15,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      
      p3
      dev.off()
    }
    
    #CZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_CZ_3Yrs.pdf",width =6,height = 4)
      
      p1_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_pine_mean_sd_site,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                        ymax= CZ.x + CZ.y),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_mean_sd_site,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                        ymax= CZ.x + CZ.y),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_mean_sd_site,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                        ymax= CZ.x + CZ.y),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_mean_sd_site,aes(x=DY, y=CZ.x,color="a"),size = 1)+
        geom_line(data = HF2018_pine_mean_sd_site,aes(x=DY, y=CZ.x,color="b"),size = 1)+
        geom_line(data = HF2019_pine_mean_sd_site,aes(x=DY, y=CZ.x,color="c"),size = 1)+
        xlim(98,300)+
        ylim(-50,200)+
        ylab("Zone width/μm")+
        scale_y_continuous(breaks = pretty(c(0,200)),limits = range(pretty(c(0,200))))+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
      p1_CZ
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_CZ_3Yrs.pdf",width =6,height = 4)
      
      p2_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                           ymax= CZ.x + CZ.y),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                           ymax= CZ.x + CZ.y),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                           ymax= CZ.x + CZ.y),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_mean_sd_site_com,aes(x=DY, y=CZ.x,color="a"),size = 1)+
        geom_line(data = HF2018_oak_mean_sd_site_com,aes(x=DY, y=CZ.x,color="b"),size = 1)+
        geom_line(data = HF2019_oak_mean_sd_site_com,aes(x=DY, y=CZ.x,color="c"),size = 1)+
        xlim(98,300)+
        ylim(0,150)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_CZ
      dev.off()
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_CZ_3Yrs.pdf",width =6,height = 4)
      
      p3_CZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                             ymax= CZ.x + CZ.y),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                             ymax= CZ.x + CZ.y),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_mean_sd_site_com,aes(x=DY,ymin= CZ.x - CZ.y, 
                                                             ymax= CZ.x + CZ.y),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_mean_sd_site_com,aes(x=DY, y=CZ.x,color="a"),size = 1)+
        geom_line(data = HF2018_maple_mean_sd_site_com,aes(x=DY, y=CZ.x,color="b"),size = 1)+
        geom_line(data = HF2019_maple_mean_sd_site_com,aes(x=DY, y=CZ.x,color="c"),size = 1)+
        xlim(100,300)+
        ylim(-50,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.2,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))  
      
      p3_CZ
      dev.off()
    }
    
    #EZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_EZ_3Yrs.pdf",width =6,height = 4)
      
      p1_EZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                           ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
        xlim(100,300)+
        ylim(-50,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_EZ_3Yrs.pdf",width =6,height = 4)
      p2_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                          ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
        xlim(98,300)+
        ylim(-50,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_EZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_EZ_3Yrs.pdf",width =6,height = 4)
      p3_EZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_EZ_mean - gam_EZ_sd, 
                                                            ymax= gam_EZ_mean + gam_EZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_EZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bE_mean-bE_sd, xmax=bE_mean+bE_sd,y=-50), width=.2,color = "darkred")+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cE_mean,y=-30),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-30), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cE_mean,y=-40),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-40), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cE_mean,y=-50),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cE_mean-cE_sd, xmax=cE_mean+cE_sd,y=-50), width=.2,color = "darkred")+
        xlim(100,300)+
        ylim(-50,200)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))  
      p3_EZ
      dev.off()
    }
    
    #WZ
    {
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_WZ_3Yrs.pdf",width =6,height = 4)
      
      p1_WZ <- ggplot() + 
        
        geom_ribbon(data = HF2019_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_pine_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                           ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_pine_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_PINE_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        geom_point(data = Pheno_PINE_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_PINE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_PINE_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_PINE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_PINE_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_PINE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
        xlim(100,300)+
        ylim(-200,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p1_WZ
      dev.off()
      
      
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_WZ_3Yrs.pdf",width =6,height = 4)
      
      p2_WZ <- ggplot() + 
        geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                          ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_oak_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_OAK_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        geom_point(data = Pheno_OAK_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_OAK_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_OAK_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_OAK_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_OAK_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_OAK_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
        xlim(100,300)+
        ylim(-200,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      p2_WZ
      dev.off()
      
      pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_WZ_3Yrs.pdf",width =6,height = 4)    
      
      p3_WZ <- ggplot() + 
        geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
                                                            ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = HF2017_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="a"),size = 1)+
        geom_line(data = HF2018_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="b"),size = 1)+
        geom_line(data = HF2019_maple_gam_mean_sd_com,aes(x=DY, y=gam_WZ_mean,color="c"),size = 1)+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=bW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=bW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=bW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=bW_mean-bW_sd, xmax=bW_mean+bW_sd,y=-200), width=.2,color = "darkred")+
        geom_point(data = Pheno_MAPLE_2017_df,aes(x=cW_mean,y=-120),color = "blue")+
        geom_errorbarh(data = Pheno_MAPLE_2017_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-120), width=.2,color = "blue")+
        geom_point(data = Pheno_MAPLE_2018_df,aes(x=cW_mean,y=-160),color = "darkgreen")+
        geom_errorbarh(data = Pheno_MAPLE_2018_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-160), width=.2,color = "darkgreen")+
        geom_point(data = Pheno_MAPLE_2019_df,aes(x=cW_mean,y=-200),color = "darkred")+
        geom_errorbarh(data = Pheno_MAPLE_2019_df,aes(xmin=cW_mean-cW_sd, xmax=cW_mean+cW_sd,y=-200), width=.2,color = "darkred")+
        xlim(100,300)+
        ylim(-200,800)+
        ylab("Zone width/μm")+
        scale_color_manual(name = '', 
                           values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = c(0.8,0.8),legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
      p3_WZ
      dev.off()
    } 
  }
  
  #maple
  {
    gpp_maple_2017_na_omit = na.omit(gpp_maple_2017)
    gpp_maple_2018_na_omit = na.omit(gpp_maple_2018)
    gpp_maple_2019_na_omit = na.omit(gpp_maple_2019)
    
    #gam
    gam_maple_2017 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)
    gam_maple_2019 <- gam(EWMZ_mean ~ s(DY), data=gpp_maple_2019_na_omit)
    gam_maple_2017_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_EZ <- gam(EZ_mean ~ s(DY), data=gpp_maple_2019_na_omit)  
    
    gam_maple_2017_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_WZ <- gam(WZ_mean ~ s(DY), data=gpp_maple_2019_na_omit) 
    
    gam_maple_2017_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2017_na_omit)
    gam_maple_2018_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2018_na_omit)  
    gam_maple_2019_MZ <- gam(MZ_mean ~ s(DY), data=gpp_maple_2019_na_omit) 
    
    gpp_maple_2017_na_omit$gam_maple_2017 = gam_maple_2017$fitted.values
    gpp_maple_2018_na_omit$gam_maple_2018 = gam_maple_2018$fitted.values
    gpp_maple_2019_na_omit$gam_maple_2019 = gam_maple_2019$fitted.values
    
    
    p4 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2017_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2017_na_omit,aes(x=DY, y=gam_maple_2017),color="black",size = 2)+
      geom_line(data = gpp_maple_2017_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p5 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2018_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2018_na_omit,aes(x=DY, y=gam_maple_2018),color="black",size = 2)+
      geom_line(data = gpp_maple_2018_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p6 <- ggplot() + 
      geom_ribbon(data = gpp_maple_2019_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                    ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_maple_2019_na_omit,aes(x=DY, y=gam_maple_2019),color="black",size = 2)+
      geom_line(data = gpp_maple_2019_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
  }
  
  #oak
  {
    gpp_oak_2017_na_omit = na.omit(gpp_oak_2017)
    gpp_oak_2018_na_omit = na.omit(gpp_oak_2018)
    gpp_oak_2019_na_omit = na.omit(gpp_oak_2019)
    
    #gam
    gam_oak_2017 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)
    gam_oak_2019 <- gam(EWMZ_mean ~ s(DY), data=gpp_oak_2019_na_omit)
    
    gam_oak_2017_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_EZ <- gam(EZ_mean ~ s(DY), data=gpp_oak_2019_na_omit)  
    
    gam_oak_2017_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_WZ <- gam(WZ_mean ~ s(DY), data=gpp_oak_2019_na_omit) 
    
    gam_oak_2017_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2017_na_omit)
    gam_oak_2018_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2018_na_omit)  
    gam_oak_2019_MZ <- gam(MZ_mean ~ s(DY), data=gpp_oak_2019_na_omit) 
    
    gpp_oak_2017_na_omit$gam_oak_2017 = gam_oak_2017$fitted.values
    gpp_oak_2018_na_omit$gam_oak_2018 = gam_oak_2018$fitted.values
    gpp_oak_2019_na_omit$gam_oak_2019 = gam_oak_2019$fitted.values
    
    
    p7 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2017_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2017_na_omit,aes(x=DY, y=gam_oak_2017),color="black",size = 2)+
      geom_line(data = gpp_oak_2017_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p8 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2018_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2018_na_omit,aes(x=DY, y=gam_oak_2018),color="black",size = 2)+
      geom_line(data = gpp_oak_2018_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
    
    p9 <- ggplot() + 
      geom_ribbon(data = gpp_oak_2019_na_omit,aes(x=DY,ymin= EWMZ_mean - EWMZ_sd, 
                                                  ymax= EWMZ_mean + EWMZ_sd),fill = "lightblue",alpha = 0.6)+
      geom_line(data = gpp_oak_2019_na_omit,aes(x=DY, y=gam_oak_2019),color="black",size = 2)+
      geom_line(data = gpp_oak_2019_na_omit,aes(x=DY, y=EWMZ_mean),color="blue",size = 1)+
      xlim(100,300)+
      ylim(-50,4000)+
      ylab("Zone width/μm")
  }
}


#phenology mixed effect model creation
{
  library("lme4")
  
  Pheno_wood_df$dE
  #phenology terms 
  #wood phenology
  mix_wp_bE_None = lmerTest::lmer(data = Pheno_wood_df,formula = bE ~ (1|Tree))
  mix_wp_bE_Year = lmerTest::lmer(data = Pheno_wood_df,formula = bE ~ Year + (1|Tree))
  mix_wp_bE_Species = lmerTest::lmer(data = Pheno_wood_df,formula = bE ~ Species + (1|Tree))
  mix_wp_bE_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = bE ~ Year+Species + (1|Tree))
  mix_wp_bE_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = bE ~ Year*Species + (1|Tree))
  anova(mix_wp_bE_None,mix_wp_bE_Year,mix_wp_bE_Species,mix_wp_bE_Year_Species_add,mix_wp_bE_Year_Species_mult)
  
  
  mix_wp_bW_Year = lmerTest::lmer(data = Pheno_wood_df,formula = bW ~ Year + (1|Tree))
  mix_wp_bW_Species = lmerTest::lmer(data = Pheno_wood_df,formula = bW ~ Species + (1|Tree))
  mix_wp_bW_Year_Species = lmerTest::lmer(data = Pheno_wood_df,formula = bW ~ Year+Species + (1|Tree))
  AIC(mix_wp_bW_Year,mix_wp_bW_Species,mix_wp_bW_Year_Species)
  
  mix_wp_bM_Year = lmerTest::lmer(data = Pheno_wood_df,formula = bM ~ Year + (1|Tree))
  mix_wp_bM_Species = lmerTest::lmer(data = Pheno_wood_df,formula = bM ~ Species + (1|Tree))
  mix_wp_bM_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = bM ~ Year+Species + (1|Tree))
  mix_wp_bM_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = bM ~ Year*Species + (1|Tree))
  anova(mix_wp_bM_Year,mix_wp_bM_Species,mix_wp_bM_Year_Species_add,mix_wp_bM_Year_Species_mult) 
  
  mix_wp_cE_None = lmerTest::lmer(data = Pheno_wood_df,formula = cE ~ (1|Tree))
  mix_wp_cE_Year = lmerTest::lmer(data = Pheno_wood_df,formula = cE ~ Year + (1|Tree))
  mix_wp_cE_Species = lmerTest::lmer(data = Pheno_wood_df,formula = cE ~ Species + (1|Tree))
  mix_wp_cE_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = cE ~ Year+Species + (1|Tree))
  mix_wp_cE_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = cE ~ Year*Species + (1|Tree))
  anova(mix_wp_cE_None,mix_wp_cE_Year,mix_wp_cE_Species,mix_wp_cE_Year_Species_add,mix_wp_cE_Year_Species_mult) 
  
  mix_wp_cW_None = lmerTest::lmer(data = Pheno_wood_df,formula = cW ~ (1|Tree))
  mix_wp_cW_Year = lmerTest::lmer(data = Pheno_wood_df,formula = cW ~ Year + (1|Tree))
  mix_wp_cW_Species = lmerTest::lmer(data = Pheno_wood_df,formula = cW ~ Species + (1|Tree))
  mix_wp_cW_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = cW ~ Year+Species + (1|Tree))
  mix_wp_cW_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = cW ~ Year*Species + (1|Tree))
  anova(mix_wp_cW_None,mix_wp_cW_Year,mix_wp_cW_Species,mix_wp_cW_Year_Species_add,mix_wp_cW_Year_Species_mult) 
  
  mix_wp_dE_None = lmerTest::lmer(data = Pheno_wood_df,formula = dE ~ (1|Tree))
  mix_wp_dE_Year = lmerTest::lmer(data = Pheno_wood_df,formula = dE ~ Year + (1|Tree))
  mix_wp_dE_Species = lmerTest::lmer(data = Pheno_wood_df,formula = dE ~ Species + (1|Tree))
  mix_wp_dE_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = dE ~ Year+Species + (1|Tree))
  mix_wp_dE_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = dE ~ Year*Species + (1|Tree))
  anova(mix_wp_dE_None,mix_wp_dE_Year,mix_wp_dE_Species,mix_wp_dE_Year_Species_add,mix_wp_dE_Year_Species_mult) 
  
  mix_wp_dX_None = lmerTest::lmer(data = Pheno_wood_df,formula = dX ~ (1|Tree))
  mix_wp_dX_Year = lmerTest::lmer(data = Pheno_wood_df,formula = dX ~ Year + (1|Tree))
  mix_wp_dX_Species = lmerTest::lmer(data = Pheno_wood_df,formula = dX ~ Species + (1|Tree))
  mix_wp_dX_Year_Species_add = lmerTest::lmer(data = Pheno_wood_df,formula = dX ~ Year+Species + (1|Tree))
  mix_wp_dX_Year_Species_mult = lmerTest::lmer(data = Pheno_wood_df,formula = dX ~ Year*Species + (1|Tree))
  anova(mix_wp_dX_None,mix_wp_dX_Year,mix_wp_dX_Species,mix_wp_dX_Year_Species_add,mix_wp_dX_Year_Species_mult) 
  
  mix_wp_OAK_dE_None = lmerTest::lmer(data = Pheno_OAK_df,formula = dE ~ (1|Tree))
  mix_wp_OAK_dE_Year = lmerTest::lmer(data = Pheno_OAK_df,formula = dE ~ Year + (1|Tree))
  anova(mix_wp_OAK_dE_None,mix_wp_OAK_dE_Year) 
  
  
  
  #for each species
  {
    mix_wp_dX_None_OAK = lmerTest::lmer(data = Pheno_OAK_df,formula = dX ~ (1|Tree),REML = F)
    mix_wp_dX_Year_OAK = lmerTest::lmer(data = Pheno_OAK_df,formula = dX ~ Year + (1|Tree),REML = F)
    anova(mix_wp_dX_None_OAK,mix_wp_dX_Year_OAK) 
  }
  
  
  
  l_pheno_spring_ind = read.csv("hf003-05-spring-mean-ind.csv")
  l_pheno_fall_ind = read.csv("hf003-07-fall-mean-ind.csv")
  l_pheno_spring_ind_sub_17_19 = subset(l_pheno_spring_ind,year >= 2017 & year <= 2019 & species == "ACRU" | species == "QURU")
  l_pheno_fall_ind_sub_17_19 = subset(l_pheno_fall_ind,year >= 2017 & year <= 2019 & species == "ACRU" | species == "QURU")  
  
  #code for okeefe data
  {
    mix_lp_bb_17_19_Year = lmer(data = l_pheno_spring_ind_sub_17_19,formula = bb.doy ~ year + (1|tree.id))
    mix_lp_bb_17_19_Species = lmer(data = l_pheno_spring_ind_sub_17_19,formula = bb.doy ~ species + (1|tree.id))
    mix_lp_bb_17_19_Year_Species_add = lmer(data = l_pheno_spring_ind_sub_17_19,formula = bb.doy ~ year + species + (1|tree.id))
    mix_lp_bb_17_19_Year_Species_mult = lmer(data = l_pheno_spring_ind_sub_17_19,formula = bb.doy ~ year * species + (1|tree.id))
    anova(mix_lp_bb_17_19_Year,mix_lp_bb_17_19_Species,mix_lp_bb_17_19_Year_Species_add,mix_lp_bb_17_19_Year_Species_mult)
  }
  #code for Tim data
  {
    mix_lp_bb_17_19_None_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = bb.doy ~ (1|tree.id),REML=FALSE)
    mix_lp_bb_17_19_Year_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = bb.doy ~ year + (1|tree.id),REML=FALSE)
    #lm_lp_bb_17_19_Year_Tim = lm(data = l_pheno_Tim,formula = bb.doy ~ year)
    mix_lp_bb_17_19_Species_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = bb.doy ~ species + (1|tree.id),REML=FALSE)
    #lm_lp_bb_17_19_Species_Tim = lm(data = l_pheno_Tim,formula = bb.doy ~ species)
    mix_lp_bb_17_19_Year_Species_add_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = bb.doy ~ year + species + (1|tree.id),REML=FALSE)
    lm_lp_bb_17_19_Year_Species_add_Tim = lm(data = l_pheno_Tim,formula = bb.doy ~ year + species)
    mix_lp_bb_17_19_Year_Species_mult_Tim = lmer(data = l_pheno_Tim,formula = bb.doy ~ year * species + (1|tree.id),REML=FALSE)
    anova(mix_lp_bb_17_19_None_Tim,mix_lp_bb_17_19_Year_Tim,mix_lp_bb_17_19_Species_Tim,mix_lp_bb_17_19_Year_Species_add_Tim,mix_lp_bb_17_19_Year_Species_mult_Tim)
    
    mix_lp_lf_17_19_None_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = lf.doy ~(1|tree.id),REML=FALSE)
    mix_lp_lf_17_19_Year_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = lf.doy ~ year + (1|tree.id),REML=FALSE)
    mix_lp_lf_17_19_Species_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = lf.doy ~ species + (1|tree.id),REML=FALSE)
    mix_lp_lf_17_19_Year_Species_add_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = lf.doy ~ year + species + (1|tree.id),REML=FALSE)
    mix_lp_lf_17_19_Year_Species_mult_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = lf.doy ~ year * species + (1|tree.id),REML=FALSE)
    anova(mix_lp_lf_17_19_None_Tim,mix_lp_lf_17_19_Year_Tim,mix_lp_lf_17_19_Species_Tim,mix_lp_lf_17_19_Year_Species_add_Tim,mix_lp_lf_17_19_Year_Species_mult_Tim) 
    
    mix_lp_dl_17_19_None_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = dl ~(1|tree.id),REML=FALSE)
    mix_lp_dl_17_19_Year_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = dl ~ year + (1|tree.id),REML=FALSE)
    mix_lp_dl_17_19_Species_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = dl ~ species + (1|tree.id),REML=FALSE)
    mix_lp_dl_17_19_Year_Species_add_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = dl ~ year + species + (1|tree.id),REML=FALSE)
    mix_lp_dl_17_19_Year_Species_mult_Tim = lmerTest::lmer(data = l_pheno_Tim,formula = dl ~ year * species + (1|tree.id),REML=FALSE)
    anova(mix_lp_dl_17_19_None_Tim,mix_lp_dl_17_19_Year_Tim,mix_lp_dl_17_19_Species_Tim,mix_lp_dl_17_19_Year_Species_add_Tim,mix_lp_dl_17_19_Year_Species_mult_Tim) 
    
  }
  
  #code for wood & leaf phenology
  Pheno_wood_leaf_df = read.csv("Pheno_wood_leaf_df.csv")
  #bb vs bE
  {
    
    
    Pheno_base_df = Pheno_wood_leaf_df[2:4]
    
    Pheno_bE_bb_df = Pheno_wood_leaf_df[c(2:5,21)]
    
    
    Pheno_bE_bb_df_long = reshape(Pheno_bE_bb_df,
                                  direction = "long",
                                  varying = list(names(Pheno_bE_bb_df)[4:5]),
                                  v.names = "doy",
                                  idvar = c("Year","Species","Tree"),
                                  timevar = "Pheno",
                                  times = c("bE","bb.doy")
    )
    
    mix_bE_bb_Year = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Year + (1|Tree))
    mix_bE_bb_Species = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Species + (1|Tree))
    mix_bE_bb_Pheno = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Pheno + (1|Tree))
    mix_bE_bb_Year_Pheno = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Year + Pheno + (1|Tree))
    mix_bE_bb_Year_Species = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Species + Year + (1|Tree))
    mix_bE_bb_Pheno_Species = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Pheno + Species + (1|Tree))
    mix_bE_bb_Pheno_Species_Year = lmerTest::lmer(data = Pheno_bE_bb_df_long,formula = doy ~ Pheno + Species + Year + (1|Tree))    
    
    anova(mix_bE_bb_Year,mix_bE_bb_Species,mix_bE_bb_Pheno,mix_bE_bb_Year_Pheno,mix_bE_bb_Year_Species,mix_bE_bb_Pheno_Species,mix_bE_bb_Pheno_Species_Year)
    
    
    
    
  }
  #lf vs cW
  {
    
    Pheno_cW_lf_df = Pheno_wood_leaf_df[c(2:4,13,26)]
    
    
    Pheno_cW_lf_df_long = reshape(Pheno_cW_lf_df,
                                  direction = "long",
                                  varying = list(names(Pheno_cW_lf_df)[4:5]),
                                  v.names = "doy",
                                  idvar = c("Year","Species","Tree"),
                                  timevar = "Pheno",
                                  times = c("cW","lf.doy")
    )
    
    mix_cW_lf_Year = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Year + (1|Tree))
    mix_cW_lf_Species = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Species + (1|Tree))
    mix_cW_lf_Pheno = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Pheno + (1|Tree))
    mix_cW_lf_Year_Pheno = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Year + Pheno + (1|Tree))
    mix_cW_lf_Year_Species = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Species + Year + (1|Tree))
    mix_cW_lf_Pheno_Species = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Pheno + Species + (1|Tree))
    mix_cW_lf_Pheno_Species_Year = lmerTest::lmer(data = Pheno_cW_lf_df_long,formula = doy ~ Pheno + Species + Year + (1|Tree))    
    
    anova(mix_cW_lf_Year,mix_cW_lf_Species,mix_cW_lf_Pheno,mix_cW_lf_Year_Pheno,mix_cW_lf_Year_Species,mix_cW_lf_Pheno_Species,mix_cW_lf_Pheno_Species_Year)
  }  
  
  leafDataObs_df$Year = c(rep(2017,50),rep(2018,55),rep(2019,56))
  stemDataObs_df$Year = c(rep(2017,60),rep(2018,60),rep(2019,60))
  
  mix_leaf_NSC_date = lmer(data = leafDataObs_df,formula = NSC ~ stage + (1|treeID))  
  mix_leaf_NSC_Year = lmer(data = leafDataObs_df,formula = NSC ~ Year + (1|treeID))
  mix_leaf_NSC_Species = lmer(data = leafDataObs_df,formula = NSC ~ species + (1|treeID))  
  mix_leaf_NSC_Year_Species_add = lmer(data = leafDataObs_df,formula = NSC ~ species + Year + (1|treeID))   
  mix_leaf_NSC_Year_Species_mult = lmer(data = leafDataObs_df,formula = NSC ~ species * Year + (1|treeID)) 
  mix_leaf_NSC_Year_Species_date_add = lmer(data = leafDataObs_df,formula = NSC ~ species + Year + stage + (1|treeID)) 
  anova(mix_leaf_NSC_date,mix_leaf_NSC_Year,mix_leaf_NSC_Year,mix_leaf_NSC_Year_Species_add,mix_leaf_NSC_Year_Species_mult,mix_leaf_NSC_Year_Species_date_add) 
  
  
  
  #mix effect model stem starch
  {
    mix_stem_starch_none = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ (1|treeID))  
    mix_stem_starch_date = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ stage + (1|treeID))  
    mix_stem_starch_Year = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ Year + (1|treeID))
    mix_stem_starch_Species = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ species + (1|treeID)) 
    mix_stem_starch_date_Year_add = lmerTest::lmer(data = stemDataObs_df,formula = starch~ stage + Year + (1|treeID))
    mix_stem_starch_Year_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = starch~ species + Year + (1|treeID))   
    mix_stem_starch_date_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ stage + species + (1|treeID)) 
    mix_stem_starch_Year_Species_date_add = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ species + Year + stage + (1|treeID)) 
    mix_stem_starch_Year_Species_date_multi1 = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ species * Year + stage + (1|treeID))
    mix_stem_starch_Year_Species_date_multi2 = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ species + Year * stage + (1|treeID))
    mix_stem_starch_Year_Species_date_multi3 = lmerTest::lmer(data = stemDataObs_df,formula = starch ~ species * Year * stage + (1|treeID))
    anova(mix_stem_starch_none,mix_stem_starch_date,mix_stem_starch_Year,mix_stem_starch_Species,mix_stem_starch_date_Year_add,mix_stem_starch_date_Species_add,mix_stem_starch_Year_Species_add,mix_stem_starch_Year_Species_date_add,mix_stem_starch_Year_Species_date_multi1,mix_stem_starch_Year_Species_date_multi2,mix_stem_starch_Year_Species_date_multi3)   
  }
  #mix effect model stem ss
  {
    mix_stem_sugar_none = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ (1|treeID))
    mix_stem_sugar_date = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ stage + (1|treeID))  
    mix_stem_sugar_Year = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ Year + (1|treeID))
    mix_stem_sugar_Species = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ species + (1|treeID)) 
    mix_stem_sugar_date_Year_add = lmerTest::lmer(data = stemDataObs_df,formula = sugar~ stage + Year + (1|treeID))
    mix_stem_sugar_Year_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = sugar~ species + Year + (1|treeID))   
    mix_stem_sugar_date_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ stage + species + (1|treeID)) 
    mix_stem_sugar_Year_Species_date_add = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ species + Year + stage + (1|treeID)) 
    mix_stem_sugar_Year_Species_date_multi1 = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ species * Year + stage + (1|treeID))
    mix_stem_sugar_Year_Species_date_multi2 = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ species + Year * stage + (1|treeID))
    mix_stem_sugar_Year_Species_date_multi3 = lmerTest::lmer(data = stemDataObs_df,formula = sugar ~ species * Year * stage + (1|treeID))
    anova(mix_stem_sugar_none,mix_stem_sugar_date,mix_stem_sugar_Year,mix_stem_sugar_Species,mix_stem_sugar_date_Year_add,mix_stem_sugar_date_Species_add,mix_stem_sugar_Year_Species_add,mix_stem_sugar_Year_Species_date_add,mix_stem_sugar_Year_Species_date_multi1,mix_stem_sugar_Year_Species_date_multi2,mix_stem_sugar_Year_Species_date_multi3)   
  }  
  #mix effect model stem NSC
  {
    mix_stem_NSC_none = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ (1|treeID))
    mix_stem_NSC_date = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ stage + (1|treeID),REML = FALSE)  
    mix_stem_NSC_Year = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ Year + (1|treeID))
    mix_stem_NSC_Species = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ species + (1|treeID)) 
    mix_stem_NSC_date_Year_add = lmerTest::lmer(data = stemDataObs_df,formula = NSC~ stage + Year + (1|treeID))
    mix_stem_NSC_Year_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = NSC~ species + Year + (1|treeID))   
    mix_stem_NSC_date_Species_add = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ stage + species + (1|treeID)) 
    mix_stem_NSC_Year_Species_date_add = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ species + Year + stage + (1|treeID)) 
    mix_stem_NSC_Year_Species_date_multi1 = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ species * Year + stage + (1|treeID))
    mix_stem_NSC_Year_Species_date_multi2 = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ species + Year * stage + (1|treeID))
    mix_stem_NSC_Year_Species_date_multi3 = lmerTest::lmer(data = stemDataObs_df,formula = NSC ~ species * Year * stage + (1|treeID))
    anova(mix_stem_NSC_none,mix_stem_NSC_date,mix_stem_NSC_Year,mix_stem_NSC_Species,mix_stem_NSC_date_Year_add,mix_stem_NSC_date_Species_add,mix_stem_NSC_Year_Species_add,mix_stem_NSC_Year_Species_date_add,mix_stem_NSC_Year_Species_date_multi1,mix_stem_NSC_Year_Species_date_multi2,mix_stem_NSC_Year_Species_date_multi3)   
  }  
  
  
  #single species
  #QURU
  #mix effect model stem starch
  {
    mix_stem_starch_none_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = starch ~ (1|treeID))  
    mix_stem_starch_date_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = starch ~ stage + (1|treeID))  
    mix_stem_starch_Year_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = starch ~ Group + (1|treeID))
    mix_stem_starch_date_Year_add_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = starch~ stage + Group + (1|treeID))
    mix_stem_starch_Year_date_multi_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = starch ~ Group * stage + (1|treeID))
    anova(mix_stem_starch_none_QURU,mix_stem_starch_date_QURU,mix_stem_starch_Year_QURU,mix_stem_starch_date_Year_add_QURU,mix_stem_starch_Year_date_multi_QURU)   
  }
  #mix effect model stem ss
  {
    mix_stem_sugar_none_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = sugar ~ (1|treeID))  
    mix_stem_sugar_date_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = sugar ~ stage + (1|treeID))  
    mix_stem_sugar_Year_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = sugar ~ Group + (1|treeID))
    mix_stem_sugar_date_Year_add_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = sugar~ stage + Group + (1|treeID))
    mix_stem_sugar_Year_date_multi_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = sugar ~ Group * stage + (1|treeID))
    anova(mix_stem_sugar_none_QURU,mix_stem_sugar_date_QURU,mix_stem_sugar_Year_QURU,mix_stem_sugar_date_Year_add_QURU,mix_stem_sugar_Year_date_multi_QURU)   
  } 
  #mix effect model stem NSC
  {
    mix_stem_nsc_none_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = nsc ~ (1|treeID))  
    mix_stem_nsc_date_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = nsc ~ stage + (1|treeID))  
    mix_stem_nsc_Year_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = nsc ~ Group + (1|treeID))
    mix_stem_nsc_date_Year_add_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = nsc~ stage + Group + (1|treeID))
    mix_stem_nsc_Year_date_multi_QURU = lmerTest::lmer(data = stemDataObs_QURU,formula = nsc ~ Group * stage + (1|treeID))
    anova(mix_stem_nsc_none_QURU,mix_stem_nsc_date_QURU,mix_stem_nsc_Year_QURU,mix_stem_nsc_date_Year_add_QURU,mix_stem_nsc_Year_date_multi_QURU)   
  } 
  
  #ACRU
  #mix effect model stem starch
  {
    mix_stem_starch_none_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = starch ~ (1|treeID))  
    mix_stem_starch_date_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = starch ~ stage + (1|treeID))  
    mix_stem_starch_Year_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = starch ~ Group + (1|treeID))
    mix_stem_starch_date_Year_add_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = starch~ stage + Group + (1|treeID))
    mix_stem_starch_Year_date_multi_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = starch ~ Group * stage + (1|treeID))
    anova(mix_stem_starch_none_ACRU,mix_stem_starch_date_ACRU,mix_stem_starch_Year_ACRU,mix_stem_starch_date_Year_add_ACRU,mix_stem_starch_Year_date_multi_ACRU)   
  }
  #mix effect model stem ss
  {
    mix_stem_sugar_none_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = sugar ~ (1|treeID))  
    mix_stem_sugar_date_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = sugar ~ stage + (1|treeID))  
    mix_stem_sugar_Year_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = sugar ~ Group + (1|treeID))
    mix_stem_sugar_date_Year_add_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = sugar~ stage + Group + (1|treeID))
    mix_stem_sugar_Year_date_multi_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = sugar ~ Group * stage + (1|treeID))
    anova(mix_stem_sugar_none_ACRU,mix_stem_sugar_date_ACRU,mix_stem_sugar_Year_ACRU,mix_stem_sugar_date_Year_add_ACRU,mix_stem_sugar_Year_date_multi_ACRU)   
  } 
  #mix effect model stem NSC
  {
    mix_stem_nsc_none_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = nsc ~ (1|treeID))  
    mix_stem_nsc_date_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = nsc ~ stage + (1|treeID))  
    mix_stem_nsc_Year_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = nsc ~ Group + (1|treeID))
    mix_stem_nsc_date_Year_add_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = nsc~ stage + Group + (1|treeID))
    mix_stem_nsc_Year_date_multi_ACRU = lmerTest::lmer(data = stemDataObs_ACRU,formula = nsc ~ Group * stage + (1|treeID))
    anova(mix_stem_nsc_none_ACRU,mix_stem_nsc_date_ACRU,mix_stem_nsc_Year_ACRU,mix_stem_nsc_date_Year_add_ACRU,mix_stem_nsc_Year_date_multi_ACRU)   
  } 
  
  #PIST
  #mix effect model stem starch
  {
    mix_stem_starch_none_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = starch ~ (1|treeID))  
    mix_stem_starch_date_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = starch ~ stage + (1|treeID))  
    mix_stem_starch_Year_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = starch ~ Group + (1|treeID))
    mix_stem_starch_date_Year_add_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = starch~ stage + Group + (1|treeID))
    mix_stem_starch_Year_date_multi_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = starch ~ Group * stage + (1|treeID))
    anova(mix_stem_starch_none_PIST,mix_stem_starch_date_PIST,mix_stem_starch_Year_PIST,mix_stem_starch_date_Year_add_PIST,mix_stem_starch_Year_date_multi_PIST)   
  }
  #mix effect model stem ss
  {
    mix_stem_sugar_none_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = sugar ~ (1|treeID))  
    mix_stem_sugar_date_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = sugar ~ stage + (1|treeID))  
    mix_stem_sugar_Year_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = sugar ~ Group + (1|treeID))
    mix_stem_sugar_date_Year_add_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = sugar~ stage + Group + (1|treeID))
    mix_stem_sugar_Year_date_multi_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = sugar ~ Group * stage + (1|treeID))
    anova(mix_stem_sugar_none_PIST,mix_stem_sugar_date_PIST,mix_stem_sugar_Year_PIST,mix_stem_sugar_date_Year_add_PIST,mix_stem_sugar_Year_date_multi_PIST)   
  } 
  #mix effect model stem NSC
  {
    mix_stem_nsc_none_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = nsc ~ (1|treeID))  
    mix_stem_nsc_date_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = nsc ~ stage + (1|treeID))  
    mix_stem_nsc_Year_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = nsc ~ Group + (1|treeID))
    mix_stem_nsc_date_Year_add_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = nsc~ stage + Group + (1|treeID))
    mix_stem_nsc_Year_date_multi_PIST = lmerTest::lmer(data = stemDataObs_PIST,formula = nsc ~ Group * stage + (1|treeID))
    anova(mix_stem_nsc_none_PIST,mix_stem_nsc_date_PIST,mix_stem_nsc_Year_PIST,mix_stem_nsc_date_Year_add_PIST,mix_stem_nsc_Year_date_multi_PIST)   
  }     
  
  
}

#use gee_species_year.csv to perform cross correlation and 
#daylength correlation
#dcell against daylength
{
  #pine
  pine_2017_subset = subset(gpp_pine_2017,!is.na(gam_EWMZ_mean)&DY<Pheno_PINE_2017_df$cW_mean&DY>Pheno_PINE_2017_df$bE_mean)
  pine_2018_subset = subset(gpp_pine_2018,!is.na(gam_EWMZ_mean)&DY<Pheno_PINE_2018_df$cW_mean&DY>Pheno_PINE_2018_df$bE_mean)
  pine_2019_subset = subset(gpp_pine_2019,!is.na(gam_EWMZ_mean)&DY<Pheno_PINE_2019_df$cW_mean&DY>Pheno_PINE_2019_df$bE_mean)
  #maple
  maple_2017_subset = subset(gpp_maple_2017,!is.na(gam_EWMZ_mean)&DY<Pheno_MAPLE_2017_df$cW_mean&DY>Pheno_MAPLE_2017_df$bE_mean)
  maple_2018_subset = subset(gpp_maple_2018,!is.na(gam_EWMZ_mean)&DY<Pheno_MAPLE_2018_df$cW_mean&DY>Pheno_MAPLE_2018_df$bE_mean)
  maple_2019_subset = subset(gpp_maple_2019,!is.na(gam_EWMZ_mean)&DY<Pheno_MAPLE_2019_df$cW_mean&DY>Pheno_MAPLE_2019_df$bE_mean)
  #oak  
  oak_2017_subset = subset(gpp_oak_2017,!is.na(gam_EWMZ_mean)&DY<Pheno_OAK_2017_df$cW_mean&DY>Pheno_OAK_2017_df$bE_mean)
  oak_2018_subset = subset(gpp_oak_2018,!is.na(gam_EWMZ_mean)&DY<Pheno_OAK_2018_df$cW_mean&DY>Pheno_OAK_2018_df$bE_mean)
  oak_2019_subset = subset(gpp_oak_2019,!is.na(gam_EWMZ_mean)&DY<Pheno_OAK_2019_df$cW_mean&DY>Pheno_OAK_2019_df$bE_mean)
  
  #subsets for single trees
  {
    gpp_growth_pine_tree_P16_2017_subset=subset(gpp_growth_pine_tree_P16_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P16")$cW &DY>subset(Pheno_PINE_2017,Tree == "P16")$bE)
    gpp_growth_pine_tree_P20_2017_subset=subset(gpp_growth_pine_tree_P20_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P20")$cW &DY>subset(Pheno_PINE_2017,Tree == "P20")$bE)
    gpp_growth_pine_tree_P21_2017_subset=subset(gpp_growth_pine_tree_P21_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P21")$cW &DY>subset(Pheno_PINE_2017,Tree == "P21")$bE)
    gpp_growth_pine_tree_P25_2017_subset=subset(gpp_growth_pine_tree_P25_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P25")$cW &DY>subset(Pheno_PINE_2017,Tree == "P25")$bE)
    gpp_growth_pine_tree_P26_2017_subset=subset(gpp_growth_pine_tree_P26_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P26")$cW &DY>subset(Pheno_PINE_2017,Tree == "P26")$bE)
    gpp_growth_pine_tree_P7_2017_subset=subset(gpp_growth_pine_tree_P7_2017,!is.na(dcell)&DY<subset(Pheno_PINE_2017,Tree == "P7")$cW &DY>subset(Pheno_PINE_2017,Tree == "P7")$bE)
    gpp_growth_pine_tree_P16_2018_subset=subset(gpp_growth_pine_tree_P16_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P16")$cW &DY>subset(Pheno_PINE_2018,Tree == "P16")$bE)
    gpp_growth_pine_tree_P20_2018_subset=subset(gpp_growth_pine_tree_P20_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P20")$cW &DY>subset(Pheno_PINE_2018,Tree == "P20")$bE)
    gpp_growth_pine_tree_P21_2018_subset=subset(gpp_growth_pine_tree_P21_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P21")$cW &DY>subset(Pheno_PINE_2018,Tree == "P21")$bE)
    gpp_growth_pine_tree_P25_2018_subset=subset(gpp_growth_pine_tree_P25_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P25")$cW &DY>subset(Pheno_PINE_2018,Tree == "P25")$bE)
    gpp_growth_pine_tree_P26_2018_subset=subset(gpp_growth_pine_tree_P26_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P26")$cW &DY>subset(Pheno_PINE_2018,Tree == "P26")$bE)
    gpp_growth_pine_tree_P7_2018_subset=subset(gpp_growth_pine_tree_P7_2018,!is.na(dcell)&DY<subset(Pheno_PINE_2018,Tree == "P7")$cW &DY>subset(Pheno_PINE_2018,Tree == "P7")$bE)
    gpp_growth_pine_tree_P16_2019_subset=subset(gpp_growth_pine_tree_P16_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P16")$cW &DY>subset(Pheno_PINE_2019,Tree == "P16")$bE)
    gpp_growth_pine_tree_P20_2019_subset=subset(gpp_growth_pine_tree_P20_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P20")$cW &DY>subset(Pheno_PINE_2019,Tree == "P20")$bE)
    gpp_growth_pine_tree_P21_2019_subset=subset(gpp_growth_pine_tree_P21_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P21")$cW &DY>subset(Pheno_PINE_2019,Tree == "P21")$bE)
    gpp_growth_pine_tree_P25_2019_subset=subset(gpp_growth_pine_tree_P25_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P25")$cW &DY>subset(Pheno_PINE_2019,Tree == "P25")$bE)
    gpp_growth_pine_tree_P26_2019_subset=subset(gpp_growth_pine_tree_P26_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P26")$cW &DY>subset(Pheno_PINE_2019,Tree == "P26")$bE)
    gpp_growth_pine_tree_P7_2019_subset=subset(gpp_growth_pine_tree_P7_2019,!is.na(dcell)&DY<subset(Pheno_PINE_2019,Tree == "P7")$cW &DY>subset(Pheno_PINE_2019,Tree == "P7")$bE)
    
    gpp_growth_maple_tree_A18_2017_subset=subset(gpp_growth_maple_tree_A18_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A18")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A18")$bE)
    gpp_growth_maple_tree_A19_2017_subset=subset(gpp_growth_maple_tree_A19_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A19")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A19")$bE)
    gpp_growth_maple_tree_A23_2017_subset=subset(gpp_growth_maple_tree_A23_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A23")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A23")$bE)
    gpp_growth_maple_tree_A28_2017_subset=subset(gpp_growth_maple_tree_A28_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A28")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A28")$bE)
    gpp_growth_maple_tree_A29_2017_subset=subset(gpp_growth_maple_tree_A29_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A29")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A29")$bE)
    gpp_growth_maple_tree_A8_2017_subset=subset(gpp_growth_maple_tree_A8_2017,!is.na(dcell)&DY<subset(Pheno_MAPLE_2017,Tree == "A8")$cW &DY>subset(Pheno_MAPLE_2017,Tree == "A8")$bE)
    gpp_growth_maple_tree_A18_2018_subset=subset(gpp_growth_maple_tree_A18_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A18")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A18")$bE)
    gpp_growth_maple_tree_A19_2018_subset=subset(gpp_growth_maple_tree_A19_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A19")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A19")$bE)
    gpp_growth_maple_tree_A23_2018_subset=subset(gpp_growth_maple_tree_A23_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A23")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A23")$bE)
    gpp_growth_maple_tree_A28_2018_subset=subset(gpp_growth_maple_tree_A28_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A28")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A28")$bE)
    gpp_growth_maple_tree_A29_2018_subset=subset(gpp_growth_maple_tree_A29_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A29")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A29")$bE)
    gpp_growth_maple_tree_A8_2018_subset=subset(gpp_growth_maple_tree_A8_2018,!is.na(dcell)&DY<subset(Pheno_MAPLE_2018,Tree == "A8")$cW &DY>subset(Pheno_MAPLE_2018,Tree == "A8")$bE)
    gpp_growth_maple_tree_A18_2019_subset=subset(gpp_growth_maple_tree_A18_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A18")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A18")$bE)
    gpp_growth_maple_tree_A19_2019_subset=subset(gpp_growth_maple_tree_A19_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A19")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A19")$bE)
    gpp_growth_maple_tree_A23_2019_subset=subset(gpp_growth_maple_tree_A23_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A23")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A23")$bE)
    gpp_growth_maple_tree_A28_2019_subset=subset(gpp_growth_maple_tree_A28_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A28")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A28")$bE)
    gpp_growth_maple_tree_A29_2019_subset=subset(gpp_growth_maple_tree_A29_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A29")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A29")$bE)
    gpp_growth_maple_tree_A8_2019_subset=subset(gpp_growth_maple_tree_A8_2019,!is.na(dcell)&DY<subset(Pheno_MAPLE_2019,Tree == "A8")$cW &DY>subset(Pheno_MAPLE_2019,Tree == "A8")$bE)
    
    gpp_growth_oak_tree_Q05_2017_subset=subset(gpp_growth_oak_tree_Q05_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q05")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q05")$bE)
    gpp_growth_oak_tree_Q06_2017_subset=subset(gpp_growth_oak_tree_Q06_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q06")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q06")$bE)
    gpp_growth_oak_tree_Q09_2017_subset=subset(gpp_growth_oak_tree_Q09_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q09")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q09")$bE)
    gpp_growth_oak_tree_Q10_2017_subset=subset(gpp_growth_oak_tree_Q10_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q10")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q10")$bE)
    gpp_growth_oak_tree_Q11_2017_subset=subset(gpp_growth_oak_tree_Q11_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q11")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q11")$bE)
    gpp_growth_oak_tree_Q12_2017_subset=subset(gpp_growth_oak_tree_Q12_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q12")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q12")$bE)
    gpp_growth_oak_tree_Q13_2017_subset=subset(gpp_growth_oak_tree_Q13_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q13")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q13")$bE)
    gpp_growth_oak_tree_Q15_2017_subset=subset(gpp_growth_oak_tree_Q15_2017,!is.na(dcell)&DY<subset(Pheno_OAK_2017,Tree == "Q15")$cW &DY>subset(Pheno_OAK_2017,Tree == "Q15")$bE)
    gpp_growth_oak_tree_Q05_2018_subset=subset(gpp_growth_oak_tree_Q05_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q05")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q05")$bE)
    gpp_growth_oak_tree_Q06_2018_subset=subset(gpp_growth_oak_tree_Q06_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q06")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q06")$bE)
    gpp_growth_oak_tree_Q09_2018_subset=subset(gpp_growth_oak_tree_Q09_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q09")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q09")$bE)
    gpp_growth_oak_tree_Q10_2018_subset=subset(gpp_growth_oak_tree_Q10_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q10")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q10")$bE)
    gpp_growth_oak_tree_Q11_2018_subset=subset(gpp_growth_oak_tree_Q11_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q11")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q11")$bE)
    gpp_growth_oak_tree_Q12_2018_subset=subset(gpp_growth_oak_tree_Q12_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q12")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q12")$bE)
    gpp_growth_oak_tree_Q13_2018_subset=subset(gpp_growth_oak_tree_Q13_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q13")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q13")$bE)
    gpp_growth_oak_tree_Q15_2018_subset=subset(gpp_growth_oak_tree_Q15_2018,!is.na(dcell)&DY<subset(Pheno_OAK_2018,Tree == "Q15")$cW &DY>subset(Pheno_OAK_2018,Tree == "Q15")$bE)
    gpp_growth_oak_tree_Q05_2019_subset=subset(gpp_growth_oak_tree_Q05_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q05")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q05")$bE)
    gpp_growth_oak_tree_Q06_2019_subset=subset(gpp_growth_oak_tree_Q06_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q06")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q06")$bE)
    gpp_growth_oak_tree_Q09_2019_subset=subset(gpp_growth_oak_tree_Q09_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q09")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q09")$bE)
    gpp_growth_oak_tree_Q10_2019_subset=subset(gpp_growth_oak_tree_Q10_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q10")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q10")$bE)
    gpp_growth_oak_tree_Q11_2019_subset=subset(gpp_growth_oak_tree_Q11_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q11")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q11")$bE)
    gpp_growth_oak_tree_Q12_2019_subset=subset(gpp_growth_oak_tree_Q12_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q12")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q12")$bE)
    gpp_growth_oak_tree_Q13_2019_subset=subset(gpp_growth_oak_tree_Q13_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q13")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q13")$bE)
    gpp_growth_oak_tree_Q15_2019_subset=subset(gpp_growth_oak_tree_Q15_2019,!is.na(dcell)&DY<subset(Pheno_OAK_2019,Tree == "Q15")$cW &DY>subset(Pheno_OAK_2019,Tree == "Q15")$bE)
    
  }
  
  #pine variables & ccf outputs
  {
    dl_pine_2017 = pine_2017_subset$DL
    ta_pine_2017 = pine_2017_subset$ta
    prep_pine_2017 = pine_2017_subset$prec
    part_pine_2017 = pine_2017_subset$part
    gpp1_pine_2017 = pine_2017_subset$gpp_ww
    dcell_pine_2017 = pine_2017_subset$gam_dcell_mean
    dgam_ez_pine_2017 = pine_2017_subset$dgam_EZ_mean
    dgam_wz_pine_2017 = pine_2017_subset$dgam_WZ_mean
    dgam_mz_pine_2017 = pine_2017_subset$dgam_MZ_mean
    
    dl_pine_2018 = pine_2018_subset$DL
    ta_pine_2018 = pine_2018_subset$ta
    prep_pine_2018 = pine_2018_subset$prec
    part_pine_2018 = pine_2018_subset$part
    gpp1_pine_2018 = pine_2018_subset$gpp_ww
    dcell_pine_2018 = pine_2018_subset$gam_dcell_mean
    dgam_ez_pine_2018 = pine_2018_subset$dgam_EZ_mean
    dgam_wz_pine_2018 = pine_2018_subset$dgam_WZ_mean
    dgam_mz_pine_2018 = pine_2018_subset$dgam_MZ_mean
    
    dl_pine_2019 = pine_2019_subset$DL
    ta_pine_2019 = pine_2019_subset$ta
    prep_pine_2019 = pine_2019_subset$prec
    part_pine_2019 = pine_2019_subset$part
    gpp1_pine_2019 = pine_2019_subset$gpp_ww
    dcell_pine_2019 = pine_2019_subset$gam_dcell_mean
    dgam_ez_pine_2019 = pine_2019_subset$dgam_EZ_mean
    dgam_wz_pine_2019 = pine_2019_subset$dgam_WZ_mean
    dgam_mz_pine_2019 = pine_2019_subset$dgam_MZ_mean 
    
    
    ccf_dl_dcell_pine_2017 = ccf(dl_pine_2017,dcell_pine_2017,8)
    ccf_dl_dcell_pine_2018 = ccf(dl_pine_2018,dcell_pine_2018,8)
    ccf_dl_dcell_pine_2019 = ccf(dl_pine_2019,dcell_pine_2019,8)
    
    ccf_ta_dcell_pine_2017 = ccf(ta_pine_2017,dcell_pine_2017,8)
    ccf_ta_dcell_pine_2018 = ccf(ta_pine_2018,dcell_pine_2018,8)
    ccf_ta_dcell_pine_2019 = ccf(ta_pine_2019,dcell_pine_2019,8)
    
    ccf_prep_dcell_pine_2017 = ccf(prep_pine_2017,dcell_pine_2017,8)
    ccf_prep_dcell_pine_2018 = ccf(prep_pine_2018,dcell_pine_2018,8)
    ccf_prep_dcell_pine_2019 = ccf(prep_pine_2019,dcell_pine_2019,8)  
    
    ccf_part_dcell_pine_2017 = ccf(part_pine_2017,dcell_pine_2017,8)
    ccf_part_dcell_pine_2018 = ccf(part_pine_2018,dcell_pine_2018,8)
    ccf_part_dcell_pine_2019 = ccf(part_pine_2019,dcell_pine_2019,8)
    
    ccf_gpp_dcell_pine_2017 = ccf(gpp1_pine_2017,dcell_pine_2017,8)
    ccf_gpp_dcell_pine_2018 = ccf(gpp1_pine_2018,dcell_pine_2018,8)
    ccf_gpp_dcell_pine_2019 = ccf(gpp1_pine_2019,dcell_pine_2019,8)
    
    ccf_gpp_dl_2017 = ccf(dl_pine_2017,gpp1_pine_2017,8)
    ccf_gpp_dl_2018 = ccf(dl_pine_2018,gpp1_pine_2018,8)
    ccf_gpp_dl_2019 = ccf(dl_pine_2019,gpp1_pine_2019,8)
    
    ccf_gpp_ta_2017 = ccf(ta_pine_2017,gpp1_pine_2017,8)
    ccf_gpp_ta_2018 = ccf(ta_pine_2018,gpp1_pine_2018,8)
    ccf_gpp_ta_2019 = ccf(ta_pine_2019,gpp1_pine_2019,8)
    
    ccf_pine_df = data.frame("lag_ww" = c(-8:8),"ccf_dl_dcell_2017" = ccf_dl_dcell_pine_2017$acf,"ccf_dl_dcell_2018" = ccf_dl_dcell_pine_2018$acf,"ccf_dl_dcell_2019" = ccf_dl_dcell_pine_2019$acf,"ccf_ta_dcell_2017" = ccf_ta_dcell_pine_2017$acf,"ccf_ta_dcell_2018" = ccf_ta_dcell_pine_2018$acf,"ccf_ta_dcell_2019" = ccf_ta_dcell_pine_2019$acf,"ccf_prep_dcell_2017" = ccf_prep_dcell_pine_2017$acf,"ccf_prep_dcell_2018" = ccf_prep_dcell_pine_2018$acf,"ccf_prep_dcell_2019" = ccf_prep_dcell_pine_2019$acf,"ccf_part_dcell_2017" = ccf_part_dcell_pine_2017$acf,"ccf_part_dcell_2018" = ccf_part_dcell_pine_2018$acf,"ccf_part_dcell_2019" = ccf_part_dcell_pine_2019$acf,"ccf_gpp_dcell_2017" = ccf_gpp_dcell_pine_2017$acf,"ccf_gpp_dcell_2018" = ccf_gpp_dcell_pine_2018$acf,"ccf_gpp_dcell_2019" = ccf_gpp_dcell_pine_2019$acf)
  }
  #single tree
  #day length vs dcell
  {
    ccf_dl_dcell_pine_P16_2017 = ccf(gpp_growth_pine_tree_P16_2017_subset$DL,gpp_growth_pine_tree_P16_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P16_2018 = ccf(gpp_growth_pine_tree_P16_2018_subset$DL,gpp_growth_pine_tree_P16_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P16_2019 = ccf(gpp_growth_pine_tree_P16_2019_subset$DL,gpp_growth_pine_tree_P16_2019_subset$dcell,8)
    
    ccf_dl_dcell_pine_P20_2017 = ccf(gpp_growth_pine_tree_P20_2017_subset$DL,gpp_growth_pine_tree_P20_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P20_2018 = ccf(gpp_growth_pine_tree_P20_2018_subset$DL,gpp_growth_pine_tree_P20_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P20_2019 = ccf(gpp_growth_pine_tree_P20_2019_subset$DL,gpp_growth_pine_tree_P20_2019_subset$dcell,8)
    
    ccf_dl_dcell_pine_P21_2017 = ccf(gpp_growth_pine_tree_P21_2017_subset$DL,gpp_growth_pine_tree_P21_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P21_2018 = ccf(gpp_growth_pine_tree_P21_2018_subset$DL,gpp_growth_pine_tree_P21_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P21_2019 = ccf(gpp_growth_pine_tree_P21_2019_subset$DL,gpp_growth_pine_tree_P21_2019_subset$dcell,8)
    
    ccf_dl_dcell_pine_P25_2017 = ccf(gpp_growth_pine_tree_P25_2017_subset$DL,gpp_growth_pine_tree_P25_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P25_2018 = ccf(gpp_growth_pine_tree_P25_2018_subset$DL,gpp_growth_pine_tree_P25_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P25_2019 = ccf(gpp_growth_pine_tree_P25_2019_subset$DL,gpp_growth_pine_tree_P25_2019_subset$dcell,8)
    
    ccf_dl_dcell_pine_P26_2017 = ccf(gpp_growth_pine_tree_P26_2017_subset$DL,gpp_growth_pine_tree_P26_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P26_2018 = ccf(gpp_growth_pine_tree_P26_2018_subset$DL,gpp_growth_pine_tree_P26_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P26_2019 = ccf(gpp_growth_pine_tree_P26_2019_subset$DL,gpp_growth_pine_tree_P26_2019_subset$dcell,8)
    
    ccf_dl_dcell_pine_P7_2017 = ccf(gpp_growth_pine_tree_P7_2017_subset$DL,gpp_growth_pine_tree_P7_2017_subset$dcell,8)
    ccf_dl_dcell_pine_P7_2018 = ccf(gpp_growth_pine_tree_P7_2018_subset$DL,gpp_growth_pine_tree_P7_2018_subset$dcell,8)
    ccf_dl_dcell_pine_P7_2019 = ccf(gpp_growth_pine_tree_P7_2019_subset$DL,gpp_growth_pine_tree_P7_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A18_2017 = ccf(gpp_growth_maple_tree_A18_2017_subset$DL,gpp_growth_maple_tree_A18_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A18_2018 = ccf(gpp_growth_maple_tree_A18_2018_subset$DL,gpp_growth_maple_tree_A18_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A18_2019 = ccf(gpp_growth_maple_tree_A18_2019_subset$DL,gpp_growth_maple_tree_A18_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A19_2017 = ccf(gpp_growth_maple_tree_A19_2017_subset$DL,gpp_growth_maple_tree_A19_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A19_2018 = ccf(gpp_growth_maple_tree_A19_2018_subset$DL,gpp_growth_maple_tree_A19_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A19_2019 = ccf(gpp_growth_maple_tree_A19_2019_subset$DL,gpp_growth_maple_tree_A19_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A23_2017 = ccf(gpp_growth_maple_tree_A23_2017_subset$DL,gpp_growth_maple_tree_A23_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A23_2018 = ccf(gpp_growth_maple_tree_A23_2018_subset$DL,gpp_growth_maple_tree_A23_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A23_2019 = ccf(gpp_growth_maple_tree_A23_2019_subset$DL,gpp_growth_maple_tree_A23_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A28_2017 = ccf(gpp_growth_maple_tree_A28_2017_subset$DL,gpp_growth_maple_tree_A28_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A28_2018 = ccf(gpp_growth_maple_tree_A28_2018_subset$DL,gpp_growth_maple_tree_A28_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A28_2019 = ccf(gpp_growth_maple_tree_A28_2019_subset$DL,gpp_growth_maple_tree_A28_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A29_2017 = ccf(gpp_growth_maple_tree_A29_2017_subset$DL,gpp_growth_maple_tree_A29_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A29_2018 = ccf(gpp_growth_maple_tree_A29_2018_subset$DL,gpp_growth_maple_tree_A29_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A29_2019 = ccf(gpp_growth_maple_tree_A29_2019_subset$DL,gpp_growth_maple_tree_A29_2019_subset$dcell,8)
    
    ccf_dl_dcell_maple_A8_2017 = ccf(gpp_growth_maple_tree_A8_2017_subset$DL,gpp_growth_maple_tree_A8_2017_subset$dcell,8)
    ccf_dl_dcell_maple_A8_2018 = ccf(gpp_growth_maple_tree_A8_2018_subset$DL,gpp_growth_maple_tree_A8_2018_subset$dcell,8)
    ccf_dl_dcell_maple_A8_2019 = ccf(gpp_growth_maple_tree_A8_2019_subset$DL,gpp_growth_maple_tree_A8_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q05_2017 = ccf(gpp_growth_oak_tree_Q05_2017_subset$DL,gpp_growth_oak_tree_Q05_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q05_2018 = ccf(gpp_growth_oak_tree_Q05_2018_subset$DL,gpp_growth_oak_tree_Q05_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q05_2019 = ccf(gpp_growth_oak_tree_Q05_2019_subset$DL,gpp_growth_oak_tree_Q05_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q06_2017 = ccf(gpp_growth_oak_tree_Q06_2017_subset$DL,gpp_growth_oak_tree_Q06_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q06_2018 = ccf(gpp_growth_oak_tree_Q06_2018_subset$DL,gpp_growth_oak_tree_Q06_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q06_2019 = ccf(gpp_growth_oak_tree_Q06_2019_subset$DL,gpp_growth_oak_tree_Q06_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q09_2017 = ccf(gpp_growth_oak_tree_Q09_2017_subset$DL,gpp_growth_oak_tree_Q09_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q09_2018 = ccf(gpp_growth_oak_tree_Q09_2018_subset$DL,gpp_growth_oak_tree_Q09_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q09_2019 = ccf(gpp_growth_oak_tree_Q09_2019_subset$DL,gpp_growth_oak_tree_Q09_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q10_2017 = ccf(gpp_growth_oak_tree_Q10_2017_subset$DL,gpp_growth_oak_tree_Q10_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q10_2018 = ccf(gpp_growth_oak_tree_Q10_2018_subset$DL,gpp_growth_oak_tree_Q10_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q10_2019 = ccf(gpp_growth_oak_tree_Q10_2019_subset$DL,gpp_growth_oak_tree_Q10_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q11_2017 = ccf(gpp_growth_oak_tree_Q11_2017_subset$DL,gpp_growth_oak_tree_Q11_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q11_2018 = ccf(gpp_growth_oak_tree_Q11_2018_subset$DL,gpp_growth_oak_tree_Q11_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q11_2019 = ccf(gpp_growth_oak_tree_Q11_2019_subset$DL,gpp_growth_oak_tree_Q11_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q12_2017 = ccf(gpp_growth_oak_tree_Q12_2017_subset$DL,gpp_growth_oak_tree_Q12_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q12_2018 = ccf(gpp_growth_oak_tree_Q12_2018_subset$DL,gpp_growth_oak_tree_Q12_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q12_2019 = ccf(gpp_growth_oak_tree_Q12_2019_subset$DL,gpp_growth_oak_tree_Q12_2019_subset$dcell,8)   
    
    ccf_dl_dcell_oak_Q13_2017 = ccf(gpp_growth_oak_tree_Q13_2017_subset$DL,gpp_growth_oak_tree_Q13_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q13_2018 = ccf(gpp_growth_oak_tree_Q13_2018_subset$DL,gpp_growth_oak_tree_Q13_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q13_2019 = ccf(gpp_growth_oak_tree_Q13_2019_subset$DL,gpp_growth_oak_tree_Q13_2019_subset$dcell,8)
    
    ccf_dl_dcell_oak_Q15_2017 = ccf(gpp_growth_oak_tree_Q15_2017_subset$DL,gpp_growth_oak_tree_Q15_2017_subset$dcell,8)
    ccf_dl_dcell_oak_Q15_2018 = ccf(gpp_growth_oak_tree_Q15_2018_subset$DL,gpp_growth_oak_tree_Q15_2018_subset$dcell,8)
    ccf_dl_dcell_oak_Q15_2019 = ccf(gpp_growth_oak_tree_Q15_2019_subset$DL,gpp_growth_oak_tree_Q15_2019_subset$dcell,8)     
    
  }
  
  #maple variables & ccf outputs
  {
    dl_maple_2017 = maple_2017_subset$DL
    ta_maple_2017 = maple_2017_subset$ta
    prep_maple_2017 = maple_2017_subset$prec
    part_maple_2017 = maple_2017_subset$part
    gpp1_maple_2017 = maple_2017_subset$gpp_ww
    dcell_maple_2017 = maple_2017_subset$gam_dcell_mean
    dgam_ez_maple_2017 = maple_2017_subset$dgam_EZ_mean
    dgam_wz_maple_2017 = maple_2017_subset$dgam_WZ_mean
    dgam_mz_maple_2017 = maple_2017_subset$dgam_MZ_mean
    
    dl_maple_2018 = maple_2018_subset$DL
    ta_maple_2018 = maple_2018_subset$ta
    prep_maple_2018 = maple_2018_subset$prec
    part_maple_2018 = maple_2018_subset$part
    gpp1_maple_2018 = maple_2018_subset$gpp_ww
    dcell_maple_2018 = maple_2018_subset$gam_dcell_mean
    dgam_ez_maple_2018 = maple_2018_subset$dgam_EZ_mean
    dgam_wz_maple_2018 = maple_2018_subset$dgam_WZ_mean
    dgam_mz_maple_2018 = maple_2018_subset$dgam_MZ_mean
    
    dl_maple_2019 = maple_2019_subset$DL
    ta_maple_2019 = maple_2019_subset$ta
    prep_maple_2019 = maple_2019_subset$prec
    part_maple_2019 = maple_2019_subset$part
    gpp1_maple_2019 = maple_2019_subset$gpp_ww
    dcell_maple_2019 = maple_2019_subset$gam_dcell_mean
    dgam_ez_maple_2019 = maple_2019_subset$dgam_EZ_mean
    dgam_wz_maple_2019 = maple_2019_subset$dgam_WZ_mean
    dgam_mz_maple_2019 = maple_2019_subset$dgam_MZ_mean 
    
    ccf_dl_dcell_maple_2017 = ccf(dl_maple_2017,dcell_maple_2017,8)
    ccf_dl_dcell_maple_2018 = ccf(dl_maple_2018,dcell_maple_2018,8)
    ccf_dl_dcell_maple_2019 = ccf(dl_maple_2019,dcell_maple_2019,8)
    
    ccf_dl_dcell_maple_high_2017 = ccf(dl_maple_2017,dcell_maple_high_2017,8)
    ccf_dl_dcell_maple_high_2018 = ccf(dl_maple_2018,dcell_maple_high_2018,8)
    ccf_dl_dcell_maple_high_2019 = ccf(dl_maple_2019,dcell_maple_high_2019,8)
    
    ccf_dl_dcell_maple_low_2017 = ccf(dl_maple_2017,dcell_maple_low_2017,8)
    ccf_dl_dcell_maple_low_2018 = ccf(dl_maple_2018,dcell_maple_low_2018,8)
    ccf_dl_dcell_maple_low_2019 = ccf(dl_maple_2019,dcell_maple_low_2019,8)
    
    ccf_ta_dcell_maple_2017 = ccf(ta_maple_2017,dcell_maple_2017,8)
    ccf_ta_dcell_maple_2018 = ccf(ta_maple_2018,dcell_maple_2018,8)
    ccf_ta_dcell_maple_2019 = ccf(ta_maple_2019,dcell_maple_2019,8)
    
    ccf_ta_dcell_maple_high_2017 = ccf(ta_maple_2017,dcell_maple_high_2017,8)
    ccf_ta_dcell_maple_high_2018 = ccf(ta_maple_2018,dcell_maple_high_2018,8)
    ccf_ta_dcell_maple_high_2019 = ccf(ta_maple_2019,dcell_maple_high_2019,8)
    
    ccf_ta_dcell_maple_low_2017 = ccf(ta_maple_2017,dcell_maple_low_2017,8)
    ccf_ta_dcell_maple_low_2018 = ccf(ta_maple_2018,dcell_maple_low_2018,8)
    ccf_ta_dcell_maple_low_2019 = ccf(ta_maple_2019,dcell_maple_low_2019,8)
    
    ccf_prep_dcell_maple_2017 = ccf(prep_maple_2017,dcell_maple_2017,8)
    ccf_prep_dcell_maple_2018 = ccf(prep_maple_2018,dcell_maple_2018,8)
    ccf_prep_dcell_maple_2019 = ccf(prep_maple_2019,dcell_maple_2019,8)  
    
    ccf_part_dcell_maple_2017 = ccf(part_maple_2017,dcell_maple_2017,8)
    ccf_part_dcell_maple_2018 = ccf(part_maple_2018,dcell_maple_2018,8)
    ccf_part_dcell_maple_2019 = ccf(part_maple_2019,dcell_maple_2019,8)
    
    ccf_gpp_dcell_maple_2017 = ccf(gpp1_maple_2017,dcell_maple_2017,8)
    ccf_gpp_dcell_maple_2018 = ccf(gpp1_maple_2018,dcell_maple_2018,8)
    ccf_gpp_dcell_maple_2019 = ccf(gpp1_maple_2019,dcell_maple_2019,8)
    
    ccf_maple_df = data.frame("lag_ww" = c(-8:8),"ccf_dl_dcell_2017" = ccf_dl_dcell_maple_2017$acf,"ccf_dl_dcell_2018" = ccf_dl_dcell_maple_2018$acf,"ccf_dl_dcell_2019" = ccf_dl_dcell_maple_2019$acf,"ccf_ta_dcell_2017" = ccf_ta_dcell_maple_2017$acf,"ccf_ta_dcell_2018" = ccf_ta_dcell_maple_2018$acf,"ccf_ta_dcell_2019" = ccf_ta_dcell_maple_2019$acf,"ccf_prep_dcell_2017" = ccf_prep_dcell_maple_2017$acf,"ccf_prep_dcell_2018" = ccf_prep_dcell_maple_2018$acf,"ccf_prep_dcell_2019" = ccf_prep_dcell_maple_2019$acf,"ccf_part_dcell_2017" = ccf_part_dcell_maple_2017$acf,"ccf_part_dcell_2018" = ccf_part_dcell_maple_2018$acf,"ccf_part_dcell_2019" = ccf_part_dcell_maple_2019$acf,"ccf_gpp_dcell_2017" = ccf_gpp_dcell_maple_2017$acf,"ccf_gpp_dcell_2018" = ccf_gpp_dcell_maple_2018$acf,"ccf_gpp_dcell_2019" = ccf_gpp_dcell_maple_2019$acf)
  }
  
  #oak variables & ccf outputs
  {
    dl_oak_2017 = oak_2017_subset$DL
    ta_oak_2017 = oak_2017_subset$ta
    prep_oak_2017 = oak_2017_subset$prec
    part_oak_2017 = oak_2017_subset$part
    gpp1_oak_2017 = oak_2017_subset$gpp_ww
    dcell_oak_2017 = oak_2017_subset$gam_dcell_mean
    dgam_ez_oak_2017 = oak_2017_subset$dgam_EZ_mean
    dgam_wz_oak_2017 = oak_2017_subset$dgam_WZ_mean
    dgam_mz_oak_2017 = oak_2017_subset$dgam_MZ_mean
    
    dl_oak_2018 = oak_2018_subset$DL
    ta_oak_2018 = oak_2018_subset$ta
    prep_oak_2018 = oak_2018_subset$prec
    part_oak_2018 = oak_2018_subset$part
    gpp1_oak_2018 = oak_2018_subset$gpp_ww
    dcell_oak_2018 = oak_2018_subset$gam_dcell_mean
    dgam_ez_oak_2018 = oak_2018_subset$dgam_EZ_mean
    dgam_wz_oak_2018 = oak_2018_subset$dgam_WZ_mean
    dgam_mz_oak_2018 = oak_2018_subset$dgam_MZ_mean
    
    dl_oak_2019 = oak_2019_subset$DL
    ta_oak_2019 = oak_2019_subset$ta
    prep_oak_2019 = oak_2019_subset$prec
    part_oak_2019 = oak_2019_subset$part
    gpp1_oak_2019 = oak_2019_subset$gpp_ww
    dcell_oak_2019 = oak_2019_subset$gam_dcell_mean
    dgam_ez_oak_2019 = oak_2019_subset$dgam_EZ_mean
    dgam_wz_oak_2019 = oak_2019_subset$dgam_WZ_mean
    dgam_mz_oak_2019 = oak_2019_subset$dgam_MZ_mean 
    
    ccf_dl_dcell_oak_2017 = ccf(dl_oak_2017,dcell_oak_2017,8)
    ccf_dl_dcell_oak_2018 = ccf(dl_oak_2018,dcell_oak_2018,8)
    ccf_dl_dcell_oak_2019 = ccf(dl_oak_2019,dcell_oak_2019,8)
    
    ccf_ta_dcell_oak_2017 = ccf(ta_oak_2017,dcell_oak_2017,8)
    ccf_ta_dcell_oak_2018 = ccf(ta_oak_2018,dcell_oak_2018,8)
    ccf_ta_dcell_oak_2019 = ccf(ta_oak_2019,dcell_oak_2019,8)
    
    ccf_prep_dcell_oak_2017 = ccf(prep_oak_2017,dcell_oak_2017,8)
    ccf_prep_dcell_oak_2018 = ccf(prep_oak_2018,dcell_oak_2018,8)
    ccf_prep_dcell_oak_2019 = ccf(prep_oak_2019,dcell_oak_2019,8)  
    
    ccf_part_dcell_oak_2017 = ccf(part_oak_2017,dcell_oak_2017,8)
    ccf_part_dcell_oak_2018 = ccf(part_oak_2018,dcell_oak_2018,8)
    ccf_part_dcell_oak_2019 = ccf(part_oak_2019,dcell_oak_2019,8)
    
    ccf_gpp_dcell_oak_2017 = ccf(gpp1_oak_2017,dcell_oak_2017,8)
    ccf_gpp_dcell_oak_2018 = ccf(gpp1_oak_2018,dcell_oak_2018,8)
    ccf_gpp_dcell_oak_2019 = ccf(gpp1_oak_2019,dcell_oak_2019,8)
    
    
    ccf_oak_df = data.frame("lag_ww" = c(-8:8),"ccf_dl_dcell_2017" = ccf_dl_dcell_oak_2017$acf,"ccf_dl_dcell_2018" = ccf_dl_dcell_oak_2018$acf,"ccf_dl_dcell_2019" = ccf_dl_dcell_oak_2019$acf,"ccf_ta_dcell_2017" = ccf_ta_dcell_oak_2017$acf,"ccf_ta_dcell_2018" = ccf_ta_dcell_oak_2018$acf,"ccf_ta_dcell_2019" = ccf_ta_dcell_oak_2019$acf,"ccf_prep_dcell_2017" = ccf_prep_dcell_oak_2017$acf,"ccf_prep_dcell_2018" = ccf_prep_dcell_oak_2018$acf,"ccf_prep_dcell_2019" = ccf_prep_dcell_oak_2019$acf,"ccf_part_dcell_2017" = ccf_part_dcell_oak_2017$acf,"ccf_part_dcell_2018" = ccf_part_dcell_oak_2018$acf,"ccf_part_dcell_2019" = ccf_part_dcell_oak_2019$acf,"ccf_gpp_dcell_2017" = ccf_gpp_dcell_oak_2017$acf,"ccf_gpp_dcell_2018" = ccf_gpp_dcell_oak_2018$acf,"ccf_gpp_dcell_2019" = ccf_gpp_dcell_oak_2019$acf)
  }
  
  #pine ccf output
  {
    #p_ccf_pine_dl_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_pine_dl_dcell.pdf",width =6,height = 4)
    {
      p_ccf_pine_dl_dcell = ggplot()+
        #geom_ribbon(data = ccf_pine_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2017, 
        #                                                    ymax= ccf_dl_dcell_high_2017),fill = "lightblue",alpha = 0.4)+
        
        #geom_ribbon(data = ccf_pine_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2018, 
        #                                                    ymax= ccf_dl_dcell_high_2018),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = ccf_pine_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2019, 
        #                                                    ymax= ccf_dl_dcell_high_2019),fill = "darksalmon",alpha = 0.4)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_dl_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_dl_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_dl_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }
    
    p_ccf_pine_dl_dcell
    
    dev.off()
    
    #p_ccf_pine_ta_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_pine_ta_dcell.pdf",width =6,height = 4)
    {
      p_ccf_pine_ta_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_ta_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_ta_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_ta_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }  
    
    p_ccf_pine_ta_dcell
    
    dev.off()
    
    #p_ccf_pine_prep_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_pine_prep_dcell.pdf",width =6,height = 4)
    {
      p_ccf_pine_prep_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_prep_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_prep_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_prep_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_pine_prep_dcell
    dev.off()
    
    #p_ccf_pine_part_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_pine_part_dcell.pdf",width =6,height = 4)
    {
      p_ccf_pine_part_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_part_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_part_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_part_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_pine_part_dcell
    dev.off()
    
    #p_ccf_pine_gpp_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_pine_gpp_dcell.pdf",width =6,height = 4)
    {
      p_ccf_pine_gpp_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_gpp_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_gpp_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_pine_df,aes(x=lag_ww, y=ccf_gpp_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.423),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_pine_gpp_dcell
    dev.off()    
  }
  
  #maple ccf output
  {
    #p_ccf_maple_dl_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_maple_dl_dcell.pdf",width =6,height = 4)
    {
      p_ccf_maple_dl_dcell = ggplot()+
        #geom_ribbon(data = ccf_maple_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2017, 
        #                                                    ymax= ccf_dl_dcell_high_2017),fill = "lightblue",alpha = 0.4)+
        
        #geom_ribbon(data = ccf_maple_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2018, 
        #                                                    ymax= ccf_dl_dcell_high_2018),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = ccf_maple_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2019, 
        #                                                    ymax= ccf_dl_dcell_high_2019),fill = "darksalmon",alpha = 0.4)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_dl_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_dl_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_dl_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }
    
    p_ccf_maple_dl_dcell
    
    dev.off()
    
    #p_ccf_maple_ta_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_maple_ta_dcell.pdf",width =6,height = 4)
    {
      p_ccf_maple_ta_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_ta_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_ta_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_ta_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }  
    
    p_ccf_maple_ta_dcell
    
    dev.off()
    
    #p_ccf_maple_prep_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_maple_prep_dcell.pdf",width =6,height = 4)
    {
      p_ccf_maple_prep_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_prep_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_prep_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_prep_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_maple_prep_dcell
    dev.off()
    
    #p_ccf_maple_part_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_maple_part_dcell.pdf",width =6,height = 4)
    {
      p_ccf_maple_part_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_part_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_part_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_part_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_maple_part_dcell
    dev.off()
    
    #p_ccf_maple_gpp_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_maple_gpp_dcell.pdf",width =6,height = 4)
    {
      p_ccf_maple_gpp_dcell = ggplot()+
        #geom_ribbon(data = HF2019_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_maple_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_gpp_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_gpp_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_maple_df,aes(x=lag_ww, y=ccf_gpp_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.456),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.444),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.433),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_maple_gpp_dcell
    dev.off()    
  }
  
  #oak ccf output
  {
    #p_ccf_oak_dl_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_oak_dl_dcell.pdf",width =6,height = 4)
    {
      p_ccf_oak_dl_dcell = ggplot()+
        #geom_ribbon(data = ccf_oak_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2017, 
        #                                                    ymax= ccf_dl_dcell_high_2017),fill = "lightblue",alpha = 0.4)+
        
        #geom_ribbon(data = ccf_oak_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2018, 
        #                                                    ymax= ccf_dl_dcell_high_2018),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = ccf_oak_df,aes(x=lag_ww,ymin= ccf_dl_dcell_low_2019, 
        #                                                    ymax= ccf_dl_dcell_high_2019),fill = "darksalmon",alpha = 0.4)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_dl_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_dl_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_dl_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }
    
    p_ccf_oak_dl_dcell
    
    dev.off()
    
    #p_ccf_oak_ta_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_oak_ta_dcell.pdf",width =6,height = 4)
    {
      p_ccf_oak_ta_dcell = ggplot()+
        #geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_ta_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_ta_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_ta_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
      
    }  
    
    p_ccf_oak_ta_dcell
    
    dev.off()
    
    #p_ccf_oak_prep_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_oak_prep_dcell.pdf",width =6,height = 4)
    {
      p_ccf_oak_prep_dcell = ggplot()+
        #geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_prep_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_prep_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_prep_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_oak_prep_dcell
    dev.off()
    
    #p_ccf_oak_part_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_oak_part_dcell.pdf",width =6,height = 4)
    {
      p_ccf_oak_part_dcell = ggplot()+
        #geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_part_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_part_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_part_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_oak_part_dcell
    dev.off()
    
    #p_ccf_oak_gpp_dcell
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\CCF_outputs\\ccf_oak_gpp_dcell.pdf",width =6,height = 4)
    {
      p_ccf_oak_gpp_dcell = ggplot()+
        #geom_ribbon(data = HF2019_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "darksalmon",alpha = 0.4)+
        
        #geom_ribbon(data = HF2018_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightgreen",alpha = 0.4)+
        #geom_ribbon(data = HF2017_oak_gam_mean_sd_com,aes(x=DY,ymin= gam_WZ_mean - gam_WZ_sd, 
        #                                                    ymax= gam_WZ_mean + gam_WZ_sd),fill = "lightblue",alpha = 0.4)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_gpp_dcell_2017,color="a"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_gpp_dcell_2018,color="b"),size = 1)+
        geom_line(data = ccf_oak_df,aes(x=lag_ww, y=ccf_gpp_dcell_2019,color="c"),size = 1)+
        geom_hline(aes(yintercept=0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.532),color = "#619cff",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#00ba38",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        geom_hline(aes(yintercept=-0.576),color = "#f8766d",linetype="dashed",size =0.2)+
        xlim(-10,10)+
        ylim(-1,1)+
        xlab("Lag between series (week)")+
        ylab("Correlation coefficient")+
        scale_color_manual(name = '', values =c("a"="#619cff","b" = "#00ba38","c" = "#f8766d"), labels = c('2017','2018',"2019"))+
        theme_set(theme_bw())+
        theme(panel.grid.major=element_line(colour=NA))+
        theme(legend.position = "bottom",legend.text = element_text(size = 12))+
        theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    }
    p_ccf_oak_gpp_dcell
    dev.off()    
  }
  
}

#for conceptual model
{
  gpp_17_19_mean = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\gpp_17_19_mean.csv")  
  gam_gpp_mean <- gam(gpp_mean ~ s(DY), data=gpp_17_19_mean)
  gpp_17_19_mean$gam_gpp_mean = gam_gpp_mean$fitted.values
  Nor_gam_gpp_mean = normalize(gam_gpp_mean$fitted.values,range =c(0,1),method = "range")
  gpp_17_19_mean$Nor_gam_gpp_mean = Nor_gam_gpp_mean
  
  gpp_17 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\gpp_17.csv")  
  gam_gpp_17 <- gam(gpp_mean ~ s(DY), data=gpp_17)
  gpp_17$gam_gpp_mean = gam_gpp_17$fitted.values
  Nor_gam_gpp_17 = normalize(gam_gpp_17$fitted.values,range =c(0,1),method = "range")
  gpp_17$Nor_gam_gpp_17 = Nor_gam_gpp_17
  
  gpp_18 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\gpp_18.csv")  
  gam_gpp_18 <- gam(gpp_mean ~ s(DY), data=gpp_18)
  gpp_18$gam_gpp_mean = gam_gpp_18$fitted.values
  Nor_gam_gpp_18 = normalize(gam_gpp_18$fitted.values,range =c(0,1),method = "range")
  gpp_18$Nor_gam_gpp_18 = Nor_gam_gpp_18
  
  gpp_19 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\gpp_19.csv")  
  gam_gpp_19 <- gam(gpp_mean ~ s(DY), data=gpp_19)
  gpp_19$gam_gpp_mean = gam_gpp_19$fitted.values
  Nor_gam_gpp_19 = normalize(gam_gpp_19$fitted.values,range =c(0,1),method = "range")
  gpp_19$Nor_gam_gpp_19 = Nor_gam_gpp_19
  
  
  #2017
  {
    PIST_growth_17 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\PIST_growth_17.csv")
    QURU_growth_17 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\QURU_growth_17.csv")
    QURU_growth_17 = QURU_growth_17[1:25,]
    gam_QURU_mean <- gam(gam_dcell_mean ~ s(DY), data=QURU_growth_17)
    Nor_gam_QURU_mean = normalize(gam_QURU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_QURU_mean = c(Nor_gam_QURU_mean,NaN,NaN)
    QURU_growth_17$Nor_gam_QURU_mean = Nor_gam_QURU_mean
    
    ACRU_growth_17 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\ACRU_growth_17.csv")
    gam_ACRU_mean <- gam(gam_dcell_mean ~ s(DY), data=ACRU_growth_17)
    Nor_gam_ACRU_mean = normalize(gam_ACRU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_ACRU_mean = c(Nor_gam_ACRU_mean,NaN,NaN,NaN)
    ACRU_growth_17$Nor_gam_ACRU_mean = Nor_gam_ACRU_mean
    
    NSC = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\NSC_seasonal_estimate.csv")
  }
  #2018
  {
    PIST_growth_18 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\PIST_growth_18.csv")
    PIST_growth_18 = PIST_growth_18[1:24,]
    gam_PIST_mean <- gam(gam_dcell_mean ~ s(DY), data=PIST_growth_18)
    Nor_gam_PIST_mean = normalize(gam_PIST_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_PIST_mean = c(Nor_gam_PIST_mean,NaN,NaN)
    PIST_growth_18$Nor_gam_PIST_mean = Nor_gam_PIST_mean
    
    QURU_growth_18 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\QURU_growth_18.csv")
    QURU_growth_18 = QURU_growth_18[1:22,]
    gam_QURU_mean <- gam(gam_dcell_mean ~ s(DY), data=QURU_growth_18)
    Nor_gam_QURU_mean = normalize(gam_QURU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_QURU_mean = c(Nor_gam_QURU_mean,NaN,NaN)
    QURU_growth_18$Nor_gam_QURU_mean = Nor_gam_QURU_mean
    
    ACRU_growth_18 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\ACRU_growth_18.csv")
    ACRU_growth_18 = ACRU_growth_18[1:16,]
    gam_ACRU_mean <- gam(gam_dcell_mean ~ s(DY), data=ACRU_growth_18)
    Nor_gam_ACRU_mean = normalize(gam_ACRU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_ACRU_mean = c(Nor_gam_ACRU_mean,NaN,NaN,NaN)
    ACRU_growth_18$Nor_gam_ACRU_mean = Nor_gam_ACRU_mean
    
    p_concept_2018 <- ggplot()+
      geom_line(data = gpp_18,aes(x=DY, y=Nor_gam_gpp_18),color="#18A188",size = 1)+
      geom_line(data = PIST_growth_18,aes(x=DY, y=Nor_gam_PIST_mean),color="#D8BD55",size = 1,linetype = "dashed")+
      geom_line(data = QURU_growth_18,aes(x=DY, y=Nor_gam_QURU_mean),color="#FF9C0E",size = 1)+
      geom_line(data = ACRU_growth_18,aes(x=DY, y=Nor_gam_ACRU_mean),color="#E75727",size = 1)+
      theme_set(theme_bw())+
      xlim(50,325)+
      xlab("DoY")+
      ylab("Normalized growth rate")+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    
  }  
  #2019
  {
    PIST_growth_19 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\PIST_growth_19.csv")
    PIST_growth_19 = PIST_growth_19[1:23,]
    gam_PIST_mean <- gam(gam_dcell_mean ~ s(DY), data=PIST_growth_19)
    Nor_gam_PIST_mean = normalize(gam_PIST_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_PIST_mean = c(Nor_gam_PIST_mean,NaN,NaN)
    PIST_growth_19$Nor_gam_PIST_mean = Nor_gam_PIST_mean
    
    QURU_growth_19 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\QURU_growth_19.csv")
    gam_QURU_mean <- gam(gam_dcell_mean ~ s(DY), data=QURU_growth_19)
    Nor_gam_QURU_mean = normalize(gam_QURU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_QURU_mean = c(Nor_gam_QURU_mean,NaN,NaN)
    QURU_growth_19$Nor_gam_QURU_mean = Nor_gam_QURU_mean
    
    ACRU_growth_19 = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\For_conceptual_model\\ACRU_growth_19.csv")
    gam_ACRU_mean <- gam(gam_dcell_mean ~ s(DY), data=ACRU_growth_19)
    Nor_gam_ACRU_mean = normalize(gam_ACRU_mean$fitted.values,range =c(0,1),method = "range")
    #Nor_gam_ACRU_mean = c(Nor_gam_ACRU_mean,NaN,NaN,NaN)
    ACRU_growth_19$Nor_gam_ACRU_mean = Nor_gam_ACRU_mean
    
    p_concept_2019 <- ggplot()+
      geom_line(data = gpp_19,aes(x=DY, y=Nor_gam_gpp_19),color="#19A198",size = 1)+
      geom_line(data = PIST_growth_19,aes(x=DY, y=Nor_gam_PIST_mean),color="#D8BD55",size = 1,linetype = "dashed")+
      geom_line(data = QURU_growth_19,aes(x=DY, y=Nor_gam_QURU_mean),color="#FF9C0E",size = 1)+
      geom_line(data = ACRU_growth_19,aes(x=DY, y=Nor_gam_ACRU_mean),color="#E75727",size = 1)+
      theme_set(theme_bw())+
      xlim(50,325)+
      xlab("DoY")+
      ylab("Normalized growth rate")+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  }
  
  
  p_concept <- ggplot()+
    geom_line(data = gpp_17,aes(x=DY, y=gam_gpp_mean),color="#18A188",size = 1)+
    geom_line(data = PIST_growth_17,aes(x=DY, y=Nor_gam_PIST_mean),color="#D8BD55",size = 1,linetype = "dashed")+
    geom_line(data = QURU_growth_17,aes(x=DY, y=Nor_gam_QURU_mean),color="#FF9C0E",size = 1)+
    geom_line(data = ACRU_growth_17,aes(x=DY, y=Nor_gam_ACRU_mean),color="#E75727",size = 1)+
    theme_set(theme_bw())+
    xlim(50,325)+
    xlab("DoY")+
    ylab("Normalized growth rate")+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  
  xylo_conceptual = read.csv("xylo_conceptual.csv")
  
  p_concept1 <- ggplot()+
    geom_line(data = gpp_17,aes(x=DY, y=Nor_gam_gpp_17),color="#18A188",size = 1)+
    geom_line(data = xylo_conceptual,aes(x=DY, y=EZ1),color="#E75727",size = 1)+
    geom_line(data = xylo_conceptual,aes(x=DY, y=WZ1),color="#FF9C0E",size = 1)+
    geom_line(data = xylo_conceptual,aes(x=DY, y=MZ1),color="#D8BD55",size = 1)+
    geom_line(data = xylo_conceptual,aes(x=DY, y=EZ2),color="#E75727",size = 1,alpha = 0.3,linetype = "dashed")+
    geom_line(data = xylo_conceptual,aes(x=DY, y=WZ2),color="#FF9C0E",size = 1,alpha = 0.3,linetype = "dashed")+
    geom_line(data = xylo_conceptual,aes(x=DY, y=MZ2),color="#D8BD55",size = 1,alpha = 0.3,linetype = "dashed")+
    theme_set(theme_bw())+
    xlim(50,325)+
    xlab("DoY")+
    ylab("Normalized growth rate")+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  
  
  
  
  p_NSC_c_QURU <- ggplot()+
    geom_point(data = NSC,aes(x=DY, y=QURU),color="#FF9C0E",size = 4)+
    geom_line(data = NSC,aes(x=DY, y=QURU),color="#FF9C0E",size = 3)+
    ylim(0,1)+
    scale_x_continuous(breaks = round(seq(100,300, by = 100),1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  p_NSC_c_ACRU <- ggplot()+
    geom_point(data = NSC,aes(x=DY, y=ACRU),color="#E75727",size = 4)+
    geom_line(data = NSC,aes(x=DY, y=ACRU),color="#E75727",size = 3)+
    ylim(0,1)+
    scale_x_continuous(breaks = round(seq(100,300, by = 100),1)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  p_NSC_c_PIST <- ggplot()+
    geom_point(data = NSC,aes(x=DY, y=PIST),color="#D8BD55",size = 4)+
    geom_line(data = NSC,aes(x=DY, y=PIST),color="#D8BD55",size = 3)+
    ylim(0,1)+
    scale_x_continuous(breaks = round(seq(100,300, by = 100),1),labels = c("Early","Mid","Late")) +
    theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(size =16))
  
}

#inter-annual connections
{
  #autumn wood phenology against the beginning of enlargement
  pheno_MAPLE_cW_17_18 = c(Pheno_MAPLE_2017$cW,Pheno_MAPLE_2018$cW)
  pheno_MAPLE_bE_18_19 = c(Pheno_MAPLE_2018$bE,Pheno_MAPLE_2019$bE)
  
  pheno_OAK_cW_17_18 = c(Pheno_OAK_2017$cW,Pheno_OAK_2018$cW)
  pheno_OAK_bE_18_19 = c(Pheno_OAK_2018$bE,Pheno_OAK_2019$bE)    
  
  pheno_PINE_cW_17_18 = c(Pheno_PINE_2017$cW,Pheno_PINE_2018$cW)
  pheno_PINE_bE_18_19 = c(Pheno_PINE_2018$bE,Pheno_PINE_2019$bE)
  
  
  
  #2017 QURU
  #  sugar     starch      NSC_stem NSC_leaf    bE
  #5 2.1148581 1.67614623  3.791                NA
  #6 1.8831102 2.09436157  3.977                119
  #9 2.0308194 1.71276735  3.744                119
  #10 2.9821318 2.23644056 5.219                119
  #11 2.4346875 2.69837317 5.133                112
  #12 1.6005097 2.86093359 4.461                126
  #13 2.6144649 2.55297322 5.167                105
  #15 2.2377341 6.23431752 8.472                112
  
  #2018 QURU
  #5 2.6362336 1.72466962 4.361                 111
  #6 1.8225569 1.78741838 3.61                  118
  #9 1.6393786 1.66399864 3.3                   111
  #10 4.0769051 2.40281530 6.48                 118
  #11 2.4600052 3.31868937 5.779                111
  #12 3.0613175 2.27388475 5.335                 NA 
  #13 2.9054993 4.44708441 7.353                111
  #15 2.2744040 8.80929993 11.084               111 
  
  #2017 ACRU
  #8  1.2636620 1.14030694 2.4                  136 
  #18 1.6190039 2.56981311 4.19                 147 
  #19 0.9593007 1.81833800 2.78                 147
  #23 1.5136812 2.56967002 4.08                 153
  #28 1.6062402 2.44352756 4.05                 133
  #29 1.1631628 2.08493681 3.25                 154
  
  #2018 ACRU
  #8  1.5727425 1.89181046 3.46                 135
  #18 1.5636858 2.40364718 3.97                 146
  #19 1.0011965 1.83724245 2.84                 153
  #23 2.1249504 2.06856560 4.19                 153
  #28 1.4928714 2.46190809 3.95                 139
  #29 1.4560955 2.28274046 3.74                 160  
  
  #2017 PIST
  #  7 1.1901348 0.4498351 1.64                 133
  # 16 1.0317755 0.2757226 1.31                 133
  # 20 1.3355428 0.3911218 1.73                 133  
  # 21 1.0267037 0.3286203 1.36                 146
  # 25 1.3142449 0.3291422 1.64                 133
  # 26 1.2468917 0.3437652 1.59                 140
  
  #2018 PIST
  # 7 0.9829834 0.2076158  1.19                 132  
  # 16 1.9707574 0.5299323 2.5                  132
  # 20 0.9132233 0.3303844 1.24                 132
  # 21 2.1736978 0.7558838 2.93                 139
  # 25 1.2399437 0.3432399 1.58                 146
  # 26 0.7887532 0.1945338 0.98                 132
  
  pheno_MAPLE_in_ann_df = data.frame("MAPLE_cw_17" = pheno_MAPLE_cW_17_18[1:6],"MAPLE_cw_18" = pheno_MAPLE_cW_17_18[7:12],"MAPLE_bE_18" = pheno_MAPLE_bE_18_19[1:6],"MAPLE_bE_19" = pheno_MAPLE_bE_18_19[7:12])
  pheno_OAK_in_ann_df = data.frame("OAK_cw_17" = pheno_OAK_cW_17_18[1:6],"OAK_cw_18" = pheno_OAK_cW_17_18[7:12],"OAK_bE_18" = pheno_OAK_bE_18_19[1:6],"OAK_bE_19" = pheno_OAK_bE_18_19[7:12])
  pheno_PINE_in_ann_df = data.frame("PINE_cw_17" = pheno_PINE_cW_17_18[1:6],"PINE_cw_18" = pheno_PINE_cW_17_18[7:12],"PINE_bE_18" = pheno_PINE_bE_18_19[1:6],"PINE_bE_19" = pheno_PINE_bE_18_19[7:12])
  
  p_M_in <- ggplot(pheno_MAPLE_in_ann_df)+
    geom_point(aes(x = MAPLE_cw_17, y = MAPLE_bE_18),color = "darkred")+
    geom_point(aes(x = MAPLE_cw_18, y = MAPLE_bE_19),color = "blue")+
    xlab("cW_Maple")+
    ylab("bE_Maple")
  
  p_Q_in <- ggplot(pheno_OAK_in_ann_df)+
    geom_point(aes(x = OAK_cw_17, y = OAK_bE_18),color = "darkred")+
    geom_point(aes(x = OAK_cw_18, y = OAK_bE_19),color = "blue")+
    xlab("cW_Oak")+
    ylab("bE_Oak")
  
  p_P_in <- ggplot(pheno_PINE_in_ann_df)+
    geom_point(aes(x = PINE_cw_17, y = PINE_bE_18),color = "darkred")+
    geom_point(aes(x = PINE_cw_18, y = PINE_bE_19),color = "blue")+
    xlab("cW_Pine")+
    ylab("bE_Pine")
  
  #NSC against the beginning of enlargement
  NSC_pheno = read.csv("Inter_annual_NSC_pheno.csv")
  NSC_pheno_QURU_17 = subset(NSC_pheno,species == "Quercus rubra" & date == "2017/10/4")
  NSC_pheno_QURU_18 = subset(NSC_pheno,species == "Quercus rubra" & date == "2018/10/10")
  NSC_pheno_ACRU_17 = subset(NSC_pheno,species == "Acer rubrum" & date == "2017/10/4")
  NSC_pheno_ACRU_18 = subset(NSC_pheno,species == "Acer rubrum" & date == "2018/10/10")
  NSC_pheno_PIST_17 = subset(NSC_pheno,species == "Pinus strobus" & date == "2017/10/4")  
  NSC_pheno_PIST_18 = subset(NSC_pheno,species == "Pinus strobus" & date == "2018/10/10")  
  
  NSC_pheno_ACRU_17_18 = rbind(NSC_pheno_ACRU_17,NSC_pheno_ACRU_18)
  
  
  #mix effect models
  {
    
    NSC_Pheno_model1 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ species + (1|treeID),REML = F)
    NSC_Pheno_model2 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ NSC_stem_abs + (1|treeID),REML = F)
    NSC_Pheno_model3 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ leaf_fall + (1|treeID),REML = F)
    NSC_Pheno_model4 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ species * leaf_fall + (1|treeID),REML = F)
    NSC_Pheno_model5 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ NSC_stem_abs + leaf_fall + (1|treeID),REML = F)
    NSC_Pheno_model6 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ NSC_stem_abs * species + (1|treeID),REML = F)
    NSC_Pheno_model7 = lmerTest::lmer(data = NSC_pheno,formula = wood_pheno_next_year~ NSC_stem_abs * species + leaf_fall * species + (1|treeID),REML = F)
    anova(NSC_Pheno_model1,NSC_Pheno_model2,NSC_Pheno_model3,NSC_Pheno_model4,NSC_Pheno_model5,NSC_Pheno_model6,NSC_Pheno_model7)
    
  }
  
  #NSC_pheno
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_Q_nsc_pheno.pdf",width =5,height = 5) 
    p_Q_nsc_pheno <- ggplot()+
      geom_point(data = NSC_pheno_QURU_17,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_QURU_18,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("NSC_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_Q_nsc_pheno
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_A_nsc_pheno.pdf",width =5,height = 5) 
    
    p_A_nsc_pheno <- ggplot()+
      geom_point(data = NSC_pheno_ACRU_17,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_ACRU_18,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("NSC_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_A_nsc_pheno
    dev.off()
    
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_P_nsc_pheno.pdf",width =5,height = 5) 
    
    p_P_nsc_pheno <- ggplot()+
      geom_point(data = NSC_pheno_PIST_17,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_PIST_18,aes(x = NSC_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("NSC_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_P_nsc_pheno
    dev.off()
    
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_Q_starch_pheno.pdf",width =5,height = 5) 
    
    p_Q_starch_pheno <- ggplot()+
      geom_point(data = NSC_pheno_QURU_17,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_QURU_18,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("Starch_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_Q_starch_pheno
    dev.off()
    
    
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_A_starch_pheno.pdf",width =5,height = 5) 
    
    p_A_starch_pheno <- ggplot()+
      geom_point(data = NSC_pheno_ACRU_17,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_ACRU_18,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("Starch_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_A_starch_pheno
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_P_starch_pheno.pdf",width =5,height = 5) 
    
    p_P_starch_pheno <- ggplot()+
      geom_point(data = NSC_pheno_PIST_17,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_PIST_18,aes(x = Starch_stem_abs, y = wood_pheno_next_year),color = "blue")+
      xlab("Starch_stem_autumn(kg)")+
      ylab("DOY_spring")+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))   
    p_P_starch_pheno
    dev.off()
    
  }
  
  #leaf-fall pheno
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_Q_leaf_fall_pheno.pdf",width =5,height = 5) 
    
    p_Q_lf_pheno <- ggplot()+
      geom_point(data = NSC_pheno_QURU_17,aes(x = leaf_fall, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_QURU_18,aes(x = leaf_fall, y = wood_pheno_next_year),color = "blue")+
      xlab("DOY_leaf_fall")+
      ylab("DOY_spring")+
      #xlim(305,320)+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p_Q_lf_pheno
    dev.off()
    
    
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_A_leaf_fall_pheno.pdf",width =5,height = 5) 
    
    p_A_lf_pheno <- ggplot()+
      geom_point(data = NSC_pheno_ACRU_17,aes(x = leaf_fall, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_ACRU_18,aes(x = leaf_fall, y = wood_pheno_next_year),color = "blue")+
      xlab("DOY_leaf_fall")+
      ylab("DOY_spring")+
      xlim(270,300)+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    p_A_lf_pheno
    dev.off()
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\Inter_annual\\p_P_leaf_fall_pheno.pdf",width =5,height = 5) 
    
    p_P_lf_pheno <- ggplot()+
      geom_point(data = NSC_pheno_PIST_17,aes(x = leaf_fall, y = wood_pheno_next_year),color = "darkred")+
      geom_point(data = NSC_pheno_PIST_18,aes(x = leaf_fall, y = wood_pheno_next_year),color = "blue")+
      xlab("DOY_leaf_fall")+
      ylab("DOY_spring")+
      xlim(270,300)+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))   
    p_P_lf_pheno
    dev.off()
    
  }
}

#maximum rate vs season length
{
  #data notes
  {
    #maximum growth rate individual trees
    #        17       18       19
    # P7     88.79    144.90   253.79
    # P16    91.56    103.71   102.21
    # P20    131.59   152.06   243.49
    # P21    68.17    33.38    47.67
    # P25    240.69   274.44   366.72
    # P26    144.61   236.95   214.72
    
    #        17       18       19
    # A8     80.29    1231.83  318.86
    # A18    217.5    262.62   1603.6
    # A19    93.44    452.71   221.11
    # A23    100.62   196.15   164.91
    # A28    342.91   1218.50  381.4
    # A29    317.37   621.43   605.15
    
    #        17       18       19
    # Q5     131.31  222.07    236.99
    # Q6     112.76  203.04    158.20
    # Q9     193.04  256       394.4
    # Q10    137     229.99    353.64
    # Q11    115.34  316.23    199.4
    # Q12    141.43  231.77    502.23
    # Q13    137.71  345.6     417.50
    # Q15    155.45  591.57    236.18 
    
    #mean growth rate individual trees
    #        17       18       19
    # P7     41.83    103.17   85.1
    # P16    47.34    44.74    27.1
    # P20    59.82    44.45    65.35
    # P21    16.71    12.41    13.02
    # P25    65.05    89.33    76.36
    # P26    48.8     80.73    88.47
    
    #        17       18       19
    # A8     25.13    248.64   73.98
    # A18    44.88    67.2     486.05
    # A19    36.57    120.08   49.59
    # A23    25.81    38.65    81.26 
    # A28    72.92    274.39   183.12
    # A29    81.34    168.93   99.9
    
    #        17       18       19
    # Q5     45.66    63.3     142.3
    # Q6     41.08    91.03    88.78
    # Q9     67.86    122.85   128.44
    # Q10    46.77    118      133.19
    # Q11    54.17    109.57   73.42
    # Q12    44.09    88.53    118.91
    # Q13    45.04    110.71   195.34
    # Q15    59.27    127.79   181.27
    
    
    #growth length
    #        17       18       19
    # P7     146      160      140   
    # P16    139      139      133
    # P20    160      139      146
    # P21    132      85       90
    # P25    139      146      139
    # P26    146      153      140
    
    #        17       18       19 
    # A8     78       94       80
    # A18    85       84       90
    # A19    85       83       42
    # A23    57       36       76
    # A28    104     118       NA
    # A29    84       83       76
    
    #        17       18       19
    # Q5     119      NA       139            
    # Q6     126      119      112
    # Q9     118      125      125
    # Q10    118      154      132
    # Q11    91       125      115
    # Q12    97       118      NA
    # Q13    125      132      132
    # Q15    126      139      139
    
    #final ring width  til predicted growth end(average after enlargement)
    #     17                     18                     19
    # P7  1143.79(1432.96)       2634.38(2717.92)       1758.17(1838.65)  
    # P16 1101.50(1646.59)       1185.68(1106.8)        652.87(666.75)   
    # P20 1463.45(1961.24)       859.3(921.27)          1607.24(1632.89)
    # P21 480.62(572.36)         206.27(261.27)         252.33(313.26)
    # P25 1743.84(2141.29)       2290.45(2118.09)       1368.96(1249.07)
    # P26 1499.53(1606.77)       1954.81(2091.11)       1681.82(1436.79)
    
    #     17                     18                     19 
    # A8  780.28(803.01)         1694.75(2003.11)       539.01(173.87)
    # A18 1102.56(1183.58)       1403.94(1214.06)       7730.24
    # A19 697.3(871.03)          2845.31(820.79)        605.22(666.97)
    # A23 514.56(495.7)          589.05(579.47)         478.78(1249.36)
    # A28 2051.64(1678.08)       4597.27(6478.7)        2087.4(2643.42)
    # A29 675.83(2730.05)        2664.92(2571.23)       713.07
    
    #     17                     18                     19
    # Q5  1263.21(1439)          1550.95(1752.42)       2829.32(3212.63)
    # Q6  1211.94(1323.7)        1809.71(2473.01)       1589.19(1950.02)
    # Q9  2259.38(2355.76)       3231.83(3412.03)       2834.75(2359.15)
    # Q10 1324.39(1302.32)       3370.18(2165.86)       2689(2423.12)
    # Q11 1062.94(1716.61)       2568.22(2460.37)       1501.78(1497.93)
    # Q12 1222.87(1426.94)       2549.68(2184.16)       2402.1(2468.41)
    # Q13 1448.34(1565.21)       3181.91(2124.76)       3781.85(4410.35)
    # Q15 1731.22(1758.56)       2974.58(2006.82)       3860.13(4302.46)
  }
  
  P_17_df = data.frame("max_grow" = c(88.79,91.56,131.59,68.17,240.69,144.61), "mean_grow" = c(41.83,47.34,59.82,16.71,65.05,48.8), "duration" = c(146,139,160,132,139,146),"growth" = c(1143.79,1101.50,1463.45,480.62,1743.84,1499.53))
  P_18_df = data.frame("max_grow" = c(144.90,103.71,152.06,33.38,274.44,236.95), "mean_grow" = c(103.17,44.74,44.45,12.41,89.33,80.73), "duration" = c(160,139,139,85,146,153),"growth" = c(2634.38,1185.68,859.3,206.27,2290.45,1954.81))
  P_19_df = data.frame("max_grow" = c(253.79,102.21,243.49,47.67,366.72,214.72), "mean_grow" = c(85.1,27.1,65.35,13.02,76.36,88.47), "duration" = c(140,133,146,90,139,140),"growth" = c(1758.17,652.87,1607.24,252.33,1368.96,1681.82))
  P_df = rbind(P_17_df,P_18_df,P_19_df)
  
  A_17_df = data.frame("max_grow" = c(80.29,217.5,93.44,100.62,342.91,317.37), "mean_grow" = c(25.13,44.88,36.57,25.81,72.92,81.34),"duration" = c(78,85,85,57,104,84),"growth" = c(780.28,1102.56,697.3,514.56,2051.64,675.83))
  A_18_df = data.frame("max_grow" = c(1231.83,262.62,452.71,196.15,1218.5,621.43), "mean_grow" = c(248.64,67.2,120.08,38.65,274.39,168.93),"duration" = c(94,84,83,36,118,83),"growth" = c(1694.75,1403.94,2845.31,589.05,4597.27,2664.92))
  #no value of length data for A28 in 2019, so 5 values for each in A_19_df
  A_19_df = data.frame("max_grow" = c(318.86,1603.6,221.11,164.91,605.15), "mean_grow" = c(73.98,486.05,49.59,81.26,99.9),"duration" = c(80,90,42,76,76),"growth" = c(539.01,7730.24,605.22,478.78,713.07))      
  A_df = rbind(A_17_df,A_18_df,A_19_df)
  
  
  Q_17_df = data.frame("max_grow" = c(131.31,112.76,193.04,137,115.34,141.43,137.71,155.45), "mean_grow" = c(45.66,41.08,67.86,46.77,54.17,44.09,45.04,59.27),"duration" = c(119,126,118,118,91,97,125,126),"growth" = c(1263.21,1211.94,2259.38,1324.39,1062.94,1222.87,1448.34,1731.22))
  Q_18_df = data.frame("max_grow" = c(203.04,256,229.99,316.23,231.77,345.6,591.57), "mean_grow" = c(63.3,91.03,122.85,118,109.57,88.53,110.71,127.79),"duration" = c(119,125,154,125,118,132,139),"growth" = c(1809.71,3231.83,3370.18,2568.22,2549.68,3181.91,2974.58))
  Q_19_df = data.frame("max_grow" = c(236.99,158.2,394.4,353.64,199.4,417.5,236.18), "mean_grow" = c(142.3,88.78,128.44,133.19,73.42,118.91,195.34,181.27),"duration" = c(139,112,125,132,115,132,139),"growth" = c(2829.32,1589.19,2834.75,2689,1501.78,3781.85,3860.13))   
  Q_df = rbind(Q_17_df,Q_18_df,Q_19_df)      
  
  library("ppcor")
  
  pcor(P_17_df)
  pcor(P_18_df)
  pcor(P_19_df)
  pcor(A_17_df)
  pcor(A_18_df)
  pcor(A_19_df)
  pcor(Q_17_df)
  pcor(Q_18_df)
  pcor(Q_19_df)
  
  pcor(P_df)
  pcor(A_df)
  pcor(Q_df)
  
} 

#maximum enlargement zone width vs enlargement length
{
  #data notes
  {
    #maximum growth rate individual trees gam_EZ
    #        17       18       19
    # P7     93.11    133.04   142.12
    # P16    53.56    73.41    115.66
    # P20    100.85   154.2    159.66
    # P21    33.72    44.79    54.97
    # P25    137.44   143.27   156.17
    # P26    57.05    116.28   99.96
    
    #        17       18       19
    # A8     36.96    166.88   78.65
    # A18    40.09    67.19    80.89
    # A19    82.12    154.14   70.47
    # A23    70.60    105.69   116.05
    # A28    53.21    161.75   160.07
    # A29    54.46    135.06   224.21
    
    #        17       18       19
    # Q5     120.02  96.9      195.53
    # Q6     176.21  177.71    118.03
    # Q9     106.11  143.8     155.79
    # Q10    156.26  96.05     144.05
    # Q11    108.42  90.49     91.14
    # Q12    77.97   101.5     171.54
    # Q13    99.72   108.57    164.01
    # Q15    136.28  82.98     111.59 
    
    #mean growth rate of enlargement individual trees gam_EZ
    #        17       18       19
    # P7     44.72    66.7     85.78
    # P16    28.49    40.88    55.23     
    # P20    45.73    57.45    65.8
    # P21    17.83    23.06    26.18
    # P25    71.09    75.8     63.36    
    # P26    29.91    63.02    57.7
    
    #        17       18       19
    # A8     18.28    75.89    46.71
    # A18    23.83    38.17    54.51
    # A19    43.11    56.96    32.53
    # A23    40.29    58.75    55.76   
    # A28    25.01    81.43    95.6
    # A29    30.47    67.99    80.85
    
    #        17       18       19
    # Q5     71.11    52.82    82.12
    # Q6     74.63    68.98    67.47  
    # Q9     56.76    73.24    78.03
    # Q10    87.39    51.12    82.48
    # Q11    59.72    46.07    50.11
    # Q12    41.67    58.53    91.64
    # Q13    52.29    64.24    97.52
    # Q15    64.91    45.84    75.14
    
    
    #growth length
    #        17       18       19
    # P7     104      118      111   
    # P16    111      104      63
    # P20    104      118      83
    # P21    63       36       63
    # P25    84       97       90
    # P26    112      84       90
    
    #        17       18       19 
    # A8     64       81       74
    # A18    71       42       90
    # A19    50       83       31
    # A23    43       36       35
    # A28    84      118       NA
    # A29    57       63       69
    
    #        17       18       19
    # Q5     87       NA       111            
    # Q6     105      108      76
    # Q9     77       104      111
    # Q10    84       97       118
    # Q11    77       97       61
    # Q12    63       97       NA
    # Q13    63       112      118
    # Q15    71       84       111
    
    #final ring width average after enlargement ends
    #     17                     18                     19
    # P7  1180.9                 2436.3                 1741.8
    # P16 1242.22                1171.9                 634.7
    # P20 1449.1                 903                    1565.1
    # P21 480.8                  179.3                  276.3
    # P25 1686.9                 2331.1                 1598.8
    # P26 1482.3                 1661.2                 1961.4
    
    #     17                     18                     19 
    # A8  818.7                 2173.7                  468.3
    # A18 1032.7                1296.6                  7730.2
    # A19 806.7                 2177.7                  705.6
    # A23 520                   710.4                   783.6
    # A28 1881.3                6017                    2375.3
    # A29 1336.6                2080.2                  798.3
    
    #     17                     18                     19
    # Q5  1311.4                 1559.8                 2660.5
    # Q6  1295.1                 1767.1                 1605.5
    # Q9  2207.9                 3126                   2657.8
    # Q10 1231.7                 2594.6                 2600.4
    # Q11 1321.9                 2232.5                 1322.8
    # Q12 1200.7                 2431.5                 2066.7
    # Q13 1409.7                 2860.7                 4009.5
    # Q15 1518.2                 2751.2                 3658.5
  }
  
  #get specific values
  {
    
    
    
  }
  
  
  
  P_17_enlarge_df = data.frame("max_grow" = c(93.11,53.56,100.85,33.72,	137.44,	57.05
  ), "mean_grow" = c(44.72,28.49,45.73,17.83,71.09,29.91),"duration" = c(104,63,84,112,104,111),"growth" = c(1180.9,	1242.22,	1449.1,	480.8,	1686.9,	1482.3))
  P_18_enlarge_df = data.frame("max_grow" = c(133.04,	73.41,	154.2,	44.79,	143.27,	116.28
  ),"mean_grow" = c(66.7,40.88,57.45,23.06,75.8,63.02), "duration" = c(118,  36,  97,  84, 118,104),"growth" = c(2436.3,	1171.9,	903,	179.3,	2331.1,	1661.2))
  P_19_enlarge_df = data.frame("max_grow" = c(142.12,	115.66,	159.66,	54.97,	156.17,	99.96), "mean_grow" = c(85.78,	55.23,	65.8,	26.18,	63.36,	57.7),"duration" = c(111,63  ,83,  63,  90,  90),"growth" = c(1741.8,	634.7,	1565.1,	276.3,	1598.8,	1961.4))
  P_enlarge_df = rbind(P_17_enlarge_df,P_18_enlarge_df,P_19_enlarge_df)
  
  A_17_enlarge_df = data.frame("max_grow" = c(36.96,	40.09,	82.12,	70.6,	53.21,	54.46
  ),"mean_grow" = c(18.28,23.83,43.11,40.29,25.01,30.47), "duration" = c(50, 43, 84, 57, 64,71),"growth" = c(818.7,	1032.7,	806.7,	520,	1881.3,	1336.6))
  A_18_enlarge_df = data.frame("max_grow" = c(166.88,	67.19,	154.14,	105.69,	161.75,	135.06
  ), "mean_grow" = c(75.89,	38.17,	56.96,	58.75,	81.43,	67.99),"duration" = c(81,42,  83,  36, 118,  63),"growth" = c(2173.7,	1296.6,	2177.7,	710.4,	6017,	2080.2))
  #no value of length data for A28 in 2019, so 5 values for each in A_19_df
  A_19_enlarge_df = data.frame("max_grow" = c(78.65,	80.89,	70.47,	116.05,	224.21
  ), "mean_grow" = c(46.71,54.51,32.53,55.76,80.85),"duration" = c(74,90, 31, 35, 69),"growth" = c(468.3,	7730.2,	705.6,	783.6,	798.3))      
  A_enlarge_df = rbind(A_17_enlarge_df,A_18_enlarge_df,A_19_enlarge_df)
  
  
  Q_17_enlarge_df = data.frame("max_grow" = c(120.02,	176.21,	106.11,	156.26,	108.42,	77.97,	99.72,	136.28), "mean_grow" = c(71.11	,74.63	,56.76	,87.39	,59.72	,41.67	,52.29,	64.91),"duration" = c(87, 105,  77,  84,  77,  63,  63,  71),"growth" = c(1311.4,	1295.1,	2207.9,	1231.7,	1321.9,	1200.7,	1409.7,	1518.2
  ))
  Q_18_enlarge_df = data.frame("max_grow" = c(177.71,	143.8,	96.05,	90.49,	101.5,	108.57,	82.98), "mean_grow" = c(68.98,	73.24,	51.12,	46.07,	58.53,	64.24,45.84),"duration" = c(108, 104,  97,  97,  97, 112,  84),"growth" = c(1767.1,	3126,	2594.6,	2232.5,	2431.5,	2860.7,	2751.2))
  Q_19_enlarge_df = data.frame("max_grow" = c(195.53,	118.03,	155.79,	144.05,	91.14,	164.01,	111.59), "mean_grow" = c(82.12,67.47,78.03,82.48,50.11,97.52,75.14),"duration" = c(111,  76, 111, 118,  61, 118, 111),"growth" = c(2660.5,	1605.5,	2657.8,	2600.4,	1322.8,	4009.5,	3658.5))   
  Q_enlarge_df = rbind(Q_17_enlarge_df,Q_18_enlarge_df,Q_19_enlarge_df)      
  
  library("ppcor")
  
  pcor(P_17_enlarge_df)
  pcor(P_18_enlarge_df)
  pcor(P_19_enlarge_df)
  pcor(A_17_enlarge_df)
  pcor(A_18_enlarge_df)
  pcor(A_19_enlarge_df)
  pcor(Q_17_enlarge_df)
  pcor(Q_18_enlarge_df)
  pcor(Q_19_enlarge_df)
  
  pcor(P_enlarge_por_df)
  pcor(A_enlarge_por_df)
  pcor(Q_enlarge_por_df)
  
  P_enlarge_por_df = P_enlarge_df
  A_enlarge_por_df = A_enlarge_df
  Q_enlarge_por_df = Q_enlarge_df
  
  
  #add year and species info. into the dfs
  P_enlarge_df$year = c(rep(2017,6),rep(2018,6),rep(2019,6))
  A_enlarge_df$year = c(rep(2017,6),rep(2018,6),rep(2019,5))
  Q_enlarge_df$year = c(rep(2017,8),rep(2018,7),rep(2019,7))
  
  P_enlarge_df$year1 = c(rep(1,6),rep(2,6),rep(3,6))
  A_enlarge_df$year1 = c(rep(1,6),rep(2,6),rep(3,5))
  Q_enlarge_df$year1 = c(rep(1,8),rep(2,7),rep(3,7))
  
  P_enlarge_df$tree = c(rep(c("P1","P2","P3","P4","P5","P6"),3))
  A_enlarge_df$tree = c(rep(c("M1","M2","M3","M4","M5","M6"),2),c("M1","M2","M3","M4","M6"))
  Q_enlarge_df$tree = c(c("O1","O2","O3","O4","O5","O6","O7","O8"),c("O2","O3","O4","O5","O6","O7","O8"),c("O1","O2","O3","O4","O5","O6","O8"))
  
  enlarge_df = rbind(P_enlarge_df,A_enlarge_df,Q_enlarge_df)
  enlarge_df$species = c(rep("Pine",18),rep("Maple",17),rep("Oak",22))
  enlarge_df$tree = c(rep(c("P1","P2","P3","P4","P5","P6"),3),rep(c("M1","M2","M3","M4","M5","M6"),2),c("M1","M2","M3","M4","M6"),c("O1","O2","O3","O4","O5","O6","O7","O8"),c("O2","O3","O4","O5","O6","O7","O8"),c("O1","O2","O3","O4","O5","O6","O8"))
  
  #for marginal R2 and Conditional R2  
  library("MuMIn")
  
  #all species
  #maximum growth correlations
  {
    mix_enlarge02 = lmerTest::lmer(data = enlarge_df,formula = growth ~ (1|tree),REML = F)
    mix_enlarge01 = lmerTest::lmer(data = enlarge_df,formula = growth ~ year + (1|tree),REML = F)
    mix_enlarge0 = lmerTest::lmer(data = enlarge_df,formula = growth ~ species + (1|tree),REML = F)
    mix_enlarge = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + (1|tree),REML = F)
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * species + (1|tree),REML = F)
    # mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    mix_enlarge2 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * year + (1|tree),REML = F)
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    mix_enlarge3 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * year * species + (1|tree),REML = F)
    mix_enlarge4 = lmerTest::lmer(data = enlarge_df,formula = growth ~ duration + (1|tree),REML = F)
    mix_enlarge5 = lmerTest::lmer(data = enlarge_df,formula = growth ~ duration * species + (1|tree),REML = F)
    mix_enlarge6 = lmerTest::lmer(data = enlarge_df,formula = growth ~ duration * year + (1|tree),REML = F)
    mix_enlarge7 = lmerTest::lmer(data = enlarge_df,formula = growth ~ duration * year * species + (1|tree),REML = F)
    mix_enlarge8 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * duration + (1|tree),REML = F)  
    mix_enlarge9 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * duration * species + (1|tree),REML = F)  
    mix_enlarge10 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * duration * year + (1|tree),REML = F)
    mix_enlarge11 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow * duration * species * year + (1|tree),REML = F) 
    mix_enlarge12 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow + (1|tree),REML = F)
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    mix_enlarge13 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * species + (1|tree),REML = F)
    # mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    mix_enlarge14 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * year + (1|tree),REML = F)
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    mix_enlarge15 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * year * species + (1|tree),REML = F)
    mix_enlarge16 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * duration + (1|tree),REML = F)  
    mix_enlarge17 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * duration * species + (1|tree),REML = F)  
    mix_enlarge18 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * duration * year + (1|tree),REML = F)
    mix_enlarge19 = lmerTest::lmer(data = enlarge_df,formula = growth ~ mean_grow * duration * species * year + (1|tree),REML = F) 
    
    
    anova(mix_enlarge02,mix_enlarge01,mix_enlarge0,mix_enlarge,mix_enlarge1,mix_enlarge2,mix_enlarge3,mix_enlarge4,mix_enlarge5,mix_enlarge6,mix_enlarge7,mix_enlarge8,mix_enlarge9,mix_enlarge10,mix_enlarge11,mix_enlarge12,mix_enlarge13,mix_enlarge14,mix_enlarge15,mix_enlarge16,mix_enlarge17,mix_enlarge18,mix_enlarge19)
  }
  
  
  #each species
  #QURU
  {
    mix_enlarge_o01 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ (1|tree),REML = F)
    mix_enlarge_o0 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ year1 + (1|tree))    
    #mix_enlarge_o0_test = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ year1 + (year1|tree))   
    mix_enlarge_o1 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ max_grow + (1|tree))
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    # mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    mix_enlarge_o2 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ max_grow * year1 + (1|tree))
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    mix_enlarge_o3 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ duration + (1|tree))
    mix_enlarge_o4 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ duration * year1 + (1|tree))
    mix_enlarge_o5 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ max_grow * duration + (1|tree))  
    mix_enlarge_o6 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ max_grow * duration * year1 + (1|tree))
    mix_enlarge_o7 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ mean_grow + (1|tree))
    mix_enlarge_o8 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ mean_grow * year1 + (1|tree))
    mix_enlarge_o9 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ mean_grow * duration + (1|tree))  
    mix_enlarge_o10 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ mean_grow * duration * year1 + (1|tree))
    mix_enlarge_o11 = lmerTest::lmer(data = Q_enlarge_df,formula = growth ~ max_grow * duration * year1 + (1|tree))
    
    
    
    anova(mix_enlarge_o01,mix_enlarge_o0,mix_enlarge_o1,mix_enlarge_o2,mix_enlarge_o3,mix_enlarge_o4,mix_enlarge_o5,mix_enlarge_o6,mix_enlarge_o7,mix_enlarge_o8,mix_enlarge_o9,mix_enlarge_o10,mix_enlarge_o11)
  } 
  #ACRU
  {
    mix_enlarge_a01 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ (1|tree))
    mix_enlarge_a0 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ year1 + (1|tree))
    mix_enlarge_a1 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ max_grow + (1|tree))
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    # mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    mix_enlarge_a2 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ max_grow * year1 + (1|tree))
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    mix_enlarge_a3 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ duration + (1|tree))
    mix_enlarge_a4 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ duration * year1 + (1|tree))
    mix_enlarge_a5 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ max_grow * duration + (1|tree))  
    mix_enlarge_a6 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ max_grow * duration *year1 + (1|tree))
    mix_enlarge_a7 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ mean_grow + (1|tree))
    mix_enlarge_a8 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ mean_grow * year1 + (1|tree))
    mix_enlarge_a9 = lmerTest::lmer(data = A_enlarge_df,formula = growth ~ mean_grow * duration * year1 + (1|tree))
    
    
    
    
    anova(mix_enlarge_a01,mix_enlarge_a0,mix_enlarge_a1,mix_enlarge_a2,mix_enlarge_a3,mix_enlarge_a4,mix_enlarge_a5,mix_enlarge_a6,mix_enlarge_a7,mix_enlarge_a8,mix_enlarge_a9)
  }   
  #PIST
  {
    mix_enlarge_p01 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ (1|tree))
    mix_enlarge_p0 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ year1 +(1|tree))
    mix_enlarge_p1 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow + (1|tree))
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    # mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    mix_enlarge_p2 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow * year1 + (1|tree))
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    mix_enlarge_p3 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ duration + (1|tree))
    mix_enlarge_p4 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ duration * year1 + (1|tree))
    mix_enlarge_p5 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow * duration + (1|tree))  
    mix_enlarge_p6 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow * duration * year1 + (1|tree))
    mix_enlarge_p7 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow + (1|tree))
    mix_enlarge_p8 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow * year1 + (1|tree))
    mix_enlarge_p9 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow * duration * year1 + (1|tree))
    
    
    anova(mix_enlarge_p01,mix_enlarge_p0,mix_enlarge_p1,mix_enlarge_p2,mix_enlarge_p3,mix_enlarge_p4,mix_enlarge_p5,mix_enlarge_p6,mix_enlarge_p7,mix_enlarge_p8,mix_enlarge_p9)
  }
  
  #PIST random effect:year
  {
    P_enlarge_df$tree1 = c(rep(c(1,2,3,4,5,6),3))
    mix_enlarge_year_p01 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ (1|year1))
    mix_enlarge_year_p0 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ tree + (1|year1))
    #mix_enlarge_year_p1 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow + (1|year1))
    #mix_enlarge1 = lmerTest::lmer(data = enlarge_df,formula = growth ~ max_grow + species + (1|tree),REML = F)
    #mix_enlarge2 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + (1|tree),REML = F)
    #mix_enlarge_year_p2 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow * tree + (1|year1))
    #mix_enlarge5 = lmer(data = enlarge_df,formula = growth ~ max_grow + year + species + (1|tree),REML = F)
    #mix_enlarge_year_p3 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ duration + (1|year1))
    #mix_enlarge_year_p4 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ duration * tree + (1|year1))
    #mix_enlarge_year_p5 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ max_grow * duration + (1|year1))  
    mix_enlarge_year_p6 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow + (1|year1))
    mix_enlarge_year_p7 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow * tree + (1|year1))  
    mix_enlarge_year_p8 = lmerTest::lmer(data = P_enlarge_df,formula = growth ~ mean_grow + tree + (1|year1))  
    
    
    anova(mix_enlarge_year_p01,mix_enlarge_year_p0,mix_enlarge_year_p6,mix_enlarge_year_p7,mix_enlarge_year_p8)
  }  
  
}

#growth rates vs daylength and temperature partial correlation
{
  #single tree
  {
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2017_subset$prec, "Dc" = gpp_growth_pine_tree_P16_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P16_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P16_2019_subset$dcell))
    
    
    #P16 Dc-DL      Dc-Ta        Dc-Prec
    #17  0.307      -0.658**     0.079           
    #18  0.983***   0.568*       0.45
    #19  0.848***   -0.748***    -0.036
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2017_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2018_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2019_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2019_subset$dcell))
    
    #P20 Dc-DL       Dc-Ta      Dc-Prec
    #17  0.208      -0.376      0.189     
    #18  0.236      -0.216      -0.279
    #19  0.772***   -0.624**    0.018      
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2019_subset$dcell))
    
    #P21 Dc-DL       Dc-Ta        Dc-Prec
    #17  0.682**     0.208        -0.0046
    #18  0.413       -0.836**     -0.123
    #19  0.811***    -0.628*      -0.1
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2019_subset$dcell))
    
    #P25 Dc-DL       Dc-Ta      Dc-Prec
    #17  0.498*      0.041      -0.13
    #18  0.853***    0.516*     -0.171
    #19  0.74***     0.38       0.026
    
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2019_subset$dcell))
    
    #P26 Dc-DL       Dc-Ta        Dc-Prec
    #17  0.41        0.519*       0.037
    #18  0.574**     -0.65**      0.21
    #19  0.65**      0.594**      -0.0429
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2019_subset$dcell))
    
    #P7  Dc-DL       Dc-Ta         Dc-Prec
    #17  0.82***      0.74***      0.037
    #18  0.921***    -0.7***       0.128
    #19  0.366       -0.13         -0.107
    
    
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2017_subset$prec, "Dc" = gpp_growth_maple_tree_A18_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A18_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A18_2019_subset$dcell))
    
    #A18 Dc-DL       Dc-Ta       Dc-Prec
    #17  0.847**     0.525      -0.122      
    #18  0.038       0.342      0.8**
    #19  -0.99***    0.81**     0.607     
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2019_subset$dcell))
    
    #A19  Dc-DL       Dc-Ta       Dc-Prec
    #17  0.64*       -0.475       0.09
    #18  -0.18        0.375       0.16
    #19  -0.487       0.895       0.467
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2019_subset$dcell))
    
    #A23  Dc-DL       Dc-Ta         Dc-Prec
    #17  0.987***     -0.082        -0.374
    #18  0.998*       -0.05*        -0.21*
    #19  -0.943***    -0.13          0.389        
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2019_subset$dcell))
    
    #A28  Dc-DL       Dc-Ta      Dc-Prec
    #17  -0.086       0.425      0.407
    #18  -0.449       -0.206     -0.224 
    #19  
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2019_subset$dcell))
    
    #A29  Dc-DL       Dc-Ta      Dc-Prec
    #17   -0.422      0.085      -0.33
    #18   0.233       0.439      0.414
    #19   0.392       0.304      0.142
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2019_subset$dcell))
    
    #A8  Dc-DL       Dc-Ta      Dc-Prec
    #17  0.829**     0.144      -0.367
    #18  0.379       0.228      0.063
    #19  0.228       0.014      0.26
    
    pcor(data.frame("DL" = gpp_growth_maple_2017_subset$DL,"Ta" = gpp_growth_maple_2017_subset$ta,"Prep" = gpp_growth_maple_2017_subset$prec,"Dc" = gpp_growth_maple_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_2018_subset$DL,"Ta" = gpp_growth_maple_2018_subset$ta,"Prep" = gpp_growth_maple_2018_subset$prec,"Dc" = gpp_growth_maple_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_maple_2019_subset$DL,"Ta" = gpp_growth_maple_2019_subset$ta,"Prep" = gpp_growth_maple_2019_subset$prec,"Dc" = gpp_growth_maple_2019_subset$dcell))  
    pcor(data.frame("DL" = maple_17_19_subset$DL,"Ta" = maple_17_19_subset$ta,"Prep" = maple_17_19_subset$prec,"Dc" = maple_17_19_subset$dcell))    
    
    
    
    
    
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2019_subset$dcell))
    
    #Q05  Dc-DL       Dc-Ta      Dc-Prec
    #17  0.672**     -0.607*     0.12
    #18  
    #19  -0.608**      0.177       0.516*
    
    
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2019_subset$dcell))
    
    #Q06  Dc-DL       Dc-Ta       Dc-Prec
    #17  0.653**      -0.6*       -0.276
    #18  -0.772**     -0.172      -0.363      
    #19  -0.586*      -0.602*     -0.02
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2019_subset$dcell))
    
    #Q09  Dc-DL       Dc-Ta     Dc-Prec
    #17   0.92***     -0.463    -0.273    
    #18  -0.18        0.12      -0.352      
    #19   0.822***    -0.451    0.386
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2019_subset$dcell))
    
    #Q10  Dc-DL       Dc-Ta        Dc-Prec
    #17   0.046       -0.202       0.013
    #18  -0.448*      -0.416       0.711***      
    #19   0.886***    -0.429       0.217
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2019_subset$dcell))
    
    #Q11  Dc-DL       Dc-Ta       Dc-Prec
    #17   0.584      -0.546       0.133
    #18   0.22        0.38        0.427
    #19   -0.88***    0.903***    -0.199
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2019_subset$dcell))
    
    #Q12  Dc-DL       Dc-Ta      Dc-Prec
    #17   0.775**     -0.505*    0.28
    #18   0.984***    -0.346     0.426      
    #19      
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2019_subset$dcell))
    
    #Q13  Dc-DL       Dc-Ta        Dc-Prec
    #17   0.845***    -0.592*      -0.304
    #18   0.305       0.624**      0.269      
    #19   0.647**     -0.506*      0.159        
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2017_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2018_subset$dcell))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2019_subset$dcell))
    
    #Q15  Dc-DL       Dc-Ta      Dc-Prec
    #17   0.798***    -0.376     -0.22
    #18   0.239       0.027      0.638**      
    #19   -0.6**     0.62**      0.115   
    
  }
  
  #single tree against gam_EZ
  {
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2017_subset$prec, "Dc" = gpp_growth_pine_tree_P16_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P16_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P16_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P16_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P16_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P16_2019_subset$gam_EZ))
    
    
    #P16 Dc-DL      Dc-Ta        Dc-Prec
    #17  0.94***    0.11         -0.01           
    #18  0.99***    -0.47*       0.21
    #19  0.87***    -0.83***     -0.098
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2017_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2018_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P20_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P20_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P20_2019_subset$prec, "Dc" = gpp_growth_pine_tree_P20_2019_subset$gam_EZ))
    
    #P20 Dc-DL       Dc-Ta      Dc-Prec
    #17  0.73***     -0.63**    0.093     
    #18  0.87***     -0.75***   -0.48*
    #19  0.83***     -0.62**    -0.089      
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P21_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P21_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P21_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P21_2019_subset$gam_EZ))
    
    #P21 Dc-DL       Dc-Ta        Dc-Prec
    #17  0.84***     -0.54*       0.13
    #18  0.48        -0.74*       0.01
    #19  0.91***     -0.42        -0.2
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P25_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P25_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P25_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P25_2019_subset$gam_EZ))
    
    #P25 Dc-DL       Dc-Ta      Dc-Prec
    #17  0.85***     -0.027     -0.13
    #18  0.92***     -0.32      0.059
    #19  0.88***     -0.51*     -0.16
    
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P26_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P26_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P26_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P26_2019_subset$gam_EZ))
    
    #P26 Dc-DL       Dc-Ta        Dc-Prec
    #17  0.96***     0.57*        -0.06
    #18  0.95***     -0.83***     -0.14 
    #19  0.84***     0.54*        0.23
    
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2017_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2017_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2017_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2018_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2018_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2018_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_pine_tree_P7_2019_subset$DL,"Ta" = gpp_growth_pine_tree_P7_2019_subset$ta,"Prec" = gpp_growth_pine_tree_P7_2019_subset$prec,"Dc" = gpp_growth_pine_tree_P7_2019_subset$gam_EZ))
    
    #P7  Dc-DL       Dc-Ta         Dc-Prec
    #17  0.86***     0.19          0.002
    #18  0.96***     -0.76***      -0.15
    #19  0.48*       -0.587*       0.089
    
    
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2017_subset$prec, "Dc" = gpp_growth_maple_tree_A18_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A18_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A18_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A18_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A18_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A18_2019_subset$gam_EZ))
    
    #A18 Dc-DL       Dc-Ta       Dc-Prec
    #17  0.87**      -0.527      -0.053      
    #18  0.78*      -0.82**     -0.193
    #19  -0.97***    0.38        0.54
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A19_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A19_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A19_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A19_2019_subset$gam_EZ))
    
    #A19  Dc-DL       Dc-Ta       Dc-Prec
    #17   0.83**      -0.669*     0.07
    #18   -0.26        0.14       -0.18
    #19   0.95       -0.89      -0.16
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A23_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A23_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A23_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A23_2019_subset$gam_EZ))
    
    #A23  Dc-DL       Dc-Ta         Dc-Prec
    #17  0.59         -0.2          0.05
    #18  
    #19  -0.467       -0.88**       0.21        
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A28_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A28_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A28_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A28_2019_subset$gam_EZ))
    
    #A28  Dc-DL       Dc-Ta      Dc-Prec
    #17   0.38        -0.68*     0.06
    #18   0.389       -0.82***   -0.33 
    #19  
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A29_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A29_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A29_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A29_2019_subset$gam_EZ))
    
    #A29  Dc-DL       Dc-Ta      Dc-Prec
    #17   0.64*       -0.166     -0.017
    #18   0.76*       -0.1       -0.33
    #19   0.57        0.14       0.01
    
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2017_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2017_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2017_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2018_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2018_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2018_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_tree_A8_2019_subset$DL,"Ta" = gpp_growth_maple_tree_A8_2019_subset$ta,"Prep" = gpp_growth_maple_tree_A8_2019_subset$prec,"Dc" = gpp_growth_maple_tree_A8_2019_subset$gam_EZ))
    
    #A8  Dc-DL       Dc-Ta      Dc-Prec
    #17  0.93***     -0.16      0.25
    #18  0.68*       -0.078     -0.17
    #19  0.57        -0.86**    -0.54
    
    pcor(data.frame("DL" = gpp_growth_maple_2017_subset$DL,"Ta" = gpp_growth_maple_2017_subset$ta,"Prep" = gpp_growth_maple_2017_subset$prec,"Dc" = gpp_growth_maple_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_2018_subset$DL,"Ta" = gpp_growth_maple_2018_subset$ta,"Prep" = gpp_growth_maple_2018_subset$prec,"Dc" = gpp_growth_maple_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_maple_2019_subset$DL,"Ta" = gpp_growth_maple_2019_subset$ta,"Prep" = gpp_growth_maple_2019_subset$prec,"Dc" = gpp_growth_maple_2019_subset$gam_EZ))  
    pcor(data.frame("DL" = maple_17_19_subset$DL,"Ta" = maple_17_19_subset$ta,"Prep" = maple_17_19_subset$prec,"Dc" = maple_17_19_subset$gam_EZ))    
    
    
    
    
    
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q05_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q05_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q05_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q05_2019_subset$gam_EZ))
    
    #Q05  Dc-DL       Dc-Ta      Dc-Prec
    #17  -0.71**      -0.18      -0.1
    #18  
    #19  -0.24        -0.43      0.5*
    
    
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q06_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q06_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q06_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q06_2019_subset$gam_EZ))
    
    #Q06  Dc-DL       Dc-Ta       Dc-Prec
    #17  -0.78***     -0.28       -0.37
    #18  -0.61        -0.53       -0.16      
    #19  -0.66*       -0.86***    -0.1
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q09_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q09_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q09_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q09_2019_subset$gam_EZ))
    
    #Q09  Dc-DL       Dc-Ta     Dc-Prec
    #17   -0.91***    -0.61*    0.2    
    #18   -0.6*       -0.82***  -0.2      
    #19   -0.11       -0.55*    0.42     
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q10_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q10_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q10_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q10_2019_subset$gam_EZ))
    
    #Q10  Dc-DL       Dc-Ta        Dc-Prec
    #17   -0.87***    -0.43        -0.26
    #18   -0.15       -0.64**      -0.33      
    #19   0.25        -0.63**      0.2
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q11_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q11_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q11_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q11_2019_subset$gam_EZ))
    
    #Q11  Dc-DL       Dc-Ta       Dc-Prec
    #17   -0.74**     -0.58       0.13
    #18   -0.66**     -0.71**     -0.17
    #19   -0.86***    -0.46       0.029
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q12_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q12_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q12_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q12_2019_subset$gam_EZ))
    
    #Q12  Dc-DL       Dc-Ta      Dc-Prec
    #17   -0.45      -0.77**     0.21
    #18   0.24       -0.8***     -0.28        
    #19      
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q13_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q13_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q13_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q13_2019_subset$gam_EZ))
    
    #Q13  Dc-DL       Dc-Ta        Dc-Prec
    #17   -0.48       -0.76***     -0.09
    #18   -0.13       -0.85***     -0.28
    #19   0.41        -0.46         0.32        
    
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2017_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2017_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2017_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2018_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2018_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2018_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_tree_Q15_2019_subset$DL,"Ta" = gpp_growth_oak_tree_Q15_2019_subset$ta,"Prep" = gpp_growth_oak_tree_Q15_2019_subset$prec,"Dc" = gpp_growth_oak_tree_Q15_2019_subset$gam_EZ))
    
    #Q15  Dc-DL       Dc-Ta      Dc-Prec
    #17   0.11        0.34       -0.35
    #18   -0.74***    -0.85***   -0.087      
    #19   0.73***     -0.96***   0.13  
    
    pcor(data.frame("DL" = gpp_growth_oak_2017_subset$DL,"Ta" = gpp_growth_oak_2017_subset$ta,"Prep" = gpp_growth_oak_2017_subset$prec,"Dc" = gpp_growth_oak_2017_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_2018_subset$DL,"Ta" = gpp_growth_oak_2018_subset$ta,"Prep" = gpp_growth_oak_2018_subset$prec,"Dc" = gpp_growth_oak_2018_subset$gam_EZ))
    pcor(data.frame("DL" = gpp_growth_oak_2019_subset$DL,"Ta" = gpp_growth_oak_2019_subset$ta,"Prep" = gpp_growth_oak_2019_subset$prec,"Dc" = gpp_growth_oak_2019_subset$gam_EZ))  
    pcor(data.frame("DL" = oak_17_19_subset$DL,"Ta" = oak_17_19_subset$ta,"Prep" = oak_17_19_subset$prec,"Dc" = oak_17_19_subset$gam_EZ))    
    
    
  }  
  
  #maple variables & ccf outputs
  {
    dl_maple_2017 = maple_2017_subset$DL
    ta_maple_2017 = maple_2017_subset$ta
    prep_maple_2017 = maple_2017_subset$prec
    part_maple_2017 = maple_2017_subset$part
    gpp1_maple_2017 = maple_2017_subset$gpp_ww
    dcell_maple_2017 = maple_2017_subset$gam_dcell_mean
    dgam_ez_maple_2017 = maple_2017_subset$dgam_EZ_mean
    dgam_wz_maple_2017 = maple_2017_subset$dgam_WZ_mean
    dgam_mz_maple_2017 = maple_2017_subset$dgam_MZ_mean
    
    dl_maple_2018 = maple_2018_subset$DL
    ta_maple_2018 = maple_2018_subset$ta
    prep_maple_2018 = maple_2018_subset$prec
    part_maple_2018 = maple_2018_subset$part
    gpp1_maple_2018 = maple_2018_subset$gpp_ww
    dcell_maple_2018 = maple_2018_subset$gam_dcell_mean
    dgam_ez_maple_2018 = maple_2018_subset$dgam_EZ_mean
    dgam_wz_maple_2018 = maple_2018_subset$dgam_WZ_mean
    dgam_mz_maple_2018 = maple_2018_subset$dgam_MZ_mean
    
    dl_maple_2019 = maple_2019_subset$DL
    ta_maple_2019 = maple_2019_subset$ta
    prep_maple_2019 = maple_2019_subset$prec
    part_maple_2019 = maple_2019_subset$part
    gpp1_maple_2019 = maple_2019_subset$gpp_ww
    dcell_maple_2019 = maple_2019_subset$gam_dcell_mean
    dgam_ez_maple_2019 = maple_2019_subset$dgam_EZ_mean
    dgam_wz_maple_2019 = maple_2019_subset$dgam_WZ_mean
    dgam_mz_maple_2019 = maple_2019_subset$dgam_MZ_mean 
    
    ccf_dl_dcell_maple_2017 = ccf(dl_maple_2017,dcell_maple_2017,8)
    ccf_dl_dcell_maple_2018 = ccf(dl_maple_2018,dcell_maple_2018,8)
    ccf_dl_dcell_maple_2019 = ccf(dl_maple_2019,dcell_maple_2019,8)
    
    ccf_dl_dcell_maple_high_2017 = ccf(dl_maple_2017,dcell_maple_high_2017,8)
    ccf_dl_dcell_maple_high_2018 = ccf(dl_maple_2018,dcell_maple_high_2018,8)
    ccf_dl_dcell_maple_high_2019 = ccf(dl_maple_2019,dcell_maple_high_2019,8)
    
    ccf_dl_dcell_maple_low_2017 = ccf(dl_maple_2017,dcell_maple_low_2017,8)
    ccf_dl_dcell_maple_low_2018 = ccf(dl_maple_2018,dcell_maple_low_2018,8)
    ccf_dl_dcell_maple_low_2019 = ccf(dl_maple_2019,dcell_maple_low_2019,8)
    
    ccf_ta_dcell_maple_2017 = ccf(ta_maple_2017,dcell_maple_2017,8)
    ccf_ta_dcell_maple_2018 = ccf(ta_maple_2018,dcell_maple_2018,8)
    ccf_ta_dcell_maple_2019 = ccf(ta_maple_2019,dcell_maple_2019,8)
    
    ccf_ta_dcell_maple_high_2017 = ccf(ta_maple_2017,dcell_maple_high_2017,8)
    ccf_ta_dcell_maple_high_2018 = ccf(ta_maple_2018,dcell_maple_high_2018,8)
    ccf_ta_dcell_maple_high_2019 = ccf(ta_maple_2019,dcell_maple_high_2019,8)
    
    ccf_ta_dcell_maple_low_2017 = ccf(ta_maple_2017,dcell_maple_low_2017,8)
    ccf_ta_dcell_maple_low_2018 = ccf(ta_maple_2018,dcell_maple_low_2018,8)
    ccf_ta_dcell_maple_low_2019 = ccf(ta_maple_2019,dcell_maple_low_2019,8)
    
    ccf_prep_dcell_maple_2017 = ccf(prep_maple_2017,dcell_maple_2017,8)
    ccf_prep_dcell_maple_2018 = ccf(prep_maple_2018,dcell_maple_2018,8)
    ccf_prep_dcell_maple_2019 = ccf(prep_maple_2019,dcell_maple_2019,8)  
    
    ccf_part_dcell_maple_2017 = ccf(part_maple_2017,dcell_maple_2017,8)
    ccf_part_dcell_maple_2018 = ccf(part_maple_2018,dcell_maple_2018,8)
    ccf_part_dcell_maple_2019 = ccf(part_maple_2019,dcell_maple_2019,8)
    
    ccf_gpp_dcell_maple_2017 = ccf(gpp1_maple_2017,dcell_maple_2017,8)
    ccf_gpp_dcell_maple_2018 = ccf(gpp1_maple_2018,dcell_maple_2018,8)
    ccf_gpp_dcell_maple_2019 = ccf(gpp1_maple_2019,dcell_maple_2019,8)
    
    ccf_maple_df = data.frame("lag_ww" = c(-8:8),"ccf_dl_dcell_2017" = ccf_dl_dcell_maple_2017$acf,"ccf_dl_dcell_2018" = ccf_dl_dcell_maple_2018$acf,"ccf_dl_dcell_2019" = ccf_dl_dcell_maple_2019$acf,"ccf_ta_dcell_2017" = ccf_ta_dcell_maple_2017$acf,"ccf_ta_dcell_2018" = ccf_ta_dcell_maple_2018$acf,"ccf_ta_dcell_2019" = ccf_ta_dcell_maple_2019$acf,"ccf_prep_dcell_2017" = ccf_prep_dcell_maple_2017$acf,"ccf_prep_dcell_2018" = ccf_prep_dcell_maple_2018$acf,"ccf_prep_dcell_2019" = ccf_prep_dcell_maple_2019$acf,"ccf_part_dcell_2017" = ccf_part_dcell_maple_2017$acf,"ccf_part_dcell_2018" = ccf_part_dcell_maple_2018$acf,"ccf_part_dcell_2019" = ccf_part_dcell_maple_2019$acf,"ccf_gpp_dcell_2017" = ccf_gpp_dcell_maple_2017$acf,"ccf_gpp_dcell_2018" = ccf_gpp_dcell_maple_2018$acf,"ccf_gpp_dcell_2019" = ccf_gpp_dcell_maple_2019$acf)
  }
  
  #oak variables & ccf outputs
  {
    dl_oak_2017 = oak_2017_subset$DL
    ta_oak_2017 = oak_2017_subset$ta
    prep_oak_2017 = oak_2017_subset$prec
    part_oak_2017 = oak_2017_subset$part
    gpp1_oak_2017 = oak_2017_subset$gpp_ww
    dcell_oak_2017 = oak_2017_subset$gam_dcell_mean
    dgam_ez_oak_2017 = oak_2017_subset$dgam_EZ_mean
    dgam_wz_oak_2017 = oak_2017_subset$dgam_WZ_mean
    dgam_mz_oak_2017 = oak_2017_subset$dgam_MZ_mean
    
    dl_oak_2018 = oak_2018_subset$DL
    ta_oak_2018 = oak_2018_subset$ta
    prep_oak_2018 = oak_2018_subset$prec
    part_oak_2018 = oak_2018_subset$part
    gpp1_oak_2018 = oak_2018_subset$gpp_ww
    dcell_oak_2018 = oak_2018_subset$gam_dcell_mean
    dgam_ez_oak_2018 = oak_2018_subset$dgam_EZ_mean
    dgam_wz_oak_2018 = oak_2018_subset$dgam_WZ_mean
    dgam_mz_oak_2018 = oak_2018_subset$dgam_MZ_mean
    
    dl_oak_2019 = oak_2019_subset$DL
    ta_oak_2019 = oak_2019_subset$ta
    prep_oak_2019 = oak_2019_subset$prec
    part_oak_2019 = oak_2019_subset$part
    gpp1_oak_2019 = oak_2019_subset$gpp_ww
    dcell_oak_2019 = oak_2019_subset$gam_dcell_mean
    dgam_ez_oak_2019 = oak_2019_subset$dgam_EZ_mean
    dgam_wz_oak_2019 = oak_2019_subset$dgam_WZ_mean
    dgam_mz_oak_2019 = oak_2019_subset$dgam_MZ_mean 
    
    ccf_dl_dcell_oak_2017 = ccf(dl_oak_2017,dcell_oak_2017,8)
    ccf_dl_dcell_oak_2018 = ccf(dl_oak_2018,dcell_oak_2018,8)
    ccf_dl_dcell_oak_2019 = ccf(dl_oak_2019,dcell_oak_2019,8)
    
    ccf_ta_dcell_oak_2017 = ccf(ta_oak_2017,dcell_oak_2017,8)
    ccf_ta_dcell_oak_2018 = ccf(ta_oak_2018,dcell_oak_2018,8)
    ccf_ta_dcell_oak_2019 = ccf(ta_oak_2019,dcell_oak_2019,8)
    
    ccf_prep_dcell_oak_2017 = ccf(prep_oak_2017,dcell_oak_2017,8)
    ccf_prep_dcell_oak_2018 = ccf(prep_oak_2018,dcell_oak_2018,8)
    ccf_prep_dcell_oak_2019 = ccf(prep_oak_2019,dcell_oak_2019,8)  
    
    ccf_part_dcell_oak_2017 = ccf(part_oak_2017,dcell_oak_2017,8)
    ccf_part_dcell_oak_2018 = ccf(part_oak_2018,dcell_oak_2018,8)
    ccf_part_dcell_oak_2019 = ccf(part_oak_2019,dcell_oak_2019,8)
    
    ccf_gpp_dcell_oak_2017 = ccf(gpp1_oak_2017,dcell_oak_2017,8)
    ccf_gpp_dcell_oak_2018 = ccf(gpp1_oak_2018,dcell_oak_2018,8)
    ccf_gpp_dcell_oak_2019 = ccf(gpp1_oak_2019,dcell_oak_2019,8)
    
    
    ccf_oak_df = data.frame("lag_ww" = c(-8:8),"ccf_dl_dcell_2017" = ccf_dl_dcell_oak_2017$acf,"ccf_dl_dcell_2018" = ccf_dl_dcell_oak_2018$acf,"ccf_dl_dcell_2019" = ccf_dl_dcell_oak_2019$acf,"ccf_ta_dcell_2017" = ccf_ta_dcell_oak_2017$acf,"ccf_ta_dcell_2018" = ccf_ta_dcell_oak_2018$acf,"ccf_ta_dcell_2019" = ccf_ta_dcell_oak_2019$acf,"ccf_prep_dcell_2017" = ccf_prep_dcell_oak_2017$acf,"ccf_prep_dcell_2018" = ccf_prep_dcell_oak_2018$acf,"ccf_prep_dcell_2019" = ccf_prep_dcell_oak_2019$acf,"ccf_part_dcell_2017" = ccf_part_dcell_oak_2017$acf,"ccf_part_dcell_2018" = ccf_part_dcell_oak_2018$acf,"ccf_part_dcell_2019" = ccf_part_dcell_oak_2019$acf,"ccf_gpp_dcell_2017" = ccf_gpp_dcell_oak_2017$acf,"ccf_gpp_dcell_2018" = ccf_gpp_dcell_oak_2018$acf,"ccf_gpp_dcell_2019" = ccf_gpp_dcell_oak_2019$acf)
  }
  
  
}

#GDD&CDD accumulation
temp_gdd_dd_17_19 = read.csv("temp_dd_17_19_GDD.csv")
temp_cdd_dd_17_19 = read.csv("temp_dd_17_19_CDD.csv")

#GDD
{
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_GDD_0.pdf",width =12,height = 5)
  p_GDD_0 <- ggplot()+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.0_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.0_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.0_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("GDD_0")+
    xlim(0,250)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_GDD_0
  dev.off()
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_GDD_2.5.pdf",width =12,height = 5)
  
  p_GDD_2.5 <- ggplot()+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.2.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.2.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.2.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("GDD_2.5")+
    xlim(0,250)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_GDD_2.5
  dev.off()      
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_GDD_5.pdf",width =12,height = 5)
  
  p_GDD_5 <- ggplot()+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("GDD_5")+
    xlim(0,250)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_GDD_5
  dev.off()    
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_GDD_7.5.pdf",width =12,height = 5)  
  
  p_GDD_7.5 <- ggplot()+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.7.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.7.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.7.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("GDD_7.5")+
    xlim(0,250)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_GDD_7.5
  dev.off()    
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_GDD_10.pdf",width =12,height = 5)  
  
  p_GDD_10 <- ggplot()+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.10_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.10_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_gdd_dd_17_19,aes(x=DY,y=X.10_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("GDD_10")+
    xlim(0,250)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_GDD_10
  dev.off()   
}

#CDD
{
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_CDD_0.pdf",width =12,height = 5)
  p_CDD_0 <- ggplot()+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.0_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.0_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.0_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("CDD_0")+
    xlim(200,400)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_CDD_0
  dev.off()
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_CDD_2.5.pdf",width =12,height = 5)
  
  p_CDD_2.5 <- ggplot()+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.2.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.2.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.2.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("CDD_2.5")+
    xlim(200,400)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_CDD_2.5
  dev.off()      
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_CDD_5.pdf",width =12,height = 5)
  
  p_CDD_5 <- ggplot()+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("CDD_5")+
    xlim(200,400)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_CDD_5
  dev.off()    
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_CDD_7.5.pdf",width =12,height = 5)  
  
  p_CDD_7.5 <- ggplot()+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.7.5_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.7.5_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.7.5_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("CDD_7.5")+
    xlim(200,400)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_CDD_7.5
  dev.off()    
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\p_CDD_10.pdf",width =12,height = 5)  
  
  p_CDD_10 <- ggplot()+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.10_2017_acc),color = "blue",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.10_2018_acc),color = "darkgreen",size = 1)+
    geom_line(data = temp_cdd_dd_17_19,aes(x=DY,y=X.10_2019_acc),color = "darkred",size = 1)+
    xlab("DY")+
    ylab("CDD_10")+
    xlim(200,400)+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_CDD_10
  dev.off()   
}

#GPP 16-18 vs QURU&ACRU TRW 16-18 from past year obs.
#TRW info is from the xylogenesis obs. of previous year
{
  gpp_QURU_ACRU_p_16_18 = read.csv("GPP_TRW_QURU_ACRU_p_16_18.csv")
  
  sd_perc_QURU = gpp_QURU_ACRU_p_16_18$TRW_QURU_sd_p / gpp_QURU_ACRU_p_16_18$TRW_QURU_mean_p
  sd_perc_ACRU = gpp_QURU_ACRU_p_16_18$TRW_ACRU_sd_p / gpp_QURU_ACRU_p_16_18$TRW_ACRU_mean_p
  
  
  gpp_yy_scaled_p = scale(gpp_QURU_ACRU_p_16_18$GPP)
  TRW_QURU_p_scaled = scale(gpp_QURU_ACRU_p_16_18$TRW_QURU_mean_p) 
  TRW_ACRU_p_scaled = scale(gpp_QURU_ACRU_p_16_18$TRW_ACRU_mean_p)
  TRW_QURU_p_sd_scaled = abs(TRW_QURU_p_scaled * sd_perc_QURU)
  TRW_ACRU_p_sd_scaled = abs(TRW_ACRU_p_scaled * sd_perc_ACRU)
  
  yy_gpp_TRW_p = c(2016:2018)
  
  gpp_yy_TRW_scaled_p = data.frame(year = yy_gpp_TRW_p,gpp_scaled = gpp_yy_scaled_p,QURU_scaled = TRW_QURU_p_scaled,ACRU_scaled = TRW_ACRU_p_scaled,QURU_sd_scaled = TRW_QURU_p_sd_scaled,ACRU_sd_scaled = TRW_ACRU_p_sd_scaled)
  
  library("ggplot2")
  p1 <- ggplot()+
    #geom_ribbon(data = gpp_yy_TRW_scaled_p,aes(x=year,ymin= (gpp_yy_TRW_scaled_p$QURU_scaled - gpp_yy_TRW_scaled_p$QURU_sd_scaled), 
    #                                             ymax= (gpp_yy_TRW_scaled_p$QURU_scaled - gpp_yy_TRW_scaled_p$QURU_sd_scaled)),alpha = 0.4)+
    #geom_ribbon(data = gpp_yy_TRW_scaled_p,aes(x=Year,ymin= min, 
    #                                              ymax= max),fill = "pink",alpha = 0.4)+
    geom_line(data = gpp_yy_TRW_scaled_p,aes(x=year, 
                                             y=gpp_scaled,color = "a"),linetype="solid",size =1)+
    geom_line(data = gpp_yy_TRW_scaled_p,aes(x=year, 
                                             y=QURU_scaled,color = "b"),linetype="solid",size =1)+
    geom_line(data = gpp_yy_TRW_scaled_p,aes(x=year,y= ACRU_scaled,color = "c"),linetype="solid",size = 1)+
    #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("z-score")+
    xlab("Year")+
    ylim(-2,2)+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("a"="black","b" = "orange","c" = "green"), labels = c('GPP','TRW_QURU','TRW_ACRU'))+
    scale_x_continuous(breaks=seq(2016, 2018, 1))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.15,0.85),legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  
}

#daily gpp output
{
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\GPP_dd.pdf",width =10,height = 5)  
  p_gpp_d <- ggplot()+
    geom_line(data = gpp_17,aes(x=DY, y=gam_gpp_mean,color="a"),size = 1)+
    geom_line(data = gpp_18,aes(x=DY, y=gam_gpp_mean,color="b"),size = 1)+
    geom_line(data = gpp_19,aes(x=DY, y=gam_gpp_mean,color="c"),size = 1)+
    theme_set(theme_bw())+
    xlim(80,320)+
    xlab("DY")+
    ylab(expression(paste("GPP/ gC/",m^{2},"")))+
    scale_color_manual(name = '', 
                       values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "none",legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12)) 
  p_gpp_d
  dev.off()
}

#GPP accumulation
{
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\GPP_dd_accu.pdf",width =10,height = 5)  
  
  p_gpp <- ggplot() + 
    geom_line(data = gpp_growth_maple_tree_A18_2017,aes(x=DY, y=gpp_dd_acc,color="a"),size = 1)+
    geom_line(data = gpp_growth_maple_tree_A18_2018,aes(x=DY, y=gpp_dd_acc,color="b"),size = 1)+
    geom_line(data = gpp_growth_maple_tree_A18_2019,aes(x=DY, y=gpp_dd_acc,color="c"),size = 1)+
    xlim(80,320)+
    ylim(-50,2000)+
    ylab(expression(paste("GPP/ gC/",m^{2},"")))+
    scale_color_manual(name = '', 
                       values =c("a"="blue","b" = "darkgreen","c" = "darkred"), labels = c('2017','2018',"2019"))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "none",legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p_gpp
  dev.off() 
  
}

#Mix effect model to test the contributions of daylength, temperature and precipitation
{
  
  #maple
  {
    HF2017_maple_mean.sdd_filled1_A18_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A18_com.csv")
    HF2017_maple_mean.sdd_filled1_A19_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A19_com.csv")
    HF2017_maple_mean.sdd_filled1_A23_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A23_com.csv")
    HF2017_maple_mean.sdd_filled1_A28_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A28_com.csv")
    HF2017_maple_mean.sdd_filled1_A29_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A29_com.csv")
    HF2017_maple_mean.sdd_filled1_A8_com1 = read.csv("HF2017_maple_mean.sdd_filled1_A8_com.csv")
    
    
    HF2018_maple_mean.sdd_filled1_A18_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A18_com.csv")
    HF2018_maple_mean.sdd_filled1_A19_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A19_com.csv")
    HF2018_maple_mean.sdd_filled1_A23_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A23_com.csv")
    HF2018_maple_mean.sdd_filled1_A28_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A28_com.csv")
    HF2018_maple_mean.sdd_filled1_A29_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A29_com.csv")
    HF2018_maple_mean.sdd_filled1_A8_com1 = read.csv("HF2018_maple_mean.sdd_filled1_A8_com.csv")
    
    HF2019_maple_mean.sdd_filled1_A18_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A18_com.csv")
    HF2019_maple_mean.sdd_filled1_A19_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A19_com.csv")
    HF2019_maple_mean.sdd_filled1_A23_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A23_com.csv")
    HF2019_maple_mean.sdd_filled1_A28_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A28_com.csv")
    HF2019_maple_mean.sdd_filled1_A29_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A29_com.csv")
    HF2019_maple_mean.sdd_filled1_A8_com1 = read.csv("HF2019_maple_mean.sdd_filled1_A8_com.csv")
  }
  
  #oak
  {HF2017_oak_mean.sdd_filled1_Q15_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q15_com.csv")
    HF2017_oak_mean.sdd_filled1_Q05_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q05_com.csv")
    HF2017_oak_mean.sdd_filled1_Q06_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q06_com.csv")
    HF2017_oak_mean.sdd_filled1_Q09_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q09_com.csv")
    HF2017_oak_mean.sdd_filled1_Q10_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q10_com.csv")
    HF2017_oak_mean.sdd_filled1_Q11_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q11_com.csv")
    HF2017_oak_mean.sdd_filled1_Q12_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q12_com.csv")
    HF2017_oak_mean.sdd_filled1_Q13_com1 = read.csv("HF2017_oak_mean.sdd_filled1_Q13_com.csv")
    
    
    HF2018_oak_mean.sdd_filled1_Q15_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q15_com.csv")
    HF2018_oak_mean.sdd_filled1_Q05_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q05_com.csv")
    HF2018_oak_mean.sdd_filled1_Q06_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q06_com.csv")
    HF2018_oak_mean.sdd_filled1_Q09_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q09_com.csv")
    HF2018_oak_mean.sdd_filled1_Q10_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q10_com.csv")
    HF2018_oak_mean.sdd_filled1_Q11_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q11_com.csv")
    HF2018_oak_mean.sdd_filled1_Q12_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q12_com.csv")
    HF2018_oak_mean.sdd_filled1_Q13_com1 = read.csv("HF2018_oak_mean.sdd_filled1_Q13_com.csv")
    
    HF2019_oak_mean.sdd_filled1_Q15_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q15_com.csv")
    HF2019_oak_mean.sdd_filled1_Q05_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q05_com.csv")
    HF2019_oak_mean.sdd_filled1_Q06_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q06_com.csv")
    HF2019_oak_mean.sdd_filled1_Q09_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q09_com.csv")
    HF2019_oak_mean.sdd_filled1_Q10_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q10_com.csv")
    HF2019_oak_mean.sdd_filled1_Q11_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q11_com.csv")
    HF2019_oak_mean.sdd_filled1_Q12_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q12_com.csv")
    HF2019_oak_mean.sdd_filled1_Q13_com1 = read.csv("HF2019_oak_mean.sdd_filled1_Q13_com.csv")}
  
  #pine
  {HF2017_pine_mean.sdd_filled1_P16_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P16_com.csv")
    HF2017_pine_mean.sdd_filled1_P20_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P20_com.csv")
    HF2017_pine_mean.sdd_filled1_P21_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P21_com.csv")
    HF2017_pine_mean.sdd_filled1_P25_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P25_com.csv")
    HF2017_pine_mean.sdd_filled1_P26_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P26_com.csv")
    HF2017_pine_mean.sdd_filled1_P7_com1 = read.csv("HF2017_pine_mean.sdd_filled1_P7_com.csv")
    
    
    HF2018_pine_mean.sdd_filled1_P16_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P16_com.csv")
    HF2018_pine_mean.sdd_filled1_P20_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P20_com.csv")
    HF2018_pine_mean.sdd_filled1_P21_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P21_com.csv")
    HF2018_pine_mean.sdd_filled1_P25_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P25_com.csv")
    HF2018_pine_mean.sdd_filled1_P26_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P26_com.csv")
    HF2018_pine_mean.sdd_filled1_P7_com1 = read.csv("HF2018_pine_mean.sdd_filled1_P7_com.csv")
    
    HF2019_pine_mean.sdd_filled1_P16_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P16_com.csv")
    HF2019_pine_mean.sdd_filled1_P20_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P20_com.csv")
    HF2019_pine_mean.sdd_filled1_P21_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P21_com.csv")
    HF2019_pine_mean.sdd_filled1_P25_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P25_com.csv")
    HF2019_pine_mean.sdd_filled1_P26_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P26_com.csv")
    HF2019_pine_mean.sdd_filled1_P7_com1 = read.csv("HF2019_pine_mean.sdd_filled1_P7_com.csv")}
  
  
  
  HF2017_maple_com1 = rbind(HF2017_maple_mean.sdd_filled1_A18_com1,HF2017_maple_mean.sdd_filled1_A19_com1,HF2017_maple_mean.sdd_filled1_A23_com1,HF2017_maple_mean.sdd_filled1_A28_com1,HF2017_maple_mean.sdd_filled1_A29_com1,HF2017_maple_mean.sdd_filled1_A8_com1)  
  
  HF2018_maple_com1 = rbind(HF2018_maple_mean.sdd_filled1_A18_com1,HF2018_maple_mean.sdd_filled1_A19_com1,HF2018_maple_mean.sdd_filled1_A23_com1,HF2018_maple_mean.sdd_filled1_A28_com1,HF2018_maple_mean.sdd_filled1_A29_com1,HF2018_maple_mean.sdd_filled1_A8_com1)
  
  HF2019_maple_com1 = rbind(HF2019_maple_mean.sdd_filled1_A18_com1,HF2019_maple_mean.sdd_filled1_A19_com1,HF2019_maple_mean.sdd_filled1_A23_com1,HF2019_maple_mean.sdd_filled1_A28_com1,HF2019_maple_mean.sdd_filled1_A29_com1,HF2019_maple_mean.sdd_filled1_A8_com1)
  
  HF2017_pine_com1 = rbind(HF2017_pine_mean.sdd_filled1_P16_com1,HF2017_pine_mean.sdd_filled1_P20_com1,HF2017_pine_mean.sdd_filled1_P21_com1,HF2017_pine_mean.sdd_filled1_P25_com1,HF2017_pine_mean.sdd_filled1_P26_com1,HF2017_pine_mean.sdd_filled1_P7_com1)  
  
  HF2018_pine_com1 = rbind(HF2018_pine_mean.sdd_filled1_P16_com1,HF2018_pine_mean.sdd_filled1_P20_com1,HF2018_pine_mean.sdd_filled1_P21_com1,HF2018_pine_mean.sdd_filled1_P25_com1,HF2018_pine_mean.sdd_filled1_P26_com1,HF2018_pine_mean.sdd_filled1_P7_com1)
  
  HF2019_pine_com1 = rbind(HF2019_pine_mean.sdd_filled1_P16_com1,HF2019_pine_mean.sdd_filled1_P20_com1,HF2019_pine_mean.sdd_filled1_P21_com1,HF2019_pine_mean.sdd_filled1_P25_com1,HF2019_pine_mean.sdd_filled1_P26_com1,HF2019_pine_mean.sdd_filled1_P7_com1)   
  
  HF2017_oak_com1 = rbind(HF2017_oak_mean.sdd_filled1_Q05_com1,HF2017_oak_mean.sdd_filled1_Q06_com1,HF2017_oak_mean.sdd_filled1_Q09_com1,HF2017_oak_mean.sdd_filled1_Q10_com1,HF2017_oak_mean.sdd_filled1_Q11_com1,HF2017_oak_mean.sdd_filled1_Q12_com1,HF2017_oak_mean.sdd_filled1_Q13_com1,HF2017_oak_mean.sdd_filled1_Q15_com1)  
  
  HF2018_oak_com1 = rbind(HF2018_oak_mean.sdd_filled1_Q05_com1,HF2018_oak_mean.sdd_filled1_Q06_com1,HF2018_oak_mean.sdd_filled1_Q09_com1,HF2018_oak_mean.sdd_filled1_Q10_com1,HF2018_oak_mean.sdd_filled1_Q11_com1,HF2018_oak_mean.sdd_filled1_Q12_com1,HF2018_oak_mean.sdd_filled1_Q13_com1,HF2018_oak_mean.sdd_filled1_Q15_com1)
  
  HF2019_oak_com1 = rbind(HF2019_oak_mean.sdd_filled1_Q05_com1,HF2019_oak_mean.sdd_filled1_Q06_com1,HF2019_oak_mean.sdd_filled1_Q09_com1,HF2019_oak_mean.sdd_filled1_Q10_com1,HF2019_oak_mean.sdd_filled1_Q11_com1,HF2019_oak_mean.sdd_filled1_Q12_com1,HF2019_oak_mean.sdd_filled1_Q13_com1,HF2019_oak_mean.sdd_filled1_Q15_com1)  
  
  HF_maple_com1 = rbind(HF2017_maple_com1,HF2018_maple_com1,HF2019_maple_com1)
  HF_oak_com1 = rbind(HF2017_oak_com1,HF2018_oak_com1,HF2019_oak_com1)
  HF_pine_com1 = rbind(HF2017_pine_com1,HF2018_pine_com1,HF2019_pine_com1)
  
  
  #separate years  
  #maple
  HF_maple_com1$Year1 = c(rep(1,209),rep(2,149),rep(3,96))
  {
    mix_clim_m01 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ (1|Tree),REML = F)
    mix_clim_m0 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ Year1 + (1|Tree),REML = F) 
    mix_clim_m1 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL + (1|Tree),REML = F) 
    mix_clim_m2 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ ta + (1|Tree),REML = F) 
    mix_clim_m3 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ prec + (1|Tree),REML = F)
    mix_clim_m4 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * Year1 + (1|Tree),REML = F) 
    mix_clim_m5 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ ta * Year1 + (1|Tree),REML = F) 
    mix_clim_m6 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ prec* Year1 + (1|Tree),REML = F)
    mix_clim_m7 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_m8 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * Year1 + prec * Year1 + (1|Tree),REML = F)
    mix_clim_m9 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ prec * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_m10 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 +  prec * Year1 + (1|Tree),REML = F)
    mix_clim_m11 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * ta * Year1 + (1|Tree),REML = F)
    mix_clim_m12 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ prec * ta * Year1 + (1|Tree),REML = F)
    mix_clim_m13 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * prec * Year1 + (1|Tree),REML = F)
    mix_clim_m14 = lmerTest::lmer(data = HF_maple_com1,formula = gam_EZ ~ DL * ta *  prec * Year1 + (1|Tree),REML = F)   
    
    anova(mix_clim_m01,mix_clim_m0,mix_clim_m1,mix_clim_m2,mix_clim_m3,mix_clim_m4,mix_clim_m5,mix_clim_m6,mix_clim_m7,mix_clim_m8,mix_clim_m9,mix_clim_m10,mix_clim_m11,mix_clim_m12,mix_clim_m13,mix_clim_m14)
  }
  
  #oak
  
  HF_oak_com1$Year1 = c(rep(1,279),rep(2,245),rep(3,187))
  {
    mix_clim_o01 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ (1|Tree),REML = F) 
    mix_clim_o0 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ Year1 + (1|Tree),REML = F) 
    mix_clim_o1 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL + (1|Tree),REML = F) 
    mix_clim_o2 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ ta + (1|Tree),REML = F) 
    mix_clim_o3 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ prec + (1|Tree),REML = F)
    mix_clim_o4 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * Year1 + (1|Tree),REML = F) 
    mix_clim_o5 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ ta * Year1 + (1|Tree),REML = F) 
    mix_clim_o6 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ prec* Year1 + (1|Tree),REML = F)
    mix_clim_o7 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_o8 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * Year1 + prec * Year1 + (1|Tree),REML = F)
    mix_clim_o9 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ prec * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_o10 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 +  prec * Year1 + (1|Tree),REML = F)
    mix_clim_o11 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * ta * Year1 + (1|Tree),REML = F)
    mix_clim_o12 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ prec * ta * Year1 + (1|Tree),REML = F)
    mix_clim_o13 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * prec * Year1 + (1|Tree),REML = F)
    mix_clim_o14 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * ta *  prec * Year1 + (1|Tree),REML = F)   
    mix_clim_o15 = lmerTest::lmer(data = HF_oak_com1,formula = gam_EZ ~ DL * ta *  prec + (1|Tree),REML = F)
    anova(mix_clim_o01,mix_clim_o0,mix_clim_o1,mix_clim_o2,mix_clim_o3,mix_clim_o4,mix_clim_o5,mix_clim_o6,mix_clim_o7,mix_clim_o8,mix_clim_o9,mix_clim_o10,mix_clim_o11,mix_clim_o12,mix_clim_o13,mix_clim_o14)
  }
  
  #HF_pine_com1$Year = c(rep(2017,208),rep(2018,174),rep(2019,161))
  #HF_pine_com1$Year1 = c(rep(1,208),rep(2,174),rep(3,161))
  
  #pine
  {
    mix_clim_p01 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ (1|Tree),REML = F) 
    mix_clim_p0 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ Year1 + (1|Tree),REML = F) 
    mix_clim_p1 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL + (1|Tree),REML = F) 
    mix_clim_p2 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ ta + (1|Tree),REML = F) 
    mix_clim_p3 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ prec + (1|Tree),REML = F)
    mix_clim_p4 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * Year1 + (1|Tree),REML = F) 
    mix_clim_p5 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ ta * Year1 + (1|Tree),REML = F) 
    mix_clim_p6 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ prec* Year1 + (1|Tree),REML = F)
    mix_clim_p7 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_p8 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * Year1 + prec * Year1 + (1|Tree),REML = F)
    mix_clim_p9 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ prec * Year1 + ta * Year1 + (1|Tree),REML = F)
    mix_clim_p10 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * Year1 + ta * Year1 +  prec * Year1 + (1|Tree),REML = F)
    mix_clim_p11 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * ta * Year1 + (1|Tree),REML = F)
    mix_clim_p12 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ prec * ta * Year1 + (1|Tree),REML = F)
    mix_clim_p13 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * prec * Year1 + (1|Tree),REML = F)
    mix_clim_p14 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * ta *  prec * Year1 + (1|Tree),REML = F)   
    mix_clim_p15 = lmerTest::lmer(data = HF_pine_com1,formula = gam_EZ ~ DL * ta *  prec + (1|Tree),REML = F)
    anova(mix_clim_p01, mix_clim_p0,mix_clim_p1,mix_clim_p2,mix_clim_p3,mix_clim_p4,mix_clim_p5,mix_clim_p6,mix_clim_p7,mix_clim_p8,mix_clim_p9,mix_clim_p10,mix_clim_p11,mix_clim_p12,mix_clim_p13,mix_clim_p14)
  }
  
  #create an overall df
  HF_pine_com1 = HF_pine_com1[ , -which(colnames(HF_pine_com1) %in% c("dgam_CZ_mean","dgam_EZ_mean","dgam_WZ_mean","dgam_MZ_mean"))]
  HF_com1 = rbind(HF_maple_com1,HF_oak_com1,HF_pine_com1)
  
  #mixed effect models
  {
    mix_clim0 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ (1|Tree),REML = F) 
    mix_clim1 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ Species + (1|Tree),REML = F) 
    mix_clim2 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ Year1 * Species + (1|Tree),REML = F) 
    mix_clim3 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL + (1|Tree),REML = F) 
    mix_clim4 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ ta + (1|Tree),REML = F)
    mix_clim5 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * Year1 + (1|Tree),REML = F) 
    mix_clim6 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ ta * Year1 + (1|Tree),REML = F)
    mix_clim7 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * Species + (1|Tree),REML = F) 
    mix_clim8 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ ta * Species + (1|Tree),REML = F)
    mix_clim9 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * Species * Year1 + (1|Tree),REML = F) 
    mix_clim10 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ ta * Species * Year1 + (1|Tree),REML = F)
    mix_clim11 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * Year1 + Year1 * ta + (1|Tree),REML = F)
    mix_clim12 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * Species + ta * Species + (1|Tree))
    mix_clim13 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * ta * Species * Year1 + (1|Tree),REML = F)
    mix_clim14 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * ta + (1|Tree),REML = F)
    mix_clim15 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * ta * Year1 + (1|Tree),REML = F)
    mix_clim16 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ DL * ta * Species + (1|Tree),REML = F)
    
    anova(mix_clim0,mix_clim1,mix_clim2,mix_clim3,mix_clim4,mix_clim5,mix_clim6,mix_clim7,mix_clim8,mix_clim9,mix_clim10,mix_clim11,mix_clim12,mix_clim13,mix_clim14,mix_clim15,mix_clim16)
    
    
    
    
    mix_clim9 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ prec + (1|Tree),REML = F) 
    mix_clim10 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ prec * Species + (1|Tree),REML = F) 
    mix_clim11 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ prec * Year + (1|Tree),REML = F) 
    mix_clim12 = lmerTest::lmer(data = HF_com1,formula = gam_EZ ~ prec * Year * Species + (1|Tree),REML = F)
    
    
    
    mix_test_m2 = lme(gam_EZ ~ DL + Year,random = ~1|Tree,data = HF_com1)
    mix_test_m3 = lme(gam_EZ ~ prec  + Year,random = ~1|Tree,data = HF_com1)
    mix_test_m4 = lme(gam_EZ ~ ta + prec,random = ~1|Tree,data = HF_com1)
    mix_test_m5 = lme(gam_EZ ~ ta + DL,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m6 = lme(gam_EZ ~ DL + prec,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m7 = lme(gam_EZ ~ ta * prec,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m8 = lme(gam_EZ ~ ta * DL,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m9 = lme(gam_EZ ~ DL * prec,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m10 = lme(gam_EZ ~ ta + prec + DL + Year,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m10_1 = lme(gam_EZ ~ ta + prec + DL,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m11 = lme(gam_EZ ~ ta * prec + DL,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m12 = lme(gam_EZ ~ ta + prec * DL,random = ~1|Tree,data = HF_maple_com1)
    mix_test_m13 = lme(gam_EZ ~ ta * prec * DL,random = ~1|Tree,data = HF_maple_com1)
    anova(mix_test_m1,mix_test_m2,mix_test_m3,mix_test_m4,mix_test_m5,mix_test_m6,mix_test_m7,mix_test_m8,mix_test_m9,mix_test_m10,mix_test_m11,mix_test_m12,mix_test_m13)
    
  }
  
}

#individual cE plot
{
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\QURU_cE_individual_2021_6_19.pdf",width =12,height = 4)
  
  p_cE_oak <- ggplot()+
    geom_line(data = Pheno_OAK_df,aes(x=Year, 
                                      y=cE,group = Tree),linetype="solid",size =1,color = "#2e4057")+
    facet_grid(~ Tree, scales = "free_x")+
    scale_x_continuous(limits = c(2016.5,2019.5),breaks = c(2017,2018,2019),label=c("2017","2018","2019"))+
    ylab("cE_date")+
    theme(axis.text = element_text(size =12))+
    theme(axis.text.x = element_text(size =12))+
    theme(axis.text.y = element_text(size =12))+
    theme(legend.position = "bottom")  
  
  p_cE_oak
  dev.off()
  
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\ACRU_cE_individual_2021_6_19.pdf",width =12,height = 4)
  
  p_cE_maple <- ggplot()+
    geom_line(data = Pheno_MAPLE_df,aes(x=Year, 
                                        y=cE,group = Tree),linetype="solid",size =1,color = "darkgreen")+
    facet_grid(~ Tree, scales = "free_x")+
    scale_x_continuous(limits = c(2016.5,2019.5),breaks = c(2017,2018,2019),label=c("2017","2018","2019"))+
    ylab("cE_date")+
    theme(axis.text = element_text(size =12))+
    theme(axis.text.x = element_text(size =12))+
    theme(axis.text.y = element_text(size =12))+
    theme(legend.position = "bottom")  
  
  p_cE_maple
  dev.off()
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Harvard Forest\\HF_obs_data_cluster\\Graph_output\\PIST_cE_individual_2021_6_19.pdf",width =12,height = 4)
  
  p_cE_pine <- ggplot()+
    geom_line(data = Pheno_PINE_df,aes(x=Year, 
                                       y=cE,group = Tree),linetype="solid",size =1,color = "darkred")+
    facet_grid(~ Tree, scales = "free_x")+
    scale_x_continuous(limits = c(2016.5,2019.5),breaks = c(2017,2018,2019),label=c("2017","2018","2019"))+
    ylab("cE_date")+
    theme(axis.text = element_text(size =12))+
    theme(axis.text.x = element_text(size =12))+
    theme(axis.text.y = element_text(size =12))+
    theme(legend.position = "bottom")  
  
  p_cE_pine
  dev.off()
  
}

