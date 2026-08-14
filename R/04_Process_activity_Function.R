#### -----------------------------
#### 04. Process Activity Function
#### -----------------------------


process_activpal<- function(ID, activpal, sleep_diary, epoch = 30, validation = "wake") {
  AP <- read.csv(activpal) %>%
    clean_names()
  
  AP <- AP %>%
    mutate(
      datetime = suppressWarnings(mdy_hm(time_approx, tz = "UTC")))
  
  
  ####epoch raw####
  AP <- AP %>%
    arrange(datetime) %>%
    group_by(datetime) %>%
    mutate(
      n_in_min = n(),
      sec_in_min = row_number() -1,
      datetime_full = datetime + seconds(sec_in_min)
    )%>%
    ungroup()
  
  
  
  ####SED, LPA, MVPA Labeling####
  AP <- AP %>%
    mutate(
      mets = as.numeric(activity_score_met_s),
      upright_s = as.numeric(upright_time_s)
    )
  
  AP <- AP %>%
    mutate(
      intensity = case_when(
        is.na(mets) | is.na(upright_s) ~ NA_character_,
        upright_s < 0.5 ~ "SED",
        upright_s >= 0.5 & mets < 3 ~ "LPA",
        upright_s >= 0.5 & mets >= 3 ~ "MVPA",
        TRUE ~ NA_character_
      ),
      intensity = factor(intensity, levels = c("SED", "LPA", "MVPA"))
    )
  
  ####Working/Sleeping time w/ Diary####
  
  diary <- read.csv(sleep_diary) %>%
    clean_names()
  
  diary <- diary %>% filter(record_id == ID)
  
  
  diary_dates <- unlist(diary[1, grep("^diary_date\\d+$", names(diary))])
  date_format <- ifelse(nchar(diary_dates[1]) == 9, 
                        "%m/%d/%Y",
                        "%m/%d/%y")
  diary_dates <- as.Date(diary_dates, format = date_format)
  
  
  start_time <-   as.POSIXct(paste0(diary_dates[1], " ",
                                    diary$diary_time1),
                             tz = "UTC"
  )
  
  end_time <-   as.POSIXct(paste0(diary_dates[8], " ",
                                  diary$diary_time8),
                           tz = "UTC"
  )
  
  sleep.df <- window_omni(diary = diary, type = "sleep", earliest_time = start_time)
  work.df <- window_omni(diary = diary, type = "work")
  nap.df <- window_omni(diary = diary, type = "nap")
  wear.df <- window_omni(diary = diary, type = "wear")
  work.day.no<-diary[,grepl("work\\d+$", colnames(diary))]
  
  
  
  ##epoch#####
  AP <- AP %>%
    mutate(
      epoch_start = as.POSIXct(floor(as.numeric(datetime_full)/epoch) * epoch,
                               origin = "1970-01-01", tz = "UTC")
    )
  
  AP_e <- AP %>%
    group_by(epoch_start) %>%
    summarize(
      intensity_mode = Mode(as.character(intensity)),
      step_counts = sum(step_count, na.rm = TRUE),
      mets_mean = mean(mets, na.rm = TRUE),
      upright_mean = mean(upright_s, na.rm = TRUE),
      
      ###Cadence###
      #cadence = sum(step_count, na.rm = TRUE) * (60 / epoch),
      stepping_seconds = sum(upright_s > 0, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      true_cadence = ifelse(stepping_seconds > 0,
                            (step_counts / stepping_seconds)*60, NA),
      intensity_freq = factor(intensity_mode, 
                              levels = c("SED", "LPA", "MVPA", "Nonwear","Sleep")),
      datetime = epoch_start
    ) %>%
    select(datetime, intensity_freq, step_counts, mets_mean, upright_mean,
           #cadence, 
           stepping_seconds, true_cadence)
  
  
  AP_e$day <- as.Date(AP_e$datetime)
  AP_e$day <- as.numeric(AP_e$day - min(AP_e$day))
  
  
  AP3 <- AP_e %>%
    mutate(
      datetime = as.POSIXct(datetime, tz = "UTC"),
      day = match(as.Date(datetime), diary_dates)
    )
  
  
  
  
  ####Cleave the beginning# ####
  if(!is.na(sleep.df[[1]][7])){
    end_time2 <-ifelse(end_time > sleep.df[[1]][7],  sleep.df[[1]][7], 
                       end_time)
  }else{(end_time2 <- end_time)}
  
  
  if(!is.na(sleep.df[[2]][1])){
    start_time2 <-ifelse(start_time < sleep.df[[2]][1], sleep.df[[2]][1], 
                         start_time)
  }else{(start_time2 <- start_time)}
  
  AP4 <- AP3 %>% filter((datetime > start_time2) & (datetime < end_time2))
  
  ###Wear detection###
  if( (wear.df[[1]] %>% is.na() %>% sum) < (wear.df[[2]] %>% is.na() %>% sum)){
    day_look <- which(!is.na(wear.df[[1]]) & is.na(wear.df[[2]]))
    for(i in length(day_look)){
      check_time <- wear.df[[1]][day_look[i]] + 20 * 60 
      AP_confirm <- which(!is.na(match(AP4$datetime, check_time)))
      subset_AP_for_looking <- AP4[(AP_confirm:nrow(AP4)),]
      time_on_identified_spot <- which(subset_AP_for_looking$step_counts > 0)[1]
      wear.df[[2]][day_look[i]] <- subset_AP_for_looking$datetime[time_on_identified_spot]
      print("Missing Diary Time - Monitor On Time - Autodetected")
    }
  }
  
  
  #####
  
  #Markers 
  sleep_start_markers <- which(!is.na(match(AP4$datetime, sleep.df[[1]])))
  sleep_end_markers <- which(!is.na(match(AP4$datetime, sleep.df[[2]])))
  
  work_start_markers <- which(!is.na(match(AP4$datetime, work.df[[1]])))
  work_end_markers <- which(!is.na(match(AP4$datetime, work.df[[2]])))
  
  
  nap_start_markers <- which(!is.na(match(AP4$datetime, nap.df[[1]])))
  nap_end_markers <- which(!is.na(match(AP4$datetime, nap.df[[2]])))
  
  monitor_off_markers <- which(!is.na(match(AP4$datetime, wear.df[[1]])))
  monitor_on_markers <- which(!is.na(match(AP4$datetime, wear.df[[2]])))
  
  
  
  
  AP4$sleep <- NA 
  AP4$sleep[sleep_start_markers] <- 1
  AP4$sleep[sleep_end_markers] <- 0
  AP4$sleep[1] <- 0
  
  AP4$work <- NA 
  AP4$work[work_start_markers] <- 1
  AP4$work[work_end_markers] <- 0
  AP4$work[1] <- ifelse(
    (work.df[[1]][1] < AP4$datetime[1]) & #check code, compares 
      (work.df[[2]][1] > AP4$datetime[1]),
    1, 0)
  
  AP4$nap <- NA 
  AP4$nap[1] <- 0
  AP4$nap[nap_start_markers] <- 1
  AP4$nap[nap_end_markers] <- 0
  
  
  AP4$monitor <- NA 
  AP4$monitor[monitor_off_markers] <- 1
  AP4$monitor[monitor_on_markers] <- 0
  AP4$monitor[1] <- 0
  
  AP5<- AP4 %>% tidyr::fill(sleep, work, nap, monitor, .direction = "down")
  
  #not a work day marking - but filled# 
  d <- which(!is.na(work.day.no) & work.day.no == 0)
  AP5$work[(AP5$day %in% d)] <- 0 
  AP6 <- AP5
  AP6$intensity_orig <- AP6$intensity_freq
  AP6$intensity_freq[AP6$sleep == 1] <- "Sleep"
  
  new_days <-  AP6$datetime[1] + c(0, 1:7 * 86400)
  new_day_markers <- which(!is.na(match(AP6$datetime, new_days)))
  AP6$new_day <- NA 
  AP6$new_day[new_day_markers] <- 1:8
  AP7<- AP6 %>% tidyr::fill(new_day, .direction = "down")
  AP7$day <- AP7$new_day
  
  ####day SUMMARIZE 
  mins_per_epoch <- epoch / 60
  
  wear_time <- AP7 %>% 
    group_by(day) %>% 
    summarize(
      total_wear = sum(monitor == 0, na.rm = TRUE)* mins_per_epoch,
      wake_wear = sum(monitor == 0 & sleep == 0, na.rm = TRUE) * mins_per_epoch,
      non_wear = sum(monitor == 1, na.rm = TRUE) * mins_per_epoch,
      non_wear_sleep = sum(monitor == 1 & sleep == 1, na.rm = TRUE) * mins_per_epoch,
      Sleep = sum(sleep == 1, na.rm = TRUE) * mins_per_epoch,
      Steps = sum(step_counts, na.rm = TRUE),
      valid_day_24 = ifelse(total_wear > (20*60), 1, 0),
      valid_day_wake = ifelse(wake_wear > (10*60), 1, 0),
      .groups = "drop"
    ) %>%
    mutate(
      Valid = if(validation == "wake") valid_day_wake
      else if (validation == "total") valid_day_24
      else stop ("Invalid Validation Criteria")
    )
  
  AP_valid <- AP7 %>% filter(day %in% wear_time$day[wear_time$Valid == 1])            
  
  #Intensity
  day_intensity <- AP7 %>%
    filter(intensity_freq %in% c("SED", "LPA", "MVPA")) %>%
    group_by(day, intensity_freq) %>%
    summarize(
      minutes = n() * mins_per_epoch,
      .groups = "drop"
    )%>%
    tidyr::pivot_wider(
      names_from = intensity_freq,
      values_from = minutes,
      values_fill = 0
    )
  
  ##Just in case if there is no specific intensity
  for (col in c("SED", "LPA", "MVPA")) {
    if (!col %in% names(day_intensity)) day_intensity[[col]] <- 0
  }
  
  #Daily Summary
  
  
  daily_summary <- wear_time %>%
    left_join(day_intensity, by = "day") %>%
    mutate(
      participant_ID = ID,
      Day = day,
      Valid = ifelse(Valid == 1, "Yes", "No") #Optional!
    ) %>%
    dplyr::select(
      participant_ID, Day,
      Valid, valid_day_24, valid_day_wake,
      MVPA, LPA, SED,
      Sleep,Steps,
      non_wear, non_wear_sleep
    ) %>%
    mutate(across(c(MVPA, LPA, SED), ~replace_na(.x, 0))) %>%
    arrange(Day)
  
  weekly_summary <- daily_summary %>%
    filter(Valid == "Yes") %>%
    summarize(
      participant_ID = ID,
      valid_days = n(),
      MVPA = mean(MVPA, na.rm=TRUE),
      LPA = mean(LPA, na.rm = TRUE),
      SED = mean(SED, na.rm = TRUE),
      Sleep = mean(Sleep[valid_day_24 == 1], na.rm = TRUE),
      Steps = mean(Steps, na.rm = TRUE),
      non_wear = mean(non_wear, na.rm = TRUE),
      non_wear_sleep = mean(non_wear_sleep, na.rm = TRUE)
    ) %>%
    dplyr::select(
      participant_ID, valid_days,
      MVPA, LPA, SED,
      Sleep, Steps,
      non_wear, non_wear_sleep
    )
  
  
  
  
  daily_diagnostic <- AP7 %>%
    group_by(day) %>%
    summarize(
      #Total wear
      total_wear = sum(monitor == 0, na.rm = TRUE)*mins_per_epoch,
      
      #wake (monitor = 0 & nap == 0) 
      wake_mins = sum(sleep == 0 & nap == 0, na.rm = TRUE) * mins_per_epoch,
      wake_MVPA = sum(sleep == 0 & nap == 0 & intensity_orig == "MVPA", na.rm = TRUE) * mins_per_epoch,
      wake_LPA = sum(sleep == 0 & nap == 0 & intensity_orig == "LPA", na.rm = TRUE) * mins_per_epoch,
      wake_SED = sum(sleep == 0 & nap == 0 & intensity_orig == "SED", na.rm = TRUE) * mins_per_epoch,
      wake_steps = sum(step_counts[sleep == 0 & nap == 0], na.rm =TRUE),
      
      #work (work == 1)
      work_mins = sum(work == 1, na.rm = TRUE) * mins_per_epoch,
      work_MVPA = sum(work == 1 & intensity_orig == "MVPA", na.rm = TRUE) * mins_per_epoch,
      work_LPA = sum(work == 1 & intensity_orig == "LPA", na.rm = TRUE) * mins_per_epoch,
      work_SED = sum(work == 1 & intensity_orig == "SED", na.rm = TRUE) * mins_per_epoch,
      work_steps = sum(step_counts[work == 1], na.rm = TRUE),
      
      #Leisure (work == 0 & sleep == 0 & nap == 0 & monitor == 0)
      leisure_mins = sum(work == 0 & sleep == 0 & nap == 0 & monitor == 0, na.rm = TRUE) * mins_per_epoch,
      leisure_MVPA = sum(work == 0 & sleep == 0 & nap == 0 & monitor == 0 & intensity_orig == "MVPA", na.rm = TRUE) * mins_per_epoch,
      leisure_LPA = sum(work == 0 & sleep == 0 & nap == 0 & monitor == 0 & intensity_orig == "LPA", na.rm = TRUE) * mins_per_epoch,
      leisure_SED = sum(work == 0 & sleep == 0 & nap == 0 & monitor == 0 & intensity_orig == "SED", na.rm = TRUE) * mins_per_epoch,
      leisure_steps = sum(step_counts[work == 0 & sleep == 0 & nap == 0 & monitor == 0], na.rm = TRUE),
      
      #Nap (nap == 1)
      nap_mins = sum(nap == 1, na.rm = TRUE)*mins_per_epoch,
      nap_MVPA = sum(nap == 1 & intensity_orig == "MVPA", na.rm = TRUE)*mins_per_epoch,
      nap_LPA = sum(nap == 1 & intensity_orig == "LPA", na.rm = TRUE)*mins_per_epoch,
      nap_SED = sum(nap == 1 & intensity_orig == "SED", na.rm = TRUE)*mins_per_epoch,
      nap_steps = sum(step_counts[nap == 1], na.rm = TRUE),
      
      #sleep (sleep == 1)
      sleep_mins = sum(sleep == 1, na.rm = TRUE) * mins_per_epoch,
      sleep_MVPA = sum(sleep == 1 & intensity_orig == "MVPA", na.rm = TRUE) * mins_per_epoch,
      sleep_LPA = sum(sleep == 1 & intensity_orig == "LPA", na.rm = TRUE) * mins_per_epoch,
      sleep_SED = sum(sleep == 1 & intensity_orig == "SED", na.rm = TRUE) * mins_per_epoch,
      sleep_steps = sum(step_counts[sleep == 1], na.rm = TRUE),
      
      #non wear during sleep OR nap
      non_wear_sleep_or_nap = sum(monitor == 1 & (sleep == 1 | nap == 1), na.rm = TRUE),
      
      #true_cadence
      wake_cadence_mean = mean(true_cadence[sleep == 0 & nap == 0], na.rm = TRUE),
      work_cadence_mean = mean(true_cadence[work == 1], na.rm = TRUE),
      leisure_cadence_mean = mean(true_cadence[work == 0 & sleep ==0 & nap == 0 & monitor == 0], na.rm = TRUE),
      
      # #Cadence
      # wake_cadence_mean = mean(cadence[sleep == 0 & nap == 0 & step_counts > 0], na.rm = TRUE),
      # work_cadence_mean = mean(cadence[work == 1 & step_counts > 0], na.rm = TRUE),
      # leisure_cadence_mean = mean(cadence[work == 0 & sleep ==0 & nap == 0 & monitor == 0 & step_counts > 0], na.rm = TRUE),
      # 
      .groups = "drop"
    ) %>%
    mutate(
      participant_ID = ID,
      Day = day,
    ) %>%
    left_join(
      wear_time %>% dplyr::select(day, Valid) %>%
        mutate(Valid = ifelse(Valid == 1, "Yes", "No")),
      by = "day"
    ) %>%
    dplyr::select(
      participant_ID, Day, Valid, total_wear,
      wake_mins, wake_MVPA, wake_LPA, wake_SED, wake_steps,
      work_mins, work_MVPA, work_LPA, work_SED, work_steps,
      leisure_mins, leisure_MVPA, leisure_LPA, leisure_SED, leisure_steps,
      nap_mins, nap_MVPA, nap_LPA, nap_SED, nap_steps,
      sleep_mins, sleep_MVPA, sleep_LPA, sleep_SED, sleep_steps,
      non_wear_sleep_or_nap,
      wake_cadence_mean, work_cadence_mean, leisure_cadence_mean
    ) %>%
    arrange(Day)
  
  
  write.csv(AP7, paste0("output/",ID,"_epoch.csv"), row.names = FALSE)
  write.csv(daily_summary, paste0("output/",ID,"_daily_summary.csv"), row.names = FALSE)
  write.csv(weekly_summary, paste0("output/",ID,"_weekly_summary.csv"), row.names = FALSE)
  write.csv(daily_diagnostic, paste0("output/",ID,"_daily_diagnostic.csv"), row.names = FALSE)
  
  
  ###GRAPHING ####
  graph.df <- AP7
  AP7$new_day
  time.group<- substring(graph.df$datetime, 1, 18)
  time.group.rep <-  substring(time.group, 18) %>% as.numeric() %>% 
    cut(breaks = c(-Inf, 2, 5),
        labels = c(0, 3))
  
  graph.df$time.group <- NA
  graph.df$time.group <- paste0(
    substring(time.group, 1, 17),
    time.group.rep
  )
  
  graph.df1 <- graph.df %>% group_by(time.group, day) %>% 
    summarize(
      mvpa = sum(intensity_orig == 'MVPA') * mins_per_epoch,
      lpa = sum(intensity_orig == 'LPA') * mins_per_epoch,
      sed = sum(intensity_orig == 'SED') * mins_per_epoch,
      sleep = sum(sleep == 1) * mins_per_epoch,
      nap = sum(nap == 1) * mins_per_epoch,
      work = sum(work == 1)* mins_per_epoch,
      nonwear = sum(monitor == 1) * mins_per_epoch,
      steps = sum(step_counts),
      day = new_day
    )
  
  
  graph.df1[graph.df1$sleep == 0.5,]
  
  scale_factor <- max(graph.df1$steps, na.rm = TRUE)
  graph.df1$highlight <- graph.df1$sleep > 0.25
  graph.df1$highlight2 <- graph.df1$work > 0.25
  
  
  graph.df1$time.group2 <- as.POSIXct(paste0(graph.df1$time.group, "0"),
                                      format = "%Y-%m-%d %H:%M:%S", tz = "UTC") 
  
  graph.df1$time.group2[is.na(graph.df1$time.group2)] <- 
    graph.df1$time.group2[which(is.na(graph.df1$time.group2))-1] + 30
  
  graph.df1 <- graph.df1 %>%
    group_by(day) %>%
    filter(time.group2 <= min(time.group2) + hours(24)) %>%
    ungroup()
  
  
  
  active_plot<- ggplot(graph.df1, aes(x = time.group2)) +
    geom_rect(
      data = subset(graph.df1, highlight),
      aes(
        xmin = time.group2,
        xmax = time.group2 + 29,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "blue",
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    geom_rect(
      data = subset(graph.df1, highlight2),
      aes(
        xmin = time.group2,
        xmax = time.group2 + 29,
        ymin = -Inf,
        ymax = Inf
      ),
      fill = "orange",
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    
    geom_line(aes(y = mvpa, group = factor(day)), linewidth = 1, color = "red") +
    geom_line(aes(y = lpa, group = factor(day)), linewidth = 1, color = "yellow") +
    geom_line(aes(y = sed, group = factor(day)), linewidth = 1, color = "green4") +
    geom_line(aes(y = steps / scale_factor, group = factor(day)), linewidth = 1, alpha = 1) +
    
    scale_y_continuous(
      name = "mvpa",
      sec.axis = sec_axis(~ . * scale_factor, name = "Steps")
    ) +
    scale_x_datetime(date_breaks = "3 hours") +
    facet_wrap(~ day, scales = "free_x", ncol = 1)
  
  ggsave(
    filename  = paste0("output/",ID,"_Active_Plot.pdf"),
    plot      = active_plot,
    width     = 14,
    height    = 40,  
    units     = "in",
    limitsize = FALSE
  )
  
  return(
    list(
      AP7 = AP7,
      wear_time = wear_time,
      daily_summary = daily_summary,
      weekly_summary = weekly_summary,
      daily_diagnostic = daily_diagnostic
    )
  )
}

