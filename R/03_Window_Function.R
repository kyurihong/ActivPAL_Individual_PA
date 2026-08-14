#### -------------------
#### 03. Window function
#### -------------------


window_omni <- function(diary, type, length =8, earliest_time = NA ){
  
  diary_dates <- unlist(diary[1, grep("^diary_date\\d+$", names(diary))])
  date_format <- ifelse(nchar(diary_dates[1]) == 9, 
                        "%m/%d/%Y",
                        "%m/%d/%y")
  diary_dates <- as.Date(diary_dates, format = date_format)
  
  list_start <- as.POSIXct(rep(NA, length(length)), tz = "UTC")
  list_end <- as.POSIXct(rep(NA, length(length)), tz = "UTC")
  
  
  if(type == "sleep"){
    start  <- fix_hm(unlist(diary[1, grep("^diary_bedtime\\d+$", names(diary))]))
    end <- fix_hm(unlist(diary[1, grep("^diary_finalwake\\d+$", names(diary))]))
  } else if(type == "work"){
    start   <- fix_hm(unlist(diary[1, grep("^diary_workstart\\d+$", names(diary))]))
    end   <- fix_hm(unlist(diary[1, grep("^diary_workstop\\d+$", names(diary))]))
  } else if(type == "wear"){
    start  <- fix_hm(unlist(diary[1, grep("^diary_monitorsofftime\\d+$", names(diary))]))
    end   <- fix_hm(unlist(diary[1, grep("^diary_monitorsontime\\d+$", names(diary))]))
  } else if(type == "nap"){
    start <- fix_hm(unlist(diary[1, grep("^diary_napstrt\\d+$", names(diary))]))
    end <- fix_hm(unlist(diary[1, grep("^diary_napend\\d+$", names(diary))]))
  } else{cat("type not recognize \n")}  
  
  
  for (j in 1:length) {
    if(!is.na(start[j])){
      ## Start of window ####
      list_start[j] <- as.POSIXct(
        paste0(
          diary_dates[j], " ",
          start[j]
        ),
        tz = "UTC"
      )
    }
    if(!is.na(end[j])){
      list_end[j] <- as.POSIXct(
        paste0(
          diary_dates[j], " ",
          end[j]
        ),
        tz = "UTC"
      )
    }
    
    if(type == "sleep"){
      if(list_start[1] < earliest_time){
        list_start[1] <- list_start[1] + 86400
      }
      if (!is.na(list_start[j]) && !is.na(list_end[j])) {
        if (list_start[j] < list_end[j]){
          list_start[j] <- list_start[j] + 86400
        } else {
          list_start[j] <- list_start[j]
        }
        
      }
    }
    
    if(type %in% c("nap", "work", "wear")){
      if (!is.na(list_start[j]) && !is.na(list_end[j])) {
        if (list_end[j] < list_start[j]){
          list_end[j] <- list_end[j] + 86400
        } else {
          list_end[j] <-list_end[j]
        }
      }
    }
  }
  return(list(list_start, list_end))
  
}

