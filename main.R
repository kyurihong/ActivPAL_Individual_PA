#### -------------------------------
#### activpal-individual-pa : main.R
#### -------------------------------

# ---- Functions ----
source("R/00_libraries.R")
source("R/01_Mode_Function.R")
source("R/02_Time _Date_Formats.R")
source("R/03_Window_Function.R")
source("R/04_Process_activity_Function.R")

# ---- Raw data ----
# The data/ folder is excluded via .gitignore,
# so raw data files must be prepared locally on your own machine.

activpal_file    <- 'data/3_FTF_activpal.csv'
sleep_diary_file <- 'sleep_diary/ActivityProcessingPr_DATA_2026-06-02_1203.csv'

# ---- Each participant ----
result <- process_activpal(
  ID          = "3",
  activpal    = activpal_file,
  sleep_diary = sleep_diary_file,
  epoch       = 30,
  validation  = "wake"
)

# Result
head(result$daily_summary)
head(result$weekly_summary)



#---- Multiple participants ----
ID_list <- c("2", "3")

all_results <- lapply(ID_list, function(id) {
  process_activpal(
    ID          = id,
    activpal    = paste0("data/", id, "_FTF_activpal.csv"), #2_FTF_activpal: Fire example/ #3_FTF_activpal: Kyuri example
    sleep_diary = sleep_diary_file,
    epoch       = 30,
    validation  = "wake"
  )
})

weekly_all <- bind_rows(lapply(all_results, function(x) x$weekly_summary))
write.csv(weekly_all, "output/All_weekly_summary.csv", row.names = FALSE)
