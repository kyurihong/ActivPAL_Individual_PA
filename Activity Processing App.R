# ============================================
# ACTIVPAL SHINY APP
# ============================================

# Increase upload size
options(shiny.maxRequestSize = 100 * 1024^2)

# ---------------------------
# LOAD CUSTOM FUNCTIONS
# ---------------------------
code_folder <- paste0(getwd(), "/R")

if (!dir.exists(code_folder)) {
  stop("Code folder not found. Check path.")
}

files <- list.files(
  path = code_folder,
  pattern = "\\.R$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No R scripts found in code folder.")
}

lapply(files, source)


# ============================================
# UI
# ============================================

ui <- fluidPage(
  
  titlePanel("ActivPAL Processing Tool"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        "activpal_files",
        "Select activPAL CSV files",
        multiple = TRUE,
        accept = ".csv"
      ),
      
      fileInput(
        "sleep_diary",
        "Upload Sleep Diary CSV",
        accept = ".csv"
      ),
      
      numericInput("epoch", "Epoch (seconds):", value = 30),
      
      selectInput(
        "validation",
        "Validation Type",
        choices = c("wake", "total")
      ),
      
      actionButton("run", "Run Processing")
      
    ),
    
    mainPanel(
      h4("Status"),
      verbatimTextOutput("status")
    )
  )
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {
  
  status_text <- reactiveVal("Ready.")
  
  observeEvent(input$run, {
    
    status_text("Starting processing...")
    
    tryCatch({
      
      req(input$activpal_files)
      req(input$sleep_diary)
      
      activpal_files <- input$activpal_files$datapath
      filenames <- input$activpal_files$name
      
      # Extract ID from START of filename
      ID <- stringr::str_extract(filenames, "^\\d+")
      
      if (any(is.na(ID))) {
        stop("Some filenames do not start with a numeric ID.")
      }
      
      sleep_diary <- input$sleep_diary$datapath
      
      results_list <- vector("list", length(ID))
      
      for (i in seq_along(ID)) {
        
        status_text(paste("Processing ID:", ID[i]))
        
        results_list[[i]] <- tryCatch({
          
          out <- process_activpal(
            ID = ID[i],
            activpal = activpal_files[i],
            sleep_diary = sleep_diary,
            epoch = input$epoch,
            validation = input$validation
          )
          
          out$weekly_summary
          
        }, error = function(e) {
          message("Error for ID ", ID[i], ": ", e$message)
          return(NULL)
        })
      }
      
      # Remove failed results
      results_list <- results_list[!sapply(results_list, is.null)]
      
      if (length(results_list) == 0) {
        stop("No valid results produced.")
      }
      
      weekly_summary_ALL <- dplyr::bind_rows(results_list)
      
      # Save output
      output_file <- file.path(getwd(), "output/ALL_weekly_summary_App.csv")
      
      write.csv(weekly_summary_ALL, output_file, row.names = FALSE)
      
      status_text(
        paste("Processing complete. Output saved to:", output_file)
      )
      
    }, error = function(e) {
      status_text(paste("ERROR:", e$message))
    })
    
  })
  
  output$status <- renderText({
    status_text()
  })
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)