library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(scales)   # For better date formatting
library(shinyjs)  # For JS control
library(plotly)   # For interactive plots
library(lubridate) # For timezone handling

# Set timezone for the application
Sys.setenv(TZ = "America/New_York")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# File to save data
data_file <- "data/observations.csv"

# Create the data file if it doesn't exist
if (!file.exists(data_file)) {
  # Create empty data frame with correct structure
  empty_df <- tibble(School = character(),
                     Tank = character(),
                     Time = as.POSIXct(character(), tz = "America/New_York"),
                     Turbidity = numeric())
  # Write with standard write.table to control format
  write.table(empty_df, data_file, 
              append = FALSE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = TRUE,
              quote = TRUE)
} else {
  # Check if the existing file has correct column types
  tryCatch({
    # Read existing data
    existing_data <- read.csv(data_file, stringsAsFactors = FALSE)
    
    # Check if existing data needs fixing
    if (any(grepl("^\\d+:\\d+", existing_data$Time)) || 
        any(!grepl("\\d{4}-\\d{2}-\\d{2}", existing_data$Time))) {
      message("Found malformed time data. Creating backup and fixing...")
      
      # Create backup of original file
      file.copy(data_file, paste0(data_file, ".backup"), overwrite = TRUE)
      
      # Try to parse the malformed times
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      
      # Fix malformed times by adding current date
      existing_data$Time <- sapply(existing_data$Time, function(t) {
        if (is.na(t) || t == "") return(NA)
        
        # Try to detect the format and fix it
        if (grepl("^\\d+:\\d+", t)) {
          # If it's just minutes:seconds
          return(paste(current_date, "00", t))
        } else if (!grepl("\\d{4}-\\d{2}-\\d{2}", t)) {
          # If it's missing the date but has hours
          return(paste(current_date, t))
        }
        return(t)
      })
      
      # Convert to POSIXct
      existing_data$Time <- as.POSIXct(existing_data$Time, tz = "America/New_York")
      
      # If conversions resulted in NA, generate sequential times from now
      if (all(is.na(existing_data$Time))) {
        message("Could not parse times, generating sequential times from now")
        base_time <- Sys.time()
        existing_data$Time <- seq(base_time, by = "5 mins", length.out = nrow(existing_data))
      }
      
      # Ensure other columns have correct types
      existing_data$School <- as.character(existing_data$School)
      existing_data$Tank <- as.character(existing_data$Tank)
      existing_data$Turbidity <- as.numeric(existing_data$Turbidity)
      
      # Rewrite the file with fixed data
      write.table(existing_data, data_file, 
                  append = FALSE, 
                  sep = ",", 
                  row.names = FALSE, 
                  col.names = TRUE,
                  quote = TRUE)
      
      message("Fixed data written to file")
    } else {
      # Time format looks correct, just convert to POSIXct
      existing_data$Time <- as.POSIXct(existing_data$Time, tz = "America/New_York")
      
      # Ensure other columns have correct types
      existing_data$School <- as.character(existing_data$School)
      existing_data$Tank <- as.character(existing_data$Tank)
      existing_data$Turbidity <- as.numeric(existing_data$Turbidity)
    }
    
  }, error = function(e) {
    warning("Existing data file has incorrect format. Creating a backup and starting with a new file.")
    message("Error details: ", e$message)
    file.copy(data_file, paste0(data_file, ".backup"), overwrite = TRUE)
    
    # Create empty data frame with correct structure
    empty_df <- tibble(School = character(),
                       Tank = character(),
                       Time = as.POSIXct(character(), tz = "America/New_York"),
                       Turbidity = numeric())
    
    # Write with standard write.table to control format
    write.table(empty_df, data_file, 
                append = FALSE, 
                sep = ",", 
                row.names = FALSE, 
                col.names = TRUE,
                quote = TRUE)
  })
}

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Oyster Water Filtration Experiment"),
  
  # Logo using server-side rendering
  imageOutput("logo", height = "80px"),
  
  # Add custom JavaScript for Enter key support
  tags$script('
    $(document).on("keydown", function(e) {
      if(e.keyCode == 13) {
        $("#submit").click();
        return false;
      }
    });
  '),
  
  sidebarLayout(
    sidebarPanel(
      textInput("school", "School Name"),
      radioButtons("tank", "Tank Type", choices = c("Oyster Tank", "Control Tank"),
                   selected = character(0)),
      numericInput("turbidity", "Turbidity (FNU)", value = NA, min = 0),
      actionButton("submit", "Submit Observation"),
      tags$p(style = "color: #666; font-style: italic; margin-top: 5px;", 
             "Press Enter to submit"),
      tags$p("Instructions:", style = "font-weight: bold"),
      tags$ul(
        tags$li("Enter school name, tank type, and turbidity reading"),
        tags$li("Submit data using the button or by pressing Enter")
        #tags$li("Click on any data point to mark it as 'censored' (e.g., for errors)"),
        #tags$li("Toggle the 'Show censored points' checkbox to view or hide censored data"),
        #tags$li("Click anywhere outside the menu to dismiss it")
      )
    ),
    
    mainPanel(
      plotlyOutput("turbidityPlot"),
      tags$div(
        style = "margin-top: 20px; color: #666;",
        tags$p("Experimental Design:", style = "font-weight: bold"),
        tags$ul(
          tags$li("Two 10 gallon tanks of water from Great Bay"),
          tags$li("Each tank has about 0.15 grams of dried ", tags$i("Spirulina"), " algae"),
          tags$li("Oysters are added to the 'Oyster Tank' while the 'Control Tank' has no oysters"),
          tags$li("We measure turbidity in Formazin Nephelometric Units (FNU) using a YSI ProDSS"),
          tags$li("Oysters (filter feeders) eat the algae and should reduce turbidity in the tank"),
          tags$li("Oysters are an important part of the Great Bay ecosystem and help keep Great Bay Estuary clean"),
        )
      )
    )
  )
)

server <- function(input, output, session) {
  message("Working directory: ", getwd())
  
  # Server-side image rendering
  output$logo <- renderImage({
    list(src = file.path("www", "PREP_Horizontal.png"),
         height = "80px",
         contentType = "image/png",
         style = "margin-bottom: 10px;")
  }, deleteFile = FALSE)
  
  # Focus on the school input field when the app starts
  observe({
    shinyjs::runjs("document.getElementById('school').focus()")
  })
  
  # Load existing data
  data_reactive <- reactiveVal({
    if (file.exists(data_file) && file.info(data_file)$size > 0) {
      tryCatch({
        # Use base R read.csv for more control over timestamp parsing
        data <- read.csv(data_file, stringsAsFactors = FALSE)
        
        if(nrow(data) > 0) {
          # Check if times need fixing
          if (any(grepl("^\\d+:\\d+", data$Time)) || 
              any(!grepl("\\d{4}-\\d{2}-\\d{2}", data$Time))) {
            message("Found malformed time data in read operation. Attempting to fix...")
            
            # Try to parse the malformed times
            current_date <- format(Sys.Date(), "%Y-%m-%d")
            
            # Fix malformed times by adding current date
            data$Time <- sapply(data$Time, function(t) {
              if (is.na(t) || t == "") return(NA)
              
              # Try to detect the format and fix it
              if (grepl("^\\d+:\\d+", t)) {
                # If it's just minutes:seconds
                return(paste(current_date, "00", t))
              } else if (!grepl("\\d{4}-\\d{2}-\\d{2}", t)) {
                # If it's missing the date but has hours
                return(paste(current_date, t))
              }
              return(t)
            })
          }
          
          # Convert time column to POSIXct with correct timezone
          data$Time <- as.POSIXct(data$Time, tz = "America/New_York")
          
          # Handle parsing errors by creating sequential times
          if (all(is.na(data$Time))) {
            message("Could not parse times, generating sequential times from now")
            base_time <- Sys.time()
            data$Time <- seq(base_time, by = "5 mins", length.out = nrow(data))
          }
          
          # Ensure Turbidity is numeric
          data$Turbidity <- as.numeric(data$Turbidity)
          
          # Ensure character columns are character type
          data$School <- as.character(data$School)
          data$Tank <- as.character(data$Tank)
        }
        data
      }, error = function(e) {
        message("Error reading data file: ", e$message)
        # Return empty tibble with correct structure
        tibble(
          School = character(),
          Tank = character(),
          Time = as.POSIXct(character(), tz = "America/New_York"),
          Turbidity = numeric()
        )
      })
    } else {
      tibble(
        School = character(),
        Tank = character(),
        Time = as.POSIXct(character(), tz = "America/New_York"),
        Turbidity = numeric()
      )
    }
  })
  
  # Improved validation function with better checks and debouncing
  validate_inputs <- reactive({
    # Explicitly get the values to ensure we have the latest
    school_val <- input$school
    tank_val <- input$tank
    turbidity_val <- input$turbidity
    
    # Enhanced validation with more detailed feedback
    if (is.null(school_val) || trimws(school_val) == "") {
      return(list(valid = FALSE, message = "Please enter a school name"))
    }
    
    if (is.null(tank_val) || tank_val == "") {
      return(list(valid = FALSE, message = "Please select a tank type"))
    }
    
    if (is.null(turbidity_val) || is.na(turbidity_val)) {
      return(list(valid = FALSE, message = "Please enter a turbidity value"))
    }
    
    # Additional validation for turbidity
    if (!is.numeric(turbidity_val)) {
      return(list(valid = FALSE, message = "Turbidity must be a number"))
    }
    
    return(list(valid = TRUE, message = "All inputs valid"))
  })
  
  observeEvent(input$submit, {
    # Use the enhanced validation function
    validation_result <- validate_inputs()
    if (!validation_result$valid) {
      showNotification(validation_result$message, type = "warning")
      return()
    }
    
    new_entry <- tibble(
      School = as.character(input$school),
      Tank = as.character(input$tank),
      Time = as.POSIXct(Sys.time(), tz = "America/New_York"),
      Turbidity = as.numeric(input$turbidity)
    )
    
    # Custom CSV writing to preserve timezone with full datetime format
    if(file.exists(data_file) && file.info(data_file)$size > 0) {
      # Format time in a standard way that will be easily parsed later
      new_entry_to_write <- new_entry
      new_entry_to_write$Time <- format(new_entry$Time, "%Y-%m-%d %H:%M:%S")
      
      # Append to existing file with correct format
      write.table(new_entry_to_write, data_file, 
                  append = TRUE, 
                  sep = ",", 
                  row.names = FALSE, 
                  col.names = FALSE,
                  quote = TRUE)
    } else {
      # Format time in a standard way that will be easily parsed later
      new_entry_to_write <- new_entry
      new_entry_to_write$Time <- format(new_entry$Time, "%Y-%m-%d %H:%M:%S")
      
      # Create new file with headers
      write.table(new_entry_to_write, data_file, 
                  append = FALSE, 
                  sep = ",", 
                  row.names = FALSE, 
                  col.names = TRUE,
                  quote = TRUE)
    }
    
    current_data <- data_reactive()
    
    # Ensure consistent data types before binding rows
    if (is.data.frame(current_data) && nrow(current_data) > 0) {
      # Ensure consistent types
      current_data$School <- as.character(current_data$School)
      current_data$Tank <- as.character(current_data$Tank) 
      current_data$Turbidity <- as.numeric(current_data$Turbidity)
      current_data$Time <- as.POSIXct(current_data$Time, tz = "America/New_York")
      
      new_entry$School <- as.character(new_entry$School)
      new_entry$Tank <- as.character(new_entry$Tank)
      new_entry$Turbidity <- as.numeric(new_entry$Turbidity)
      new_entry$Time <- as.POSIXct(new_entry$Time, tz = "America/New_York")
      
      # Only bind if the structures are compatible
      if (identical(sapply(current_data, class), sapply(new_entry, class))) {
        combined_data <- bind_rows(current_data, new_entry)
        data_reactive(combined_data)
      } else {
        # Handle case where structures don't match
        message("Data structures don't match. Reloading from file.")
        # Re-read the file which should now include the new entry
        updated_data <- read.csv(data_file, stringsAsFactors = FALSE)
        updated_data$Time <- as.POSIXct(updated_data$Time, tz = "America/New_York")
        updated_data$Turbidity <- as.numeric(updated_data$Turbidity)
        updated_data$School <- as.character(updated_data$School)
        updated_data$Tank <- as.character(updated_data$Tank)
        data_reactive(updated_data)
      }
    } else {
      # If there was no existing data, just use the new entry
      data_reactive(new_entry)
    }
    
    # Success notification
    showNotification("Observation successfully recorded!", type = "message")
    
    # Clear fields after submission
    updateTextInput(session, "school", value = "")
    updateSelectInput(session, "tank", selected = "")
    updateNumericInput(session, "turbidity", value = NA)
    
    # Focus on school field again
    shinyjs::runjs("document.getElementById('school').focus()")
  })
  
  output$turbidityPlot <- renderPlotly({
    data <- data_reactive()
    
    if (nrow(data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No data yet. Submit observations to see the plot.") +
        theme_void()
      return(ggplotly(p))
    }
    
    data <- data %>%
      arrange(Time) %>%
      mutate(
        tooltip = paste0(
          "School: ", School, "\n",
          "Tank: ", Tank, "\n",
          "Time: ", format(Time, "%H:%M", tz = "America/New_York"), "\n",
          "Turbidity: ", Turbidity, " FNU"
        )
      )
    
    p <- ggplot(data, aes(
      x = Time,
      y = Turbidity,
      color = Tank,
      text = paste0(
        "School: ", School, "\n",
        "Tank: ", Tank, "\n",
        "Time: ", format(Time, "%H:%M"), "\n",
        "Turbidity: ", Turbidity, " FNU"
      )
    )) +
      geom_point(size = 3) +
      geom_line(aes(group = Tank), linewidth = 1) +
      scale_x_datetime(labels = date_format("%H:%M", tz = "America/New_York"), breaks = pretty_breaks(n = 8)) +
      labs(title = "Turbidity Over Time",
           x = "Time",
           y = "Turbidity (FNU)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.2
      ))
  })
}

shinyApp(ui, server)