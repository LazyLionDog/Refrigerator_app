library(shiny)
library(DT)
library(tibble)
library(dplyr)
library(openxlsx)

# Sample data for the refrigerator stock list
stock_data <- tibble(
  Item = c("Antibody A", "Enzyme B", "Chemical C", "Buffer D", "Cell Line E"),
  Quantity = c(10, 5, 15, 8, 3),
  Expiry_Date = as.Date(c("2025-01-15", "2024-11-20", "2025-06-30", "2024-12-10", "2024-10-25")),
  Storage_Location = c("Shelf 1", "Shelf 2", "Shelf 3", "Shelf 1", "Shelf 4"),
  Vendor = c("Vendor A", "Vendor B", "Vendor C", "Vendor D", "Vendor E"),
  Catalog_Number = c("CAT123", "CAT456", "CAT789", "CAT012", "CAT345"),
  Added_By = c("", "", "", "", ""),
  Added_Date = Sys.Date() - c(5, 4, 3, 2, 1),
  ID = 1:5
)

# Load stock data from local file if available
if (file.exists("stock_data.rds")) {
  stock_data <- readRDS("stock_data.rds")
  
  
} else {
  saveRDS(stock_data, "stock_data.rds")
}

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Biomedical Laboratory Refrigerator Stock List"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_import", "Import Stock List (Excel File)", accept = c(".xlsx")),
      textInput("item", "Item Name", ""),
      numericInput("quantity", "Quantity", value = 1, min = 1),
      dateInput("expiry_date", "Expiry Date", value = Sys.Date()),
      textInput("storage_location", "Storage Location", ""),
      textInput("vendor", "Vendor", ""),
      textInput("catalog_number", "Catalog Number", ""),
      textInput("added_by", "Added By (Your Name)", ""),
      actionButton("add", "Add Item"),
      actionButton("remove", "Remove Selected Item"),
      downloadButton("export", "Export to Excel"),
      actionButton("stop_app", "Stop App")
    ),
    
    mainPanel(
      actionButton("find_duplicates", "Find Duplicate Items"),
      br(),
      DT::dataTableOutput("stock_table")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  # Stop the app when 'Stop App' button is pressed
  observeEvent(input$stop_app, {
    stopApp()
  })
  # Create a reactive values object to hold the stock data
  stock <- reactiveVal(stock_data)
  
  # Find and highlight duplicate items
  observeEvent(input$find_duplicates, {
    current_stock <- stock()
    duplicate_items <- current_stock %>% count(Item) %>% filter(n > 1)
    if (nrow(duplicate_items) > 0) {
      showNotification(paste0("Found ", nrow(duplicate_items), " duplicate items."), type = "warning")
    } else {
      showNotification("No duplicate items found.", type = "message")
    }
  })
  
  # Render the stock list table
  output$stock_table <- renderDT({
    datatable(stock() %>% arrange(desc(Added_Date)), selection = "single", editable = TRUE, options = list(pageLength = 10))
  })
  
  
  # Import stock list from an Excel file
  observeEvent(input$file_import, {
    req(input$file_import)
    tryCatch({
      imported_data <- read.xlsx(input$file_import$datapath, detectDates = TRUE)
      required_columns <- c("Item", "Quantity", "Expiry_Date", "Storage_Location", "Vendor", "Catalog_Number", "Added_By", "Added_Date")
      
      # Add missing columns if not present in the imported data
      for (col in required_columns) {
        if (!col %in% colnames(imported_data)) {
          imported_data[[col]] <- NA
        }
      }
      
      imported_data <- imported_data %>% 
        select(any_of(required_columns)) %>% 
        mutate(ID = max(stock()$ID, na.rm = TRUE) + 1:n())
      
      imported_data$Expiry_Date <- as.Date(imported_data$Expiry_Date, format = "%Y/%m/%d")
      imported_data$Added_Date <- as.Date(imported_data$Added_Date, format = "%Y/%m/%d")
      updated_stock <- dplyr::bind_rows(stock(), imported_data)
      stock(updated_stock)
      saveRDS(updated_stock, "stock_data.rds")
    }, error = function(e) {
      showNotification(paste0("Error importing file: ", as.character(e$message)), type = "error")
    })
  })
  
  # Add an item to the stock list
  observeEvent(input$add, {
    current_stock <- stock()
    new_id <- ifelse(nrow(current_stock) == 0, 1, max(current_stock$ID) + 1)
    new_item <- tibble(
      ID = new_id,      Item = input$item,
      Quantity = input$quantity,
      Expiry_Date = input$expiry_date,
      Storage_Location = input$storage_location,
      Vendor = input$vendor,
      Catalog_Number = input$catalog_number,
      Added_By = input$added_by,
      Added_Date = Sys.Date()
    )
    updated_stock <- dplyr::bind_rows(stock(), new_item)
    stock(updated_stock)
    saveRDS(updated_stock, "stock_data.rds")
  })
  
  # Remove the selected item from the stock list
  observeEvent(input$remove, {
    selected <- input$stock_table_rows_selected
    if (!is.null(selected) && length(selected) > 0) {
      current_stock <- stock() %>% arrange(desc(Added_Date))
      selected_row <- current_stock %>% slice(selected)
      updated_stock <- current_stock %>% filter(ID != selected_row$ID)
      stock(updated_stock)
      stock(updated_stock)
      saveRDS(updated_stock, "stock_data.rds")
    }
  })
  
  # Export the stock list to an Excel file
  output$export <- downloadHandler(
    filename = function() {
      paste("refrigerator_stock_list", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(stock(), file)
    }
  )
}

# Run the application 
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)
