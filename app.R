# This app uses for testing multiple selectors and DT table interactivity 
library(shiny)
library(shinydashboard)
library(DT)


 
  
# Data: "iris"
myData <- iris
# Add identifiers to the records by using row indices
myData$record_id <- as.numeric(rownames(myData))



  
# Define server logic 
server <- function(session,input, output) {
  
  # Reactive value for sepecies selection (for synchronizing sepecies selector in tab 1 and 2)
  species_reactive <- reactiveVal()
  

  # Tab 1 for Species selector and DT table
  
  # Species selector (server side process)
  output$species_sel <- renderUI({
    selectInput(inputId = "species",
                label = "Species:",
                choices = unique(myData$Species),
                selected = species_reactive())
  })
  
  # Update selector value to reactive value when selection is made
  observeEvent(input$species,{
    species_reactive(input$species)
    updateSelectInput(session, 'id_sel')
  })
  
  
  # Data table
  output$dt_table <- renderDataTable({
    datatable(
      # Data filtered by selecor "species_sel"
      data <- subset(myData,myData$Species == species_reactive()),
      # Single selection, selected whole row
      selection = list(target = 'row', mode = 'single'),
      # Table Style
      class = 'cell-border strip hover' 
    ) %>% formatStyle('record_id', cursor = 'pointer')    # Point changes when hover on record_id column 


  })
  

  
  
  
  # Tab 2 for Species selector and Record Id selector
  
  # Sepecies selector in Tab 2
  output$species_sel2 <- renderUI({
    selectInput(inputId = "species2",
                label = "Species:",
                choices = unique(myData$Species),
                selected = species_reactive())
  })
  
  # Update selector value to reactive value when selection is made
  observeEvent(input$species2,{
    species_reactive(input$species2)
    updateSelectInput(session, 'id_sel')
  })
  
  # Reactive value for Record ID
  id_reactive <- reactiveVal()
  
  
  # Record Id selector dependent on species selector
  output$id_sel <- renderUI({
    
    # Subset data based on species selection
    myData <- subset(myData,myData$Species == species_reactive())
    
    # Selector
    selectInput(inputId = "id_sel",
                label = "Record ID:",
                choices = unique(myData$record_id) ,
                selected = id_reactive()
    )
    
  })
  

  # Update id selector
  observeEvent(input$id_sel,{
    id_reactive(input$id_sel)
  })

  
  
  # Update id selector by observing the selection in DT table
  observeEvent(input$dt_table_cell_clicked, {
    info = input$dt_table_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 6yh column
    if (is.null(info$value) || info$col != 6) return()
    id_reactive(info$value)
    updateSelectInput(session, 'id_sel', selected = id_reactive())
    
    #tab  change
    newtab <- switch(input$tabs,
                     "DTtable" = "selectors")
    updateTabItems(session, "tabs", newtab)
  })
  
  
  # Print selected value
  output$selected_print <- renderUI({
    HTML("input$species:", input$species,"<br/>",
         "input$species2:", input$species2,"<br/>",
         "species_reactive:", paste0(species_reactive()),"<br/>",
         "input$id_sel:", input$id_sel,"<br/>",
         "id_reactive()", paste0(id_reactive()),"<br/>")
  })
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Header
  dashboardHeader(title = 'Iris'),
  
  # Side Bar
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data Table", tabName = "DTtable", icon = icon("list")),
      menuItem("Selectors", tabName = "selectors", icon = icon("bar-chart"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    
    tabItems(
      
      # Tab 1: Iris DT table
      tabItem(
        tabName = "DTtable",
        # Title for this page
        h3("Iris"),
        h4("Click on the record_id cell to switch tab"),
        # Species Selecor
        htmlOutput("species_sel"),
        # Data Table
        dataTableOutput("dt_table")
        
      ),
      
      # Tab 2: Iris Species and ID selectors
      tabItem(
        tabName = "selectors",
        # Title
        h3("Iris Species and ID selectors"),
        # Species Selecor
        htmlOutput("species_sel2"),
        # ID Selecor
        htmlOutput("id_sel"),
        # Print Selected Values
        htmlOutput("selected_print")
      )
    )
  )
)



# Run the application 
shinyApp(ui = ui, server = server)

