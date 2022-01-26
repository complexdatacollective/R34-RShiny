#UI
ui <- fluidPage(
  
  # App title
  titlePanel("Partner Services Network Canvas Data Upload"),
  
  # Sidebar 
  sidebarLayout(
    
    # Sidebar panel 
    sidebarPanel(
      
      # Input: Select a file 
      fileInput("ego_data", "Upload .graphml File",
                multiple = TRUE,
                accept = c(".graphml")),
    ), 
    
    # Main panel for displaying outputs 
    mainPanel(
      
      titlePanel("Question Package Name"),
      
      # Output
      column(3, 
             verbatimTextOutput("caseID")),
      column(10, 
             tableOutput("caseid")),
      column(3, 
             verbatimTextOutput("sexTypes")),
      column(10, 
             tableOutput("sextypes")),
      column(3, 
             verbatimTextOutput("condomUse")),
      column(10, 
             tableOutput("condomuse")),
      column(10, 
             tableOutput("alldata")),
      
    )
    
  )
)

#server
server <- function(input, output) {
  
  output$alldata <- renderTable({
    
    req(input$ego_data)
    
    df <- read.csv(input$ego_data$datapath)
    
    return(df)
  }) 
  
  output$caseid <- renderTable({
    
    req(input$ego_data)
    
    df <- read.csv(input$ego_data$datapath)
    
    return(df[1,2])
  }) 
  
  output$sextypes <- renderTable({
    
    req(input$ego_data)
    
    df <- read.csv(input$ego_data$datapath)
    
    data <- c(df[1, 17], df[1,18], df[1,19])
    return(data)
  }) 
  
  output$condomuse <- renderTable({
    
    req(input$ego_data)
    
    df <- read.csv(input$ego_data$datapath)
    
    data <- c(df[1, 12], df[1,13], df[1,14], df[1,15], df[1,16])
    return(data)
  })
  
  #labels
  output$caseID <- renderText({"case id: "})
  output$sexTypes <- renderText({"types of sex: "})
  output$condomUse <- renderText({"condom use: "})
}
# Run the app 
shinyApp(ui, server)