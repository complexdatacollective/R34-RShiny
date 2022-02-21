#App to display Partner Services data collected in Network Canvas

#install dependencies if needed
if (!require(shiny))  install.packages("shiny")
# if (!require(igraph)) install.packages("igraph")
if (!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")

#load libraries
library(shiny); library(dplyr); library(DT)
# library(igraph)

# source some data cleaning functions
source("r34_cleaning.R")

#UI
ui <- navbarPage("Partner Services Network Canvas Data Upload",
    tabPanel("Data",
             # Sidebar 
             sidebarLayout(
                 # Sidebar panel 
                 sidebarPanel(
                     
                     # Input: Select a file 
                     fileInput("ego_data", "Upload .zip File",
                               multiple = TRUE,
                               accept = c(".zip")),
                 ),
                 mainPanel(
                     "Something to show the data were correctly loaded here"
                           )
    
             )
                 
    ),
    navbarMenu("Sexual behavior",
               tabPanel("Within Interview Period",
                        "Blank for now"
                     ),
               tabPanel("Within 12 months",
                        fluidRow(
                            column(12,
                                   DTOutput("sexbehav12m"))
                        )
                        
                        )
               ),
    navbarMenu("Substance Use",
               tabPanel("Within Interview Period",
                        "Blank for now"),
               tabPanel("Within 12 months",
                        fluidRow(
                            column(12,
                                   DTOutput("druguse12m"))
                        )
                        ),
               ),
    tabPanel("Referral contacts",
             "Blank for now")


)

#server
server <- function(input, output) {
  
    graph_dat <- reactive({
        
        req(input$ego_data)
        
        return(data_cleaning(input$ego_data$datapath))
        
        # igraph::read_graph(input$ego_data$datapath,"graphml")
        
    }) # now can use graph_dat() to call the data throughout
    
  output$alldata <- renderTable({
      
      req(input$ego_data)
      
      ego_dat <- graph_dat()$egodat
      
      # ego_dat <- unlist(igraph::graph_attr(graph_dat()))
      
      return(ego_dat)
  }) 
  
  output$caseid <- renderTable({
      
      req(input$ego_data)
      
      # vertices_dat <- igraph::as_data_frame(graph_dat(),"vertices")
      
      # vertices_dat$id
      
  }) 
  
  output$sextypes <- renderTable({
    
      req(input$ego_data)
      
      # ego_dat <- igraph::graph_attr(graph_dat())
    
    # df <- read.csv(input$ego_data$datapath)
    
    # data <- c(df[1, 17], df[1,18], df[1,19])
    return(data)
  }) 
  
  output$condomuse <- renderTable({
    
      req(input$ego_data)      
      
      
  })
  
  # output$sexbehav12m <- renderTable({
  #     req(input$ego_data)
  # 
  #     return(graph_dat()$sexbehav12m)
  # }, bordered = TRUE)
  
  output$sexbehav12m <- renderDT({
      req(input$ego_data)
      
      data <-   DT::datatable(graph_dat()$sexbehav12m,
                              options = list(pageLength = 25),
                              class = "cell-border stripe",
                              rownames = FALSE,
                              caption = "Sexual Behaviors within 12 Months",
                              escape = FALSE)
      
      return(data)
  })
  
  output$druguse12m <- renderDT({
      req(input$ego_data)
      
      data <-   DT::datatable(graph_dat()$druguse12m,
                              options = list(pageLength = 25),
                              class = "cell-border stripe",
                              rownames = FALSE,
                              caption = "Sexual Behaviors within 12 Months",
                              escape = FALSE)
      
      return(data)
  })
  
  #labels
  output$caseID <- renderText({"case id: "})
  output$sexTypes <- renderText({"types of sex: "})
  output$condomUse <- renderText({"condom use: "})
}
# Run the app 
shinyApp(ui, server)

#### NOTES:
# is this going to be hosted on a web server? if yes, need to save this as "app.R"? (or could have ui.R and server.R)
