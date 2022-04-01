#App to display Partner Services data collected in Network Canvas

#install dependencies if needed
if (!require(shiny))  install.packages("shiny")
# if (!require(igraph)) install.packages("igraph")
if (!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(rclipboard)) install.packages("rclipboard")


#load libraries
library(shiny); library(dplyr); library(DT); library(rclipboard)
# library(igraph)

# source some data cleaning functions
source("r34_cleaning.R")

#UI
ui <- navbarPage("Partner Services Network Canvas Data Upload",
                 id = "navpage",
                 
    tabPanel("Data",
             # Sidebar 
             sidebarLayout(
                 # Sidebar panel 
                 sidebarPanel(
                     
                     # Input: Select a file 
                     fileInput("all_data", "Upload .zip File",
                               multiple = TRUE,
                               accept = c(".zip")),
                 ),
                 mainPanel(
                     textOutput("check_load"),
                     value = "Data",
                     
                     fluidRow(
                         column(6,
                                actionButton('jumpToSexint','Next',width='200px')
                                ),
                         # textOutput("text"),
                              ),
                           )
  
    
             )
                 
    ),
    navbarMenu("Sexual behavior",
                   tabPanel(title = "Within Interview Period",
                            value = "interview",
                            h3("Sexual Behavior Within Interview Period"),
                            fluidRow(
                                column(3,
                                       actionButton('jumpToData','Previous',width='200px')),
                                column(3,
                                       actionButton('jumpToSex12m','Next',width='200px'))
                            )
                            
                   ),
                   tabPanel("Within 12 months",value = "12m",
                            h3("Sexual Behavior Within 12 months"),
                            fluidRow(
                                rclipboardSetup(),
                                column(12,
                                       DT::dataTableOutput("sexbehav12m")),
                                column(3,
                                       actionButton('jumpBackToSexint','Previous',width='200px')),
                                column(3,
                                       actionButton('jumpToSubint','Next',width='200px'))
                            ),
                            br()
                   )
                
               ),
    navbarMenu("Substance Use",
               tabPanel(title = "Within Interview Period",
                        value = "interviewsub",
                        h3("Substance Use Within Interview Period"),
                        fluidRow(
                            column(3,
                                   actionButton('jumpBackToSex12m','Previous',width='200px')),
                            column(3,
                                   actionButton('jumpToSub12m','Next',width='200px'))
                        )),
               tabPanel(title = "Within 12 months",
                        value = "12msub",
                        h3("Substance Use Within 12 months"),
                        fluidRow(
                            rclipboardSetup(),
                            column(12,
                                   DTOutput("druguse12m")),
                            column(3,
                                   actionButton('jumpBackToSubint','Previous',width='200px')),
                            column(3,
                                   actionButton('jumpToReferrals','Next',width='200px'))
                        ),
                        br()
                        ),
               ),
    tabPanel(title = "Referral contacts",
             value = "Referral",
             h3("Referral Contacts"),
             fluidRow(
                 column(12, 
                        selectInput('Contact','Contact:',choices = 1)),
                 column(12,
                        DTOutput("referral_table")),
                 column(6,
                        actionButton('jumpBackToSub12m','Previous',width='200px'))
             ),
             br()
             )


)

#server
server <- function(input, output) {
  
    graph_dat <- reactive({
        
        req(input$all_data)
        
        return(data_cleaning(input$all_data$datapath))
        
        # igraph::read_graph(input$all_data$datapath,"graphml")
        
    }) # now can use graph_dat() to call the data throughout
    
  # output$alldata <- renderTable({
  #     
  #     req(input$all_data)
  #     
  #     ego_dat <- graph_dat()$egodat
  #     
  #     # ego_dat <- unlist(igraph::graph_attr(graph_dat()))
  #     
  #     return(ego_dat)
  # }) 
  
  # output$caseid <- renderTable({
  #     
  #     req(input$all_data)
  #     
  #     # vertices_dat <- igraph::as_data_frame(graph_dat(),"vertices")
  #     
  #     # vertices_dat$id
  #     
  # }) 
  
  # output$sextypes <- renderTable({
  #   
  #     req(input$all_data)
  #     
  #     # ego_dat <- igraph::graph_attr(graph_dat())
  #   
  #   # df <- read.csv(input$all_data$datapath)
  #   
  #   # data <- c(df[1, 17], df[1,18], df[1,19])
  #   return(data)
  # }) 
  # 
  # output$condomuse <- renderTable({
  #   
  #     req(input$all_data)      
  #     
  #     
  # })
  
  # output$sexbehav12m <- renderTable({
  #     req(input$all_data)
  # 
  #     return(graph_dat()$sexbehav12m)
  # }, bordered = TRUE)
    
    output$check_load <- renderText({
        req(input$all_data)
        
        check1 <- "egodat" %in% names(graph_dat())
        check2 <- ncol(graph_dat()$contact_referral)-1
        if(check1) {
            outstring <- paste0("Respondent data successfully loaded, with ",check2,
                                " contact referrals provided.")
        } else {
            outstring <- "No respondent data loaded."
        }
        return(outstring)
    })
  
  output$sexbehav12m <- renderDT({
      req(input$all_data)
      
      data <- graph_dat()$sexbehav12m
      data$Copy <- unlist(lapply(data$Responses,
                                 function(x) {
                                     rclipButton(
                                         inputId = "clipbtn",
                                         label = "Copy",
                                         clipText = x,
                                         icon = icon("clipboard")
                                     ) %>% as.character()
                                 }))
      
      data <-   DT::datatable(data,
                              options = list(pageLength = 25),
                              class = "cell-border stripe",
                              rownames = FALSE,
                              # caption = "Sexual Behaviors within 12 Months",
                              escape = FALSE)
      
      return(data)
  })
  
  output$druguse12m <- renderDT({
      req(input$all_data)
      
      data <- graph_dat()$druguse12m
      data$Copy <- unlist(lapply(data$Responses,
                                 function(x) {
                                     rclipButton(
                                         inputId = "clipbtn",
                                         label = "Copy",
                                         clipText = x,
                                         icon = icon("clipboard")
                                     ) %>% as.character()
                                 }))
      
      data <-   DT::datatable(data,
                              options = list(pageLength = 25),
                              class = "cell-border stripe",
                              rownames = FALSE,
                              # caption = "Substance use within 12 Months",
                              escape = FALSE)
      
      return(data)
  })
  
  #labels
  # output$caseID <- renderText({"case id: "})
  # output$sexTypes <- renderText({"types of sex: "})
  # output$condomUse <- renderText({"condom use: "})
  # output$text <- renderText({"Copy This!"})
  
  observeEvent(input$jumpToSexint, {
      updateNavbarPage(inputId = "navpage",
                        selected = "interview")
  })
  
  observeEvent(input$jumpToData, {
      updateNavbarPage(inputId = "navpage",
                       selected = "Data")
  })
  
  observeEvent(input$jumpToSex12m, {
      updateNavbarPage(inputId = "navpage",
                       selected = "12m")
  })
  
  observeEvent(input$jumpBackToSexint, {
      updateNavbarPage(inputId = "navpage",
                       selected = "interview")
  })
  
  observeEvent(input$jumpToSubint, {
      updateNavbarPage(inputId = "navpage",
                       selected = "interviewsub")
  })
  
  observeEvent(input$jumpBackToSex12m, {
      updateNavbarPage(inputId = "navpage",
                       selected = "12m")
  })
  
  observeEvent(input$jumpBackToSubint, {
      updateNavbarPage(inputId = "navpage",
                       selected = "interviewsub")
  })
  
  observeEvent(input$jumpToSub12m, {
      updateNavbarPage(inputId = "navpage",
                       selected = "12msub")
  })
  
  observeEvent(input$jumpBackToSub12m, {
      updateNavbarPage(inputId = "navpage",
                       selected = "12msub")
  })
  
  observeEvent(input$jumpToReferrals, {
      updateNavbarPage(inputId = "navpage",
                       selected = "Referral")
  })
  
  # Add clipboard buttons
  # output$clip <- renderUI({
  #     output$clip <- renderUI({
  #         rclipButton(
  #             inputId = "clipbtn",
  #             label = "Copy",
  #             clipText = graph_dat()$sexbehav12m[15,2], 
  #             icon = icon("clipboard")
  #         )
  #     })
  # })
  
  observe({
      updateSelectInput(inputId="Contact", choices = 1:(ncol(graph_dat()$contact_referral)-1))
  })
  
  output$referral_table <- renderDT({
      req(input$all_data)
      
      data <- graph_dat()$contact_referral[,c(1,as.numeric(input$Contact)+1)]
      names(data)[2] <- "Responses"
      data$Copy <- unlist(lapply(data[,2],
                                 function(x) {
                                     rclipButton(
                                         inputId = "clipbtn",
                                         label = "Copy",
                                         clipText = x,
                                         icon = icon("clipboard")
                                     ) %>% as.character()
                                 }))

      data <-   DT::datatable(data,
                              options = list(pageLength = 25),
                              class = "cell-border stripe",
                              rownames = FALSE,
                              # caption = "Substance use within 12 Months",
                              escape = FALSE)
      
      return(data)
  })

}
# Run the app 
shinyApp(ui, server)

#### NOTES:
# is this going to be hosted on a web server? if yes, need to save this as "app.R"? (or could have ui.R and server.R)

### TO DO:
# - make the question sections collapsible? and automatically collapse no's 
#       - errors out even in example code...: (https://www.rdocumentation.org/packages/shinydashboardPlus/versions/0.8.0.9000/topics/accordion)
#       - might need to be built into the context of a dashboard...: (https://cran.r-project.org/web/packages/bs4Dash/vignettes/extra-elements.html)
# - button that copies adjacent text to the clipboard (https://cran.r-project.org/web/packages/rclipboard/readme/README.html)
# - deal w/missing data!

