#App to display Partner Services data collected in Network Canvas

#install dependencies if needed
if (!require(shiny))  install.packages("shiny")
if (!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(rclipboard)) install.packages("rclipboard")

#load libraries
library(shiny); library(dplyr); library(DT); library(rclipboard)

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
                     # this textOuput checks that the data have been loaded correctly
                     # and the number of referral contacts included
                     # probably could do something fancier, but not sure it's necessary
                     textOutput("check_load"),
                     value = "Data",
                     
                     fluidRow(
                         column(6,
                                # these actionButtons allow us to navigate between
                                # the different navbarpage/menu/panels/etc
                                # this one uses the observeEvent 'jumpToSexint"
                                # has the text "Next" on it, and has width 200px
                                actionButton('jumpToVenues','Next',width='200px')
                                ),
                              ),
                           )
  
    
             )
                 
    ),
    tabPanel(title = "Venues",
             value = "venues",
             h3("Venues"),
             fluidRow(
               rclipboardSetup(),
               column(12, 
                      # this selectInput allows people to choose which contact
                      # the data will be displayed for - choices here gets updated
                      # using the observe in the server function
                      selectInput('Venues','Venues:',choices = 1)),
               column(12,
                      # output our datatable w/venues
                      DT::dataTableOutput("venues")),
               column(3,
                      # another navigation button
                      actionButton('jumpToData','Previous',width='200px')),
               column(3,
                      # another navigation button
                      actionButton('jumpToSexint','Next',width='200px'))
             ),
             br()
    ),
    navbarMenu("Sexual behavior",
                   tabPanel(title = "Within Interview Period",
                            value = "interview",
                            h3("Sexual Behavior Within Interview Period"),
                            fluidRow(
                              rclipboardSetup(),
                              column(12, 
                                     # this selectInput allows people to choose which interview period
                                     # the data will be displayed for 
                                     selectInput('IP','Interview Period:',
                                                 choices = list("3 months", "7 months", "12 months"))),
                              column(12,
                                     #select interview period start and end dates
                                     dateInput("date_start", "Interview period start date: ")),
                              column(12,
                                     # output our datatable w/sexual behaviour in selected interview period
                                     DT::dataTableOutput("sexbehavIP")),
                              column(3,
                                     # another navigation button
                                     actionButton('jumpToVenues','Previous',width='200px')),
                              column(3,
                                     # another navigation button
                                     actionButton('jumpToSex12m','Next',width='200px'))
                            ),
                            
                   ),
                   tabPanel("Within 12 months",value = "12m",
                            h3("Sexual Behavior Within 12 months"),
                            fluidRow(
                                # I don't know if you need the rclipboardSetup()
                                # on each tabpanel/etc or just once, but doesn't seem
                                # to hurt having it on each one - this is so that we
                                # can have copy buttons in the datatable outputted, "sexbehav12m"
                                rclipboardSetup(),
                                column(12,
                                       # output our datatable w/sexual behaviour in the past 12 months
                                       DT::dataTableOutput("sexbehav12m")),
                                column(3,
                                       # another navigation button
                                       actionButton('jumpBackToSexint','Previous',width='200px')),
                                column(3,
                                       # another navigation button
                                       actionButton('jumpToSub12m','Next',width='200px'))
                            ),
                            # added a line break to make there be a little white space below
                            # the buttons
                            br()
                   )
                
               ),
    navbarMenu("Substance Use",
               tabPanel(title = "Within 12 months",
                        value = "12msub",
                        h3("Substance Use Within 12 months"),
                        fluidRow(
                            # allows for the copy buttons in "druguse12m" table
                            rclipboardSetup(),
                            column(12,
                                   # datatable with drug use in the previous 12 months
                                   DTOutput("druguse12m")),
                            column(3,
                                   # another navigation button
                                   actionButton('jumpBackToSex12m','Previous',width='200px')),
                            column(3,
                                   # another navigation button
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
                        # this selectInput allows people to choose which contact
                        # the data will be displayed for - choices here gets updated
                        # using the observe in the server function
                        selectInput('Contact','Contact:',choices = 1)),
                 column(12,
                        # data table with referral contact info
                        DTOutput("referral_table")),
                 column(6,
                        # another navigation button
                        actionButton('jumpBackToSub12m','Previous',width='200px'))
             ),
             br()
             )


)

#server
server <- function(input, output) {
  
    # use the reactive() function to allow us to have an object we can use throughout
    # that stores the cleaned data - after running this 
    # can use graph_dat() to call the data throughout
    graph_dat <- reactive({
        
        req(input$all_data)
        req(input$date_start)
        
        # run our data_cleaning function from the r34_cleaning script
        return(data_cleaning(input$all_data$datapath, input$date_start))
        
    }) 
    
    # this just checks that our data has loaded in and creates a message to tell the user
    # that it was loaded ok
    output$check_load <- renderText({
        req(input$all_data)
        
        
        # is there something called "egodat" in our graph_dat() object?
        check1 <- "egodat" %in% names(graph_dat())
        # how many referrals did we have (first column in "contact_referral" dataframe
        # is the responses, so the number of columns - 1 is the number of referrals)
        check2 <- ncol(graph_dat()$contact_referral)-1
        if(check1) {
            # concatenate a string with our check
            outstring <- paste0("Respondent data successfully loaded, with ",check2,
                                " contact referrals provided.")
        } else {
            # if not tell the user that the zip file they provided didn't have what
            # we expected it to have
            outstring <- "No respondent data loaded."
        }
        return(outstring)
    })
  
    # data table with all sexual behavior for the past 12 months in it
  output$sexbehav12m <- renderDT({
      req(input$all_data)
      
      data <- graph_dat()$sexbehav12m
      # add a column with a "Copy" button - this is super finnicky and I don't
      # understand how the rclipButton function works - I guess it's outputting
      # HTML for the clip button, and then because we use "escape=FALSE" below
      # that HTML gets rendered into a clip button...
      data$Copy <- unlist(lapply(data$Responses,
                                 function(x) {
                                     rclipButton(
                                         # not sure what this does
                                         inputId = "clipbtn",
                                         # the button will say "Copy" on it
                                         label = "Copy",
                                         # text that's getting copied is data$Responses
                                         clipText = x,
                                         # there'll be a little picture of a clipboard
                                         icon = icon("clipboard")
                                     ) %>% as.character()
                                 }))
      
      data <-   DT::datatable(data,
                              # this tells us how many things in the table we want to have
                              # on a single page, could fiddle with this instead of 25
                              options = list(pageLength = 25),
                              # this is just an appearance thing
                              class = "cell-border stripe",
                              # turn off rownames
                              rownames = FALSE,
                              # Need "escape = FALSE" to render the HTML for the clip button
                              # correctly
                              escape = FALSE)
      
      return(data)
  })
  
  # data table with sexual behavior for interview period
  # determines which interview period was selected and renders the corresponding table
  output$sexbehavIP <- renderDT({
    req(input$all_data)
    
    #if statement, if choice is 90 days, render 90 days, etc
    
    if (input$IP == "3 months") {
      data <- graph_dat()$sexbehav90days
    }
    if (input$IP == "7 months"){
      data <- graph_dat()$sexbehav7mo
    }
    if (input$IP == "12 months"){
      data <- graph_dat()$sexbehav12m
    }
    
    # add a column with a "Copy" button
    data$Copy <- unlist(lapply(data$Responses,
                               function(x) {
                                 rclipButton(
                                   # not sure what this does
                                   inputId = "clipbtn",
                                   # the button will say "Copy" on it
                                   label = "Copy",
                                   # text that's getting copied is data$Responses
                                   clipText = x,
                                   # there'll be a little picture of a clipboard
                                   icon = icon("clipboard")
                                 ) %>% as.character()
                               }))
    
    data <-   DT::datatable(data,
                            # this tells us how many things in the table we want to have
                            # on a single page, could fiddle with this instead of 25
                            options = list(pageLength = 25),
                            # this is just an appearance thing
                            class = "cell-border stripe",
                            # turn off rownames
                            rownames = FALSE,
                            # Need "escape = FALSE" to render the HTML for the clip button
                            # correctly
                            escape = FALSE)
    
    return(data)
  })
  
  
  # This one is basically identical to the sexbehav12m one but for druguse12m
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
  
  
  
  
  # These are all of the observeEvents for the navigation buttons
  # tell us that when someone has clicked on the different buttons, which
  # page do they want to move to (the "selected" argument) - these names
  # are in the "value" argument of the tabPanel etc
  observeEvent(input$jumpToSexint, {
      updateNavbarPage(inputId = "navpage",
                        selected = "interview")
  })
  
  observeEvent(input$jumpToData, {
      updateNavbarPage(inputId = "navpage",
                       selected = "Data")
  })
  
  observeEvent(input$jumpToVenues, {
    updateNavbarPage(inputId= "navpage",
                     selected = "venues")
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
  
  # This updates the Contact dropdown to respond to the number of contact referrals in the 
  # graph_dat()$contact_referral object (ncols-1 because first column is "Response" column)
  observe({
      updateSelectInput(inputId="Contact", choices = 1:(ncol(graph_dat()$contact_referral)-1))
  })
 
  # This updates the Venues dropdown to respond to the number of venues  in the 
  # graph_dat()$venues object (ncols-1 because first column is "Response" column)
  observe({
    updateSelectInput(inputId="Venues", choices = 1:(ncol(graph_dat()$venues)-1))
  }) 
  
  
  # This table is similar to the sexbehav12m and druguse12m but adds in needing to figure out
  # which referral we want to be displaying data for
  output$referral_table <- renderDT({
      req(input$all_data)
      
      # choosing to only display the contact_referral columns 1 ("Responses" column)
      # and as.numeric(input$Contact)+1 which is using the dropdown menu input
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
  
  output$venues <- renderDT({
    req(input$all_data)
    
    #choosing to display venues columns 1 ("Responses" column)
    # and as.numeric(input$Venues)+1 which is using the dropdown menu input
    data <- graph_dat()$venues[,c(1,as.numeric(input$Venues)+1)]
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

