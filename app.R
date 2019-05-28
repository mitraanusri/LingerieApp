library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)



# load the data (retrieve and clean raw data if this is the first time)
Lingerie <- read.csv("LingerieData.csv")



ui <- shinyUI(fluidPage(
  
  
  
  titlePanel(
    
    h1(style = "font-family: Segoe Script; color: black; text-align: center ", "Lingerie ")),  
  
  hr(),

  navbarPage("Navbar",
             
             tabPanel("Lingerie Graph",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h4(
                            "If love is blind, why is lingerie so popular? -Dorothy Parker."
                          ),
                          
                          h4(
                            "This app will help you find the best lingerie for you! :p  Just use the filters below..."
                          ),
                          br(),
                          
                          sliderInput("mrpInput", "Retail Price", 0, 100, c(25, 40), pre = "$"),
                          uiOutput("colorSelectOutput"),
                          checkboxInput("filterCategory", "Filter by product category", FALSE),
                          conditionalPanel(
                            condition = "input.filterCategory",
                            uiOutput("categorySelectorOutput")
                          ),
                          
                          
                          
                          
                          h6("TOP BRANDS:"),
                          tags$img(src = 'topshop.jpg', height=80, width=80 ), 
                          tags$img(src = '1.png', height=80, width=80),
                          tags$img(src = '0.png', height=80, width=80), 
                          tags$img(src = '0.gif', height=80, width=80),
                          
                          h6("BEST SELLING PRODUCTS: "),
                          tags$img(src = 's1.jpg', height=150, width=140 ), 
                          tags$img(src = 's2.jpg', height=150, width=140),
                          tags$img(src = 's3.jpg', height=150, width=140)
                          
                          
                          
                        ),
                        
                        mainPanel(style = "color: hotpink; background-color: purple",
                                  setBackgroundColor("hotpink"),
                                  
                                  h3(textOutput("summaryText")),
                                  
                                  
                                  br(),
                                  plotOutput("plot"),
                                  br(), br()
                                  #tableOutput("price")
                                  #DT::dataTableOutput("mrp")
                        )
                      )
             ),
             
             
             tabPanel("Lingerie Data Table",
                      
                      
                      # Show Word Cloud
                      mainPanel(
                        DT::dataTableOutput("mrp")
                      )
             )
  )
)
)


server <- function(input, output, session) {
  
  output$categorySelectorOutput <- renderUI({
    selectInput("categoryInput", "Category",
                sort(unique(Lingerie$product_category)),
                selected = "sleepwear")
  })
  
  output$colorSelectOutput <- renderUI({
    selectInput("colorInput", "Color of Lingerie",
                sort(unique(Lingerie$color)),
                multiple = TRUE,
                selected = c("dark midnight", "viva pink"))
  })
  
  output$summaryText <- renderText({
    numOptions <- nrow(mrp())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })
  
  mrp <- reactive({
    mrp <- Lingerie
    
    if (is.null(input$categoryInput)) {
      return(NULL)
    }
    
    mrp <- dplyr::filter(mrp, color %in% input$colorInput)
    if (input$filterCategory) {
      mrp <- dplyr::filter(mrp, product_category == input$categoryInput)
    }
    mrp <- dplyr::filter(mrp, mrp >= input$mrpInput[1],
                         mrp <= input$mrpInput[2])
    
    if(nrow(mrp) == 0) {
      return(NULL)
    }
    mrp
  })
  
  output$plot <- renderPlot({
    if (is.null(mrp())) {
      return(NULL)
    }
    
    ggplot(mrp(), aes(x=brand, fill = color)) +
      geom_bar() + coord_flip() +
      theme_classic(20)
  })
  
  output$mrp <- DT::renderDataTable({
    mrp()
  })
  
  
  
}

shinyApp(ui = ui, server = server)



