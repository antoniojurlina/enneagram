library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)

load("data.RData")

ui <- fluidPage(
    tags$head(
        HTML(
            "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
        )
    ),
    navbarPage('Enneagram', id = "enneagram", 
               tabPanel("Test", value = "test",
                        fluidRow(
                            column(12, align="center", 
                                   lapply(1:144, function(i) {radioGroupButtons(
                                       inputId = paste0('a', i),
                                       label = paste('Number', i),
                                       choices = c(paste0(data$question[2*i-1]), 
                                                   paste0(data$question[2*i])),
                                       status = "primary",
                                       selected = NA,
                                       checkIcon = list(
                                           yes = icon("ok", 
                                                      lib = "glyphicon"),
                                           no = icon("remove",
                                                     lib = "glyphicon")),
                                       direction = "vertical",
                                       width = "100%"
                                   )}),
                                   
                                   actionBttn(
                                       inputId = "calculate",
                                       label = "Calculate",
                                       style = "unite", 
                                       color = "primary"
                                   ),
                                   HTML("<br><br><br><br><br>")
                                   )
                        )
                          
               ),
               tabPanel("Results", value = "results",
                        sidebarLayout(sidebarPanel(uiOutput("info1"),
                                                   textOutput("text"),
                                                   uiOutput("info2")),
                                      mainPanel(textOutput("error"),
                                                plotOutput("cplot", 
                                                           width = "100%"))
                                      )
                        ),
               tabPanel("Data", value = "data",
                        sidebarLayout(sidebarPanel(downloadButton("downloadData", 
                                                                  "Download entire data set")),
                                      mainPanel(tableOutput("answer"),
                                                textOutput("keepAlive")))
                                     )
             )
)

server <- function(input, output, session) {
    
    rv_answers <- reactive({
        lapply(1:144, function(i) input[[paste0('a', i)]])
    })
    
    observeEvent(input$calculate, {
        updateTabsetPanel(session, "enneagram",
                          selected = "results")
    })
    
    rv_filtered <- eventReactive(input$calculate, {
        data %>%
            filter(question %in% rv_answers()) %>%
            group_by(type) %>%
            summarize(count = n(),
                      triad = first(triad),
                      name = first(name),
                      number = first(number))
    })
    
    output$info1 <- renderUI({
        row <- which(rv_filtered()$count == max(rv_filtered()$count))
        type <- rv_filtered()$type[row]
        
        if(type == "One") {
            tagList(tags$h3("1. THE REFORMER"))
        } else if(type == "Two") {
            tagList(tags$h3("2. THE HELPER"))
        } else if(type == "Three") {
            tagList(tags$h3("3. THE ACHIEVER"))
        } else if(type == "Four") {
            tagList(tags$h3("4. THE INDIVIDUALIST"))
        } else if(type == "Five") {
            tagList(tags$h3("5. THE INVESTIGATOR"))
        } else if(type == "Six") {
            tagList(tags$h3("6. THE LOYALIST"))
        } else if(type == "Seven") {
            tagList(tags$h3("7. THE ENTHUSIAST"))
        } else if(type == "Eight") {
            tagList(tags$h3("8. THE CHALLENGER"))
        } else {
            tagList(tags$h3("9. THE PEACEMAKER"))
        }
    })
    
    output$text <- renderText({
        row <- which(rv_filtered()$count == max(rv_filtered()$count))
        number <- rv_filtered()$number[row]
        triad <- rv_filtered()$triad[row]
        
        if (rv_filtered()$number[row] == 9) {
            wing1 <- 8
            wing2 <- 1
            
            if(rv_filtered()$count[rv_filtered()$number == wing1] > rv_filtered()$count[rv_filtered()$number == wing2]) {
                wing <- wing1
            } else {
                wing <- wing2
            }
            
        } else if (rv_filtered()$number[row] == 1) {
            wing1 <- 9
            wing2 <- 2
            
            if(rv_filtered()$count[rv_filtered()$number == wing1] > rv_filtered()$count[rv_filtered()$number == wing2]) {
                wing <- wing1
            } else {
                wing <- wing2
            }
            
        } else {
            wing1 <- rv_filtered()$number[row]-1
            wing2 <- rv_filtered()$number[row]+1
            
            if(rv_filtered()$count[rv_filtered()$number == wing1] > rv_filtered()$count[rv_filtered()$number == wing2]) {
                wing <- wing1
            } else {
                wing <- wing2
            }
        }
        
        paste0(number, "w", wing)
        
    })
    
    output$info2 <- renderUI({
        row <- which(rv_filtered()$count == max(rv_filtered()$count))
        type <- rv_filtered()$type[row]
        
        if(type == "One") {
            tagList(br(),
                    "The Rational, Idealistic Type: Principled, Purposeful, 
                    Self-Controlled, and Perfectionistic",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-1"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/nine-enneagram-types"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-1-perfectionist"))
        } else if(type == "Two") {
            tagList(br(),
                    "The Caring, Interpersonal Type: Demonstrative, Generous, 
                    People-Pleasing, and Possessive",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-2"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-2-helper"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-2-giver"))
        } else if(type == "Three") {
            tagList(br(),
                    "The Success-Oriented, Pragmatic Type: Adaptive, Excelling,
                    Driven, and Image-Conscious",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-3"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-3-performer"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-3-achiever"))
        } else if(type == "Four") {
            tagList(br(),
                    "The Sensitive, Withdrawn Type: Expressive, Dramatic, 
                    Self-Absorbed, and Temperamental",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-4"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-4-romantic"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-4-individualist"))
        } else if(type == "Five") {
            tagList(br(),
                    "The Intense, Cerebral Type: Perceptive, Innovative, 
                    Secretive, and Isolated",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-5"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-5-observer"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-5-investigator"))
        } else if(type == "Six") {
            tagList(br(),
                    "The Committed, Security-Oriented Type: Engaging, 
                    Responsible, Anxious, and Suspicious",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-6"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-6-loyal-skeptic"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-6-skeptic"))
        } else if(type == "Seven") {
            tagList(br(),
                    "The Busy, Fun-Loving Type: Spontaneous, Versatile, 
                    Distractible, and Scattered",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-7"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-7-epicure"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-7-enthusiast"))
        } else if(type == "Eight") {
            tagList(br(),
                    "The Powerful, Dominating Type: Self-Confident, Decisive,
                    Willful, and Confrontational",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-8"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-8-protector"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-8-challenger"))
        } else {
            tagList(br(),
                    "The Easygoing, Self-Effacing Type: Receptive, Reassuring,
                    Agreeable, and Complacent",
                    br(), br(),
                    "For more information about your type:",
                    br(),
                    a("The Enneagram Institute", 
                      href="https://www.enneagraminstitute.com/type-9"),
                    br(),
                    a("The Enneagram At Work", 
                      href="https://theenneagramatwork.com/type-9-mediator"),
                    br(),
                    a("Truity", 
                      href="https://www.truity.com/enneagram/personality-type-9-peacemaker"))
        }
        
    })
    
    output$error <- renderText({
        questions <- c(1:144)
        answered <- data %>%
            filter(question %in% rv_answers()) %>%
            select(question_no) %>%
            unique() %>%
            unlist()
        
        missed <- dplyr::setdiff(questions, answered)
        
        if(length(answered) != 144) {
            paste("You missed question(s): ", 
                  paste(missed, collapse = ", "))
        }

    })
    
    output$cplot <- renderPlot({
        answered <- data %>%
            filter(question %in% rv_answers()) %>%
            select(question_no) %>%
            unique() %>%
            unlist()
        
        if(length(answered) == 144) {
            rv_filtered() %>%
            ggplot(aes(x = factor(type, 
                                  levels = c("One", "Two", "Three", 
                                             "Four", "Five", "Six",
                                             "Seven", "Eight", "Nine")),
                               y = count)) +
                coord_polar(start = 20*pi/180) +
                geom_col(aes(fill = triad), color = "grey40", alpha = 0.7) +
                geom_hline(yintercept = c(8, 12, 16, 20, 24), linetype = "dashed", size = 0.3, color = "grey40") +
                geom_text(aes(2.4*pi, 7.5, label = "low", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
                geom_text(aes(2.4*pi, 11.5, label = "below average", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
                geom_text(aes(2.4*pi, 15.5, label = "median", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
                geom_text(aes(2.4*pi, 19.5, label = "above average", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
                geom_text(aes(2.4*pi, 23.5, label = "high", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
                scale_color_ptol() +
                scale_fill_ptol() +
                theme_minimal() +
                theme(axis.title = element_blank(),
                      axis.text.y = element_blank(),
                      legend.title = element_blank(),
                      axis.text.x = element_text(size = 13, face = "bold"),
                      legend.position = "top")
        }
        
    }, width = 800, height = 600)
    
    output$answer <- renderTable({ 
        rv_filtered() %>%
            select(-number) %>%
            arrange(desc(count))
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("enneagram_data", ".csv")
        },
        content = function(file) {
            write_csv(data, file)
        }
    )
    
    output$keepAlive <- renderText({
        req(input$count)
        paste("keep alive ", input$count)
    })
}

shinyApp(ui, server)