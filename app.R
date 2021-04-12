#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app is a morbidity calculator for infectious disease. It predicts the 
# number of people who might die in a population of an input size. The user can
# input the predicted rate of death for the infection, the population size, the
# percentage of population who is, or might be, infected. In addition, the user 
# can add in their own prediction numbers and see what the likelihood of their 
# prediction being the minimum number of deaths.

library(shiny)

# Defines the UI for an app that calculates probable mortality
ui <- fluidPage(
    titlePanel("Infectious disease mortality calculator"),
    sidebarLayout(
        sidebarPanel(
            h4("This app is designed to measure mortality probabilities in a community due to an infectious
         disease, like COVID-19."),
            # numeric input for the percentage of infected individuals who die due to the infection
            numericInput("numericprob", "What is the probability of death? Type as a percentage.", 
                         value = 1, max = 100), 
            # numeric input for the size of the population you are interest in
            numericInput("numericpopulation", "What is the population size of the community?", 
                         value = 1000, min = 1),
            #slider of the percentage of the population that has been infected
            sliderInput("slideratrisk", "What percentage of the community was has been infected?", 
                        1, 100, value = 50),
            #numeric input for the number of individuals you predict will die of the infection
            numericInput("numericpred", "How many people with the illness do you think might die?",
                         value = 50),
            #submit button to pause page changes until changes have been made
            submitButton("Submit") 
        ),
        #shows the output of the questions shown below in a main web panel
        mainPanel(
            h4("The number of people predicted to die due to the infectious disease, based on population size
         and the percentage of the population exposed to the disease."),
            textOutput("pred1"),
            h4("The probability that, at a minimum, the community will suffer the number of deaths you
         predicted. This is expressed as a percentage."),
            textOutput("pred2")
        )
    )
) 
# Define server logic required to calculate the predictions/probabilities
server <- function(input, output) {
    # this calculates the number of people in the population predicted to die
    riskpred <- reactive({
        round(((input$numericprob/100) * input$numericpopulation * (input$slideratrisk/100)), 0)
    })
    # this calculates the probability that the number of people predicted to die (at least)
    # will die, given the probability of death (if infected) and the size of the infected
    # population)
    probpred <- reactive({
        (pbinom(input$numericpred, input$numericpopulation * input$slideratrisk/100, input$numericprob/100,
                lower.tail = FALSE))*100
    })
    #the following renders submit the predictions to the outputs in the ui
    output$pred1 <- renderText({
        riskpred()
    })
    
    output$pred2 <- renderText({
        probpred()
    })
}
shinyApp(ui = ui, server = server)
