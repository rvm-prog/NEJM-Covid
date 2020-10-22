library(shiny)
library(dplyr)
library(tidyr)

library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "bottom", text = element_text(size=20), strip.text.x = element_text(size = 18)))
colours <- c("#E69F00", "#009E73")

ui <- fluidPage(
    titlePanel("Predictive Values"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("spec", "Specificity:", min = 0, max = 100, value = 95, step = 1, post = "%"),
            sliderInput("sens", "Sensitivity:", min = 0, max = 100, value = 70, step = 1, post = "%")
        ),
        mainPanel(
           plotOutput("res_plot")
        )
    )
)

server <- function(input, output) {

    res_tbl <- reactive({
        # prevalence values
        prev <- seq(0, 1, 0.01)
        n_obs <- length(prev)
        # percentage sick and not sick
        perc_sick <- prev * 100
        perc_notsick <- 100 - perc_sick
        # true positives
        TP <- perc_sick * (input$sens / 100)
        # true negatives
        TN <- perc_notsick * (input$spec / 100)
        # false negatives
        FN <- perc_sick - TP
        # false positives
        FP <- perc_notsick - TN
        # summary variables
        # sum_sick_test <- TP + FN
        # sum_notsick_test <- TN + FP
        # get predictive values
        FPPV <- (FP / (FP + TP)) * 100
        FNPV <- (FN / (FN + TN)) * 100
        TPPV <- (TP / (FP + TP)) * 100
        TNPV <- (TN / (FN + TN)) * 100
        # combine results to tbl
        tibble(
            perc_sick = rep(perc_sick, 4),
            test_acc = rep(c("False", "True", "False", "True"), each=n_obs),
            test_res = rep(c("Negative Test Result", "Positive Test Result"), each=n_obs*2),
            pv = c(FNPV, TNPV, FPPV, TPPV)
        )
    })
    
    output$res_plot <- renderPlot({
        ggplot(res_tbl(), aes(perc_sick, pv, colour=test_acc)) +
            geom_line(size = 1.5) +
            facet_wrap(vars(test_res)) +
            labs(
                x = "Pre-Test Probability of Sickness\n(% Sick in Population)",
                y = "Predictive Value",
                colour = "Test Accuracy"
            ) +
            scale_colour_manual(values=colours)
    }, height=600)
}

shinyApp(ui = ui, server = server)
