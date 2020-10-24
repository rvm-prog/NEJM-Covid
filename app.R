library(shiny)
library(dplyr)

library(plotly)
library(ggplot2)
theme_set(theme_bw() + theme(legend.position = "bottom", text = element_text(size=18), strip.text.x = element_text(size = 14)))
# colours <- c("#E69F00", "#009E73")
# colours <- c("darkred", "darkgreen")
colours <- c("#940C09", "#34E06E")
plot_height <- 600

ui <- fluidPage(
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    titlePanel("Predictive Values in COVID-19 Testing"),
    
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            fluidRow(
                column(12, HTML("<p>(Text explaining app and linking to paper here)</p><br></br>")),
                column(12, sliderInput("sens", "Sensitivity:", min = 0, max = 100, value = 70, step = 1, post = "%")),
                column(12, sliderInput("spec", "Specificity:", min = 0, max = 100, value = 95, step = 1, post = "%")),
                column(12, HTML("<h4>Plot Options</h4>")),
                column(6, checkboxInput("ind_points", "Show example points")),
                column(6, uiOutput("ui_point_freq"))
            )
        ),
        mainPanel = mainPanel(
            plotlyOutput("res_plot", height=plot_height)
        )
    )
)

server <- function(input, output) {

    output$ui_point_freq <- renderUI({
        if (input$ind_points) tags$div(id = "input-inline", numericInput("point_freq", "Every x%  ", min=1, max=50, step=1, value=5)) else NULL
    })
    
    res_tbl <- reactive({
        # percentage sick and not sick
        perc_sick <- seq(0, 100, 0.5)
        n_obs <- length(perc_sick)
        perc_notsick <- 100 - perc_sick
        # example points
        eg_freq <- if (is.null(input$point_freq)) 5 else input$point_freq
        eg_pnts <- seq(0, 100, eg_freq)
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
            test_acc = rep(c("False Result", "True Result", "False Result", "True Result"), each=n_obs),
            test_corr = rep(c("Incorrect", "Correct", "Incorrect", "Correct"), each=n_obs),
            test_res = rep(c("Negative Test Result", "Positive Test Result"), each=n_obs*2),
            pv = c(FNPV, TNPV, FPPV, TPPV),
            plt_text = sprintf("Test accuracy: %s<br>Pre-test Probability: %s%%<br>Predictive value: %s%%", test_corr, perc_sick, round(pv, 1))
        ) %>%
            mutate(is_example = perc_sick %in% eg_pnts)
    })
    
    output$res_plot <- renderPlotly({
        
        # generate the relevant ggplot and make plotly object
        if (input$ind_points) {
            plt <- ggplotly(
                ggplot(res_tbl(), aes(perc_sick, pv, colour=test_acc)) +
                    geom_line(size = 1.5, alpha = 0.2) +
                    geom_point(aes(text=plt_text), data=filter(res_tbl(), is_example), size=2)  +
                    facet_wrap(vars(test_res)) +
                    labs(x = NULL, y = NULL, colour = NULL) +
                    scale_colour_manual(values=colours),
                tooltip = "text"
            )
        } else {
            plt <- ggplotly(
                ggplot(res_tbl(), aes(perc_sick, pv, colour=test_acc)) +
                    geom_line(aes(text=plt_text, group=test_acc), size = 1.5) +
                    facet_wrap(vars(test_res)) +
                    labs(x = NULL, y = NULL, colour = NULL) +
                    scale_colour_manual(values=colours),
                tooltip = "text"
            )
        }
        
        # return plotly object with nice layout
        plt %>%
            layout(hovermode = "x") %>%
            config(displayModeBar = FALSE) %>%
            layout(
                legend = list(orientation = "h", x = 0.5, y = 1.25, xanchor = "center"),
                xaxis = list(fixedrange = TRUE),
                yaxis = list(fixedrange = TRUE, automargin=TRUE, title="Predictive Value (%)", titlefont=list(size = 20)),
                margin = list(l = 75, b = 100)
            ) %>%
            # add x axis as a manual annotation (otherwise applies to only one facet)
            add_annotations(
                text = "Pre-Test Probability of Sickness (%)",
                x = 0.5,
                y = 0,
                yref = "paper",
                xref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                yshift = -75,
                showarrow = FALSE,
                font = list(size = 20)
            )
        
    })
}

shinyApp(ui = ui, server = server)
