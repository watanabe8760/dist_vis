library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


# UI ##########################################################################
WIDTH_SIDE <- 3
WIDTH_MAIN <- 9

ui <- fluidPage(
  titlePanel("Probability Distributions"),
  withMathJax(),
  
  navlistPanel(
    widths = c(2, 10),
    
    # Normal Distribution
    tabPanel(
      "Normal",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          sliderInput('mu',
                      'Mean (\\(\\mu\\))',
                      min = -5,
                      max = 5,
                      step = 0.1,
                      value = 0),
          numericInput('sigma',
                       'Standard Deviation (\\(\\sigma\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 1)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("normal")
        )
      )
    ),
    
    # Binomial Distribution
    tabPanel(
      "Binomial",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          sliderInput('p',
                      'Success Probability (\\(p\\))',
                      min = 0,
                      max = 1,
                      step = 0.01,
                      value = 0.5),
          numericInput('n',
                       'Number of Trials (\\(n\\))',
                       min = 1,
                       step = 1,
                       value = 20)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("binomial")
        )
      )
    ),
    
    # Beta Distribution
    tabPanel(
      "Beta",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          numericInput('alpha',
                       'Shape (\\(\\alpha\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 2),
          numericInput('beta',
                       'Shape (\\(\\beta\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 2)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("beta")
        )
      )
    ),
    
    tabPanel(
      "Gamma"
    ),
    tabPanel(
      "Poisson"
    ),
    tabPanel(
      "Exponential"
    ),
    tabPanel(
      "Geometric"
    )
  )
)


# Server ######################################################################
server <- function(input, output) {
  BLUES9 <- brewer.pal(9, 'Blues')
  
  # Normal Distribution
  output$normal <- renderPlot({
    if (is.na(input$sigma)) return(NULL)
    mu <- as.numeric(input$mu)
    sigma <- as.numeric(input$sigma)

    pos_1_sig <- c(mu - sigma, mu + sigma)
    pos_2_sig <- c(mu - sigma * 2, mu + sigma * 2)
    pos_3_sig <- c(mu - sigma * 3, mu + sigma * 3)
    
    x_range_def <- c(-5, 5)
    if (3 * sigma < x_range_def[2]) {
      x_range_adj <- x_range_def
      x_ticks <- seq(x_range_def[1], x_range_def[2])
    } else {
      x_range_adj <-
        if (abs(pos_3_sig[1]) > pos_3_sig[2]) {
          c(pos_3_sig[1], abs(pos_3_sig[1])) 
        } else {
          c(-pos_3_sig[2], pos_3_sig[2])
        }
      x_ticks <- waiver()
    }

    x <- seq(from = x_range_adj[1], to = x_range_adj[2], length.out = 1000)
    y <- dnorm(x, mean = mu, sd = sigma)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      scale_x_continuous(limits = x_range_adj, breaks = x_ticks) +
      scale_y_continuous(limits = c(0, ifelse(max(y) < 1, 1, max(y)))) +
      geom_vline(xintercept = mu, color = BLUES9[8]) +
      geom_vline(xintercept = pos_1_sig, color = BLUES9[6]) +
      geom_vline(xintercept = pos_2_sig, color = BLUES9[4]) +
      geom_vline(xintercept = pos_3_sig, color = BLUES9[3]) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
  
  # Binomial Distribution
  output$binomial <- renderPlot({
    if (is.na(input$n)) return(NULL)
    x <- seq(from = 0, to = input$n, by = 1)
    y <- dbinom(x, size = input$n, prob = input$p)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_point(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), color = BLUES9[6]) +
      xlab('Number of Successes') +
      ylab('Probability') +
      theme_hc()
  })
  
  # Beta Distribution
  output$beta <- renderPlot({
    if (is.na(input$alpha) | is.na(input$beta)) return(NULL)
    x <- seq(from = 0, to = 1, length.out = 1000)
    y <- dbeta(x, input$alpha, input$beta)
    mu <- input$alpha / (input$alpha + input$beta)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      geom_vline(xintercept = mu, color = BLUES9[8]) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

