library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)


# UI ##########################################################################
WIDTH_SIDE <- 3
WIDTH_MAIN <- 9

ui <- fluidPage(
  titlePanel(div("Probability Distributions",
                 a(img(src = 'GitHub-Mark-32px.png', align = 'right'),
                   href = 'https://github.com/watanabe8760/dist_vis')),
             "Probability Distributions"),

  withMathJax(),
  
  navlistPanel(
    widths = c(2, 10),
    
    'Continuous', #############################################################
    
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
    
    # Uniform Distribution (Continuous)
    tabPanel(
      "Uniform",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          sliderInput('ab',
                      'Range (\\(a, b\\))',
                      min = -3,
                      max = 3,
                      step = 0.1,
                      ticks = F,
                      value = c(0, 1))
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("uniform_cont")
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
                       value = 0.9),
          numericInput('beta',
                       'Shape (\\(\\beta\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 0.9)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("beta")
        )
      )
    ),
    
    # Gamma Distribution
    tabPanel(
      "Gamma",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          numericInput('k',
                       'Shape (\\(k\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 2),
          numericInput('theta',
                       'Scale (\\(\\theta\\))',
                       min = 0.1,
                       step = 0.1,
                       value = 1)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("gamma")
        )
      )
    ),
    
    # Exponential Distribution
    tabPanel(
      "Exponential",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          numericInput('lambda',
                       'Rate (\\(\\lambda\\))',
                       min = 1,
                       step = 1,
                       value = 1)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("exponential")
        )
      )
    ),
    
    # Chi-squared Distribution
    tabPanel(
      "Chi-squared",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          numericInput('df',
                       'Degree of Freedom (\\(k\\))',
                       min = 1,
                       step = 1,
                       value = 1)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("chi_squared")
        )
      )
    ),
    
    'Discrete', ###############################################################
    
    # Uniform Distribution (Discrete)
    tabPanel(
      "Uniform",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          sliderInput('ab_',
                      'Range (\\(a, b\\))',
                      min = -10,
                      max = 10,
                      step = 1,
                      ticks = F,
                      value = c(-3, 3))
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("uniform_disc")
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
    
    # Poisson Distribution
    tabPanel(
      "Poisson",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          numericInput('lambda2',
                       'Rate (\\(\\lambda\\))',
                       min = 1,
                       step = 1,
                       value = 100)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("poisson")
        )
      )
    ),
    
    # Geometric Distribution
    tabPanel(
      "Geometric",
      sidebarLayout(
        sidebarPanel(
          width = WIDTH_SIDE,
          sliderInput('p2',
                      'Success Probability (\\(p\\))',
                      min = 0,
                      max = 1,
                      step = 0.01,
                      value = 0.5)
        ),
        mainPanel(
          width = WIDTH_MAIN,
          plotOutput("geometric")
        )
      )
    )
  )
)


# Server ######################################################################
server <- function(input, output) {
  BLUES9 <- brewer.pal(9, 'Blues')
  
  # Continuous Distributions ##################################################
  
  # Normal Distribution
  output$normal <- renderPlot({
    if (is.na(input$sigma)) return(NULL)

    pos_1_sig <- c(input$mu - input$sigma, input$mu + input$sigma)
    pos_2_sig <- c(input$mu - input$sigma * 2, input$mu + input$sigma * 2)
    pos_3_sig <- c(input$mu - input$sigma * 3, input$mu + input$sigma * 3)
    
    x_range_def <- c(-5, 5)
    if (3 * input$sigma < x_range_def[2]) {
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
    y <- dnorm(x, mean = input$mu, sd = input$sigma)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      scale_x_continuous(limits = x_range_adj, breaks = x_ticks) +
      scale_y_continuous(limits = c(0, ifelse(max(y) < 0.5, 0.5, max(y)))) +
      geom_vline(xintercept = input$mu, color = BLUES9[8]) +
      geom_vline(xintercept = pos_1_sig, color = BLUES9[6]) +
      geom_vline(xintercept = pos_2_sig, color = BLUES9[4]) +
      geom_vline(xintercept = pos_3_sig, color = BLUES9[3]) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
  
  # Uniform Distribution (Continuous)
  output$uniform_cont <- renderPlot({
    if (input$ab[1] == input$ab[2]) return(NULL)
    a <- input$ab[1]
    b <- input$ab[2]
    d <- 1 / (b - a)
    
    data.frame(x_stt = c(-3, a, b),
               x_end = c( a, b, 3),
               y_stt = c( 0, d, 0),
               y_end = c( 0, d, 0)) %>%
      ggplot() +
      geom_segment(aes(x = x_stt, y = y_stt, xend = x_end, yend = y_end),
                   color = BLUES9[9], size = 1, alpha = 0.8) +
      geom_point(x = a, y = 0, size = 3, color = BLUES9[9], shape = 1) +
      geom_point(x = a, y = d, size = 3, color = BLUES9[9]) +
      geom_point(x = b, y = d, size = 3, color = BLUES9[9]) +
      geom_point(x = b, y = 0, size = 3, color = BLUES9[9], shape = 1) +
      xlab('X') +
      ylab('Density') +
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
  
  # Gamma Distribution
  output$gamma <- renderPlot({
    if (is.na(input$k) | is.na(input$theta)) return(NULL)
    x_max <- qgamma(0.999, shape = input$k, scale = input$theta) %>% ceiling()
    x <- seq(from = 0, to = x_max, length.out = 1000)
    y <- dgamma(x, shape = input$k, scale = input$theta)
    mu <- input$k * input$theta
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      geom_vline(xintercept = mu, color = BLUES9[8]) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
  
  # Exponential Distribution
  output$exponential <- renderPlot({
    if (is.na(input$lambda)) return(NULL)
    x_max <- qexp(0.999, input$lambda)
    x <- seq(from = 0, to = x_max, length.out = 1000)
    y <- dexp(x, input$lambda)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      geom_vline(xintercept = 1 / input$lambda, color = BLUES9[8]) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
  
  # Chi-squared Distribution
  output$chi_squared <- renderPlot({
    if (is.na(input$df) | input$df == 0) return(NULL)
    x_max <- qchisq(0.999, input$df) %>% ceiling()
    x <- seq(0, x_max, length.out = 1000)
    y <- dchisq(x, input$df)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_path(aes(x = x, y = y), color = BLUES9[9], size = 1, alpha = 0.8) +
      xlab('X') +
      ylab('Density') +
      theme_hc()
  })
  
  # Discrete Distributions ####################################################
  
  # Uniform Distribution (Discrete)
  output$uniform_disc <- renderPlot({
    a <- input$ab_[1]
    b <- input$ab_[2]
    x <- seq(a, b)
    y <- ifelse(x >= a & x <= b, 1/(b-a+1), 0)
    
    data.frame(x = x, y = y) %>%
      ggplot() +
      geom_point(aes(x = x, y = y), color = BLUES9[9], size = 1.5) +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), color = BLUES9[6]) +
      scale_x_continuous(limits = c(-10, 10)) +
      scale_y_continuous(limits = c(0, ifelse(max(y) <= 0.25, 0.25, max(y)))) +
      xlab('X') +
      ylab('Probability') +
      theme_hc()
  })
  
  # Binomial Distribution
  output$binomial <- renderPlot({
    if (is.na(input$n)) return(NULL)
    x <- seq(from = 0, to = input$n, by = 1)
    y <- dbinom(x, size = input$n, prob = input$p)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_point(aes(x = x, y = y), color = BLUES9[9], size = 1.5) +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), color = BLUES9[6]) +
      xlab('Number of Successes') +
      ylab('Probability') +
      theme_hc()
  })
  
  # Poisson Distribution
  output$poisson <- renderPlot({
    if (is.na(input$lambda2) | input$lambda2 == 0) return(NULL)
    x_max <- qpois(0.999, input$lambda2)　%>% ceiling()
    x <- seq(from = 0, to = x_max)
    y <- dpois(x, input$lambda2)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_point(aes(x = x, y = y), color = BLUES9[9], size = 1.5) +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), color = BLUES9[6]) +
      xlab('Number of Occurrence') +
      ylab('Probability') +
      theme_hc()
  })
  
  # Geometric Distribution
  output$geometric <- renderPlot({
    if (input$p2 == 0) return(NULL)
    x_max <-qgeom(0.999, prob = input$p2) %>% ceiling()
    x_ticks <- if(x_max <= 10) seq(1, 10) else waiver()
    x <- seq(from = 0, to = ifelse(x_max <= 10, 10, x_max), by = 1)
    y <- dgeom(x, prob = input$p2)
    
    data.frame(x, y) %>%
      ggplot() +
      geom_point(aes(x = x, y = y), color = BLUES9[9], size = 1.5) +
      geom_segment(aes(x = x, y = 0, xend = x, yend = y), color = BLUES9[6]) +
      scale_x_continuous(breaks = x_ticks) +
      xlab('Number of Trials') +
      ylab('Probability') +
      theme_hc()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

