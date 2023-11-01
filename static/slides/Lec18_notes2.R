library(tidyverse)
library(shiny)

pal = c("#7fc97f", "#beaed4", "#dfc086")
pal_names = c("Green", "Purple", "Orange")


shinyApp(
  ui = fluidPage(
    title = "Beta-Binomial",
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=7),
        sliderInput("n", "# of flips", min=0, max=100, value=10),
        h4("Prior:"),
        numericInput("alpha", "Prior # of head", min=0, value=5),
        numericInput("beta", "Prior # of tails", min=0, value=5),
        checkboxInput("options", "Show Options", value = FALSE),
        conditionalPanel(
          "input.options == true",
          checkboxInput("bw", "Use theme_bw", value = FALSE),
          checkboxInput("facet", "Use facets", value = FALSE),
          
          selectInput("prior", "Color for prior", choices = pal_names, selected = pal_names[1]),
          selectInput("likelihood", "Color for likelihood", choices = pal_names, selected = pal_names[2]),
          selectInput("posterior", "Color for posterior", choices = pal_names, selected = pal_names[3])
        )
      ),
      mainPanel = mainPanel(
        plotOutput("plot"),
        tableOutput("table")
      )
    )
  ),
  server = function(input, output, session) {
    
    observe({
      choices = c(input$prior, input$likelihood, input$posterior)
      if (input$prior == input$likelihood) {
        updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
      } else if (input$prior == input$posterior) {
        updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
      }   
    }) %>%
      bindEvent(input$prior)
    
    observe({
      choices = c(input$likelihood, input$likelihood, input$posterior)
      if (input$likelihood == input$prior) {
        updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
      } else if (input$likelihood == input$posterior) {
        updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
      }   
    }) %>%
      bindEvent(input$likelihood)
    
    observe({
      choices = c(input$prior, input$likelihood, input$posterior)
      if (input$posterior == input$prior) {
        updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
      } else if (input$posterior == input$likelihood) {
        updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
      }   
    }) %>%
      bindEvent(input$posterior)
    
    observe({
      updateSliderInput(session, "x", max = input$n)
    }) %>%
      bindEvent(input$n)
    
    d = reactive({
      tibble(
        p = seq(0, 1, length.out = 1000)
      ) %>%
        mutate(
          prior = dbeta(p, input$alpha, input$beta),
          likelihood = dbinom(input$x, size = input$n, prob = p) %>% 
            {. / (sum(.) / n())},
          posterior = dbeta(p, input$alpha + input$x, input$beta + input$n - input$x)
        ) %>%
        pivot_longer(
          cols = -p,
          names_to = "distribution",
          values_to = "density"
        ) %>%
        mutate(
          distribution = forcats::as_factor(distribution)
        )
    })
    
    output$plot = renderPlot({
      
      color_choices = c(
        which(pal_names == input$prior),
        which(pal_names == input$likelihood),
        which(pal_names == input$posterior)
      )
      
      color_pal = pal[color_choices]
      
      g = ggplot(d(), aes(x=p, y=density, color=distribution)) +
        geom_line(size=1.5) +
        geom_ribbon(aes(ymax=density, fill=distribution), ymin=0, alpha=0.5) +
        scale_color_manual(values = color_pal) +
        scale_fill_manual(values = color_pal)
      
      if (input$bw)
        g = g + theme_bw()
      
      if (input$facet) 
        g = g + facet_wrap(~distribution)
      
      g
    })
    
    output$table = renderTable({
      d() %>%
        group_by(distribution) %>%
        summarize(
          mean = sum(p * density) / n(),
          median = p[(cumsum(density/n()) >= 0.5)][1],
          q025 = p[(cumsum(density/n()) >= 0.025)][1],
          q975 = p[(cumsum(density/n()) >= 0.975)][1]
        )
    })
  }
)
