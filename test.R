library(htmltools)
library(shiny)

# Set the path to the generated image
image_path <- "colors.png"


if (interactive()) {
  ui <- fluidPage(
    sidebarPanel(
      

      tags$img(src = image_path, width = "100%", height = "20%"),
      
      
      selectInput("plotType", "Plot Type",
                  c(Scatter = "scatter", Histogram = "hist")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plotType == 'hist'",
        selectInput(
          "breaks", "Breaks",
          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
        ),
        # Only show this panel if Custom is selected
        conditionalPanel(
          condition = "input.breaks == 'custom'",
          sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
        )
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
  
  server <- function(input, output) {
    x <- rnorm(100)
    y <- rnorm(100)
    
    output$plot <- renderPlot({
      if (input$plotType == "scatter") {
        plot(x, y)
      } else {
        breaks <- input$breaks
        if (breaks == "custom") {
          breaks <- input$breakCount
        }
        
        hist(x, breaks = breaks)
      }
    })
  }
  
  shinyApp(ui, server)
}

png("colors.png",width=450,height=200)
plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('Max Val', 'Mid Val', 'Min Val'),pt.cex=3, cex=1.5,bty='n',
       fill = c('blue', 'white', 'red'), horiz=TRUE)
mtext("Values", at=0.2, cex=2)
dev.off()
