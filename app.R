#
# This is a Shiny web application that helps users set
# the values of three variables: p, varA and h2 using
# dynamic sliders and see the effect changes have to
# Selection Response which depends on these variables.
#
# selResp= dnorm(qnorm(1-p))/p*sqrt(h2)*sqrt(varA)
#

library(shiny)
library(radarchart)

# Define UI for application that draws several plots and charts
ui <- fluidPage(title = "Selection Response Explorer",
                
    titlePanel("Selection Response Explorer"),            

    sidebarLayout(
      
      sidebarPanel(
  
        sliderInput(inputId = "p",
                    label = "Proportion of individuals selected (p):",
                    min = 0.01,
                    max = 1,
                    step = 0.01,
                    value = 0.5),
        sliderInput(inputId = "varA",
                    label = "Additive genetic variance (varA):",
                    min = 0,
                    max = 10,
                    step = 0.01,
                    value = 1),
        sliderInput(inputId = "h2",
                    label = "Narrow-sense heritability (h2):",
                    min = 0,
                    max = 1,
                    step = 0.01,
                    value = 0.5),
        sliderInput(inputId = "thres",
                    label = "Selection Response Threshold:",
                    min = 0,
                    max = 2,
                    step = 0.01,
                    value = 1)
      ),
      # Show plots and charts 
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Bar Chart",
                   plotOutput("barPlot")
          ), # endof tabPanel   
          tabPanel(title = "Line Plot",
                   plotOutput("linePlot")
          ), # endof tabPanel
          tabPanel(title = "Radar Chart",
                   chartJSRadarOutput("radar", width = "450", height = "300")
                   # plotOutput("gglinePlot")
          ), # endof tabPanel
          tabPanel(title = "Monotonicity",
                   plotOutput("monoPlot")
          ), # endof tabPanel  
          tabPanel(title = "Help",
                   tags$h2("Help"),
                   tags$br(),
                   tags$p("The selection response is an important factor in breeding.
                     This application aims to help breeders explore the effect
                     of three parameters in the selection response."),
                   tags$p("These parameters are:"),
                   tags$ol(
                     tags$li("Proportion of individuals selected (p) - (0, 1]"),
                     tags$li("Additive genetic variance (varA) - [0, Inf)"),
                     tags$li("Narrow-sense heritability (h2) - [0, 1]")
                   ),
                   tags$br(),
                   tags$p("To use the app, move the three sliders of the left sidebar
                     and explore how the parameters affect the selection response."),
                   tags$br(),
                   tags$p("Move the fourth slider to set a threshold for selection response.
                     When selection response passes this threshold, the colour changes
                     from blue to red.")
          ) # endof tabPanel             
      )
    ), # endof mainPanel
  ), # endof sidebarLayout

)
    

# Define server logic required to draw charts
server <- function(input, output) {

  # Calculates selection response
  # based on the value of three variables.
  selResp = function(p, varA, h2){
    # Standardized selection differential
    i = dnorm(qnorm(1-p))/p
    return(i*sqrt(h2)*sqrt(varA))
  }    
      
  # First Tab 
  output$barPlot <- renderPlot({
    varA=input$varA 
    h2=input$h2
    p=input$p
    sr=selResp(p,varA,h2)
    x<-c(p, varA/10, h2, sr)
    barplot(x, xlab = "Variable", ylab = "Value", ylim = range(pretty(c(0, 2))), main = paste("Selection Response = ",toString(round(sr, digits=2))), horiz = FALSE, names.arg = c("p","varA/10","h2", "Selection Response"), col = c("grey","grey","grey",ifelse(x[4]<input$thres,'blue','red')))
  })    

  # Second Tab    
    output$linePlot <- renderPlot({
      varA=input$varA 
      h2=input$h2
      p=input$p
      sr=selResp(p,varA,h2)
      x<-c(p,varA/10,h2,sr)
      plot(x, type = "o", lwd = (3*sr+0.5), col = ifelse(x[4]<input$thres,'blue','red'), xlab = "Variable", ylab = "Value", ylim = range(pretty(c(0, 2))), main = paste("Selection Response = ",toString(round(sr, digits=2))), axes = TRUE, xaxt = "n")
      axis(1, at=c(1,2,3,4), labels = c("p","varA/10","h2","Selec_Response"))
    })   
    
  # Third Tab 
    output$radar <- renderChartJSRadar({
      varA=input$varA 
      h2=input$h2
      p=input$p
      sr=selResp(p,varA,h2)
      df<-data.frame(variable = c("Selection Response","p","varA/10","h2"), value = c(sr,p,varA/10,h2))
      if (sr<input$thres)
        chartJSRadar(df, macScale = 10, showToolTipLabel=TRUE, showLegend=FALSE, polyAlpha = 0, lineAlpha = sr+0.1, addDots = TRUE, colMatrix = matrix(data=c(c(0,0,255),c(0,255,0),c(255,0,0)), nrow = 3, ncol = 3),  main="Radar Chart") # maxScale = 2, lineAlpha = 0.8, colMatrix = diag(255, nrow = 3)
      else
        chartJSRadar(df, macScale = 10, showToolTipLabel=TRUE, showLegend=FALSE, polyAlpha = 0, lineAlpha = sr+0.1, addDots = TRUE, colMatrix = matrix(data=c(c(255,0,0),c(0,255,0),c(0,0,255)), nrow = 3, ncol = 3),  main="Radar Chart") # maxScale = 2, lineAlpha = 0.8, colMatrix = diag(255, nrow = 3)
    })
    
    # Fourth Tab  
    output$monoPlot <- renderPlot({
      
      old.par <- par(mfcol=c(1,3))
      
      varA=input$varA 
      h2=input$h2
      p= seq(0.05,1,0.05)
      sr=selResp(p,varA,h2)
      
      plot(p, sr,
           xlab = "Proportion Selected (p)",
           ylab = "Response to Selection")
    
      varA=seq(0.05,100,5)
      h2=input$h2
      p=input$p
      sr=selResp(p,varA,h2)  
      
      plot(varA, sr,
           xlab = "Additive Genetic Variance (varA)",
           ylab = "Response to Selection")
      
      varA=input$varA
      h2=seq(0,1,0.05)
      p=input$p
      sr=selResp(p,varA,h2)  
      
      plot(h2, sr,
           xlab = "Narrow-sense Heritability (h2)",
           ylab = "Response to Selection")
      
      par(old.par)
    })      
}

# Run the application 
shinyApp(ui = ui, server = server)
