options(warn=-1)
suppressMessages(library(ggplot2))
suppressMessages(library(corrplot)) 
suppressMessages(library(imputeTS))
suppressMessages(library(psych))
suppressMessages(library(pscl))
suppressMessages(library(plotly))
suppressMessages(library(shiny))
suppressMessages(library(caret))
suppressMessages(library(pROC))

logisticPseudoR2s <- function(LogModel) { 
    
    dev <- LogModel$deviance 
    nullDev <- LogModel$null.deviance 
    modelN <-  length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    cat("Pseudo R^2 for logistic regression\n")
    cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
    cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
    cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

rotation3 <- read.csv('rotation3.csv')






ui <- fluidPage(
    plotlyOutput('scatter', width = 800, height = 600)  
)

server <- function(input, output, session) {
    
    
    output$scatter <- renderPlotly({
        
        
        
        fig <- plot_ly(rotation3, x = ~RC1, y = ~RC2, z = ~RC3, color = ~as.factor(phenotype) ,
                       colors = c('#BF382A','#0C4B8E'), alpha = .6)
        fig <- fig %>% add_markers()
        fig <- fig %>% layout(title="3 Rotated Components Over Top 10 SNPs")
        fig <- fig %>% layout(scene = list(xaxis = list(title = 'RC1'),
                                           yaxis = list(title = 'RC2'),
                                           zaxis = list(title = 'RC3')))
        fig
        
    })
}

shinyApp(ui = ui, server = server)


