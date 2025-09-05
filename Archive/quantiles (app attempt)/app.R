library(extrafont)
library(shiny)
library(ggplot2)
library(caret)
library(e1071)
library(formattable)
library(scales)
windowsFonts(Times=windowsFont("TT Times New Roman"))

ui <- shinyUI(navbarPage("Quantiles",

# START DEMO PAGE ---------------------------------------------------------
  tabPanel("Demo",
    fluidPage(
      fluidRow(
      column(width=2,
        helpText(h5(strong("Sample data properties: "))),            
        sliderInput("demo.r",     label = h5("Correlation"), min = -1, max = 1, value = .50, step = .01),
        numericInput("demo.quant", label = h5("Number Quantiles"), value = 5, step = 1),
        downloadButton('demo.download', label = "Download Plot"),
        textInput("demo.y.axis.title", "Y-Axis Title", "Mean Actual Criterion Score"),
        textInput("demo.x.axis.title", "X-Axis Title", "Predicted Criterion Score Quantiles"),
        numericInput("demo.y.axis.limit.lower", label = h5("Y-axis Lower Limit"), value = 40, step = 1),
        numericInput("demo.y.axis.limit.upper", label = h5("Y-axis Upper Limit"), value = 60, step = 1),
        numericInput("demo.font.size", label = h5("Font Size"), value = 20, step = 1),
        br(),
        h5(a("@AJThurston", href="https://twitter.com/AJThurston", target="_blank"), align = "center")
       ), #Closes sidebarPanel
      
      column(width = 10, align = "center",
             plotOutput("demo.plot")
      )
      
      
) #Closes fluidRow
) #Closes fluidPage
),

# END DEMO PAGE ---------------------------------------------------------

# START UPLOAD PAGE ---------------------------------------------------------
tabPanel("Upload",
         fluidPage(
           fluidRow(
             column(width=2,
                    helpText(h5(strong("Sample data properties: "))),            
                    fileInput("file", label = "csv file"),
                    textInput("xcol", "X-Axis Value Column:", "pred"),
                    textInput("ycol", "Y-Axis Value Column:", "actu"),
                    numericInput("upload.quant", label = h5("Number Quantiles"), value = 5, step = 1),
                    textInput("upload.plot.title", "Plot Title", "Mean Actual Criterion by Predicted Criterion Quantiles"),
                    textInput("upload.y.axis.title", "Y-Axis Title", "Mean Actual Criterion Score"),
                    textInput("upload.x.axis.title", "X-Axis Title", "Predicted Criterion Score Quantiles"),
                    br(),
                    h2(a("@AJThurston", href="https://twitter.com/AJThurston", target="_blank"), align = "center")
                  ),
             
            
           column(width = 10, align = "center",
                    plotOutput("p")
                 )
                 )  
           ) 
         ), 


# END UPLOAD PAGE ---------------------------------------------------------

# START DOCUMENTATION PAGE ---------------------------------------------------------

tabPanel("Documentation",
         fluidPage(
           fluidRow(
             column(width=6,
                    
                    p(strong("Formulas for values calculated on App page:")),
                    hr()
                    ),
             
             column(width=6,
                    p(strong("Contact: ")),
                    hr(),
                    h2(a("Website: AJThurston.com", href="https://AJThurston.com", target="_blank")),
                    br(),
                    h2(a("Twitter: @AJThurston", href="https://twitter.com/AJThurston", target="_blank")),
                    br(),
                    h2(a("LinkedIn: AJThurston", href="https://www.linkedin.com/in/ajthurston/", target="_blank")),
                    br(),
                    h2(a("Medium:  @AJThurston", href="https://medium.com/@AJThurston", target="_blank"))
             )
           )#Closes fluidRow
         )#Closes fluidPage
) #Closes tabPanel
) #Closes ShinyUI
)



# Server ------------------------------------------------------------------


server <- shinyServer(function(input, output) {

# Demo plot ------------------------------------------------------------------  
  
  output$demo.plot = renderPlot({

  #Demo Data Properties
  set.seed(33620) #Set seed so result is reproducable
  n = 1000    #Sample size
  M = 50      #Variable Means
  SD = 10     #Variable SD
  deci = 0    #Decimals to round data

  names = c("actu","pred")
  nvars = length(names)
  
  R = matrix(cbind(  1,input$demo.r,  
                     input$demo.r,  1  
  ),nrow=nvars)
  
  #Choelsky Decomposition for Simulating Correlated Data
  U = t(chol(R))
  nvars = dim(U)[1]
  random.normal = matrix(rnorm(nvars*n,0,1), nrow=nvars, ncol=n);
  data = as.data.frame(t(U %*% random.normal))
  data = round(data*SD+M, digits = deci)
  names(data) = names
  rm(U,R,random.normal,names,M,n,nvars,SD,deci)

  data$quant <- as.numeric(cut(data$pred, quantile(data$pred, probs = seq(0,1,1/input$demo.quant)), include.lowest=TRUE))

# Simulated Plot --------------------------------------------------------------------
demo.plot = ggplot(data) +
    scale_y_continuous(name=input$demo.y.axis.title, limits = c(input$demo.y.axis.limit.lower,input$demo.y.axis.limit.upper) , oob = rescale_none) + 
    scale_x_continuous(name=input$demo.x.axis.title, oob = rescale_none) +
    geom_bar(aes(x = quant, y = actu), position = "dodge", stat = "summary", fun.y = "mean",
             color = 'black',
             fill = '#006747',
             width = .5) +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      text = element_text(size = input$demo.font.size, family = "Times", color = "black")
    )
  demo.plot}, height = 600, width = 600) #Close Render Plot

# Upload Data and Plot ----------------------------------------------------  

  observe({
    #data = input$file1
    #if(is.null(data))
    #  return(NULL)
    
    #df = read.csv(data$datapath)
    #output$plot = renderPlot({
    #  ggplot(df, aes_string(x = input$x, y = input$y)) +
    #    geom_line()
    #})
  
  
  upload.data <- #reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.csv(file=file1$datapath, header=TRUE)
  #})
  output$xselect <- renderUI({
    selectInput("xcol","X variable",names(upload.data()))
  })
  output$yselect <- renderUI({
    selectInput("ycol","Y variable", names(upload.data()))
  })
  output$p <- renderPlot({
    validate(need(input$file,"need filename"))
    df <- upload.data()
#    plot(df[[input$xcol]], df[[input$ycol]],
#         xlab=input$xcol,ylab=input$ycol)
  

ggplot(df) +
        scale_y_continuous(name=upload.y.axis.title, limits = upload.y.axis.limits, oob = rescale_none) + 
        scale_x_continuous(name=upload.x.axis.title, oob = rescale_none) +
        geom_bar(aes_string(x = input$xcol, y = input$ycol), 
                 position = "dodge", 
                 stat = "summary", 
                 fun.y = "mean",
                 color = 'black',
                 fill = '#006747',
                 width = .5) +
        theme(
          panel.background = element_rect(fill = "white", color = "black"),
          text = element_text(size = input$upload.font.size, family = "Times", color = "black")
              
        )
      }, height = 600, width = 600) #Close Render Plot  

# Upload Data and Plot ----------------------------------------------------

  })#closes observe

})#Close server

shinyApp(ui = ui, server = server)

