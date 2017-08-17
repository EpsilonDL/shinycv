# Libraries ----

library(shiny)
library(plotly)
library(wordcloud)

# Data ----

datos.Educacion<- data.frame(Theme=c("Simulations","Machine Learning",
                                     "Mathematical Modeling","Probability",
                                     "Stadistics","Numerical Analisis",
                                     "Time Series","Risk Management",
                                     "Team Management","Project Management",
                                     "Work Methologies","Team Work"),
                             USB=c(0.7,0.5,1,0.8,0.8,0.7,0.4,0.2,0,0,0.3,0.4),
                             JHU=c(0.1,0.8,0.6,0.5,0.5,0,0,0,0,0,0.7,0),
                             AFT=c(0,0,0,0,0,0,0.8,0,0,0,0.2,0))
datos.Educacion$Theme<- as.character(datos.Educacion$Theme)
datos.experiencia<- data.frame(Enterprise=c("Intraffic C.A.",
                                            "BCV (Central bank of Venezuela)",
                                            "USB (Simon Bolivar University)"),
                               Text=c("Data Science - 2 Years 4 Months\n-Data Mining\n-Bi Projects\n-Desing and application of mathematical models",
                                      "Researcher - 3 Months\n-Desing and application of mathematical models\n-Management of concentration risk",
                                      "Academic Assistant - 9 Months\n-Scientific computing practical class assistant"),
                               Time=c(28,3,9))
datos.conocimientos<- data.frame(Knowledge=c("Simulations","Machine Learning",
                                             "Mathematical Modeling","Probability",
                                             "Stadistics","Numerical Analisis",
                                             "Time Series","Risk Management",
                                             "Team Management","Project Management",
                                             "Work Methologies","Team Work"),
                                 Level=c(10,5,8,7,7,6,4,3,6,8,7,7))
datos.habilidaddes<- data.frame(Skills=c("R","PDI","SQL","Tableau","Matlab",
                                         "Excel","Python","Weka","C","Cinema4D",
                                         "Blender","Git"),
                                Level=c(10,8,7,7,8,6,2,4,3,5,3,6))

# ui ----
ui<- fluidPage(title = "Eloy Alfonso Chang Castro",
               # Basic info ----
               sidebarLayout(
                   sidebarPanel(
                       titlePanel("Eloy Alfonso Chang Castro"),
                       # includeText("e-mail: eloy.chang.182@gmail.com | Skype: echang1802"),
                       # includeText("Tlf: +58 4122124262"),
                       # includeText("Caracas - Venezuela")
                       # textOutput("Nombre"),
                       textOutput("MailContacto"),
                       textOutput("NumContacto"),
                       textOutput("Direccion")
                   ),
                   mainPanel(
                       tabsetPanel(
                           # Education ----
                           tabPanel(
                               title = "Education",
                               titlePanel("Education:"),
                               pre(includeText("Education.txt")),
                               titlePanel("Topics by Institute"),
                               selectInput("Institute","Select a institute:",
                                           selected = "USB",
                                           choices = c("USB","JHU","AFT")),
                               plotlyOutput("PlotEducacion")
                           ),
                           # Experience ----
                           tabPanel(
                               title = "Experience",
                               plotlyOutput("PlotExperiencia")
                           ),
                           # Skills ----
                           tabPanel(
                               title = "Skills",
                               mainPanel(
                                   plotOutput("PlotHabilidad")
                               )
                           ),
                           # Knowledge ----
                           tabPanel(
                               title = "Knowledge",
                               mainPanel(
                                   titlePanel("Distribution of own knowledge"),
                                   selectInput("Conocimiento","Select a knowledge area",
                                               c("All","Data Science","Numerical Analisis",
                                                 "Simulations","Management","Others"),
                                               "All"),
                                   plotlyOutput("PlotConocimiento")
                               )
                           )
                       )
                   )
               ))

# Server ----

server<- function(input,output){
    # Basic Info ----
        # Personal Info ----
    
    output$MailContacto<- renderText({
        print("e-mail: eloy.chang.182@gmail.com / Skype: echang1802\n")
    })
    
    output$NumContacto<- renderText({
        print("Tlf: +58 4122124262\n")
    })
    
    output$Direccion<- renderText({
        print("Caracas - Venezuela.")
    })
    
        # Education ----
    
    N<- nrow(datos.Educacion)
    centers<- data.frame(x=runif(N),y=runif(N),Theme=datos.Educacion$Theme)
    centers$Theme<- as.character(centers$Theme)
    
    output$PlotEducacion<- renderPlotly({
        q<- which(names(datos.Educacion) == input$Institute)
        centers<- merge(centers,datos.Educacion[,c(1,q)], by = "Theme")
        names(centers)<- c("Theme","x","y","Institute")
        centers$Text<- paste(input$Institute," | ",centers$Theme," | ",
                             round(centers$Institute*100),"%",sep = "")
        graph<- plot_ly(data = centers, x = ~x, y = ~y, color = ~Theme, size = ~Institute,
                        text = ~Text)
        axis<- list(title = "", showgrid = FALSE,showticklabels = FALSE, 
                    zeroline = FALSE, autoticks = FALSE, range = c(-0.2,1.2))
        graph <- layout(
            graph,
            title = paste("Education on",input$Institute),
            showlegend = FALSE,
            xaxis = axis,
            yaxis = axis
        ) 
        graph
    })
    
        # Experience ----
    
    output$PlotExperiencia<- renderPlotly({
        plot_ly(datos.experiencia, x = ~Enterprise, y = ~Time, 
                text = ~as.character(Text), type = "bar")
    })
    
    # Skills ----
    
    output$PlotHabilidad<- renderPlot({
        wordcloud(datos.habilidaddes$Skills,datos.habilidaddes$Level,rot.per = .3,
                  c(7,.1),colors=brewer.pal(12, "Paired"),min.freq = 1)
    })
    
    # Knowledge ----
    
    output$PlotConocimiento<- renderPlotly({
        if(input$Conocimiento == "All") p<- 1:12
        else if(input$Conocimiento == "Data Science") p<- 1:6
        else if(input$Conocimiento == "Numerical Analisis") p<- c(1,3,6)
        else if(input$Conocimiento == "Simulations") p<- c(1,3:5)
        else if(input$Conocimiento == "Management") p<- 9:12
        else p<- c(7,8)
        plot_ly(datos.conocimientos[p,], labels = ~Knowledge, values = ~Level, 
                type = "pie",textposition = "inside",
                textinfo = "label+percent")
    })
}

# Shiny ----

shinyApp(ui,server)

# rsconnect::deployApp("C:/Users/Eloy Chang/Documents/UNI/Epsilon/app")
