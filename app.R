library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(dplyr)
library(bslib)
library(shinythemes)
library(thematic)

#in this part is got the data to analize

my_theme <- bs_theme(bootswatch = "flatly",version = 4)
# in this part is biult the UI of the dashboards
#which cotanins 3 parts, title, sidebar & body
#in this case is needed to use the shinydashboard lib 
thematic_shiny(font = "auto")
ui <-navbarPage("FTCH"
                ,collapsible = TRUE, 
                #is added the theme with botswatch
                theme = my_theme,
                #is espcified the fixed nav bar
                position = c("fixed-top"),
                #the tabpanel with all options of the dashboard
                tabPanel("Plot",
                         #this is de custom css while understand who add an external file
                         tags$style(type="text/css", "
                                            body {padding-top: 70px;
                                            background-color: #edf2f9}
                                            
                                            .sidebarCustom{
                                                  position: fixed;
                                                  top: 0;
                                                  bottom: 0;
                                                  left: 0;
                                                  z-index: 100;
                                                  padding: 48px 0 0;
                                                  
                                                } 
                                            .marginDiv div{margin:auto;}
                                            .backGroundColor{background-color: #edf2f9}
                           "
                         ),
                         #main frame when is structured skeleton of the vie, sidebar and main frame
                         fluidRow(div( 
                             #classes to make responsive the sidebar
                             class="col-md-3 col-lg-2",
                             #sidebar made with tga of html because i don´t know how cutomize it to mi idea 
                             tags$nav(id="sidebarMenu" ,
                                      class=" col-md-3 col-lg-2 d-md-block sidebar collapse sidebarCustom backGroundColor",
                                      #this clases make to the side to be stuck while the main content is scrolled
                                      div(
                                          class="position-sticky pt-5 marginDiv mt-5 px-0 card shadow bg-white rounded",
                                          #checkgroup for the types of tags
                                          checkboxGroupInput("checkGroup", 
                                                             label = "Tipo Tarjetas", 
                                                             choices = list("Azules" = 1, "Rojas" = 2),
                                                             selected = c(1,2),width = "90%",
                                          ),
                                          #selectinput for the areas
                                          selectInput('area', 
                                                      'Area',
                                                      c("AGLOMERADOS",
                                                        "CHOCOLATES",
                                                        "CONFITERIA"),
                                                      width = "90%",
                                                      #selectinput for the lines      
                                          ),selectInput('linea', 
                                                        'Lineas',
                                                        NULL,
                                                        width = "90%",
                                                        
                                          )
                                      )
                             )
                         )
                         ,
                         #main panle wich shows all the data for this tabpanel
                         #the main calses allow to give the principal struture of the dashboards contents
                         mainPanel(class="col-lg-10 col-md-9 col-sm-12  backGroundColor container-fluid px-0",
                                   
                                   div( class="row justify-content-around mx-0 px-0",
                                        div(
                                            class="col-lg-6 col-md-12 col-sm-12 p-2 mx-auto",
                                            div(
                                                class="row col-12 p-2 mx-auto",
                                                div(class="col-12 card shadow bg-white rounded mx-auto",
                                                    div(class="col-1 mt-1",
                                                        actionButton("show", 
                                                                     "",
                                                                     icon = icon("dashboard")
                                                        )
                                                    ),
                                                    plotlyOutput("plotTA",width ="100%",height = "280px"),
                                                    
                                                ),
                                                
                                            ),
                                            
                                        ),
                                        div(
                                            class="col-lg-6 col-md-12 col-sm-12 p-2 mx-auto",
                                            div(
                                                class="row col-12 p-2 mx-auto",
                                                div(
                                                    class="col-12 card shadow bg-white rounded mx-auto",
                                                    plotlyOutput("plotME",width ="100%",height = "280px"),
                                                    div(class="col-1",
                                                        
                                                        actionButton("show", 
                                                                     "",
                                                                     icon = icon("dashboard")
                                                        )
                                                    )
                                                ),
                                            ),
                                            
                                            
                                        ),
                                        
                                        div(plotlyOutput("plotMEp",height = "280px"),
                                            
                                            class="col-lg-5 col-md-12 col-sm-12 card shadow bg-white rounded p-2 m-1 ",
                                        )
                                        
                                   )
                         )
                         )
                         
                )
)
# Define server logic required to draw a histogram ----
server <- function(session, input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    
    ##logica para poder hacer el conteto exaxto de la realcion de cuantso tipos de anolamias se repiten
    # y poderlo graficar en un grafico de barras
    observe({
        
        lineas <- dataFrameTarjetas %>% filter(Area==input$area) %>% select(Linea)
        
        updateSelectInput(session,"linea","Lineas",choices = lineas)
        
    })
    #ligic of the plots about tags
    
    #   #queries and data tranformation of
    tr<-dataFrameTarjetas %>% 
        group_by(TipoAnomalia,Tipo)%>%
        filter(Tipo=="Roja")%>%
        count(TipoAnomalia)
    
    ta<-dataFrameTarjetas %>% 
        group_by(TipoAnomalia,Tipo)%>%
        filter(Tipo=="Azul")%>%
        count(TipoAnomalia)
    
    #queries and data transformation
    taME <- dataFrameTarjetas %>% 
        group_by(Maquina,Tipo)%>%
        filter(Tipo=="Azul")%>%
        count(Maquina)
    
    trME <- dataFrameTarjetas%>% 
        group_by(Maquina,Tipo)%>%
        filter(Tipo=="Roja")%>%
        count(Maquina)
    
    #this plot is about data of tags cross type
    output$plotTA <- renderPlotly({
        #the plot is rederized for a select input which its id is tipo
        if(length(input$checkGroup)==2 | is.null(input$checkGroup)){
            p<-plot_ly()
            p<- p %>% add_bars(x = ta$TipoAnomalia,y=ta$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p<- p  %>%add_bars(x = tr$TipoAnomalia,y=tr$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
            p
        }else if(length(input$checkGroup)==1 & input$checkGroup=="2"){
            
            p<-plot_ly()
            p<- p  %>%add_bars(x = tr$TipoAnomalia,y=tr$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
            p
            
        }else if(length(input$checkGroup)==1 & input$checkGroup=="1"){
            
            p<-plot_ly()
            p<- p %>% add_bars(x = ta$TipoAnomalia,y=ta$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p
        }
    })
    #this plot is about data of tags cross machine/equipment
    output$plotME <- renderPlotly({
        
        if(length(input$checkGroup)==2 | is.null(input$checkGroup)){
            p<-plot_ly()
            p<- p %>% add_bars(x = taME$Maquina,y=taME$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p<- p  %>%add_bars(x = trME$Maquina,y=trME$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
            p
        }else if(length(input$checkGroup)==1 & input$checkGroup=="2"){
            
            p<-plot_ly()
            p<- p  %>%add_bars(x = trME$Maquina,y=trME$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
            p
            
        }else if(length(input$checkGroup)==1 & input$checkGroup=="1"){
            
            p<-plot_ly()
            p<- p %>% add_bars(x = taME$Maquina,y=taME$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p
        }
        
    })
    
    output$plotMEp <- renderPlotly({
        if(length(input$checkGroup)==2 | is.null(input$checkGroup)){
            
            p<-plot_ly()
            p<- p %>% add_bars(x = taME$Maquina,y=taME$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p<- p  %>%add_bars(x = trME$Maquina,y=trME$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
        }else if(length(input$checkGroup)==1 & input$checkGroup=="2"){
            
            p<-plot_ly()
            p<- p  %>%add_bars(x = trME$Maquina,y=trME$n,name="Rojas",marker = list(
                color = 'red'
            )) %>%
                layout(barmode = "stack")
            p
            
        }else if(length(input$checkGroup)==1 & input$checkGroup=="1") {
            p<-plot_ly()
            p<- p %>% add_bars(x = taME$Maquina,y=taME$n,name="Azules",marker = list(
                color = 'blue'
            ))
            p
        }
    })
    #this is a listener wich when a event is osbserved in this case when a buton is pressed, it shows us a modal with a plot
    observeEvent(input$show, {
        showModal(modalDialog(
            size = "l",
            title = "Important message",
            renderPlot({
                ggplot(dataFrameTarjetas, aes(x = Linea, fill = Tipo)) + geom_bar()
                
            }),
            easyClose = TRUE
        ))
    })
}

shinyApp(ui, server)
