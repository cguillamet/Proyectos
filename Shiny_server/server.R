library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

datos <- read.csv("base2015_2020.csv")
datos$año <- substr(datos$FECDIAG, start = 1, stop = 4)
datos <- filter(datos, año %in% c("2015", "2016", "2017", "2018", "2019"))

datos2 <- datos %>%
    group_by(TOP..cat.) %>%
    tally() %>%
    mutate(porc = round(n/sum(n),2)) %>%
    filter(porc > 0.02)

tipo <- datos %>%
    group_by(TOP..cat., año) %>%
    tally() %>% rename(tipo = "TOP..cat.")

tipo2 <- datos %>%
    group_by(TOP..cat., año, SEXO..desc.) %>%
    tally() %>% rename(tipo = "TOP..cat.", sexo = "SEXO..desc.")

server <- function(input, output) {
    
    dat <- reactive({
        req(input$tipo)
        filter(tipo, tipo %in% input$tipo)
    })
    
    dat2 <- reactive({
        req(input$carac)
        filter(tipo2, tipo %in% input$carac)
    })
    
    dto <- reactive({
        req(input$carac)
        filter(datos, TOP..cat. %in% input$carac)
    })
    
    # Fill in the spot we created for a plot
    output$cancerPlot <- renderPlot({
        
        # Render a plot
        ggplot(dat(), aes(x = año, y = n, group = tipo)) + 
            geom_line(size = 1, aes(colour = tipo)) +
            labs(x = "Año", y = "Frecuencia", group = "Topografía", 
                 color = "Topografía", title = "Cantidad de nuevos casos según tipo de cáncer")
    })
    
    output$cplot <- renderPlot({
        
        ggplot(dat2(), aes(x = año, y = n, fill = sexo)) + 
            geom_bar(stat = "identity", position=position_dodge()) +
            labs(x = "Año", y = "Frecuencia", fill = "Sexo", title = "Cantidad de nuevos casos según tipo de cáncer y sexo")
    })
    
    output$edadplot <- renderPlot({
        
        ggplot(dto(), aes(x = SEXO..desc., y = EDAD, group = SEXO..desc.))+
            geom_boxplot(aes(colour = SEXO..desc.)) +
            labs(x = "Sexo", y = "Edad", color = "Sexo", title = "Boxplot de las edades de las personas que presentaron los nuevos casos de cáncer según tipo y sexo")
    })
    
}

