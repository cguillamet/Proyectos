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


# Use a fluid Bootstrap layout
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Cáncer en TDF"),
    dashboardSidebar(sidebarMenu(
        menuItem("Casos en TDF", icon = icon("bar-chart-o"),
                 selectInput("tipo", "Tipo de cáncer:", 
                             choices=sort(unique(datos$TOP..cat.)),
                             multiple = TRUE),
                 helpText("Para eliminar un tipo utilice la tecla",
                          br(), #salto de línea
                          "'delete' de su teclado")),
        menuItem("Características", icon = icon("bar-chart-o"),
                 radioButtons("carac", "Tipo de cáncer:", 
                              choices=sort(unique(datos2$TOP..cat.)))))),
    dashboardBody(
        tags$head(tags$style(HTML(
            '.main-header .logo{
        font-family: "Calibri", sans-serif;
        font-size: 24px;}
        .body{
        background-color: green;
        }'
        ))),
        fluidRow(
            box(plotOutput("cancerPlot"), width = 12),
            box(plotOutput("cplot")),
            box(plotOutput("edadplot"))
        )
    )
)
