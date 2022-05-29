library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(rvest)
library(lubridate)
library(cowplot)
library(ggsci)
library(splines)
library(qqplotr)
library(broom)
library(infer)
library(plotly)
library(bslib)
library(thematic)
library(shinyWidgets)

source("R/plot_distribution.R", encoding = "UTF-8")
source("R/plot_time.R", encoding = "UTF-8")

datos <- read_csv("data/EncuestasColombia2022-Update.csv") %>% 
  rename(voto_en_blanco = blanco)

candidatos <-
  datos %>%
  select(voto_en_blanco:enrique_gomez) %>%
  select(-ingrid_betancourt) %>% 
  names()

estimaciones <- read_csv("data/estimaciones-finales.csv") %>% 
  mutate(Candidato = str_replace_all(Candidato, "blanco", "voto_en_blanco")) 

thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    bg = "#202123",
    fg = "#FDF7F7",
    primary = "#ED79F9",
    base_font = font_google("Prompt")
  ),
  
  HTML(
    "<div style='float:left'></div><hr color='#FDF7F7' size=1px width=1000px>"
  ), 
  
  titlePanel(
    title = div("ELECCIONES PRESIDENCIALES - COLOMBIA 2022", align = "center"),
    windowTitle = "ELECCIONES PRESIDENCIALES - COLOMBIA 2022"
  ),
  
  HTML(
    "<div style='float:left'></div><hr color='#FDF7F7' size=1px width=1000px>"
  ),
  
  wellPanel(fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "id_candidato",
        label = "Candidato:",
        choices = candidatos,
        multiple = FALSE
      )
    ),
    column(
      width = 4,
      radioButtons(
        inputId = "id_infer",
        label = "Método Estadístico:",
        choices = c(
          "Método 1" = "t-student" ,
          "Método 2" = "t-student - log",
          "Método 3" = "BootPromedio-ICQ",
          "Método 4" = "BootPromedio-ICEE",
          "Método 5" = "BootMediana-ICQ",
          "Método 6" = "BootMáximo-ICQ"
        ),
        inline = TRUE
      )
    ),
    column(
      width = 4,
      numericInput(
        inputId = "id_votantes",
        label = "Total Votantes:",
        value = 15000000
      )
    )
  )),
  
  fluidRow(
    column(
      width = 6,
      radioGroupButtons(
        justified = TRUE,
        inputId = "id_tipogg",
        label = "",
        choices = c("Distribución", "Serie Temporal"),
        checkIcon = list(yes = icon("ok",
                                    lib = "glyphicon"))
      ),
      plotOutput(outputId = "res1")
    ),
    column(
      width = 6,
      tags$h4("Estimación de intención de voto (%)"),
      dataTableOutput(outputId = "res2"),
      tags$br(),
      tags$h4("Total de votantes estimados"),
      dataTableOutput(outputId = "res3"),
      tags$br(),
      div(img(src="logo1.png", width = 100), tags$a(href="https://github.com/Edimer/Encuestas-Colombia-2022", "¡Código fuente en Github!") , img(src="logo2.png", width = 95), align = "center")
    )
  )
  
)


server <- function(input, output) {
  output$res1 <- renderPlot({
    if(input$id_tipogg == "Distribución"){
      plot_distribution(
        data = datos,
        agrupado = FALSE,
        tipo = "densidad",
        candidato = input$id_candidato
        
      )
    } else if(input$id_tipogg == "Serie Temporal"){
      plot_time(data = datos,
                tipo = "Serie Individual",
                candidato = input$id_candidato)
    }
  })
  
  output$res2 <- renderDataTable({
    estimaciones %>%
      filter(Candidato == input$id_candidato) %>%
      filter(Método == input$id_infer) %>%
      select(-Método) %>%
      datatable(rownames = FALSE,
                options = list(dom = 'B'))
  })
  
  output$res3 <- renderDataTable({
    
    estimaciones %>%
      filter(Candidato == input$id_candidato) %>%
      filter(Método == input$id_infer) %>%
      select(-Método) %>%
      rename(prop = `Intención de voto (%)`,
             li = `L. Inferior`,
             ls = `L. Superior`) %>%
      mutate(
        `Total Votos` = round((prop * input$id_votantes) / 100, digits = 0),
        `L. Inferior` = round((li * input$id_votantes) / 100, digits = 0),
        `L.Superior` = round((ls * input$id_votantes) / 100, digits = 0)
      ) %>%
      select(-c(prop, li, ls)) %>%
      datatable(rownames = FALSE,
                options = list(dom = 'B'))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
