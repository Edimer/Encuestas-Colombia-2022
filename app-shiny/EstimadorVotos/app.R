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

source("../../functions-R/explor_candidate.R")

datos <- read_csv("../../data/EncuestasWikipedia-Colombia2022.csv") %>%
  mutate(fecha_publicacion = as.Date(fecha_publicacion, format = "%d-%m-%Y")) %>% 
  select(fuente:margen_de_error,
         alfredo_saade,
         arelis_uriana,
         camilo_romero,
         francia_marquez,
         gustavo_petro,
         
         alejandro_gaviria,
         carlos_amaya,
         jorge_enrique_robledo,
         juan_manuel_galan,
         sergio_fajardo,
         
         alejandro_char,
         aydee_lizarazo,
         david_barguil,
         enrique_penalosa,
         federico_gutierrez,
         
         blanco,
         ninguno,
         ns_nr
  )


estimaciones <- read_csv("../../data/estimaciones_consultas_presidenciales.csv") %>%
  pivot_longer(cols = -candidato) %>%
  separate(name, into = c("tipo", "metodo")) %>%
  pivot_wider(names_from = tipo, values_from = value)


ui <- fluidPage(
  
  titlePanel(title = "Consultas presidenciales en Colombia 2022"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      
      width = 3,
      
      selectInput(
        inputId = "id_candidato",
        label = "Candidato:",
        choices = datos %>% 
          select(alfredo_saade:federico_gutierrez) %>% 
          names(),
        multiple = FALSE,
        selected = "gustavo_petro"
      ),
      
      radioButtons(
        inputId = "id_infer",
        label = "Método:",
        choices = estimaciones %>% pull(metodo) %>% unique()
      ),
      
      numericInput(
        inputId = "id_votantes",
        label = "Votantes:",
        value = 15000000
      )
      
    ),
    
    mainPanel = mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Resultados Exploratorios",
          plotOutput(outputId = "res1", height = "650px")
        ),
        tabPanel(
          title = "Resultados Inferenciales",
          dataTableOutput(outputId = "res2")
        )
      )
    )
    
  )
  
)


server <- function(input, output) {
  
  output$res1 <- renderPlot({
    explor_candidate(data = datos, candidate = input$id_candidato)
  })
  
  output$res2 <- renderDataTable({
    estimaciones %>% 
      filter(candidato == input$id_candidato) %>% 
      filter(metodo == input$id_infer) %>% 
      mutate(
        estimate_votantes = (estimate * input$id_votantes) / 100,
        li_votantes = (li * input$id_votantes) / 100,
        ls_votantes = (ls * input$id_votantes) / 100
      ) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
