library(shiny)
library(dplyr)
library(ggplot2)

setwd("C:/Users/nazgo/Desktop/Courses/Practical Data Science/Week 10 Interactive UI")
income <- read.csv("income.csv")

ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "subset_income",
                  label = "Select Income",
                  choices = "<=50K","50K"),
      selectInput(inputId = "set_yaxis",
                  label = "Select y-axis",
                  choices = "hours_per_week", "capital_loss"),
      checkboxGroupInput(inputId = "subset_occupation",
                         label = "Include Occupation",
                         choices = unique(income$occupation),
                         selected = unique(income$occupation))),
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)

server <- function(input, output) {
  create_subset <- reactive(income %>%
                              filter(capital_loss>0 & 
                                       income == input$subset_income & 
                                       occupation %in% input$subset_occupation))
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  geom_boxplot(aes_string(x="occupation",
                                                          y=input$set_yaxis)) +
                                  theme_bw(18) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

# ---- Run App ----
shinyApp(ui, server)