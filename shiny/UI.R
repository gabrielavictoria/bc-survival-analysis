# UI
library(shiny)
library(readxl)

# Load the dataset (to access max OS_mos for the slider input)
data <- read_excel("data/20240424_BreastCancer_n90.xlsx")

# Define the UI
shinyUI(navbarPage("Breast Cancer Survival Analysis",
                   tabPanel("Kaplan-Meier Survival Graph",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Survival Graph"),
                                
                                # Dropdown for selecting the categorical variable
                                selectInput('sur_var', 'Factor of Survival', 
                                            choices = c("Chemotherapy", "BMI", "age60"), 
                                            selected = "Chemotherapy"),
                                
                                # Slider for selecting the time point (in months)
                                sliderInput('xvalue', 'Survival Months = ', 
                                            value = 24, min = 1, max = 12)
                              ),
                              mainPanel(
                                h3(textOutput("caption")),
                                plotOutput("plot1"),
                                tableOutput("center")
                              )
                            )
                   ),
                   tabPanel("Descriptive Statistics",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("desc_var", "Select Variable", 
                                            choices = c("Chemotherapy", "BMI", "Age", "MarkerB"),
                                            selected = "Chemotherapy")
                              ),
                              mainPanel(
                                h3("Descriptive Statistics"),
                                tableOutput("desc_table")
                              )
                            )
                   ),
                   tabPanel("Cox Proportional Hazards Model",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("cox_var", "Select Predictor Variable", 
                                            choices = c("Chemotherapy", "BMI", "Age", "MarkerB"),
                                            selected = "Chemotherapy"),
                                actionButton("run_cox", "Run Cox Model")
                              ),
                              mainPanel(
                                h3("Cox Model Results"),
                                tableOutput("cox_results"),
                                verbatimTextOutput("cox_summary")
                              )
                            )
                   )
                   
))