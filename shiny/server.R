library(survival)
library(shiny)
library(ggplot2)
library(readxl)
library(broom)
library(kableExtra)
library(dplyr)

# Load your dataset
data <- read_excel("data/20240424_BreastCancer_n90.xlsx")

# Create the binary variable for age
data$age60 <- ifelse(data$Age >= 60, 1, 0)

shinyServer(function(input, output, session) {
  
  # Reactive expression for the selected data
  selectedData <- reactive({
    data[, input$sur_var]
  })
  
  # Dynamic caption
  output$caption <- renderText({
    paste("Kaplan-Meier Plot for", input$sur_var, sep="\n")
  })
  
  # Running the survival function
  runSur <- reactive({
    survfit(as.formula(paste("Surv(OS_mos, V_status) ~ ", paste(input$sur_var))), data = data)
  })
  
  output$plot1 <- renderPlot({
    fit <- runSur()
    
    # Define colors for the strata
    strata_colors <- c("red", "sky blue", "green", "purple", "orange", "yellow")
    
    # Get the selected variable (e.g., BMI, Chemotherapy)
    selected_var <- input$sur_var
    strata_levels <- levels(as.factor(data[[selected_var]]))
    
    # Create a mapping for BMI or any other categorical variables
    strata_labels <- strata_levels # default to levels
    
    if (selected_var == "BMI") {
      strata_labels <- c("Underweight", "Normal", "Overweight", "Obese")
    } else if (selected_var == "Chemotherapy") {
      strata_labels <- c("No Chemotherapy", "Chemotherapy")
    } else if (selected_var == "age60") {
      strata_labels <- c("<60 years", "≥60 years")
    }
    
    # Plot the survival curves
    plot(fit, 
         col = strata_colors, 
         xlab = "Months", ylab = "S(t)")
    
    # Add a legend with the descriptive labels
    legend("bottomleft", cex = 0.9, legend = strata_labels, 
           fill = strata_colors[1:length(strata_labels)])
    
    # Add a vertical line at the selected time point
    abline(v = input$xvalue, col = 1, lty = 2)
  })
  
  # Table for survival probabilities
  output$center <- renderTable({
    # Use the `summary` function with `extend = TRUE`
    surv_summary <- summary(runSur(), times = input$xvalue, extend = TRUE)
    
    # Extract the relevant columns from the summary output
    surv_df <- as.data.frame(surv_summary[c("surv", "time", "strata")])
    
    # Rename the columns
    colnames(surv_df) <- c("S(t)", "Time (mo.)", "Strata")
    
    # Modify strata labels
    surv_df$Strata <- gsub("Chemotherapy=1", "No Chemotherapy", surv_df$Strata)
    surv_df$Strata <- gsub("Chemotherapy=2", "Chemotherapy", surv_df$Strata)
    surv_df$Strata <- gsub("BMI=1", "Underweight", surv_df$Strata)
    surv_df$Strata <- gsub("BMI=2", "Normal", surv_df$Strata)
    surv_df$Strata <- gsub("BMI=3", "Overweight", surv_df$Strata)
    surv_df$Strata <- gsub("BMI=4", "Obese", surv_df$Strata)
    surv_df$Strata <- gsub("age60=0", "<60 years", surv_df$Strata)
    surv_df$Strata <- gsub("age60=1", ">=60 years", surv_df$Strata)
    
    # Reorder the columns to: Strata, Time (mo.), and S(t)
    surv_df <- surv_df[, c("Strata", "Time (mo.)", "S(t)")]
    
    return(surv_df)
  })
  
  output$desc_table <- renderTable({
    desc_var <- input$desc_var
    
    if (desc_var == "Chemotherapy") {
      summary_table <- data %>%
        group_by(Chemotherapy) %>%
        summarize(
          Count = n(),
          Percent = (n() / nrow(data)) * 100,
          .groups = 'drop'
        ) %>%
        mutate(Chemotherapy = ifelse(Chemotherapy == 1, "No Chemotherapy", "Chemotherapy")) %>%
        select(Chemotherapy, Count, Percent)
      
    } else if (desc_var == "BMI") {
      summary_table <- data %>%
        group_by(BMI) %>%
        summarize(
          Count = n(),
          Percent = (n() / nrow(data)) * 100,
          .groups = 'drop'
        ) %>%
        mutate(BMI = recode(BMI,
                            `1` = "Underweight",
                            `2` = "Normal",
                            `3` = "Overweight",
                            `4` = "Obese")) %>%
        select(BMI, Count, Percent)
      
    } else if (desc_var == "Age") {
      summary_table <- data %>%
        summarize(
          Mean_Age = mean(Age, na.rm = TRUE),
          SD_Age = sd(Age, na.rm = TRUE),
          Count_Under_60 = sum(Age < 60),
          Count_60_or_Older = sum(Age >= 60),
          Percent_Under_60 = (sum(Age < 60) / n()) * 100,
          Percent_60_or_Older = (sum(Age >= 60) / n()) * 100
        )
      
    } else if (desc_var == "MarkerB") {
      summary_table <- data %>%
        summarize(
          Mean_MarkerB = mean(MarkerB, na.rm = TRUE),
          SD_MarkerB = sd(MarkerB, na.rm = TRUE)
        )
    }
    
    return(summary_table)
  })
  # Prediction
  
  cox_model <- coxph(Surv(OS_mos, V_status) ~ Chemotherapy + Age + BMI + MarkerB, data = data)
  observeEvent(input$run_cox, {
    cox_model <- coxph(Surv(OS_mos, V_status) ~ data[[input$cox_var]], data = data)
    
    output$cox_results <- renderTable({
      # Get the model coefficients
      tidy_cox <- tidy(cox_model)
      tidy_cox
    })
    
    output$cox_summary <- renderPrint({
      summary(cox_model)
    })
  })
  
})
