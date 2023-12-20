library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)

# Define UI
ui <- fluidPage(
  titlePanel("File Browse and Display"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      verbatimTextOutput("summary_text"),
      actionButton("clean_button", "Clean"),
      selectInput("x_axis", "X-axis", ""),
      selectInput("y_axis", "Y-axis", ""),
      selectInput("color_plots", "Choose Color for Plots", 
                  choices = c("red", "green", "blue", "yellow","black")),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Display", DTOutput("table")),
        tabPanel("Boxplot", 
                 plotOutput("boxplot"),
                 actionButton("stats_button", "Stats"),
                 verbatimTextOutput("boxplot_stats_text")),
        tabPanel("Histogram", 
                 plotOutput("histogram"),
                 verbatimTextOutput("histogram_text")),
        tabPanel("Scatter", 
                 plotOutput("scatter"),
                 verbatimTextOutput("scatterplot_text")),
        tabPanel("Bar", 
                 plotOutput("barplot"),
                 verbatimTextOutput("barplot_text")),
        tabPanel("Pie", 
                 plotOutput("piechart")),
        tabPanel("Tree",
                 plotOutput("decision_tree"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  boxplot_stats <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    # Read CSV and replace empty strings with NA
    data(read.csv(input$file$datapath, na.strings = c("", "NA")))
    
    # Update the X-axis and Y-axis choices based on the selected file
    updateSelectInput(session, "x_axis", choices = colnames(data()))
    updateSelectInput(session, "y_axis", choices = colnames(data()))
  })
  
  output$table <- renderDT({
    datatable(data())
  })
  
  output$summary_text <- renderPrint({
    summary_data <- data()
    
    if (!is.null(summary_data)) {
      cat("Sum of Null Values in Dataframe: ", sum(is.na(summary_data)), "\n\n")
      cat("Sum of Null Values in Each Column:\n")
      print(colSums(is.na(summary_data)))
      
      cat("\n\nSum of Duplicated Values in Dataframe: ", sum(duplicated(summary_data)), "\n\n")
    } else {
      cat("Please select a file.")
    }
  })
  
  observeEvent(input$clean_button, {
    if (!is.null(data())) {
      cleaned_data <- data()
      
      cleaned_data$is_promoted <- as.logical(cleaned_data$is_promoted) #convert data type of column
      
      # Remove rows with null values
      cleaned_data <- na.omit(cleaned_data)
      
      # Age outlier removal
      #  remove the outliers from age
      cleaned_data_withoutOutliers_age <- cleaned_data
      for (i in 1:nrow(cleaned_data_withoutOutliers_age)) {
        tmp <- cleaned_data_withoutOutliers_age$age[i]
        if (tmp > 47) {
          cleaned_data_withoutOutliers_age$age[i] <- NA
        }
      }
      cleaned_data_withoutOutliers_age <- na.omit(cleaned_data_withoutOutliers_age)
      #---------------------------------------------------------------------
      #---------------------------------------------------------------------
      #  remove the outliers from length_of_service
      cleaned_data_withoutOutliers_length_of_service <- cleaned_data_withoutOutliers_age
      for(i in 1:nrow(cleaned_data_withoutOutliers_length_of_service)){
        tmp <- cleaned_data_withoutOutliers_length_of_service$length_of_service[i]
        if(tmp > 13){
          cleaned_data_withoutOutliers_length_of_service$length_of_service[i] <- NA
        }
      }
      cleaned_data_withoutOutliers_length_of_service <- na.omit(cleaned_data_withoutOutliers_length_of_service)
      #---------------------------------------------------------------------
      #---------------------------------------------------------------------
      #  remove the outliers from previous_year_rating
      cleaned_data_withoutOutliers <- cleaned_data_withoutOutliers_length_of_service
      for(i in 1:nrow(cleaned_data_withoutOutliers)){
        tmp <- cleaned_data_withoutOutliers$previous_year_rating[i]
        if(tmp == 1){
          cleaned_data_withoutOutliers$previous_year_rating[i] <- NA
        }
      }
      cleaned_data_withoutOutliers <- na.omit(cleaned_data_withoutOutliers)
      
      # Update the reactive value with the cleaned data
      data(cleaned_data_withoutOutliers)
    } else {
      cat("Please select a file before clicking the Clean button.")
    }
  })
  
  output$boxplot <- renderPlot({
    x_var <- input$x_axis
    
    if (!is.null(x_var) && !is.null(data())) {
      if (x_var %in% c("no_of_trainings", "age", "previous_year_rating","length_of_service","avg_training_score")){
        ggplot(data(), aes_string(x = x_var)) +
          geom_boxplot(color = input$color_plots) +
          labs(title = paste("Boxplot of", x_var),
               y = "Values",
               x = x_var) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      }
    }
  })
  
  observeEvent(input$stats_button, {
    x_var <- input$x_axis
    
    if (!is.null(x_var) && !is.null(data())) {
      if (x_var %in% c("no_of_trainings", "age", "previous_year_rating","length_of_service","avg_training_score")){# Calculate boxplot stats
        boxplot_outliers <- isolate(boxplot(data()[, x_var])$out)
        
        # Update the reactive value with boxplot stats and outliers
        boxplot_stats(list(
          outliers = boxplot_outliers,
          stats = boxplot(data()[, x_var])$stats
        ))}
    }
  })
  output$boxplot_stats_text <- renderPrint({
    if (!is.null(boxplot_stats())) {
      cat("Outliers:\n")
      print(boxplot_stats()$outliers)
      
      cat("\nBoxplot Stats:\n")
      print(boxplot_stats()$stats)
    } else {
      cat("Please click the 'Stats' button after selecting the X-axis variable.")
    }
  })
  
  output$histogram <- renderPlot({
    x_var <- input$x_axis
    
    if (!is.null(x_var) && !is.null(data())) {
      if (x_var %in% c("no_of_trainings", "age", "previous_year_rating","length_of_service","avg_training_score")){ggplot(data(), aes_string(x = x_var)) +
          geom_histogram(binwidth = 1, fill = input$color_plots) +
          labs(title = paste("Histogram of", x_var),
               y = "Frequency",
               x = x_var) +
          theme_minimal()}
    }
  })
  
  output$histogram_text <- renderText({
    if (!is.null(input$color_plots)) {
      paste("Plot color selected:", input$color_plots)
    }
  })
  
  output$scatter = renderPlot({
    x_var <- input$x_axis
    y_var <- input$y_axis
    
    if (!is.null(x_var) && !is.null(data()) && !is.null(y_var)) {
      if (x_var %in% c("no_of_trainings", "age", "previous_year_rating","length_of_service","avg_training_score","awards_won.")){
        ggplot(data(), aes_string(x = x_var, y = y_var)) +
          geom_point(color = input$color_plots) +
          labs(title = "Scatterplot",
               y = y_var,
               x = x_var) +
          theme_minimal()
      }
    }
  })
  
  
  output$scatterplot_text <- renderText({
    if (!is.null(input$color_plots)) {
      paste("Plot color selected:", input$color_plots)
    }
  })
  
  output$barplot <- renderPlot({
    x_var <- input$x_axis
    
    if (!is.null(x_var) && !is.null(data())) {
      ggplot(data(), aes_string(x = x_var)) +
        geom_bar(fill = input$color_plots) +
        labs(title = paste("Barplot of", x_var),
             y = "Frequency",
             x = x_var) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    }
  })
  
  output$barplot_text <- renderText({
    if (!is.null(input$color_plots)) {
      paste("Plot color selected:", input$color_plots)
    }
  })
  
  output$piechart <- renderPlot({
    x_var <- input$x_axis
    
    if (!is.null(x_var) && !is.null(data())) {
      if (x_var %in% c("education", "gender", "is_promoted","awards_won.", "recruitment_channel")) {
        x <- table(data()[, x_var])
        percentage <- paste0(round(100 * (x / sum(x))), "%")
        
        pie(x, main = paste("Pie Chart of", x_var), labels = percentage,col=c("cyan","yellow","green"))
        legend("bottomright", legend = names(x), fill = c("cyan", "yellow", "green"), title = "Categories")
      }
    }
  })
  
  output$decision_tree <- renderPlot({
    if (!is.null(data())) {
      tree <- rpart(is_promoted ~ department + previous_year_rating + awards_won. + avg_training_score, 
                    data = data(), minsplit = 2)
      
      rpart.plot(tree, type = 2, extra = 100, cex = 0.8, box.col = c("lightpink", "mistyrose", "seashell", "thistle", "thistle1", "seashell2"))
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)