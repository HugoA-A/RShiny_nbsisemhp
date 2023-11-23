### Define the server
server <- function(input, output, session) {
  set.seed(1)
  
  nb_model <- reactiveVal(naivebayes_classifier$new())
  
  # Store loaded data
  data <- reactiveVal(NULL)
  
  # Store the names of all columns
  column_names <- reactiveVal(NULL)
  
  # Store the name of the selected y column
  selected_y_column <- reactiveVal(NULL)
  
  # Store the value of processed_data
  processed_data_val <- reactiveVal(NULL)
  
  # Observe event for data loading
  observeEvent(input$file, {
    inFile <- input$file
    if (!is.null(inFile)) {
      if (grepl(".csv$", inFile$name)) {
        data <- read.csv(inFile$datapath)
      } else if (grepl(".xlsx$", inFile$name)) {
        data <- read_excel(inFile$datapath)
      }
      data(data)
      
      # Store the names of all columns
      column_names(names(data))
    }
  })
  
  # Drop-down menu to select column y
  output$chooseYColumn <- renderUI({
    if (!is.null(column_names())) {
      selectInput("yColumn", "Choisir votre y", choices = column_names())
    }
  })
  
  # observe event for the choice of conon y
  observeEvent(input$yColumn, {
    # Store the name of the selected y column
    selected_y_column(input$yColumn)
  })
  
  
  
  # Validate button to trigger processed_data
  observeEvent(input$validateButton, {
    # Check whether a column has been selected there
    if (!is.null(selected_y_column())) {
      # Store the name of the selected y
      selected_y <- selected_y_column()
      # Create to separate dataframe for X and y 
      X <- data()[, !(names(data()) %in% selected_y)] 
      y <- data()[,selected_y] 
      processed_data_list <- list(X = X, y = y)
      processed_data_val(list(X = X, y = y))
      # Store names of X data
      available_X_columns <- setdiff(names(data()), selected_y_column())
      # Update the selection of explanatory variables 
      updateSelectInput(session, "selectedXVariables", choices = available_X_columns)
    }
  })
  
  
  # Observe event for the View data button
  observeEvent(input$viewDataButton, {
    selected_y <- selected_y_column()
    numRows <- input$numRows
    processed <- processed_data_val()
    processed$X <- as.data.frame(processed$X)
    processed$y <- as.data.frame(processed$y)
    names(processed$y) <- selected_y
    output$X_output <- renderPrint({
      head(processed$X, n = numRows)
    })
    output$y_output <- renderPrint({
      head(processed$y, n = numRows)
    })
  })
  
  # Generate statistics
  output$summaryTable <- renderDataTable({
    req(input$generateStats)
    descr_stats <- descr(data())
    datatable(descr_stats)
  })
  
  # Store split results in a reactive variable
  split_results <- reactiveVal(NULL)
  
  # Observe event for the splitdata button
  observeEvent(input$splitDataButton, {
    
    test_size_react <- input$testSize
    # Dividing data in a stratified way
    split_result <- nb_model()$stratified_split(processed_data_val()$X, processed_data_val()$y, test_size = test_size_react)
    
    # Store drive and test assemblies in a reactive object
    split_results(split_result)
    output$message_split <- renderText("Data successfully split!")
  })
  
  
  
  # Store the prediction
  predictions <- reactiveVal(NULL)
  
  
  
  # Observer pour le bouton "Fit"
  observeEvent(input$trainButton, {
    stored_results <- split_results()
    
    # Use only selected variables
    selected_variables <- input$selectedXVariables
    X_train <- as.data.frame(stored_results$X_train[, selected_variables])
    
    # Use the training set to train the model
    
    nb_model()$fit(X_train, stored_results$y_train)
    
    output$message <- renderText("Model successfully trained!")
  })
  
  
  # Observe event for the Predict button
  observeEvent(input$predictButton, {
    # Accessing stored results
    stored_results <- split_results()
    
    selected_variables <- input$selectedXVariables
    X_test <- as.data.frame(stored_results$X_test[, selected_variables])
    
    
    # Using the test set to make predictions
    preds <- nb_model()$predict(X_test)
    # Store predictions
    predictions(preds)
  })
  
  # Export results
  output$exportButton <- downloadHandler(
    filename = function() {
      "predictions.csv"
    },
    content = function(file) {
      # Accessing stored results
      stored_results <- split_results()
      
      # Export results in CSV format
      write.csv(nb_model()$predict(stored_results$X_test), file, row.names = FALSE)
    }
  )
  
  # Display predictions in a table
  output$predictionTable <- renderDT({
    predictions_data <- predictions()
    if (!is.null(predictions_data)) {
      data.frame(Prediction = unlist(predictions_data))
    } else {
      return(NULL)  # Return NULL if predictions are empty
    }
  })
  
  # Calculate the accuracy of predictions
  accuracy_score <- reactiveVal(NULL)
  
  # Observe event for the score button
  observeEvent(input$scoreButton, {
    
    # Accessing stored results
    stored_results <- split_results()
    
    # Retrieve the selected score type
    score_type <- input$scoreType
    
    # Calculate the accuracy of predictions on the test set
    accuracy <- nb_model()$score(predictions(), stored_results$y_test, as = score_type, pred_f = FALSE)
    # Storing precision
    accuracy_score(accuracy)
  })
  
  # Display precision output
  output$accuracyOutput <- renderPrint({
    accuracy <- accuracy_score()
    if (!is.null(accuracy && input$scoreType == "accuracy")) {
      print(paste("Model accuracy:", round(accuracy * 100, 2), "%"))
    } else {
      return(NULL)
    }
  })
  
  # Display the table as output if the score type is "table".
  output$scoreTable <-  renderPrint({
    accuracy <- accuracy_score()
    if (!is.null(accuracy) && input$scoreType == "table") {
      as.matrix(accuracy)
    } else {
      return(NULL)
    }
  })
  
  # Initialize reactiveVal for probabilities
  prediction_proba <- reactiveVal(NULL)
  
  # Observe event for the Generate Probabilities button
  observeEvent(input$generateProbabilitiesButton, {
    # Accessing stored results
    stored_results <- split_results()
    
    selected_variables <- input$selectedXVariables
    X_test <- as.data.frame(stored_results$X_test[, selected_variables])
    
    # Using the test set to generate probabilities
    preds_prob <- nb_model()$predict_proba(X_test)
    # Store probabilities
    prediction_proba(preds_prob)
  })
  
  # Output for displaying probabilities
  output$probabilityOutput <- renderPrint({
    probs <- prediction_proba()
    if (!is.null(probs)) {
      print(probs)
    } else {
      return(NULL)
    }
  })
  
  # Observer for the "Plot" button
  observeEvent(input$plotButton, {
    # Check if nb_model has been trained

    # Call the plot function and store the plot
    nb_plot <- plot(nb_model())
    # Display the plot
    output$plot_freq <- renderPlotly({
      nb_plot
     })
  })
  
}
