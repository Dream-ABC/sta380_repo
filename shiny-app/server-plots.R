source("../R/data_simulation.R")
source("../R/evaluation.R")
source("../R/permutation_test.R")

global_seed <- reactive(input$example_seed)

sim_data <- reactive({
  set.seed(global_seed())
  
  simulate_mixture_data(
    n = input$sample_size,
    p = input$num_features,
    n_relevant = min(input$num_relevant, input$num_features),
    alpha = 0.8,
    seed = global_seed()
  )
})

# Dynamically update selectors when num_features changes
observe({
  p <- input$num_features
  choices <- as.character(seq_len(p))
  updateSelectInput(session, "selected_feature_1",
                    choices = choices, selected = "1")
  updateSelectInput(session, "selected_feature_2",
                    choices = choices, selected = as.character(min(2, p)))
  updateSliderInput(session, "num_relevant",
                    max = p)
})

#####################################################
# KS                                                #
#####################################################

ks_pvalues <- reactive({
  permutation_pvalues(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_ks,
    n_shuffles = input$num_permutations
  )
})

ks_statistics <- reactive({
  apply(sim_data()$X, 2, function(x) stat_ks(x, sim_data()$Y))
})

ks_selected <- reactive({
  select_features_permutation(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_ks,
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

#####################################################
# CvM                                               #
#####################################################

cvm_pvalues <- reactive({
  permutation_pvalues(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_cvm,
    n_shuffles = input$num_permutations
  )
})

cvm_statistics <- reactive({
  apply(sim_data()$X, 2, function(x) stat_cvm(x, sim_data()$Y))
})

cvm_selected <- reactive({
  select_features_permutation(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_cvm,
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

#####################################################
# Evaluation                                        #
#####################################################

evaluation_results <- reactive({
  selected_features <- switch(
    input$stat_method_type,
    "ks" = ks_selected(),
    "cvm" = cvm_selected()
  )
  
  eval_obj <- evaluate_precision_recall(
    S_truth = sim_data()$S,
    S_hat = selected_features
  )
  
  data.frame(
    Method = switch(
      input$stat_method_type,
      "ks" = "KS",
      "cvm" = "CvM"
    ),
    Precision = format(
      round(eval_obj$precision, input$evaluation_digits),
      nsmall = input$evaluation_digits
    ),
    Recall = format(
      round(eval_obj$recall, input$evaluation_digits),
      nsmall = input$evaluation_digits
    ),
    F1 = format(
      round(f1_score(eval_obj$precision, eval_obj$recall),
            input$evaluation_digits),
      nsmall = input$evaluation_digits
    ),
    Selected_Features = length(selected_features),
    True_Relevant_Features = length(sim_data()$S)
  )
})

#####################################################
# Simulated Data Overview                           #
#####################################################

output$ks_plot_overview <- renderPlot({
  feature_1 <- min(as.numeric(input$selected_feature_1), input$num_features)
  feature_2 <- min(as.numeric(input$selected_feature_2), input$num_features)
  
  plot(sim_data()$X[, feature_1], sim_data()$X[, feature_2],
       xlab = paste("Feature", feature_1),
       ylab = paste("Feature", feature_2),
       main = "Scatter plot of the simulated data",
       pch = 19,
       cex.lab = 1,
       cex.axis = 1,
       cex.main = 1,
       col = ifelse(sim_data()$Y == 0, "steelblue", "tomato"))
  legend("topright",
         legend = c("Class 0", "Class 1"),
         col = c("steelblue", "tomato"),
         pch = 19,
         cex = 0.8)
}, res = 120, execOnResize = TRUE)

output$cvm_plot_overview <- renderPlot({
  feature_1 <- min(as.numeric(input$selected_feature_1), input$num_features)
  feature_2 <- min(as.numeric(input$selected_feature_2), input$num_features)
  
  plot(sim_data()$X[, feature_1], sim_data()$X[, feature_2],
       xlab = paste("Feature", feature_1),
       ylab = paste("Feature", feature_2),
       main = "Scatter plot of the simulated data",
       pch = 19,
       cex.lab = 1,
       cex.axis = 1,
       cex.main = 1,
       col = ifelse(sim_data()$Y == 0, "steelblue", "tomato"))
  legend("topright",
         legend = c("Class 0", "Class 1"),
         col = c("steelblue", "tomato"),
         pch = 19,
         text.col = "black",
         bg = "white")
}, res = 120, execOnResize = TRUE)

#####################################################
# Permutation p-values                              #
#####################################################

output$ks_plot_pvalues <- renderPlot({
  barplot(ks_pvalues(),
          names.arg = seq_along(ks_pvalues()),
          xlab = "Feature Index",
          ylab = "Permutation p-value",
          main = "Permutation p-values for all features",
          border = "black",
          # lwd = input$pvalue_bar_lwd,
          cex.lab = 1,
          cex.axis = 1,
          cex.main = 1,
          col = "lightblue")
  abline(h = input$alpha_level, col = "red", lty = 2, lwd = 2)
}, res = 120, execOnResize = TRUE)

output$cvm_plot_pvalues <- renderPlot({
  barplot(cvm_pvalues(),
          names.arg = seq_along(cvm_pvalues()),
          xlab = "Feature Index",
          ylab = "Permutation p-value",
          main = "Permutation p-values for all features",
          border = "black",
          # lwd = input$pvalue_bar_lwd,
          cex.lab = 1,
          cex.axis = 1,
          cex.main = 1,
          col = "lightblue")
  abline(h = input$alpha_level, col = "red", lty = 2, lwd = 2)
}, res = 120, execOnResize = TRUE)

#####################################################
# Test Statistics                                   #
#####################################################

output$ks_plot_statistics <- renderPlot({
  barplot(ks_statistics(),
          names.arg = seq_along(ks_statistics()),
          xlab = "Feature Index",
          ylab = "Test Statistic",
          main = "KS statistics for all features",
          border = "black",
          # lwd = input$stat_bar_lwd,
          cex.lab = 1,
          cex.axis = 1,
          cex.main = 1,
          col = "lightgreen")
}, res = 120, execOnResize = TRUE)

output$cvm_plot_statistics <- renderPlot({
  barplot(cvm_statistics(),
          names.arg = seq_along(cvm_statistics()),
          xlab = "Feature Index",
          ylab = "Test Statistic",
          main = "CvM statistics for all features",
          border = "black",
          # lwd = input$stat_bar_lwd,
          cex.lab = 1,
          cex.axis = 1,
          cex.main = 1,
          col = "lightgreen")
}, res = 120, execOnResize = TRUE)

#####################################################
# Selected Features                                 #
#####################################################

output$ks_table_selected <- renderTable({
  selected_df <- data.frame(
    Feature_Index = ks_selected(),
    P_Value = round(ks_pvalues()[ks_selected()], 4)
  )
  head(selected_df, input$selected_table_nrows)
})

output$cvm_table_selected <- renderTable({
  selected_df <- data.frame(
    Feature_Index = cvm_selected(),
    P_Value = round(cvm_pvalues()[cvm_selected()], 4)
  )
  head(selected_df, input$selected_table_nrows)
})

#####################################################
# Evaluation Table                                  #
#####################################################

output$evaluation_table <- renderTable({
  evaluation_results()
})

