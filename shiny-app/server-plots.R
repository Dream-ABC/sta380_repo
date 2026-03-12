# source needed functions
source("../R/data_simulation.R", local = TRUE)
source("../R/permutation_test.R", local = TRUE)
source("../R/evaluation.R", local = TRUE)

global_seed <- reactive(input$example_seed)

sim_data_comp <- reactive({
  set.seed(global_seed())

  simulate_mixture_data(
    n = input$sample_size,
    p = input$num_features,
    n_relevant = input$num_relevant,
    alpha = 0.8,  # TODO: do we need user input for this?
    seed = global_seed()
  )
})

mean_diff_stat_comp <- reactive({
  apply(sim_data_comp()$X, 2, function(x) {
    stat_mean_diff(x, sim_data_comp()$Y)
  })
})

ks_stat_comp <- reactive({
  apply(sim_data_comp()$X, 2, function(x) {
    stat_ks(x, sim_data_comp()$Y)
  })
})

cvm_stat_comp <- reactive({
  apply(sim_data_comp()$X, 2, function(x) {
    stat_cvm(x, sim_data_comp()$Y)
  })
})

mean_diff_pval_comp <- reactive({
  permutation_pvalues(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_mean_diff,
    n_shuffles = input$num_permutations
  )
})

ks_pval_comp <- reactive({
  permutation_pvalues(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_ks,
    n_shuffles = input$num_permutations
  )
})

cvm_pval_comp <- reactive({
  permutation_pvalues(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_cvm,
    n_shuffles = input$num_permutations
  )
})

mean_diff_selected_comp <- reactive({
  select_features_permutation(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_mean_diff,
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

ks_selected_comp <- reactive({
  select_features_permutation(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_ks,
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

cvm_selected_comp <- reactive({
  select_features_permutation(
    X = sim_data_comp()$X,
    Y = sim_data_comp()$Y,
    stat_fun = stat_cvm,
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

#####################################################
# Mean Difference                                   #
#####################################################

output$mean_diff_plot_overview <- renderPlot({
  plot(sim_data_comp()$X[, as.numeric(input$selected_feature_1)],
       sim_data_comp()$X[, as.numeric(input$selected_feature_2)],
       xlab = paste("Feature", input$selected_feature_1),
       ylab = paste("Feature", input$selected_feature_2),
       main = "Scatter plot of the simulated data for selected features
       under the Mean Difference statistic",
       pch = 19,
       col = ifelse(sim_data_comp()$Y == 1, "red", "blue"))
})

output$mean_diff_plot_pvalues <- renderPlot({
  barplot(mean_diff_pval_comp(),
          names.arg = 1:length(mean_diff_pval_comp()),
          xlab = "Feature Index",
          ylab = "Permutation p-value",
          main = "Permutation p-values based on the Mean Difference statistic",
          border = "black",
          lwd = as.numeric(input$pvalue_bar_lwd))
  abline(h = input$alpha_level, col = "red", lty = 2, lwd = 2)
})

output$mean_diff_plot_statistics <- renderPlot({
  barplot(mean_diff_stat_comp(),
          names.arg = 1:length(mean_diff_stat_comp()),
          xlab = "Feature Index",
          ylab = "Test Statistic",
          main = "Feature-wise test statistics based on the Mean Difference statistic",
          border = "black",
          lwd = as.numeric(input$stat_bar_lwd))
})

output$mean_diff_table_selected <- renderTable({
  data.frame(
    Selected_Features = mean_diff_selected_comp()
  )
})