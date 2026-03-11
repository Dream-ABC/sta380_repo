sim_inputs <- div(
  conditionalPanel(
    condition = "input.modify_type == 'simulation'",

    numericInput(inputId = "example_seed",
                 label = "Insert the seed for simulation.",
                 value = 42),

    numericInput(inputId = "sample_size",
                 label = "Insert the sample size.",
                 value = 100),

    numericInput(inputId = "num_features",
                 label = "Insert the number of features.",
                 value = 20,
                 min = 2),

    numericInput(inputId = "num_relevant",
                 label = "Insert the number of true relevant features.",
                 value = 5,
                 min = 1),

    numericInput(inputId = "num_permutations",
                 label = "Insert the number of permutations.",
                 value = 100,
                 min = 1),

    numericInput(inputId = "alpha_level",
                 label = "Insert the significance level.",
                 value = 0.05,
                 min = 0,
                 max = 1,
                 step = 0.01),

    selectInput(inputId = "stat_method_type",
                label = "Select the test statistic method.",
                choices = list("Mean Difference" = "mean_diff",
                               "KS" = "ks",
                               "CvM" = "cvm"),
                selected = "mean_diff"),
  ), # End conditionalPanel
)

# Conditional panel for modifying the five graph outputs
sim_graph_inputs <- div(
  # Conditional panel for modifying the simulated data overview
  conditionalPanel(
    condition = "input.graph_version == 'overview'",

    selectInput(inputId = "selected_feature_1",
                label = "Select feature 1 for the x-axis.",
                choices = 1:20,  # TODO: need to be consistent with num_features input
                selected = 1),

    selectInput(inputId = "selected_feature_2",
                label = "Select feature 2 for the y-axis.",
                choices = 1:20,  # TODO: need to be consistent with num_features input
                selected = 2),
  ), # End conditionalPanel

  # Conditional panel for modifying the permutation p-values plot
  conditionalPanel(
    condition = "input.graph_version == 'pvalues'",

    sliderInput(inputId = "pvalue_bar_lwd",
                label = "Insert the bar border width.",
                min = 0, max = 5, value = 1, step = 0.5),
  ), # end of conditionalPanel

  # Conditional panel for modifying the test statistics plot
  conditionalPanel(
    condition = "input.graph_version == 'statistics'",

    sliderInput(inputId = "stat_bar_lwd",
                label = "Insert the bar border width.",
                min = 0, max = 5, value = 1, step = 0.5),
  ), # end of conditionalPanel

  # Conditional panel for modifying the selected features table
  conditionalPanel(
    condition = "input.graph_version == 'selected'",

    numericInput(inputId = "selected_table_nrows",
                 label = "Insert the number of rows to display.",
                 value = 10,
                 min = 1),
  ), # end of conditionalPanel

  # Conditional panel for modifying the evaluation table
  conditionalPanel(
    condition = "input.graph_version == 'evaluation'",

    numericInput(inputId = "evaluation_digits",
                 label = "Insert the number of digits to display.",
                 value = 3,
                 min = 1),
  ), # end of conditionalPanel
)