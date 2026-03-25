library(bslib)
library(bsicons)
library(shiny)

sim_inputs <- div(
  conditionalPanel(
    condition = "input.modify_type == 'simulation'",

    # TODO: fix all the numerical settings so they are reasonable
    numericInput(inputId = "example_seed",
                 label = "Insert the seed for simulation.",
                 value = 1),

    # numericInput(inputId = "sample_size",
    #             label = "Insert the sample size.",
    #             value = 10000),

    # tooltip(
    #  sliderInput(inputId = "sample_size",
    #              label = "Select the sample size (n).",
    #              min = 50, max = 500, value = 200, step = 10),
    #  "The total number of sample observations. A larger sample size (n) improves the accuracy of the permutation test but also increases the computation time.",
    #  placement = "right"
    #),

    sliderInput(inputId = "sample_size",
                label = tags$span("Sample Size (n) ", 
                                  tooltip(bs_icon("info-circle", class = "ms-1 text-muted"), 
                                          "The total number of sample observations. A larger sample size improves the accuracy of the permutation test but also increases the computation time.")),
                min = 50, max = 500, value = 200, step = 10),
    
    # numericInput(inputId = "num_features",
    #             label = "Insert the number of features.",
    #             value = 5,
    #             min = 2),

    tooltip(
      sliderInput(inputId = "num_features",
                  label = "Select the total number of features (p).",
                  min = 10, max = 200, value = 100, step = 5),
      "The total number of features (p) in the dataset, consisting of both true signals and irrelevant noise features. High-dimensional analysis focuses on cases where p is larger than n.",
      placement = "right"
    ),
    
    # numericInput(inputId = "num_relevant",
    #             label = "Insert the number of true relevant features.",
    #             value = 1,
    #             min = 1),

    tooltip(
      sliderInput(inputId = "num_relevant",
                  label = "Select the number of true relevant features (s).",
                  min = 1, max = 50, value = 5, step = 1),
      "The number of features (s) with actual signal. The goal of feature selection is to correctly identify these true relevant features while ignoring the remaining noise features.",
      placement = "right"
    ),
    
    # numericInput(inputId = "num_permutations",
    #             label = "Insert the number of permutations.",
    #             value = 100,
    #             min = 1, max = 1000),

    tooltip(
      sliderInput(inputId = "num_permutations",
                  label = "Select the number of permutations.",
                  min = 10, max = 1000, value = 100, step = 10),
      "The number of times that the labels are randomly shuffled to build the null distribution. A larger value improves the precision of the p-value but also increases the computation time.",
      placement = "right"
    ),

    # numericInput(inputId = "alpha_level",
    #             label = "Insert the significance level.",
    #             value = 0.05,
    #             min = 0,
    #             max = 1,
    #             step = 0.01),

    tooltip(
      numericInput(inputId = "alpha_level",
                   label = "Insert the significance level.",
                   value = 0.05,
                   min = 0.01,
                   max = 1,
                   step = 0.01),
      "The probability threshold (alpha) for identifying 'significant' features.",
      placement = "right"
    )
  ), # End conditionalPanel
)

# Conditional panel for modifying the five graph outputs
sim_graph_inputs <- div(
  # Conditional panel for modifying the simulated data overview
  conditionalPanel(
    condition = "input.graph_version == 'overview'",

    selectInput(inputId = "selected_feature_1",
                label = "Select feature 1 for the x-axis.",
                choices = 1:2,  # placeholder
                selected = 1),

    selectInput(inputId = "selected_feature_2",
                label = "Select feature 2 for the y-axis.",
                choices = 1:2,  # placeholder
                selected = 2),
  ), # End conditionalPanel

  ### doesn't make sense, commenting out for now
  # # Conditional panel for modifying the permutation p-values plot
  # conditionalPanel(
  #   condition = "input.graph_version == 'pvalues'",

  #   sliderInput(inputId = "pvalue_bar_lwd",
  #               label = "Insert the bar border width.",
  #               min = 0, max = 5, value = 1, step = 0.5),
  # ), # end of conditionalPanel

  # # Conditional panel for modifying the test statistics plot
  # conditionalPanel(
  #   condition = "input.graph_version == 'statistics'",

  #   sliderInput(inputId = "stat_bar_lwd",
  #               label = "Insert the bar border width.",
  #               min = 0, max = 5, value = 1, step = 0.5),
  # ), # end of conditionalPanel

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