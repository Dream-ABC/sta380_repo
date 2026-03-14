library(shiny)
library(bslib)
library(shinycssloaders)

options(spinner.type = 8, spinner.color = "#6990EE")

ui <- page_sidebar(
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   "navbar-bg" = "#2C3E50"),
  
  title = "Permutation-Based Feature Selection Using Simulated Data",
  
  sidebar = sidebar(
    
    selectInput(inputId = "stat_method_type",
                label = 'What method would you like to see?',
                choices = list("Mean Difference" = "mean_diff",
                               "KS" = "ks",
                               "CvM" = "cvm"),
                selected = "mean_diff"),
    
    selectInput(inputId = "modify_type",
                label = 'What would you like to modify?',
                choices = list("Simulation Inputs" = "simulation",
                               "Graph Output" = "graph"),
                selected = "simulation"),
    
    # inputs based on the simulation; see .R/ui-sim_input.R
    sim_inputs,
    
    conditionalPanel(
      condition = "input.modify_type == 'graph'",
      
      selectInput(inputId = "graph_version",
                  label = 'Which graph would you like to view and modify?',
                  # five components to show for the graph outputs
                  choices = list("Simulate Data Overview" = "overview",
                                 "Permutation p-values" = "pvalues",
                                 "Features' Test Statistics" = "statistics",
                                 "Selected Features" = "selected",
                                 "Evaluation" = "evaluation"),
                  selected = "overview"),
      
      # inputs based on the graph types; see ./R/ui-sim_input.R
      sim_graph_inputs,
      
    ), # End conditionalPanel
    
    width = 400,
    open = "always"), # End sidebar
  
  
  # mainPanel for the three methods
  # (make sure all input.xx var_names are consistent in sim_input.R)
  conditionalPanel(
    condition = "input.stat_method_type == 'mean_diff'",
    
    conditionalPanel(
      condition = "input.graph_version == 'overview'",
      withSpinner(plotOutput("mean_diff_plot_overview", height = "700px"))  # name to be changed
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'pvalues'",
      withSpinner(plotOutput("mean_diff_plot_pvalues", height = "700px"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'statistics'",
      withSpinner(plotOutput("mean_diff_plot_statistics", height = "700px"))
    ), # end of Conditional Panel
    
    
    conditionalPanel(
      condition = "input.graph_version == 'selected'",
      withSpinner(tableOutput("mean_diff_table_selected"))
    ), # end of Conditional Panel
    
  ), # end of Conditional Panel (for mean difference)
  
  conditionalPanel(
    condition = "input.stat_method_type == 'ks'",
    
    conditionalPanel(
      condition = "input.graph_version == 'overview'",
      withSpinner(plotOutput("ks_plot_overview"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'pvalues'",
      withSpinner(plotOutput("ks_plot_pvalues"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'statistics'",
      withSpinner(plotOutput("ks_plot_statistics"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'selected'",
      withSpinner(tableOutput("ks_table_selected"))
    ), # end of Conditional Panel
    
  ), # end of Conditional Panel (for KS)
  
  
  conditionalPanel(
    condition = "input.stat_method_type == 'cvm'",
    
    conditionalPanel(
      condition = "input.graph_version == 'overview'",
      withSpinner(plotOutput("cvm_plot_overview"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'pvalues'",
      withSpinner(plotOutput("cvm_plot_pvalues"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'statistics'",
      withSpinner(plotOutput("cvm_plot_statistics"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'selected'",
      withSpinner(tableOutput("cvm_table_selected"))
    ), # end of Conditional Panel
    
    conditionalPanel(
      condition = "input.graph_version == 'evaluation'",
      withSpinner(tableOutput("evaluation_table"))
    ), # end of Conditional Panel
  ), # end of Conditional Panel (for CvM)
  
  # evaluation table is shared across all methods
  conditionalPanel(
    condition = "input.graph_version == 'evaluation'",
    withSpinner(tableOutput("evaluation_table"))
  ),
  
  # put outputs later...
)

server <- function(input, output, session) {
  source(file.path("server-plots.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)