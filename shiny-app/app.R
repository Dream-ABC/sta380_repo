library(shiny)
library(bslib)

ui <- page_sidebar(
  theme = bs_theme(version = 5,
                   bootswatch = "minty",
                   "navbar-bg" = "#FFFFFF",
                   "body-bg" = "#F9FAFB"),
  
  title = "Permutation Test Playground",
  
  tags$head(
    tags$style(HTML("
      .btn { border-radius: 0; }
      .alert { border-radius: 0; font-weight: 500; padding: 12px; border: 1px solid transparent; }
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      h4 { font-weight: bold; font-size: 1.05rem; color: #2c3e50; margin-bottom: 12px; }
      .sidebar { padding: 16px; }
      .results-main .card-header { font-size: 0.95rem; letter-spacing: 0.01em; color: black !important; font-weight: bold !important; }
      .results-main .table { font-size: 0.82rem; margin-bottom: 0; }
      .results-main .eval-interpret { font-size: 0.88rem; line-height: 1.45; }
      .shiny-table.table > :not(caption) > * > * { padding: 0.28rem 0.45rem; }
      .shiny-html-output { max-width: 100%; }
      
      .alert-info { background-color: #dbeafe; color: #1e40af; border-color: #bfdbfe; }
      .alert-primary { background-color: #e0e7ff; color: #3730a3; border-color: #c7d2fe; }
      .alert-warning { background-color: #ffedd5; color: #9a3412; border-color: #fed7aa; }
      .alert-success { background-color: #d8ebd9; color: #2a5a3b; border-color: #c8dec9; }
    "))
  ),
  
  sidebar = sidebar(
    uiOutput("step_indicator"),
    conditionalPanel(
      condition = "output.current_step == 'instructions'",
      h4("Welcome to the Permutation Test Playground"),
      p("This app allows you to explore permutation-based feature selection using simulated data."),
      p("Follow these steps:"),
      tags$ol(
        tags$li("Set Monte Carlo parameters to generate noisy data."),
        tags$li("Configure permutation test parameters."),
        tags$li("Analyze the results and evaluation metrics.")
      ),
      actionButton("start_btn", "Get Started", class = "btn-primary")
    ),
    conditionalPanel(
      condition = "output.current_step == 'mc'",
      h4("Step 1: Data Generation"),
      p("Configure parameters for Monte Carlo data simulation."),
      numericInput(inputId = "example_seed",
                   label = "Random Seed",
                   value = 1),
      sliderInput(inputId = "sample_size",
                  label = "Sample Size (n)",
                  min = 50, max = 500, value = 100, step = 10),
      sliderInput(inputId = "num_features",
                  label = "Number of Features (p)",
                  min = 10, max = 100, value = 20, step = 5),
      sliderInput(inputId = "num_relevant",
                  label = "True Relevant Features (s)",
                  min = 1, max = 10, value = 2, step = 1),
      actionButton("generate_btn", "Generate Data", class = "btn-success",
                   title = "Generate simulated data with the specified parameters")
    ),
    conditionalPanel(
      condition = "output.current_step == 'pt'",
      h4("Step 2: Permutation Test"),
      p("Set parameters for the permutation test."),
      selectInput(inputId = "stat_method_type",
                  label = 'Test Method',
                  choices = list("Kolmogorov-Smirnov" = "ks",
                                 "Cramér-von Mises" = "cvm"),
                  selected = "ks"),
      sliderInput(inputId = "num_permutations",
                  label = "Number of Permutations",
                  min = 10, max = 2000, value = 100, step = 10),
      sliderInput(inputId = "alpha_level",
                  label = "Significance Level (α)",
                  min = 0.001, max = 0.2, value = 0.05, step = 0.001),
      tags$div(
        class = "mt-2 d-flex gap-2",
        actionButton("back_to_mc_btn", "Back", class = "btn-secondary",
                     title = "Go back to Step 1"),
        actionButton("test_btn", "Run Test", class = "btn-warning",
                     title = "Perform permutation test on the generated data")
      )
    ),
    conditionalPanel(
      condition = "output.current_step == 'results'",
      h4("Step 3: Results & Evaluation"),
      p("Review the ground truth, your test results, and evaluation metrics."),
      actionButton("try_another_btn", "Try Another Permutation Test", class = "btn-info",
                   title = "Go back to adjust test parameters"),
      actionButton("reset_btn", "Start Over", class = "btn-secondary",
                   title = "Reset to beginning")
    ),
    
    width = 350,
    open = "always"
  ),
  
  conditionalPanel(
    condition = "output.current_step == 'mc'",
    uiOutput("step1_preview")
  ),
  
  conditionalPanel(
    condition = "output.current_step == 'pt'",
    uiOutput("step2_preview")
  ),
  
  conditionalPanel(
    condition = "output.current_step == 'results'",
    tags$div(
      class = "results-main",
      card(
        class = "mb-3 border shadow-sm",
        card_header(class = "py-2 px-3 fw-semibold small text-secondary",
                    "Ground truth — relevant feature indices"),
        card_body(
          class = "py-2 px-2",
          uiOutput("ground_truth_labels")
        )
      ),
      card(
        class = "mb-3 border shadow-sm",
        card_header(class = "py-2 px-3 fw-semibold small text-secondary",
                    "Permutation test"),
        card_body(
          class = "py-2 px-2",
          fluidRow(
            column(6,
                   tags$div(class = "small text-muted mb-1",
                            "P-values"),
                   plotOutput("pvalues_plot", height = "270px")),
            column(6,
                   tags$div(class = "small text-muted mb-1", "Test statistic"),
                   plotOutput("statistics_plot", height = "270px"))
          )
        )
      ),
      card(
        class = "mb-3 border shadow-sm",
        card_header(class = "py-2 px-3 fw-semibold small text-secondary",
                    "Evaluation — precision / recall for this run"),
        card_body(
          class = "py-2 px-2",
          uiOutput("evaluation_interpretation"),
          tags$div(class = "small fw-semibold text-muted mt-2 mb-1",
                   "Summary"),
          tableOutput("evaluation_summary_table"),
          tags$div(class = "small fw-semibold text-muted mt-2 mb-1",
                   "Per-feature detail"),
          tableOutput("selected_features_detail_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  current_step <- reactiveVal("instructions")
  
  output$current_step <- reactive({ current_step() })
  outputOptions(output, "current_step", suspendWhenHidden = FALSE)
  
  output$step_indicator <- renderUI({
    step <- current_step()
    if (step == "instructions") {
      tags$div(class = "alert alert-info", "Step: Instructions")
    } else if (step == "mc") {
      tags$div(class = "alert alert-primary", "Step 1/3: Data Generation")
    } else if (step == "pt") {
      tags$div(class = "alert alert-warning", "Step 2/3: Test Configuration")
    } else {
      tags$div(class = "alert alert-success", "Step 3/3: Results")
    }
  })
  
  observeEvent(input$start_btn, {
    current_step("mc")
  })
  
  observeEvent(input$generate_btn, {
    current_step("pt")
  })
  
  observeEvent(input$test_btn, {
    current_step("results")
  })
  
  observeEvent(input$back_to_mc_btn, {
    current_step("mc")
  })
  
  observeEvent(input$try_another_btn, {
    current_step("pt")
  })
  
  observeEvent(input$reset_btn, {
    current_step("instructions")
  })
  
  source(file.path("server-plots.R"), local = TRUE)
}

shinyApp(ui = ui, server = server)
