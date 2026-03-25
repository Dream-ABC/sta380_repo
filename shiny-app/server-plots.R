source("../R/data_simulation.R")
source("../R/evaluation.R")
source("../R/permutation_test.R")

plot_feature_barplot <- function(heights,
                                 ylab,
                                 main,
                                 highlight_idx = NULL,
                                 col_signal = "#74B9FF",
                                 col_rel = "#FF6B6B",
                                 col_irrel = "#4ECDC4",
                                 hline = NULL,
                                 hline_col = "#FF6B6B",
                                 ylim = NULL) {
  p <- length(heights)
  par(mar = c(4.2, 5.2, 2.4, 1), mgp = c(2.15, 0.65, 0), oma = rep(0, 4))
  if (is.null(highlight_idx)) {
    bar_col <- rep(col_signal, p)
  } else {
    bar_col <- ifelse(seq_len(p) %in% highlight_idx, col_rel, col_irrel)
  }
  show_names <- p <= 24
  mp <- barplot(
    heights,
    names.arg = if (show_names) seq_len(p) else rep("", p),
    xlab = "Feature index",
    ylab = ylab,
    main = main,
    col = bar_col,
    border = NA,
    space = if (show_names) 0.25 else 0.12,
    las = 1,
    cex.names = if (show_names) 0.7 else 0.01,  # Fix: 0 is invalid for cex
    cex.lab = 0.82,
    cex.main = 0.98,
    ylim = ylim
  )
  if (!show_names) {
    n_lab <- min(12L, p)
    idx <- unique(as.integer(round(seq(1L, p, length.out = n_lab))))
    idx <- idx[idx >= 1 & idx <= length(mp)]
    if (length(idx) > 0) {
      axis(1, at = mp[idx], labels = idx, tcl = -0.22)
    }
  }
  if (!is.null(hline)) {
    abline(h = hline, col = hline_col, lty = 2, lwd = 1.4)
  }
  if (!is.null(highlight_idx)) {
    legend(
      "topright",
      legend = c("Truly relevant", "Noise"),
      fill = c(col_rel, col_irrel),
      bty = "n",
      cex = 0.78
    )
  }
  invisible(mp)
}

output$step1_preview <- renderUI({
  s <- min(input$num_relevant, input$num_features)
  p <- input$num_features
  ratio <- round(s / p * 100, 1)
  card(
    class = "mb-3 border shadow-sm",
    card_header(class = "py-2 px-3 fw-semibold small text-secondary",
                "Feature Composition"),
    card_body(
      class = "py-2 px-3",
      tags$p(
        class = "small text-muted",
        tags$span("Signal-to-noise ratio: "),
        tags$code(paste0(s, " / ", p, " = ", ratio, "%"))
      ),
      tags$p(
        class = "small text-muted",
        paste0(s, " true relevant feature(s) among ", p - s, " noise features.")
      ),
      plotOutput("snr_pie", height = "220px")
    )
  )
})

output$snr_pie <- renderPlot({
  s <- min(input$num_relevant, input$num_features)
  p <- input$num_features
  par(mar = c(0, 0, 0.5, 0), fg = "#888888")
  pie(
    c(s, p - s),
    labels = c(paste0("Relevant (", s, ")"), paste0("Noise (", p - s, ")")),
    col = c("#FFADAD", "#A8DADC"),
    cex = 0.9,
    border = "white"
  )
}, res = 100)

output$step2_preview <- renderUI({
  method <- input$stat_method_type
  if (method == "ks") {
    method_title <- "Kolmogorov-Smirnov Test"
    method_desc  <- "Measures the maximum gap between two empirical CDFs. Good at detecting sharp, localized differences between groups."
  } else {
    method_title <- "Cramér-von Mises Test"
    method_desc  <- "Measures the average squared difference between two empirical CDFs. More stable when differences are spread across the distribution."
  }
  card(
    class = "mb-3 border shadow-sm",
    card_header(class = "py-2 px-3 fw-semibold small text-secondary",
                "Test Method"),
    card_body(
      class = "py-2 px-3",
      tags$h6(tags$strong(method_title)),
      tags$p(class = "small text-muted", method_desc),
      tags$hr(),
      tags$p(class = "small text-muted",
             "In practice, both methods tend to select similar features. Try switching between them to see if results change.")
    )
  )
})

sim_data <- reactive({
  seed <- input$example_seed
  set.seed(seed)
  simulate_mixture_data(
    n = input$sample_size,
    p = input$num_features,
    n_relevant = min(input$num_relevant, input$num_features),
    alpha = 0.8,
    seed = seed
  )
})

observe({
  updateSliderInput(session, "num_relevant", max = input$num_features)
})

stat_fun <- reactive({
  switch(input$stat_method_type, "ks" = stat_ks, "cvm" = stat_cvm)
})

permutation_pvals <- reactive({
  permutation_pvalues(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_fun(),
    n_shuffles = input$num_permutations
  )
})

permutation_stats <- reactive({
  f <- stat_fun()
  apply(sim_data()$X, 2, function(x) f(x, sim_data()$Y))
})

permutation_selected <- reactive({
  select_features_permutation(
    X = sim_data()$X,
    Y = sim_data()$Y,
    stat_fun = stat_fun(),
    n_shuffles = input$num_permutations,
    alpha_level = input$alpha_level
  )
})

evaluation_snapshot <- reactive({
  selected <- permutation_selected()
  truth <- sim_data()$S
  p <- ncol(sim_data()$X)
  pred_mask <- seq_len(p) %in% selected
  truth_mask <- seq_len(p) %in% truth
  tp <- sum(pred_mask & truth_mask)
  fp <- sum(pred_mask & !truth_mask)
  fn <- sum(!pred_mask & truth_mask)
  tn <- sum(!pred_mask & !truth_mask)
  eval_obj <- evaluate_precision_recall(S_truth = truth, S_hat = selected)
  prec <- eval_obj$precision
  rec <- eval_obj$recall
  f1v <- f1_score(prec, rec)
  list(
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    precision = prec,
    recall = rec,
    f1 = f1v,
    n_selected = length(selected),
    n_truth = length(truth),
    method_label = ifelse(input$stat_method_type == "ks", "KS", "CvM"),
    alpha = input$alpha_level,
    n_perm = input$num_permutations
  )
})

output$ground_truth_labels <- renderUI({
  S <- sort(sim_data()$S)
  p_dim <- ncol(sim_data()$X)
  tags$div(
    class = "ground-truth-labels",
    tags$p(
      class = "mb-2",
      tags$strong("Features: "),
      tags$span(class = "text-muted", "labels are column indices "),
      tags$code(paste0("1, …, ", p_dim))
    ),
    tags$p(
      class = "mb-1",
      tags$strong("True signal indices (S): "),
      if (length(S)) tags$code(paste(S, collapse = ", ")) else tags$em("none")
    ),
    tags$p(
      class = "small text-muted mb-0",
      "All other indices are noise features (irrelevant)."
    )
  )
})

output$pvalues_plot <- renderPlot({
  pv <- pmin(pmax(as.numeric(permutation_pvals()), 0), 1)
  plot_feature_barplot(
    pv,
    ylab = "Permutation p-value",
    main = "P-values",
    col_signal = "#74B9FF",
    hline = input$alpha_level,
    hline_col = "#E74C3C",
    ylim = c(0, 1)
  )
}, res = 100)

output$statistics_plot <- renderPlot({
  plot_feature_barplot(
    permutation_stats(),
    ylab = "Test statistic",
    main = "Observed statistic per feature",
    col_signal = "#A29BFE"
  )
}, res = 100)

output$evaluation_interpretation <- renderUI({
  tags$div(
    class = "eval-interpret text-muted",
    tags$p("Evaluation metrics are shown in the table below.")
  )
})

output$evaluation_summary_table <- renderTable({
  sn <- evaluation_snapshot()
  data.frame(
    Method = sn$method_label,
    TP = sn$tp,
    FP = sn$fp,
    FN = sn$fn,
    TN = sn$tn,
    Precision = round(sn$precision, 3),
    Recall = round(sn$recall, 3),
    F1 = round(sn$f1, 3),
    N_selected = sn$n_selected,
    N_true_signals = sn$n_truth
  )
}, striped = TRUE, hover = TRUE, spacing = "xs", align = "c", digits = 3)

output$selected_features_detail_table <- renderTable({
  selected <- permutation_selected()
  pvals <- permutation_pvals()
  stats <- permutation_stats()
  this_truth <- sim_data()$S
  pred <- as.integer(seq_along(stats) %in% selected)
  truth <- as.integer(seq_along(stats) %in% this_truth)
  outcome <- ifelse(
    pred == 1 & truth == 1, "TP",
    ifelse(pred == 1 & truth == 0, "FP",
           ifelse(pred == 0 & truth == 1, "FN", "TN"))
  )
  
  df <- data.frame(
    Feature = seq_along(stats),
    Truly_relevant = ifelse(outcome %in% c("FP", "FN"),
                            sprintf('<span style="color: red; font-weight: bold;">%s</span>', ifelse(truth == 1, "Yes", "No")),
                            ifelse(truth == 1, "Yes", "No")),
    Selected = ifelse(outcome %in% c("FP", "FN"),
                      sprintf('<span style="color: red; font-weight: bold;">%s</span>', ifelse(pred == 1, "Yes", "No")),
                      ifelse(pred == 1, "Yes", "No")),
    Outcome = ifelse(outcome %in% c("FP", "FN"),
                     sprintf('<span style="color: red; font-weight: bold;">%s</span>', outcome),
                     outcome),
    P_value = signif(pvals, 4),
    Statistic = signif(stats, 4)
  )
  
  df[order(-pred, df$Feature), ]
}, striped = TRUE, hover = TRUE, spacing = "xs", digits = 4, sanitize.text.function = function(x) x)