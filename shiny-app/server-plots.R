
mu_list = reactive({create_necessary_vector(input$mean_vector)})
sigma_list = reactive({create_necessary_vector(input$sd_vector)})

global_seed <- reactive(input$example_seed)

indmh_comp <- reactive({
  set.seed(global_seed())
  sigma_matrix <- matrix(c(sigma_list()[1], input$rho,
                           input$rho, sigma_list()[2]), nrow = 2, ncol = 2)
  test_pt1 <- rmvnorm(n = 1, mean = mu_list(), sigma = sigma_matrix)

  rbivariate.mh_ind(N = input$sample_size,
                    burn = 100,
                    Y0 = test_pt1,
                    sigma_matrix = sigma_matrix,
                    mu_vec = mu_list())
})

rwmh_comp <- reactive({
  set.seed(global_seed())
  sigma_matrix <- matrix(c(sigma_list()[1], input$rho,
                           input$rho, sigma_list()[2]), nrow = 2, ncol = 2)
  test_pt2 <- rmvnorm(n = 1, mean = mu_list(), sigma = sigma_matrix)

  rbivariate.rwmh(N = input$sample_size,
                  burn = 100,
                  Y0 = test_pt2,
                  sigma_matrix = sigma_matrix,
                  mu_vec = mu_list())
})

gibbs_comp <- reactive({
  set.seed(global_seed())
  sigma_matrix <- matrix(c(sigma_list()[1], input$rho,
                           input$rho, sigma_list()[2]), nrow = 2, ncol = 2)
  test_pt3 <- rmvnorm(n = 1, mean = mu_list(), sigma = sigma_matrix)

  gibbs_sampler(N = input$sample_size,
                burn = 100,
                Y0 = test_pt3,
                sigma_matrix = sigma_matrix,
                mu_vec = mu_list())
})

#####################################################
# Independent MH                                    #
#####################################################

output$indmh_plot_scat <- renderPlot({
  plot(indmh_comp()[,1], indmh_comp()[,2],
       xlab = "Variable 1",
       ylab = "Variable 2",
       main = "Scatter plot of the simulated bivariate Gaussian samples obtained
     via the independence Metropolis–Hastings algorithm",
       pch = as.numeric(input$scat_pch),
       bg = input$scat_line_col,
       col = input$scat_col)
})

output$indmh_plot_hist <- renderPlot({
  par(mfcol=c(1,2))

  xx = seq(-4,4,by=0.01)
  hist(indmh_comp()[,1], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 1",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx, mean = mu_list()[1], sd = sigma_list()[1]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))

  hist(indmh_comp()[,2], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 2",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx,  mu_list()[2], sd = sigma_list()[2]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))
})

output$indmh_plot_qqplot <- renderPlot({
  par(mfcol=c(1,2))

  qqnorm(indmh_comp()[,1],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 1",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(indmh_comp()[,1],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))

  qqnorm(indmh_comp()[,2],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 2",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(indmh_comp()[,2],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))
})

#####################################################
# Random Walk MH                                    #
#####################################################

output$rwmh_plot_scat <- renderPlot({
  plot(rwmh_comp()[,1], rwmh_comp()[,2],
       xlab = "Variable 1",
       ylab = "Variable 2",
       main = "Scatter plot of the simulated bivariate Gaussian samples obtained
     via the random walk Metropolis–Hastings algorithm",
       pch = as.numeric(input$scat_pch),
       bg = input$scat_line_col,
       col = input$scat_col)
})

output$rwmh_plot_hist <- renderPlot({
  par(mfcol=c(1,2))

  xx = seq(-4,4,by=0.01)
  hist(rwmh_comp()[,1], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 1",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx, mean = mu_list()[1], sd = sigma_list()[1]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))

  hist(rwmh_comp()[,2], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 2",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx,  mu_list()[2], sd = sigma_list()[2]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))
})

output$rwmh_plot_qqplot <- renderPlot({
  par(mfcol=c(1,2))

  qqnorm(rwmh_comp()[,1],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 1",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(rwmh_comp()[,1],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))

  qqnorm(rwmh_comp()[,2],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 2",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(rwmh_comp()[,2],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))
})


#####################################################
# Gibbs Sampler                                     #
#####################################################

output$gibbs_plot_scat <- renderPlot({
  plot(gibbs_comp()[,1], gibbs_comp()[,2],
       xlab = "Variable 1",
       ylab = "Variable 2",
       main = "Scatter plot of the simulated bivariate Gaussian samples obtained
     via the Gibbs sampler",
       pch = as.numeric(input$scat_pch),
       bg = input$scat_line_col,
       col = input$scat_col)
})

output$gibbs_plot_hist <- renderPlot({
  par(mfcol=c(1,2))

  xx = seq(-4,4,by=0.01)
  hist(gibbs_comp()[,1], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 1",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx, mean = mu_list()[1], sd = sigma_list()[1]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))

  hist(gibbs_comp()[,2], breaks=100, freq=FALSE,
       main = "Histogram of Marginal Distribution of Variable 2",
       xlab = "Variable 1",
       col = input$hist_col,
       border = "white")
  lines(xx, dnorm(xx,  mu_list()[2], sd = sigma_list()[2]),
        col = input$hist_line_col,
        lwd = as.numeric(input$hist_line_lwd),
        lty = as.numeric(input$hist_line_lty))
})

output$gibbs_plot_qqplot <- renderPlot({
  par(mfcol=c(1,2))

  qqnorm(gibbs_comp()[,1],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 1",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(gibbs_comp()[,1],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))

  qqnorm(gibbs_comp()[,2],
         main = "Normal Q–Q Plot for the Marginal Distribution of Variable 2",
         pch = as.numeric(input$qqplot_pch),
         col = input$qqplot_col,
         bg = input$qqplot_border_col)
  qqline(gibbs_comp()[,2],
         col = input$qqplot_line_col,
         lwd = as.numeric(input$qqplot_line_lwd),
         lty = as.numeric(input$qqplot_line_lty))
})