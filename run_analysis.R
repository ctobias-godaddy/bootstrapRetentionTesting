# sql params
sql_script_path <- 'experiment_retention_script.sql'

platform <- 'android'
exp_name <- 'android-customer-202211-free-content'
exp_variants <- c('control', 'treatment-7')
effect_type <- 'observed_effect' # observed_effect or true_effect where true_effect only includes users in their first session (not just new users)

start_dt <- '2022-01-09'
end_dt <- Sys.Date()

# bootdtrap params
seed = 123 # set seed for reproducibility with sampling
b <- 10000  # number of bootstrap samples
num_var <- 'pct_users_retained' # numeric variable containing the values to test
group_var <- 'variant_name' # grouping variable - used to identify the variants we wish to test

# plot params
plot_ttl = "Bootstrapped Difference in Retention"
html_ttl = paste0("Android Free Content Experiment: Retention Testing - 7day vs Control: ", Sys.Date())

source("read_sql.R")
source("run_bootstrap.R")

# --------------------------------------------------------------------------------------------------------------

# Get data from BQ
bq_tbl <- read_sql(sql_script_path, exp_name, exp_variants, platform, start_dt, end_dt, effect_type)

bq_data <- bq_tbl$data[[1]]

bq_data_set1 <- dplyr::filter(bq_data, variant_name %in% c(exp_variants[1], exp_variants[2]))
# bq_data_set2 <- dplyr::filter(bq_data, variant_name %in% c(exp_variants[1], exp_variants[3]))

# Run bootstrap
bootstrap_data_set1 <- run_bootstrap(bq_data_set1, exp_variants, num_var, group_var, b_samples = b, seed = seed)
# bootstrap_data_set2 <- run_bootstrap(bq_data_set2, exp_variants, num_var, group_var, b_samples = b, seed = seed)

# Plot
boxplot_data <- dplyr::filter(bootstrap_data_set1, name == "data") %>% tidyr::unnest(value)

box_plot <- ggplot2::ggplot(boxplot_data, ggplot2::aes_string(x = group_var, y = num_var)) +
  ggplot2::geom_boxplot() +
  ggplot2::stat_summary(ggplot2::aes_string(y = num_var), fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red") +
  ggplot2::labs(ylab = "Users Retained (%)",
                xlab = "Variant",
                title = "Observed Retention Distribution")

densityplot_data <- dplyr::filter(bootstrap_data_set1, name == "bootstrapped_data") %>% tidyr::unnest(value)
abs_diff_mean <- dplyr::filter(bootstrap_data_set1, name == "diff_mean") %>% tidyr::unnest(value)
abs_diff_mean <- round(abs_diff_mean$value, 2)
t_test_pub <- dplyr::filter(bootstrap_data_set1, name == "t_results") %>% tidyr::unnest(value)

density_plot <- ggplot2::ggplot(densityplot_data, ggplot2::aes(x = boot_abs_diff_mean)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), colour = "black", fill = "white") +
  ggplot2::geom_density(alpha = 0.2, fill = "#FF6666") +
  ggplot2::geom_vline(ggplot2::aes(xintercept = abs_diff_mean),
                      color = "blue", linetype = "dashed", linewidth = 1) +
  ggplot2::geom_text(label = "Observed difference",
                     x = abs_diff_mean,
                     y = 0.3,
                     angle = 90,
                     vjust = 1) +
  ggplot2::labs(x = "Absolute Difference in Means (ppt)",
                y = "Probability Density",
                title = plot_ttl)

# ----------------------------------------- render results

widget_out <- manipulateWidget::combineWidgets(ncol = 2,
                                               title = html_ttl,
                                               plotly::ggplotly(box_plot),
                                               plotly::ggplotly(density_plot),
                                               manipulateWidget::combineWidgets(
                                                 DT::datatable(t_test_pub, rownames = FALSE, filter = "none")
                                                )
                                               )

htmlwidgets::saveWidget(
  widget = widget_out,
  file = "android_free_trial_retention_bootstrapping.html",
  selfcontained = TRUE
)
