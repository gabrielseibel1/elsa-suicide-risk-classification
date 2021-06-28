# estimate ensemble performance
save_aggregation_evaluations()

# summary
summary <- get_save_and_plot_performance_summary_all_models()
print_section("Evaluation summary")
print(summary, n = nrow(summary))
print_section("FINISHED")
plot_var_imps()
plot_rfe_results()
plot_hyperparameters()
plot_all_multi_ROC()