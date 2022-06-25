
## extract and visualize posterior predictive distributions
extract_post_pred <- function(stan_fit) {
  tidybayes::tidy_draws(stan_fit) %>%
    select(.chain, .iteration, .draw, starts_with("y_rep")) %>%
    gather(key = key, value = value, starts_with("y_rep")) %>%
    mutate(key = gsub("y_rep|\\[|\\]", "", key) %>% as.integer())
}

##
plot_draws <- function(stan_fit, n_sample, data_count = data_count) {
  draw_data <- extract_post_pred(stan_fit)
  sample_indices <- sample(seq_len(max(draw_data$.iteration)), size = n_sample)
  draw_data %>%
    group_by(.chain, .iteration, .draw) %>%
    count(value) %>%
    filter(.iteration %in% sample_indices) %>%
    ggplot(mapping = aes(x = value, y = n, group = interaction(.chain, .iteration, .draw))) +
    ## Plot random draws from posterior
    geom_line(alpha = 0.5, size = 0.1) +
    ## Include actual data distribution
    geom_line(data = data_count, color = "gray", alpha = 0.7, size = 2,
              mapping = aes(x = y, group = NULL)) +
    facet_wrap( ~ .chain) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank())
}