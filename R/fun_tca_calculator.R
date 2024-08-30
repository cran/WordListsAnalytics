function_to_find_root <- function(x, Type_Concept, s_avg_aux, C_target) {
  if (Type_Concept == "Concrete") {
    Q1 <- round(13.933 + 1.017 * x - 0.013 * (x^2))
    Q2 <- round(1.911 + 0.532 * x - 0.004 * (x^2))
  } else {
    Q1 <- round(13.063 + 1.467 * x - 0.017 * (x^2))
    Q2 <- round(0.961 + 0.592 * x - 0.003 * (x^2))
  }

  print(Q1)
  print(Q2)

  Q1_T_minus_1 <- Q1 * round(x - 1)
  U <- round(s_avg_aux * round(x)) + 1e-10
  result <- (1 - Q1 / U * (Q1_T_minus_1 / (Q1_T_minus_1 + 2 * Q2))) - C_target
  return(result)
}

# Define the root calculation function
calculate_root <- function(Type_Concept, s_avg, C_target) {
  s_avg_aux <- s_avg
  value_C_comp <- function_to_find_root(2, Type_Concept, s_avg_aux, C_target) + C_target

  print(value_C_comp)
  if (value_C_comp > C_target) {

    stop(paste("With current s_avg & C_target cannot calculate T. C with T = 2 = ", round(value_C_comp, 2), " > C_target. Decrease s_avg and/or increase C_target."))
  }

  half_upper_bound_error <- 1e-10
  s <- 2
  t <- 90
  side <- 0
  r <- 0

  for (i in 1:1000) {
    fs <- function_to_find_root(s, Type_Concept, s_avg_aux, C_target)
    ft <- function_to_find_root(t, Type_Concept, s_avg_aux, C_target)
    r <- (fs * t - ft * s) / ((fs - ft) + 1e-20)

    if (abs(t - s) < half_upper_bound_error * abs(t + s)) {
      return(show_solution(r, Type_Concept, s_avg_aux, C_target))
    }

    fr <- function_to_find_root(r, Type_Concept, s_avg_aux, C_target)

    if (fr * ft > 0) {
      t <- r
      ft <- fr
      if (side == -1) fs <- fs / 2
      side <- -1
    } else if (fs * fr > 0) {
      s <- r
      fs <- fr
      if (side == 1) ft <- ft / 2
      side <- 1
    } else {
      return(show_solution(r, Type_Concept, s_avg_aux, C_target))
    }
  }

  return(show_solution(r, Type_Concept, s_avg_aux, C_target))
}

# Define the function to show solution and generate plots
show_solution <- function(x, Type_Concept, s_avg_aux, C_target) {
  if (x <= 0) {
    stop("With current s_avg & C_target cannot calculate T. Decrease s_avg and/or increase C_target.")
  }

  mult <- ifelse(Type_Concept == "Concrete", 1.05, 1.25)
  T_uncorrected <- ceiling(x)
  T_corrected <- ceiling(x * mult)

  # Data for Coverage vs. T for constant s_avg
  coverage_T <- data.frame(steps = 2:100)
  coverage_T$C_plot <- sapply(coverage_T$steps, function(s) function_to_find_root(s, Type_Concept, s_avg_aux, C_target) + C_target)
  coverage_T$C_plot_COR <- sapply(coverage_T$steps, function(s) function_to_find_root(round(s / mult), Type_Concept, s_avg_aux, C_target) + C_target)

  # Data for Coverage vs. s_avg for constant T
  coverage_s_avg <- data.frame(steps = 1:20)
  coverage_s_avg$s_avg_aux <- coverage_s_avg$steps
  coverage_s_avg$C_plot_s_COR <- sapply(coverage_s_avg$s_avg_aux, function(s) function_to_find_root(round(x / mult), Type_Concept, s, C_target) + C_target)
  coverage_s_avg$C_plot_s_UN <- sapply(coverage_s_avg$s_avg_aux, function(s) function_to_find_root(round(x), Type_Concept, s, C_target) + C_target)

  # Plot Coverage vs. T
  plot1 <- ggplot2::ggplot(coverage_T, ggplot2::aes_string(x = "steps")) +
    ggplot2::geom_line(ggplot2::aes_string(y = "C_plot", color = "'T uncorrected'"), linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes_string(y = "C_plot_COR", color = "'T corrected'"), linetype = "solid") +
    ggplot2::geom_hline(yintercept = C_target, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Coverage vs. T for constant s_avg", y = "Coverage", x = "T") +
    ggplot2::scale_y_continuous(limits = c(0, 2)) +
    ggplot2::scale_color_manual(values = c("T uncorrected" = "blue", "T corrected" = "green")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom")





  # Plot Coverage vs. s_avg
  plot2 <- ggplot2::ggplot(coverage_s_avg, ggplot2::aes_string(x = "steps")) +
    ggplot2::geom_line(ggplot2::aes_string(y = "C_plot_s_UN", color = "'T uncorrected'"), linetype = "solid") +
    ggplot2::geom_line(ggplot2::aes_string(y = "C_plot_s_COR", color = "'T corrected'"), linetype = "solid") +
    ggplot2::geom_hline(yintercept = C_target, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Coverage vs. s_avg for constant T uncorrected/corrected", y = "Coverage", x = "s_avg") +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_manual(values = c("T uncorrected" = "blue", "T corrected" = "green")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom")



  return(list(plot1 = plot1, plot2 = plot2, T_uncorrected = T_uncorrected, T_corrected = T_corrected))
}
