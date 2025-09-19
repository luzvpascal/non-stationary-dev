plot_voi_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(EVPI = (opt - stat)*100 / opt) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = EVPI
    )) +
    # geom_raster(interpolate = TRUE) +
    # geom_point() +
    geom_tile() +
    scale_fill_gradient(low = "lightblue", high = "blue") +
    labs(
      fill = TeX("Value of modelling\nnon-stationary rewards (%)")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_Tmax_opt <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_opt)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("Optimal $T_{max}$")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_Tmax_stat <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_stat)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("$T_{max}$ stationary")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
}

plot_Tmax_diff <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(Tmax_diff = (Tmax_opt - Tmax_stat)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = Tmax_diff
    )) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      fill = TeX("Difference between $T_{max}$")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_start_invest_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  plot <- df %>%
    mutate(start_diff = (Tmax_opt_start)) %>%
    # mutate(start_diff = (Tmax_opt_start-Tmax_stat_start)) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = start_diff
    )) +
    # geom_raster(interpolate = TRUE) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      # x = TeX("Slope change baseline rewards $\\alpha$"),
      # y = TeX("Slope change deployment rewards $\\beta$"),
      fill = TeX("Difference between\nstart times")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}

plot_value_result <- function(df,params) {
  # grab first two column names dynamically
  xcol <- names(df)[1]
  ycol <- names(df)[2]

  df %>%
    mutate(rEVPI = (V_opt-V_stat)*100/V_opt) %>%
    ggplot(aes(
      x = abs(.data[[xcol]]),
      y = abs(.data[[ycol]]),
      fill = rEVPI
    )) +
    # geom_raster(interpolate = TRUE) +
    geom_raster() +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      # x = TeX("Slope change baseline rewards $\\alpha$"),
      # y = TeX("Slope change deployment rewards $\\beta$"),
      fill = TeX("rEVPI (%)")
    )
  if (params$log_scale_x){
    plot <- plot+
      scale_x_log10()
  }

  if (params$log_scale_y){
    plot <- plot+
      scale_y_log10()
  }
  return(plot)
}
