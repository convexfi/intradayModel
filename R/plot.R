#' @title Plot Analysis and Forecast Result
#'
#' @description Generate plots for the analysis and forecast results.
#'
#' @author Shengjie Xiu, Yifan Yu and Daniel P. Palomar
#'
#' @param analysis_forecast_result Analysis/forecast result from \code{decompose_volume()} or \code{forecast_volume()}.
#'
#' @return A list of \code{patchwork} objects:
#'        \itemize{
#'        \item{\code{components}: }{Plot of components of intraday volume;}
#'        \item{\code{log_components}: }{Plot of components of intraday volume in their log10 scale;}
#'        \item{\code{original_and_smooth} / \code{original_and_forecast}: }{Plot of the original and the smooth/forecast intraday volume.}}
#'
#' @import patchwork
#' @import ggplot2
#' @importFrom scales trans_breaks trans_format math_format
#' @importFrom magrittr %>%
#' @examples
#' library(intradayModel)
#' data(volume_aapl)
#' volume_aapl_training <- volume_aapl[, 1:20]
#' volume_aapl_testing <- volume_aapl[, 21:50]
#'
#' # obtain analysis and forecast result
#' model_fit <- fit_volume(volume_aapl_training, fixed_pars = list(a_mu = 0.5, var_mu = 0.05),
#'                         init_pars = list(a_eta = 0.5))
#' analysis_result <- decompose_volume(purpose = "analysis", model_fit, volume_aapl_training)
#' forecast_result <- forecast_volume(model_fit, volume_aapl_testing)
#'
#' # plot the analysis and forecast result
#' generate_plots(analysis_result)
#' generate_plots(forecast_result)
#' 
#' @export
generate_plots <- function(analysis_forecast_result) {
  plot_list <- list()

  plot_list$components <- plot_components(analysis_forecast_result, log = FALSE)
  plot_list$log_components <- plot_components(analysis_forecast_result, log = TRUE)
  if ("analysis" %in% attr(analysis_forecast_result, "type")) {
    plot_list$original_and_smooth <- plot_performance(analysis_forecast_result)
  } else {
    plot_list$original_and_forecast <- plot_performance(analysis_forecast_result)
  }
  return(plot_list)
}

plot_components <- function(analysis_forecast_result, log = TRUE) {
  if ("analysis" %in% attr(analysis_forecast_result, "type")) {
    if (log == TRUE) {
      title <- "Components of Log Intraday Volume (analysis)"
    } else {
      title <- "Components of Intraday Volume (analysis)"
    }
  } else {
    if (log == TRUE) {
      title <- "Components of Log Intraday Volume (forecast)"
    } else {
      title <- "Components of Intraday Volume (forecast)"
    }
  }

  i <- original <- daily <- seasonal <- dynamic <- residual <- NULL
  components <- analysis_forecast_result[[grep("components", names(analysis_forecast_result))]]

  plt_data <-
    data.frame(
      original = analysis_forecast_result$original_signal,
      daily = components$daily,
      seasonal = components$seasonal,
      dynamic = components$dynamic,
      residual = components$residual
    )
  if (log == TRUE) {
    plt_data <- log10(plt_data)
  }
  plt_data$i <- plt_data$i <- c(1:nrow(plt_data))

  text_size <- 10
  .x <- NULL

  if (log == TRUE) {
    p1 <- plt_data %>%
      ggplot() +
      geom_line(aes(x = i, y = original), alpha = 0.8, color = "steelblue", size = 0.4) +
      ylab("Original") +
      theme_bw() +
      theme(
        axis.title = element_text(size = text_size, face = "bold"),
        legend.position = "right",
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.margin = margin(8, 8, 8, 8),
        legend.text = element_text(size = text_size, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )

    p2 <- plt_data %>%
      ggplot() +
      geom_line(aes(x = i, y = daily), alpha = 0.8, color = "steelblue", size = 0.6) +
      ylab("Daily") +
      theme_bw() +
      theme(
        axis.title = element_text(size = text_size, face = "bold"),
        legend.position = "right",
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.margin = margin(8, 8, 8, 8),
        legend.text = element_text(size = text_size, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  } else {
    p1 <- plt_data %>%
      ggplot() +
      geom_line(aes(x = i, y = original), alpha = 0.8, color = "steelblue", size = 0.4) +
      ylab("Original") +
      scale_y_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = text_size, face = "bold"),
        legend.position = "right",
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.margin = margin(8, 8, 8, 8),
        legend.text = element_text(size = text_size, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )

    p2 <- plt_data %>%
      ggplot() +
      geom_line(aes(x = i, y = daily), alpha = 0.8, color = "steelblue", size = 0.6) +
      ylab("Daily") +
      scale_y_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = text_size, face = "bold"),
        legend.position = "right",
        legend.justification = c(0, 1),
        legend.box.just = "left",
        legend.margin = margin(8, 8, 8, 8),
        legend.text = element_text(size = text_size, face = "bold"),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }


  p3 <- plt_data %>%
    ggplot() +
    geom_line(aes(x = i, y = seasonal), alpha = 0.8, color = "steelblue", size = 0.4) +
    ylab("Seasonal") +
    theme_bw() +
    theme(
      axis.title = element_text(size = text_size, face = "bold"),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.margin = margin(8, 8, 8, 8),
      legend.text = element_text(size = text_size, face = "bold"),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    )

  p4 <- plt_data %>%
    ggplot() +
    geom_line(aes(x = i, y = dynamic), alpha = 0.8, color = "steelblue", size = 0.4) +
    ylab("Intraday\nDynamic") +
    theme_bw() +
    theme(
      axis.title = element_text(size = text_size, face = "bold"),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.margin = margin(8, 8, 8, 8),
      legend.text = element_text(size = text_size, face = "bold"),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    )

  p5 <- plt_data %>%
    ggplot() +
    geom_line(aes(x = i, y = residual), alpha = 0.8, color = "steelblue", size = 0.4) +
    ylab("Residual") +
    theme_bw() +
    xlab("time (bins)") +
    theme(
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = text_size, face = "bold"),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.margin = margin(8, 8, 8, 8),
      legend.text = element_text(size = text_size, face = "bold"),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )

  p <- p1 / p2 / p3 / p4 / p5 +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  return(p)
}

plot_performance <- function(analysis_forecast_result) {
  i <- value <- variable <- .x <- NULL

  # determine type
  if ("analysis" %in% attr(analysis_forecast_result, "type")) {
    approximated_signal <- analysis_forecast_result$smooth_signal
    title <- "Original and Smooth Signals (analysis)"
    legend_name <- "smooth"
  } else {
    approximated_signal <- analysis_forecast_result$forecast_signal
    title <- "Original and One-bin-ahead Forecast signal (forecast)"
    legend_name <- "forecast"
  }

  plt_data <-
    data.frame(
      original = analysis_forecast_result$original_signal,
      output = approximated_signal
    )

  plt_data$i <- c(1:nrow(plt_data))

  plt_reshape <- plt_data %>%
    reshape2::melt(
      id.vars = c("i"),
      variable.name = "variable", value.name = "value"
    )

  text_size <- 14
  p <- plt_reshape %>%
    ggplot() +
    geom_line(aes(x = i, y = value, color = variable), alpha = 0.8, size = 0.4) +
    scale_colour_manual(values = c(original = "steelblue", output = "#FD6467"), labels = c("original", legend_name)) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    xlab("time (bins)") +
    ylab("Intraday Volume") +
    theme_bw() +
    theme(
      axis.title = element_text(size = text_size, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = text_size, face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    ) +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  return(p)
}
