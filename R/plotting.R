#' @title Plot Analysis and Forecast Result
#'
#' @description Autoplot the results of analysis and forecast.
#'
#' @param analysis_forecast_result Analysis/forecast result from \code{use_model}.
#'
#' @return A list of \code{patchwork} objects:
#'        \itemize{
#'        \item{\code{log_components}: }{Plot of components of intraday signal in their log10 scale;}
#'        \item{\code{original_and_resultant}: }{Plot of the original and the resulant signals.}}
#'
#' @import patchwork
#' @import ggplot2
#' @import scales
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'
#' data(aapl_volume)
#' aapl_volume_training <- aapl_volume[, 1:104]
#' aapl_volume_testing <- aapl_volume[, 105:124]
#'
#' # Obtain smoothing and forecasting result
#' model_fit <- fit_volume(aapl_volume_training)
#' analysis_result <- use_model(purpose = "analysis", model_fit, aapl_volume_training)
#' forecast_result <- use_model(purpose = "forecast", model_fit, aapl_volume_testing)
#' 
#' # Plot components
#' autoplot(analysis_result)
#' autoplot(forecast_result)
#' }
#' @export
autoplot <- function(analysis_forecast_result) {
  plot_list <- list()
  
  plot_list$log_components <- plot_components(analysis_forecast_result)
  plot_list$original_and_resultant <- plot_performance(analysis_forecast_result)
  
  return(plot_list)
}

plot_components <- function(analysis_forecast_result) {
  i <- original <- daily <- seasonal <- dynamic <- NULL
  components <- analysis_forecast_result[["components"]]

  plt_data <-
    data.frame(
      original = analysis_forecast_result$original_signal,
      daily = components[[grep("daily", names(components))]],
      seasonal = components[[grep("seasonal", names(components))]],
      dynamic = components[[grep("dynamic", names(components))]],
      residual = components$residual
    )
  plt_data_log <- log10(plt_data)
  plt_data_log$i <- plt_data$i <- c(1:nrow(plt_data))

  text_size <- 10
  p1 <- plt_data_log %>%
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

  p2 <- plt_data_log %>%
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

  p3 <- plt_data_log %>%
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

  p4 <- plt_data_log %>%
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

  p5 <- plt_data_log %>%
    ggplot() +
    geom_line(aes(x = i, y = residual), alpha = 0.8, color = "steelblue", size = 0.4) +
    ylab("Residual") +
    theme_bw() +
    xlab("time") +
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
      title = "Decomposition of intraday signal",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  return(p)
}

plot_performance <- function(analysis_forecast_result) {
  i <- value <- variable <- NULL

  # determine type
  if (sum(grepl("smooth", names(analysis_forecast_result)))) {
    type <- "smooth"
    title <- "Smoothing result"
  } else {
    type <- "forecast"
    title <- "One-bin-ahead forecast"
  }

  plt_data <-
    data.frame(
      original = analysis_forecast_result$original_signal,
      output = analysis_forecast_result[[grep(type, names(analysis_forecast_result))]]
    )
#  plt_data_log <- log(plt_data)
#  plt_data_log$i <- plt_data$i <- c(1:nrow(plt_data))

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
    scale_colour_manual(values = c(original = "steelblue", output = "#FD6467"), labels = c("original", type)) +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    xlab("time") +
    ylab("Intraday Signal") +
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
