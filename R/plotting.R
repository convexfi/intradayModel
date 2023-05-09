#' @title Plot Smoothed/Forecast Components
#'
#' @description Plot the components of smoothing/forecasting result in one figure.
#'
#' @param smooth_forecast_result Smoothing/forecasting result from \code{smooth_unimodel} or \code{forecast_unimodel}.
#'
#' @return A \code{patchwork} object composed of 4 patches.
#'
#' @import patchwork
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'
#' data(aapl_volume)
#' aapl_volume_training <- aapl_volume[, 1:104]
#'
#' # Obtain smoothing and forecasting result
#' unimodel_fit <- fit_unimodel(aapl_volume_training)
#' smooth_result <- smooth_unimodel(aapl_volume_training, unimodel_fit)
#' forecast_result <- forecast_unimodel(aapl_volume, unimodel_fit, out.sample = 20)
#'
#' # Plot smoothed and forecast components
#' plot_components(smooth_result)
#' plot_components(forecast_result)
#' }
#' @export
plot_components <- function(smooth_forecast_result) {
  i <- original <- daily <- seasonal <- dynamic <- NULL
  components <- smooth_forecast_result[["components"]]

  plt_data <-
    data.frame(
      original = smooth_forecast_result$original.signal,
      daily = components[[grep("daily", names(components))]],
      seasonal = components[[grep("seasonal", names(components))]],
      dynamic = components[[grep("dynamic", names(components))]]
    )
  plt_data_log <- log(plt_data)
  plt_data_log$i <- plt_data$i <- c(1:nrow(plt_data))

  text_size <- 10
  p1 <- plt_data_log %>%
    ggplot() +
    geom_line(aes(x = i, y = original), alpha = 0.8, color = "steelblue", size = 0.4) +
    xlab(expression(tau)) +
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
    xlab(expression(tau)) +
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
    xlab(expression(tau)) +
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
    xlab(expression(tau)) +
    ylab("Intraday\nDynamic") +
    theme_bw() +
    xlab(expression(tau)) +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = text_size, face = "bold"),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.margin = margin(8, 8, 8, 8),
      legend.text = element_text(size = text_size, face = "bold"),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )

  p <- p1 / p2 / p3 / p4 +
    plot_annotation(
      title = "Decomposition of intraday signal (log scale)",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  return(p)
}

#' @title Plot Smoothing/Forecasting Performance
#'
#' @description Compares the original signal with the smoothed/forecast signal in one plot.
#'
#' @param smooth_forecast_result Smoothing/forecasting result from \code{smooth_unimodel} or \code{forecast_unimodel}.
#'
#' @return A \code{patchwork} object composed of 1 patch.
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'
#' data(aapl_volume)
#' aapl_volume_training <- aapl_volume[, 1:104]
#'
#' # Obtain smoothing and forecasting result
#' unimodel_fit <- fit_unimodel(aapl_volume_training)
#' smooth_result <- smooth_unimodel(aapl_volume_training, unimodel_fit)
#' forecast_result <- forecast_unimodel(aapl_volume, unimodel_fit, out.sample = 20)
#'
#' # Plot smoothing and forecasting performance
#' plot_performance(smooth_result)
#' plot_performance(forecast_result)
#' }
#' @export
plot_performance <- function(smooth_forecast_result) {
  i <- value <- variable <- NULL

  # determine type
  if (sum(grepl("smooth", names(smooth_forecast_result)))) {
    type <- "smooth"
    title <- "Smoothing result (log scale)"
  } else {
    type <- "forecast"
    title <- "One-bin-ahead forecast (log scale)"
  }

  plt_data <-
    data.frame(
      original = smooth_forecast_result$original.signal,
      output = smooth_forecast_result[[grep(type, names(smooth_forecast_result))]]
    )
  plt_data_log <- log(plt_data)
  plt_data_log$i <- plt_data$i <- c(1:nrow(plt_data))


  plt_reshape <- plt_data_log %>%
    reshape2::melt(
      id.vars = c("i"),
      variable.name = "variable", value.name = "value"
    )

  text_size <- 14
  p <- plt_reshape %>%
    ggplot() +
    geom_line(aes(x = i, y = value, color = variable), alpha = 0.8, size = 0.4) +
    scale_colour_manual(values = c(original = "steelblue", output = "#FD6467"), labels = c("original", type)) +
    xlab(expression(tau)) +
    ylab("Intraday Signal") +
    theme_bw() +
    theme(
      axis.title = element_text(size = text_size, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = text_size, face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text.x = element_blank()
    ) +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )

  return(p)
}
