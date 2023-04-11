#' @title Create plot from `uniModelfilter` result
#' 
#' @description After performing a filtering step with \code{\link{uniModelfilter}} 
#' this function creates four plots (daily average part, intraday dynamic part, seasonality and intraday volume) 
#' from the input data and decomposition result. 
#' @param data The same data used in the \code{\link{uniModelfilter}}. 
#' @param filter.result Decomposition result as obtained from the function \code{uniModelfilter}.
#'
#' @author Shengjie Xiu and Yifan Yu
#' @seealso \code{\link{uniModelfilter}}
#' @import wesanderson
#' @import patchwork 
#' @import ggplot2
#' @importFrom magrittr %>%
#'
#' @examples
plot_decomposition <- function(data, filter_result) {
  data <- as.matrix(data) # convert df to matrix
  plt.data.log <- 
    data.frame(
      signal = as.vector(data),
      daily = filter_result$daily,
      seasonal = filter_result$seasonal,
      dynamic = filter_result$dynamic,
      i = 1:length(data)
    )
  
  text_size = 10
  p1 <- plt.data.log %>%
    ggplot() +
    geom_line(aes(x = i, y= signal), alpha = 0.8, color = "steelblue", size = 0.4) +
    xlab(expression(tau)) +
    ylab("Intraday\nSignal") +
    theme_bw() +
    theme(
      axis.title = element_text(size = text_size, face = "bold"),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.box.just = "left",
      legend.margin = margin(8, 8, 8, 8),
      legend.text = element_text(size = text_size, face = "bold"),
      # legend.title = element_blank(),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size=18, face = "bold", hjust = 0.5),
      axis.title.x=element_blank(),
      axis.text.x=element_blank()
    )
  
  p2 <- plt.data.log %>%
    ggplot() +
    geom_line(aes(x = i, y= daily), alpha = 0.8, color = "steelblue", size = 0.6) +
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
      # legend.title = element_blank(),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size=18, face = "bold", hjust = 0.5),
      axis.title.x=element_blank(),
      axis.text.x=element_blank()
    )
  
  p3 <- plt.data.log %>%
    ggplot() +
    geom_line(aes(x = i, y= seasonal), alpha = 0.8, color = "steelblue", size = 0.4) +
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
      # legend.title = element_blank(),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size=18, face = "bold", hjust = 0.5),
      axis.title.x=element_blank(),
      axis.text.x=element_blank()
    )
  
  p4 <- plt.data.log %>%
    ggplot() +
    geom_line(aes(x = i, y= dynamic), alpha = 0.8, color = "steelblue", size = 0.4) +
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
      # legend.title = element_blank(),
      legend.key.size = unit(1, "cm"),
      plot.title = element_text(size=18, face = "bold", hjust = 0.5)
    )
  
  fig <- p1/p2/p3/p4 + 
    plot_annotation(title = "Decomposition of intraday signal (log scale)",
                    theme =  theme(plot.title = element_text(size=16, face = "bold", hjust = 0.5)))
                    
  return(fig)
}


# uniModelPlot <- function(data, filter.result, type){
#   data.reform <- data %>%
#     as.list() %>%
#     unlist()
#   plt.data.log <-
#     data.frame(
#       volume = as.vector(data.reform),
#       daily = filter.result[["daily"]],
#       seasonal = filter.result[["seasonal"]],
#       dynamic = filter.result[["dynamic"]],
#       i = 1:(nrow(data_log_volume) * ncol(data_log_volume))
#     )
#   
#   text_size = 10
#   y.value <- y.lab <- NULL
#   switch (type,
#           "volume" = {y.value = quote(volume)
#           y.lab = "Intraday\nVolume"},
#           "daily" = {y.value = quote(daily)
#           y.lab = "Daily"},
#           "seasonal" = {y.value = quote(seasonal)
#           y.lab = "seasonal"},
#           "dynamic" = {y.value = quote(dynamic)
#           y.lab = "Intraday\nDynamic"}
#   )
#   p1 <- plt.data.log %>%
#     ggplot() +
#     geom_line(aes(x = i, y= !!y.value), alpha = 0.8, color = "steelblue", size = 0.4) +
#     xlab(expression(tau)) +
#     ylab(y.lab) +
#     theme_bw() +
#     theme(
#       axis.title = element_text(size = text_size, face = "bold"),
#       legend.position = "right",
#       legend.justification = c(0, 1),
#       legend.box.just = "left",
#       legend.margin = margin(8, 8, 8, 8),
#       legend.text = element_text(size = text_size, face = "bold"),
#       # legend.title = element_blank(),
#       legend.key.size = unit(1, "cm"),
#       plot.title = element_text(size=18, face = "bold", hjust = 0.5),
#       axis.title.x=element_blank(),
#       axis.text.x=element_blank()
#     )
#   p1
# }
