#' Title
#'
#' @param data
#' @param filter.result
#'
#' @return
#' @export
#' @import wesanderson
#' @import patchwork
#' @importFrom magrittr %>%
#' @importFrom dplyr !!
#' @importFrom ggplot2 ggplot geom_line xlab ylab theme_bw theme
#'
#' @examples
uniModelPlot <- function(data, filter.result, type){
  data.reform <- data %>%
    as.list() %>%
    unlist()
  plt.data.log <-
    data.frame(
      volume = as.vector(data.reform),
      daily = filter.result[["daily"]],
      seasonal = filter.result[["seasonal"]],
      dynamic = filter.result[["dynamic"]],
      i = 1:(nrow(data_log_volume) * ncol(data_log_volume))
    )
  
  text_size = 10
  y.value <- y.lab <- NULL
  switch (type,
          "volume" = {y.value = quote(volume)
          y.lab = "Intraday\nVolume"},
          "daily" = {y.value = quote(daily)
          y.lab = "Daily"},
          "seasonal" = {y.value = quote(seasonal)
          y.lab = "seasonal"},
          "dynamic" = {y.value = quote(dynamic)
          y.lab = "Intraday\nDynamic"}
  )
  p1 <- plt.data.log %>%
    ggplot() +
    geom_line(aes(x = i, y= !!y.value), alpha = 0.8, color = "steelblue", size = 0.4) +
    xlab(expression(tau)) +
    ylab(y.lab) +
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
  p1
}

# p1/p2/p3/p4
