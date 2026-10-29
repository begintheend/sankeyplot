#' Draw a stacked Sankey diagram
#'
#' At each time point the categories are stacked as bars, largest on top,
#' and each category's bars are joined by ribbons. The default settings
#' match the Excel chart in `Stacked_column_stroke_blacks_noAF.xlsx`.
#'
#' @param data A data frame with one row per category and time point.
#' @param category,time,value Names of the columns holding the category,
#'   the time point and the value.
#' @param gap Vertical white space between bands of different colours, in
#'   the units of `value`. Use `0` for none.
#' @param white_lines If `TRUE`, draw white lines between each bar and its
#'   ribbons (horizontal white space between shapes of the same colour).
#' @param colours Colours for the categories, named by category.
#' @param digits Number of decimals in the value labels.
#' @param title Plot title.
#' @return A ggplot object.
#' @examples
#' sankey_plot(stroke_blacks, title = "Risk Factors for Stroke in Blacks")
#' sankey_plot(stroke_blacks, gap = 0, white_lines = FALSE)
#' @import ggplot2
#' @importFrom dplyr %>% group_by arrange mutate ungroup row_number
#' @importFrom dplyr group_modify filter distinct tibble
#' @export
sankey_plot <- function(data, category = "risk_factor", time = "year",
                        value = "par", gap = 0.05, white_lines = TRUE,
                        colours = excel_colours, digits = 2, title = NULL) {
  # Standard column names, whatever the input calls them
  df <- data.frame(category = data[[category]],
                   time = data[[time]],
                   value = data[[value]])
  if (!is.factor(df$category)) {
    df$category <- factor(df$category, levels = unique(df$category))
  }

  # Where each band sits: smallest at the bottom, `gap` under each band
  bands <- df %>%
    group_by(time) %>%
    arrange(value, .by_group = TRUE) %>%
    mutate(position = row_number(),
           lower = cumsum(value) - value + position * gap,
           upper = lower + value,
           mid   = (lower + upper) / 2) %>%
    ungroup() %>%
    mutate(j = match(time, sort(unique(time))) - 1,
           x_mid = 3 * j + 1,
           label = excel_label(value, digits))

  # One polygon per category
  polys <- bands %>%
    group_by(category) %>%
    group_modify(~ outline(.x)) %>%
    ungroup()

  n_times <- length(unique(bands$time))
  top <- max(bands$upper)
  y_min <- gap / 5
  y_max <- max(pretty(c(0, 1.1 * top)))
  first_time <- filter(bands, j == 0)

  # Use the colours by name if they cover every category, otherwise in order
  if (!all(levels(df$category) %in% names(colours))) {
    colours <- rep_len(unname(colours), nlevels(df$category))
    names(colours) <- levels(df$category)
  }

  p <- ggplot() +
    geom_polygon(data = polys,
                 aes(x, y, group = category, fill = category, colour = category),
                 linewidth = pt(4.25), linejoin = "round")

  if (white_lines && n_times > 1) {
    k <- seq_len(n_times - 1) - 1
    p <- p + geom_segment(data = data.frame(x = c(3 * k + 2, 3 * k + 3)),
                          aes(x = x, xend = x), y = y_min, yend = 1.1 * top,
                          colour = "white", linewidth = pt(1.5))
  }

  p +
    geom_text(data = bands, aes(x_mid, mid, label = label),
              colour = "white", fontface = "bold", size = 10 / .pt) +
    geom_text(data = distinct(bands, time, x_mid),
              aes(x_mid, label = time), y = 1.05 * top, size = 12 / .pt) +
    scale_fill_manual(values = colours) +
    scale_colour_manual(values = colours) +
    scale_y_continuous(breaks = first_time$mid,
                       labels = as.character(first_time$category)) +
    coord_cartesian(xlim = c(-2, 3 * n_times), ylim = c(y_min, y_max),
                    expand = FALSE, clip = "off") +
    labs(title = title) +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", colour = NA),
          axis.text.y = element_text(size = 12, hjust = 1, margin = margin(r = 4)),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.margin = margin(12, 16, 12, 12))
}

# The Excel chart's colours
excel_colours <- c(Obesity = "#4D6D94", Diabetes = "#DA654F",
                   Smoking = "#7EACC5", Hypercholesterolemia = "#D88F54",
                   Hypertension = "#8ABA8F")

# Excel line width in points -> ggplot2 linewidth
pt <- function(points) points * 96 / 72 / .pt

# Round the way Excel does (0.045 -> 0.05)
excel_label <- function(x, digits = 2) {
  m <- 10^digits
  formatC(floor(x * m + 0.5 + 1e-9) / m, format = "f", digits = digits)
}

# Outline of one category: along the tops left to right, back along the bottoms
outline <- function(d) {
  d <- arrange(d, j)
  x <- as.vector(rbind(3 * d$j, 3 * d$j + 2))
  tibble(x = c(x, rev(x)),
         y = c(rep(d$upper, each = 2), rev(rep(d$lower, each = 2))))
}

# Column names used inside dplyr and ggplot2 code
utils::globalVariables(c("value", "position", "lower", "upper", "mid", "j",
                         "x_mid", "label", "category", "x", "y"))
