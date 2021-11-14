library(hexSticker)
library(tidyverse)

returns <- tribble(
    ~ x, ~y, ~linetype, ~color,
    1, 1, "pred", "grey",
    2, 1.5, "pred", "grey",
    3, 0.9, "pred", "grey",
    4, 1.6, "pred", "grey",
    1, 1.7, "obs", "green",
    2, 2.0, "obs", "green",
    3, 0.5, "obs", "red",
    4, 1.9, "obs", "green"
)

p <- ggplot(data = returns, aes(x = x, y = y)) +
    geom_line(aes(linetype = linetype), color = "#859596") +
    geom_point(aes(color = color), size = 4) +
    scale_color_manual(values=c("#238823", "#859596", "#D2222D")) +
    ylim(0.3, 2.3) +
    theme_void() +
    theme_transparent() +
    theme(legend.position = "none")


sticker(
    p,
    package = "estudy2",
    p_size = 20,
    s_x = 1,
    s_y = .75,
    s_width = 1.5,
    s_height = 1.2,
    filename = "man/figures/logo.png",
    h_color = "#b58900",
    p_color = "#859596",
    h_fill = "#092b36"
)

sticker(
    p,
    package = "estudy2",
    p_size = 20,
    s_x = 1,
    s_y = .75,
    s_width = 1.5,
    s_height = 1.2,
    filename = "inst/app/www/logo.png",
    h_color = "#b58900",
    p_color = "#859596",
    h_fill = "#092b36"
)
