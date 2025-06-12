library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

overlap <- modelvars_df |>
    select(group, vsoa_bin, word) |>
    distinct() |>
    nest(words = word) |>
    pivot_wider(
        id_cols = vsoa_bin,
        names_from = group,
        values_from = words
    ) |>
    arrange(vsoa_bin) |>
    pmap_dbl(\(vsoa_bin, autistic, nonautistic) {
        length(intersect(autistic$word, nonautistic$word)) / length(union(autistic$word, nonautistic$word))
    })

overlap_df <- modelvars_df |>
    select(group, vsoa_bin, word) |>
    distinct() |>
    nest(words = word) |>
    pivot_wider(
        id_cols = vsoa_bin,
        names_from = group,
        values_from = words
    ) |>
    arrange(vsoa_bin)


xx <- overlap_df |>
    pmap(\(vsoa_bin, autistic, nonautistic) {
        x <- autistic$word
        y <- nonautistic$word
        tibble(
            vsoa_bin = vsoa_bin,
            autistic = list(x),
            nonautistic = list(y),
            n_aut = length(x),
            n_non = length(y),
            overlap = length(intersect(x, y)) / length(union(x, y))
        )
    }) |> list_rbind()

p_n <- xx |>
    pivot_longer(cols = c(n_aut, n_non), names_to = "group", values_to = "n", names_prefix = "n_") |>
    mutate(vsoa_bin = as.numeric(vsoa_bin)) |>
    ggplot(aes(x = vsoa_bin, y = n, fill = group)) +
    geom_smooth(aes(color = group), se = FALSE) +
    geom_point(color = "black", shape = 21) +
    theme_bw(base_size = 12) +
    scale_color_grey() +
    scale_fill_grey()

ggsave(
    "figures/count-by-bin.pdf",
    plot = p_n + theme(legend.position = "none"),
    width = 3.5, height = 2, units = "in")

p_o <- xx |>
    mutate(vsoa_bin = as.numeric(vsoa_bin)) |>
    ggplot(aes(x = vsoa_bin, y = overlap)) +
    geom_point() +
    ylim(c(0, 1)) +
    theme_bw(base_size = 12)

ggsave(
    "figures/overlap-by-bin.pdf",
    plot = p_o,
    width = 3.5, height = 2, units = "in")

#(stat = "identity", position = position_dodge())


m <- readRDS("model-fits-split/z-overall/growth-model-2.rds")


m$asd$assoc$loa_pac$model
