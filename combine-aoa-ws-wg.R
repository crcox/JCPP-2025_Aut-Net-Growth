library(dplyr)
library(purrr)
library(tidyr)

x <- bind_rows(
    WG = readRDS("data/CDI_English (American)_WG_14May2025_AoA.rds"),
    WS = readRDS("data/CDI_English (American)_WS_14May2025_AoA.rds"),
    .id = "form"
) |>
    mutate(form = as.factor(form)) |>
    pivot_wider(
        id_cols = c(num_item_id, definition, category, lexical_category),
        names_from = form,
        values_from = aoa_glm
    ) |>
    mutate(aoa = if_else(is.na(WS), WG, WS))

filter(x, is.na(WS), !is.na(WG))
filter(x, !is.na(WS), !is.na(WG))

aoa <- readRDS("data/CDI_English (American)_WS_14May2025_AoA.rds") |>
    left_join(
        readRDS("data/CDI_English (American)_WG_14May2025_AoA.rds") |>
            select(definition, aoa_wg = aoa_glm)
        ) |>
    rename(aoa_ws = aoa_glm) |>
    mutate(aoa = if_else(is.na(aoa_ws), aoa_wg, aoa_ws))


saveRDS(aoa, "data/aoa-nonautistic-20250514.rds")
