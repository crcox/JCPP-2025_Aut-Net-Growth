library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(psych)


# Rename variables ----
modelvars_df <- readRDS("./network/modelvars-vsoa-20250520.rds") |>
    mutate(
        label = paste(group, num_item_id)
    ) |>
    rename(
        step = month,
        gv_pat1 = preferential_attachment_child,
        gv_pat2 = preferential_attachment_childes,
        gv_pac1 = preferential_acquisition_child,
        gv_pac2 = preferential_acquisition_childes,
        gv_loa1 = lure_of_the_associates_child,
        gv_loa2 = lure_of_the_associates_childes
    )


# Standardize growth ----
## by step ----
modelvars_df <- modelvars_df |>
    group_by(group, known, step) |>
    mutate(
        across(starts_with("gv_"),
               list(z = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)))
    ) |>
    ungroup()


## over all steps ----
modelvars_df <- modelvars_df |>
    group_by(group, known) |>
    mutate(
        across(starts_with("gv_") & !ends_with("_z"),
               list(Z = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)))
    ) |>
    ungroup()

## add suffix to raw growth values
modelvars_df <- modelvars_df |>
    rename_with(~ paste0(.x, "_r"), starts_with("gv") & !(ends_with("z") | ends_with("Z")))


# Visualize Growth Values by Step ----
modelvars_df |>
    select(group, known, learned, step, num_item_id, word, vsoa, starts_with("gv_")) |>
    pivot_longer(
        cols = starts_with("gv_"),
        names_to = c("growth_model", "source", "scale"),
        names_pattern = "gv_([pl][ao][act])([12])_([rzZ])",
        values_to = "growth_value"
    ) |>
    mutate(source = factor(as.numeric(source), 1:2, c("assoc", "childes"))) |>
    drop_na() |>
    group_by(group, known, step, growth_model, source, scale) |>
    summarize(
        across(growth_value, list(nnz = ~{sum(.x != 0)}, sum = sum, mean = mean, sd = sd, sumz = ~{sum(.x) / sd(.x)}, max = max))
    ) |>
    ungroup() |>
    filter(known == FALSE) |>
    ggplot(aes(x = step, y = growth_value_mean, color = growth_model)) +
        geom_point() +
        facet_grid(scale + source ~ group, scales = "free_y")


# Orthogonalizing Confounds ----

## correlation ----
# Number of phonemes is strongly (anti)correlated with frequency and
# phonological neighborhood density. It is moderately correlated with average
# biphone probability. Additionally, neighborhood density is correlated with frequency.
confounds <- modelvars_df |>
    select(num_item_id, word, nphon, CHILDES_Freq, BiphonProb.avg, PNDC.avg) |>
    distinct() |>
    mutate(across(c(nphon, CHILDES_Freq), list(log = ~ log(.x + 1)))) |>
    mutate(across(c(nphon_log, CHILDES_Freq_log, BiphonProb.avg, PNDC.avg), list(z = ~ (.x - mean(.x)) / sd(.x) )))


confounds |>
    select(nphon_log, CHILDES_Freq_log, BiphonProb.avg, PNDC.avg) |>
    cor() |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "x") |>
    readr::write_excel_csv("confound-correlations.csv")

## pca ----
# A principal components analysis which selects and rotates the first three
# factors obtains a solution where, essentially, number of phonemes is folded
# into each factor proportional to its correlation, and the three factors
# correspond more or less with Frequency, neighborhood density, and biphon
# probability. These rotated components may be a little cleaner to work with.
pca_varimax <- principal(
    select(confounds, nphon_log, CHILDES_Freq_log, BiphonProb.avg, PNDC.avg),
    nfactors = 3,
    rotate = "varimax"
)


## add confound factors to dataframe ----
confounds <- bind_cols(
    confounds,
    pca_varimax$scores |> as_tibble()
)
modelvars_df <- modelvars_df |>
    left_join(select(confounds, num_item_id, RC1, RC2, RC3)) |>
    select(-nphon, -CHILDES_Freq, -BiphonProb.avg, -PNDC.avg) |>
    relocate(label, num_item_id, .before = word) |>
    rename_with(
        ~ stringr::str_remove(.x, "gv_"),
        starts_with("gv_")
    )


# Set contrast code for group variable ----
contrasts(modelvars_df$group) <- c(-.5, .5)


# Save revised dataframe ----
saveRDS(modelvars_df, "network/modelvars_vsoa_RC_z.rds")


# Split dataframe by group ----
modelvars <- modelvars_df |>
    group_by(group) |>
    group_split()

names(modelvars) <- modelvars_df |>
    group_by(group) |>
    group_keys() |>
    pull(group) |>
    factor(levels = c("autistic", "nonautistic"), labels = c("asd", "td"))


## Append group label to growth model variables ----
modelvars_rn <- imap(modelvars, function(.x, group) {
    rename_with(
        .data = .x,
        .fn = function(x, y) {
            paste(x, y, sep = "_")
        },
        .cols = starts_with("pat") | starts_with("loa") | starts_with("pac"),
        y = group
    )
})


## standardize growth values (skip, this happens earlier) ----
# modelvars_rn <- imap(modelvars_rn, function(x, g) {
#     x |>
#         mutate(across(ends_with(g), list(z_overall = ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T)))) |>
#         group_by(step) |>
#         mutate(across(ends_with(g), list(z = ~ (.x - mean(.x, na.rm = T)) / sd(.x, na.rm = T))))
# })

saveRDS(modelvars_rn, "network/modelvars_vsoa_RC_z_split.rds")
