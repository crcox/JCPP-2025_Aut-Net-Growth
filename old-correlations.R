d_meta_preproc <- readRDS("data/cdi-metadata-preproc.rds")
tmp <- readRDS("data/vsoa-autistic-nonautistic-diff-ndar-id-fix-remodel-v2.rds") |>
    left_join(
        select(
            mutate(d_meta_preproc, to_include = TRUE),
            num_item_id, aoa_NA = aoa_produces, to_include
        )
    ) |>
    filter(to_include) |>
    mutate(
        trimmed_vsoa_na = if_else(vsoa_NA <   1,  1, vsoa_NA),
        trimmed_vsoa_na = if_else(vsoa_NA > 680, NA, vsoa_NA),
        trimmed_vsoa_asd = if_else(vsoa_ASD <   1,  1, vsoa_ASD),
        trimmed_vsoa_asd = if_else(vsoa_ASD > 680, NA, vsoa_ASD),
        trimmed_aoa_na  = if_else(aoa_NA <  12, 12,  aoa_NA),
        trimmed_aoa_na  = if_else(aoa_NA >  30, NA,  aoa_NA)
    ) |>
    rename(
        raw_vsoa_na = vsoa_NA,
        raw_vsoa_asd = vsoa_ASD,
        raw_aoa_na = aoa_NA
    ) |>
    select(-ends_with("diff"), -ci_type, -to_include) |>
    pivot_longer(
        cols = c(starts_with("raw"), starts_with("trimmed")),
        names_to = c("type", "metric", "group"),
        values_to = "value",
        names_sep = "_"
    ) |>
    pivot_wider(
        id_cols = c(num_item_id, word, type),
        names_from = c(metric, group),
        values_from = "value"
    )


tmp |>
    drop_na(vsoa_na, vsoa_asd) |>
    group_by(type) |>
    summarize(
        r_pearson = cor(vsoa_na, vsoa_asd, method = "pearson"),
        r_kendall = cor(vsoa_na, vsoa_asd, method = "kendall")
    ) |>
    left_join(
        tmp |>
            drop_na(vsoa_na, vsoa_asd) |>
            count(type)
    )


tmp |>
    drop_na(aoa_na, vsoa_asd) |>
    group_by(type) |>
    summarize(
        r_pearson = cor(aoa_na, vsoa_asd, method = "pearson", use = "complete"),
        r_kendall = cor(aoa_na, vsoa_asd, method = "kendall", use = "complete")
    ) |>
    left_join(
        tmp |>
            drop_na(aoa_na, vsoa_asd) |>
            count(type)
    )


tmp |>
    drop_na(aoa_na, vsoa_na) |>
    group_by(type) |>
    summarize(
        r_pearson = cor(aoa_na, vsoa_na, method = "pearson", use = "complete"),
        r_kendall = cor(aoa_na, vsoa_na, method = "kendall", use = "complete")
    ) |>
    left_join(
        tmp |>
            drop_na(aoa_na, vsoa_na) |>
            count(type)
    )
