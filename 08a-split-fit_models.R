library(dplyr)
library(purrr)
library(tidyr)
library(netgrowr)
library(parallel)
library(progressr)

# Helper functions ----
fit_models <- function(x_split, formulas) {
    p <- progressor(length(x_split) * length(formulas) * length(formulas[[1]]))
    map(x_split, \(df, formula_list) {
        map_depth(formula_list, 2, \(formula, .data) {
            m <- netgrowr::mle_network_growth(
                formula = formula,
                data = na.omit(.data),
                split_by = "vocab_step",
                label_with = "num_item_id"
            ) |> list_assign(formula = formula)
            p()
            return(m)
        }, .data = df)
    }, formula_list = formulas)
}
add_formulas <- function(models, formulas) {
    map2(models, formulas, \(mm, ff) {
        assertthat::are_equal(names(mm), names(ff))
        map2(mm, ff, \(m, f) {
            list_assign(m, formula = f)
        })
    })
}


# Load and prep data ----
modelvars_split <- readRDS("network/modelvars_vsoa_RC_z_split.rds") |>
    map(~{
        .x |>
            relocate(vid, .before = num_item_id) |>
            relocate(vsoa, vsoa_bin, .after = word) |>
            relocate(vocab_step, known, learned, .after = step) |>
            relocate(RC1, RC2, RC3, .after = learned) |>
            rename_with(\(.x) stringr::str_remove(.x, "_asd$"), ends_with("_asd")) |>
            rename_with(\(.x) stringr::str_remove(.x, "_td$"), ends_with("_td")) |>
            select(-label)
    })

# Baseline models ----
fE <- formula(vsoa_bin ~ 1)
ME <- map(modelvars_split, \(.x, formula) {
    netgrowr::mle_network_growth(
        formula = formula,
        data = na.omit(.x),
        split_by = "vocab_step",
        label_with = "num_item_id"
    ) |> list_assign(formula = formula)
}, formula = fE)
saveRDS(ME, "model-fits-split/empty-model.rds")

f0 <- update(fE, ~ . + RC1 + RC2 + RC3)
M0 <- map(modelvars_split, \(.x, formula) {
    netgrowr::mle_network_growth(
        formula = formula,
        data = na.omit(.x),
        split_by = "vocab_step",
        label_with = "num_item_id"
    ) |> list_assign(formula = formula)
}, formula = f0)
saveRDS(M0, "model-fits-split/baseline-model.rds")

# Growth Value models (RAW) ----

## 1 value ----
f1 <- list(
    assoc = list(
        pat = update(f0, ~ . + pat1_r),
        loa = update(f0, ~ . + loa1_r),
        pac = update(f0, ~ . + pac1_r)
    ),
    childes = list(
        pat = update(f0, ~ . + pat2_r),
        loa = update(f0, ~ . + loa2_r),
        pac = update(f0, ~ . + pac2_r)
    ),
    both = list(
        pat = update(f0, ~ . + pat1_r + pat2_r),
        loa = update(f0, ~ . + loa1_r + loa2_r),
        pac = update(f0, ~ . + pac1_r + pac2_r)
    )
)
M1 <- with_progress(fit_models(modelvars_split, f1))
saveRDS(M1, "model-fits-split/raw/growth-model-1.rds")

## 2 values ----
f2 <- list(
    assoc = list(
        pat_loa = update(f0, ~ . + pat1_r + loa1_r),
        pat_pac = update(f0, ~ . + pat1_r + pac1_r),
        loa_pac = update(f0, ~ . + loa1_r + pac1_r)
    ),
    childes = list(
        pat_loa = update(f0, ~ . + pat2_r + loa2_r),
        pat_pac = update(f0, ~ . + pat2_r + pac2_r),
        loa_pac = update(f0, ~ . + loa2_r + pac2_r)
    ),
    both = list(
        pat_loa = update(f0, ~ . + pat1_r + pat2_r + loa1_r + loa2_r),
        pat_pac = update(f0, ~ . + pat1_r + pat2_r + pac1_r + pac2_r),
        loa_pac = update(f0, ~ . + loa1_r + loa2_r + pac1_r + pac2_r)
    )
)
M2 <- with_progress(fit_models(modelvars_split, f2))
saveRDS(M2, "model-fits-split/raw/growth-model-2.rds")

## 3 values ----
f3 <- list(
    assoc = list(
        pat_loa_pac = update(f0, ~ . + pat1_r + loa1_r + pac1_r)
    ),
    childes = list(
        pat_loa_pac = update(f0, ~ . + pat2_r + loa2_r + pac2_r)
    ),
    both = list(
        pat_loa_pac = update(f0, ~ . + pat1_r + pat2_r + loa1_r + loa2_r + pac1_r + pac2_r)
    )
)
M3 <- with_progress(fit_models(modelvars_split, f3))
saveRDS(M3, "model-fits-split/raw/growth-model-3.rds")


# Growth Value models (z-overall) ----

## 1 value ----
f1_Z <- list(
    assoc = list(
        pat = update(f0, ~ . + pat1_Z),
        loa = update(f0, ~ . + loa1_Z),
        pac = update(f0, ~ . + pac1_Z)
    ),
    childes = list(
        pat = update(f0, ~ . + pat2_Z),
        loa = update(f0, ~ . + loa2_Z),
        pac = update(f0, ~ . + pac2_Z)
    ),
    both = list(
        pat = update(f0, ~ . + pat1_Z + pat2_Z),
        loa = update(f0, ~ . + loa1_Z + loa2_Z),
        pac = update(f0, ~ . + pac1_Z + pac2_Z)
    )
)
M1_Z <- with_progress(fit_models(modelvars_split, f1_Z))
saveRDS(M1_Z, "model-fits-split/z-overall/growth-model-1.rds")

## 2 values ----
f2_Z <- list(
    assoc = list(
        pat_loa = update(f0, ~ . + pat1_Z + loa1_Z),
        pat_pac = update(f0, ~ . + pat1_Z + pac1_Z),
        loa_pac = update(f0, ~ . + loa1_Z + pac1_Z)
    ),
    childes = list(
        pat_loa = update(f0, ~ . + pat2_Z + loa2_Z),
        pat_pac = update(f0, ~ . + pat2_Z + pac2_Z),
        loa_pac = update(f0, ~ . + loa2_Z + pac2_Z)
    ),
    both = list(
        pat_loa = update(f0, ~ . + pat1_Z + pat2_Z + loa1_Z + loa2_Z),
        pat_pac = update(f0, ~ . + pat1_Z + pat2_Z + pac1_Z + pac2_Z),
        loa_pac = update(f0, ~ . + loa1_Z + loa2_Z + pac1_Z + pac2_Z)
    )
)
M2_Z <- with_progress(fit_models(modelvars_split, f2_Z))
saveRDS(M2_Z, "model-fits-split/z-overall/growth-model-2.rds")

## 3 values ----
f3_Z <- list(
    assoc = list(
        pat_loa_pac = update(f0, ~ . + pat1_Z + loa1_Z + pac1_Z)
    ),
    childes = list(
        pat_loa_pac = update(f0, ~ . + pat2_Z + loa2_Z + pac2_Z)
    ),
    both = list(
        pat_loa_pac = update(f0, ~ . + pat1_Z + pat2_Z + loa1_Z + loa2_Z + pac1_Z + pac2_Z)
    )
)
M3_Z <- with_progress(fit_models(modelvars_split, f3_Z))
saveRDS(M3_Z, "model-fits-split/z-overall/growth-model-3.rds")


# Growth Value models (z-by-step) ----

## 1 value ----
f1_z <- list(
    assoc = list(
        pat = update(f0, ~ . + pat1_z),
        loa = update(f0, ~ . + loa1_z),
        pac = update(f0, ~ . + pac1_z)
    ),
    childes = list(
        pat = update(f0, ~ . + pat2_z),
        loa = update(f0, ~ . + loa2_z),
        pac = update(f0, ~ . + pac2_z)
    ),
    both = list(
        pat = update(f0, ~ . + pat1_z + pat2_z),
        loa = update(f0, ~ . + loa1_z + loa2_z),
        pac = update(f0, ~ . + pac1_z + pac2_z)
    )
)
M1_z <- with_progress(fit_models(modelvars_split, f1_z))
saveRDS(M1_z, "model-fits-split/z-by-step/growth-model-1.rds")

## 2 values ----
f2_z <- list(
    assoc = list(
        pat_loa = update(f0, ~ . + pat1_z + loa1_z),
        pat_pac = update(f0, ~ . + pat1_z + pac1_z),
        loa_pac = update(f0, ~ . + loa1_z + pac1_z)
    ),
    childes = list(
        pat_loa = update(f0, ~ . + pat2_z + loa2_z),
        pat_pac = update(f0, ~ . + pat2_z + pac2_z),
        loa_pac = update(f0, ~ . + loa2_z + pac2_z)
    ),
    both = list(
        pat_loa = update(f0, ~ . + pat1_z + pat2_z + loa1_z + loa2_z),
        pat_pac = update(f0, ~ . + pat1_z + pat2_z + pac1_z + pac2_z),
        loa_pac = update(f0, ~ . + loa1_z + loa2_z + pac1_z + pac2_z)
    )
)
M2_z <- with_progress(fit_models(modelvars_split, f2_z))
saveRDS(M2_z, "model-fits-split/z-by-step/growth-model-2.rds")

## 3 values ----
f3_z <- list(
    assoc = list(
        pat_loa_pac = update(f0, ~ . + pat1_z + loa1_z + pac1_z)
    ),
    childes = list(
        pat_loa_pac = update(f0, ~ . + pat2_z + loa2_z + pac2_z)
    ),
    both = list(
        pat_loa_pac = update(f0, ~ . + pat1_z + pat2_z + loa1_z + loa2_z + pac1_z + pac2_z)
    )
)
M3_z <- with_progress(fit_models(modelvars_split, f3_z))
saveRDS(M3_z, "model-fits-split/z-by-step/growth-model-3.rds")
