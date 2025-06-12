library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(netgrowr)

# Helper functions ----
gvals_over_baseline <- function(gvals, baseline) {
    map2(gvals, baseline, \(m_full_list, m_reduced) {
        map_depth(m_full_list, 2, \(m_full, m_redux) {
            model_comparison(m_full, m_redux) |>
                as.list() |>
                as_tibble()
        }, m_redux = m_reduced) |>
            map(~ list_rbind(.x, names_to = "contrast")) |>
            list_rbind(names_to = "network")
    }) |>
        list_rbind(names_to = "group") |>
        mutate(contrast = paste0(contrast, ">base"))
}

gvals_over_gvals <- function(full, reduced) {
    map2(full, reduced, ~ {
        map2(.x, .y, ~ {
            bind_rows(
                `pac>loa` = model_comparison(.x$loa_pac, .y$loa),
                `loa>pac` = model_comparison(.x$loa_pac, .y$pac),
                .id = "contrast"
            )
        }) |> list_rbind(names_to = "network")
    }) |> list_rbind(names_to = "group")
}

net_over_net <- function(x) {
    map(x, ~ {
        bind_rows(
            bind_rows(
                `assoc>childes` = model_comparison(.x$both$pat, .x$childes$pat),
                `childes>assoc` = model_comparison(.x$both$pat, .x$assoc$pat),
                .id = "contrast"
            ) |> mutate(gval = "pat"),
            bind_rows(
                `assoc>childes` = model_comparison(.x$both$loa, .x$childes$loa),
                `childes>assoc` = model_comparison(.x$both$loa, .x$assoc$loa),
                .id = "contrast"
            ) |> mutate(gval = "loa"),
            bind_rows(
                `assoc>childes` = model_comparison(.x$both$pac, .x$childes$pac),
                `childes>assoc` = model_comparison(.x$both$pac, .x$assoc$pac),
                .id = "contrast"
            ) |> mutate(gval = "pac")
        )
    }) |> list_rbind(names_to = "group")
}

ME <- readRDS("model-fits-split-540max/empty-model.rds")
M0 <- readRDS("model-fits-split-540max/baseline-model.rds")
M0_nofreq <- readRDS("model-fits-split-540max/baseline-model-nofreq.rds")
M1_Z <- readRDS("model-fits-split-540max/z-overall/growth-model-1.rds")
M2_Z <- readRDS("model-fits-split-540max/z-overall/growth-model-2.rds")
M1_Z_nofreq <- readRDS("model-fits-split-540max/z-overall/growth-model-1-nofreq.rds")
M2_Z_nofreq <- readRDS("model-fits-split-540max/z-overall/growth-model-2-nofreq.rds")

# Baseline over empty ----
df_base_empty <- map2(M0, ME, ~ {
    model_comparison(.x, .y) |>
        as.list() |>
        as_tibble()
}) |> list_rbind(names_to = "group") |>
    mutate(contrast = "base>empty") |>
    relocate(contrast, .after = group)


# 1 GV over baseline ----
df_gv_base <- bind_rows(
    #raw = gvals_over_baseline(M1_r, M0),
    z_overall = gvals_over_baseline(M1_Z, M0),
    #z_by_step = gvals_over_baseline(M1_z, M0),
    .id = "scale"
)

df_gv_base_nofreq <- bind_rows(
    #raw = gvals_over_baseline(M1_r, M0),
    z_overall = gvals_over_baseline(M1_Z_nofreq, M0_nofreq),
    #z_by_step = gvals_over_baseline(M1_z, M0),
    .id = "scale"
)

df_gv_base |>
    filter(
        network == "both",
        scale == "z_overall"
    )

df_gv_base |>
    filter(
        p < .05,
        network == "both",
        scale == "z_by_step"
    )


# Net over Net ----
df_net_net <- bind_rows(
    z_overall = net_over_net(M1_Z)
)

df_net_net_nofreq <- bind_rows(
    z_overall = net_over_net(M1_Z_nofreq)
)

# GV over GV ----
df_gv_gv <- bind_rows(
    #raw = gvals_over_gvals(M2_r, M1_r),
    z_overall = gvals_over_gvals(M2_Z, M1_Z),
    #z_by_step = gvals_over_gvals(M2_z, M1_z),
    .id = "scale"
)

df_gv_gv_nofreq <- bind_rows(
    #raw = gvals_over_gvals(M2_r, M1_r),
    z_overall = gvals_over_gvals(M2_Z_nofreq, M1_Z_nofreq),
    #z_by_step = gvals_over_gvals(M2_z, M1_z),
    .id = "scale"
)

bind_rows(
    yes = df_gv_base,
    yes = df_gv_gv,
    yes = df_net_net,
    no = df_gv_base_nofreq,
    no = df_gv_gv_nofreq,
    no = df_net_net_nofreq,
    .id = "freq"
) |>
    readr::write_csv("model-comparisons-20250520-540max.csv")

d_coefs <- bind_rows(
    autistic  = bind_cols(gvals = "none", M0$asd |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "none", M0$td |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "assoc.", M1_Z$asd$assoc$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "assoc.", M1_Z$td$assoc$pac |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "CHILDES", M1_Z$asd$childes$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "CHILDES", M1_Z$td$childes$pac |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "both", M1_Z$asd$both$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "both", M1_Z$td$both$pac |> coef() |> as.list() |> as_tibble()),
    .id = "group"
)

p <- d_coefs |>
    pivot_longer(
        3:8,
        names_to = "param",
        values_to = "coef"
    ) |>
    mutate(
        param = factor(
            param,
            c("(intercept)", "RC1", "RC2", "RC3", "pac1_Z", "pac2_Z"),
            c("(intercept)", "RC freq", "RC biphon.", "RC PND", "Assoc.", "CHILDES"),
        ),
        gvals = factor(
            gvals,
            c("none", "assoc.", "CHILDES", "both"),
            c("none", "Assoc.", "CHILDES", "both")
        )
    ) |>
    ggplot(aes(x = param, y = coef, fill = gvals)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_wrap(vars(group)) +
        theme_bw()

ggsave(
    "coefficients.pdf",
    plot = p + theme(legend.position = "none"),
    width = 6.5,
    height = 2.5,
    units = "in",
    dpi = 300
)

d_coefs_nofreq <- bind_rows(
    autistic  = bind_cols(gvals = "none", M0_nofreq$asd |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "none", M0_nofreq$td |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "assoc.", M1_Z_nofreq$asd$assoc$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "assoc.", M1_Z_nofreq$td$assoc$pac |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "CHILDES", M1_Z_nofreq$asd$childes$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "CHILDES", M1_Z_nofreq$td$childes$pac |> coef() |> as.list() |> as_tibble()),
    autistic  = bind_cols(gvals = "both", M1_Z_nofreq$asd$both$pac |> coef() |> as.list() |> as_tibble()),
    `nonaut.` = bind_cols(gvals = "both", M1_Z_nofreq$td$both$pac |> coef() |> as.list() |> as_tibble()),
    .id = "group"
)

p <- d_coefs_nofreq |>
    pivot_longer(
        3:7,
        names_to = "param",
        values_to = "coef"
    ) |>
    mutate(
        param = factor(
            param,
            c("(intercept)", "RC1", "RC2", "RC3", "pac1_Z", "pac2_Z"),
            c("(intercept)", "RC freq", "RC biphon.", "RC PND", "Assoc.", "CHILDES"),
        ),
        gvals = factor(
            gvals,
            c("none", "assoc.", "CHILDES", "both"),
            c("none", "Assoc.", "CHILDES", "both")
        )
    ) |>
    filter(param != "(intercept)") |>
    ggplot(aes(x = param, y = coef, fill = gvals)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        facet_wrap(vars(group)) +
        theme_bw()

ggsave(
    "coefficients_nofreq.pdf",
    plot = p + theme(legend.position = "bottom"),
    width = 6.5,
    height = 2.5,
    units = "in",
    dpi = 300
)

readr::write_csv(d_coefs_nofreq, "pac-coef-assoc-childes-nofreq.csv")


bind_rows(
    autistic = M1_Z$asd$both$pac |> coef(),
    `nonaut.` = M1_Z$td$both$pac |> coef(),
    .id = "group"
)

df_gv_gv |>
    filter(
        p < .05,
        network == "both",
        scale == "z_overall"
    )

df_gv_gv |>
    filter(
        p < .05,
        network == "both",
        scale == "z_by_step"
    )

df_gv_gv |>
    filter(
        network == "both",
        scale == "raw"
    )
