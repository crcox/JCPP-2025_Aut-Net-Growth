library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(boot)

meta <- readRDS("data/cdi-metadata.rds")

# When creating this data frame, each row must specify the cluster ID and
# process ID associated with the saved model output that should be loaded for
# each word. You will need to know what this is based on your knowledge of what
# happened on OSG. This code is only an example, and will need to be revised to
# accommodate your own cluster and process IDs.
models_to_load <- read_csv(
    "./item-id-label.csv",
    col_names = c("num_item_id", "label"),
    col_types = list(col_integer(), col_character())
) |>
    mutate(
        clust_id = 32251163,
        proc_id = num_item_id - 1
    ) |>
    relocate(clust_id, proc_id)

# Again, note that each file is distinguished by a cluster ID and a process ID.
# This, coupled with appropriate documentation, provides information about when
# and under conditions data were generated.
read_vsoa_bsci <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results",
        "ci_bonf",
        "bs_ci",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
word_cis <- pmap(
    models_to_load,
    read_vsoa_bsci,
    .progress = TRUE
)

x <- map(word_cis, function(x) {
    d <- expand_grid(
        ci_type = c("basic", "bca", "percentile"),
        variable = c("ASD-NA", "NA", "ASD")
    )
    d$ci_l <- c(
        map_dbl(x, function(ci) {ci$basic[4]}),
        map_dbl(x, function(ci) {ci$bca[4]}),
        map_dbl(x, function(ci) {ci$perc[4]})
    )
    d$ci_u <- c(
        map_dbl(x, function(ci) {ci$basic[5]}),
        map_dbl(x, function(ci) {ci$bca[5]}),
        map_dbl(x, function(ci) {ci$perc[5]})
    )
    d$diff <- rep(map_dbl(x, ~{.$t0}), 3)
    d$na <- x[[2]]$t0 # non-autistic VSOA estimate based on the true data
    d$asd <- x[[3]]$t0 # autistic VSOA estimate based on the true data
    return(d)
}, .progress = TRUE)

names(x) <- models_to_load |>
    left_join(select(meta, num_item_id, word), by = "num_item_id") |>
    pull(word)

df_vsoa <- x |>
    bind_rows(.id = "word") |>
    left_join(select(meta, word, num_item_id), by = "word") |>
    relocate(num_item_id) |>
    mutate(
        num_item_id = as.integer(num_item_id),
        word = factor(num_item_id, meta$num_item_id, meta$word),
        ci_type = factor(ci_type, c("basic", "bca", "percentile")),
        group = factor(variable, c("ASD-NA", "NA", "ASD"))
    ) |>
    select(-na, -asd, -variable) |>
    rename(vsoa = diff) |>
    relocate(group, vsoa, .after = word) |>
    arrange(num_item_id, group, ci_type) |>
    filter(ci_type == "bca")

df_vsoa_diff <- df_vsoa |>
    pivot_wider(
        id_cols = c(num_item_id, word, ci_type),
        names_from = group,
        values_from = c(vsoa, ci_l, ci_u)
    ) |>
    rename_with(~ sub("ASD-NA", "diff", .x), ends_with("ASD-NA")) |>
    select(-ci_l_ASD, -ci_u_ASD, -ci_l_NA, -ci_u_NA)


saveRDS(df_vsoa, "data/vsoa-autistic-nonautistic.rds")
saveRDS(df_vsoa_diff, "data/vsoa-autistic-nonautistic-diff.rds")


## Export a spreadsheet ----
write_csv(df_vsoa_diff, file = "data/vsoa-autistic-nonautistic-diff.csv")
