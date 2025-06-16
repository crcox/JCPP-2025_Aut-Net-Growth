library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(igraph)
library(netgrowr)

# Load id key ----
d <- readRDS("./data/vsoa-autistic-nonautistic-diff.rds")
m <- readRDS("./data/cdi-metadata-preproc.rds")
adjmat_lst <- list(
    child = readRDS("./network/assocnet-child-preproc.rds"),
    childes = readRDS("./network/assocnet-childes-preproc.rds")
)
graphs <- map(adjmat_lst, ~ {
    igraph::graph_from_adjacency_matrix(.x, mode = "directed", weighted = NULL)
})

# N.B. all.equal(rowSums(adjmat_lst), degree(g, mode = "out")) == TRUE
#     Thus, a row in `adjmat_lst` represents the connection "from" the word
#     associated with the row "to" the word associated with each column.

vertex_ids <- tibble(
    vid = seq_len(vcount(graphs$child)),
    word_child = names(V(graphs$child)),
    word_childes = names(V(graphs$childes)),
    word = if_else(word_child == word_childes, word_child, NA)
)

# Check for mismatches between word_child and word_childes
filter(vertex_ids, is.na(word))

# If there are no mismatches, simplify
vertex_ids <- select(vertex_ids, vid, word)
str(m)

map_lgl(select(m, lemma, compound), ~ {all(vertex_ids$word %in% .x)})


vsoa_df <- d |>
    select(num_item_id, vsoa_nonautistic = vsoa_NA, vsoa_autistic = vsoa_ASD) |>
    left_join(m |> select(num_item_id, word = lemma), by = "num_item_id") |>
    left_join(vertex_ids, by = "word") |>
    filter(!is.na(vid)) |>
    select(vid, num_item_id, word, vsoa_nonautistic, vsoa_autistic) |>
    arrange(vid) |>
    pivot_longer(
        cols = starts_with("vsoa_"),
        names_to = c("group"),
        names_prefix = "vsoa_",
        values_to = "vsoa"
    )

vsoa_df <- vsoa_df |>
    mutate(
        by_20 = cut(vsoa, c(-Inf, seq(20, 540, by = 20), Inf), ordered_result = TRUE),
        by_40 = cut(vsoa, c(-Inf, seq(20, 540, by = 40), Inf), ordered_result = TRUE),
        by_60 = cut(vsoa, c(-Inf, seq(20, 540, by = 60), Inf), ordered_result = TRUE)
    )

vsoa_plot <- vsoa_df |>
    mutate(across(starts_with("by_"), as.character)) |>
    pivot_longer(
        cols = starts_with("by_"),
        names_to = "binsize",
        values_to = "bin"
    )

ggplot(vsoa_plot, aes(x = bin, fill = group)) +
    geom_bar(position = position_dodge()) +
    facet_wrap(~binsize, scales = "free_x")

vsoa_keys <- vsoa_df |>
    group_by(group) |>
    group_keys()

vsoa_lst <- vsoa_df |>
    group_by(group) |>
    group_split()

names(vsoa_lst) <- vsoa_keys$group

tmp <- netgrowr::growth_values(
    adjmat_lst$child,
    aoa_tbl = vsoa_lst$autistic |> select(word, by_20) |> mutate(by_20 = as.numeric(by_20)),
    growth_models = c("preferential_attachment", "lure_of_the_associates", "preferential_acquisition")
) |> as_tibble()
growthvalues <- map(vsoa_lst, function (vsoa, adjmat_lst) {
    map(adjmat_lst, function(adjmat, vsoa) {
        netgrowr::growth_values(
            adjmat,
            aoa_tbl = vsoa |> select(word, by_20) |> mutate(by_20 = as.numeric(by_20)),
            growth_models = c("preferential_attachment", "lure_of_the_associates", "preferential_acquisition")
        ) |>
            left_join(
                vsoa |> select(vocab_step = by_20) |> mutate(month = as.numeric(vocab_step)) |> distinct(),
                by = "month"
            ) |>
            left_join(
                vsoa |> select(word, vsoa, vsoa_bin = by_20) |> distinct(),
                by = "word"
            ) |>
            as_tibble()
    }, vsoa = vsoa) |>
        list_rbind(names_to = "network")
}, adjmat_lst = adjmat_lst) |>
    list_rbind(names_to = "group")

# Save growth values ----
saveRDS(
    growthvalues,
    file = "./network/growthvalues-autistic-nonautistic.rds"
)

# Save wide-form data for lexical growth modeling ----
# Specify and incorporate phonological baseline variables
phono_baseline <- m |>
    select(
        word = lemma,
        num_item_id,
        nphon,
        CHILDES_Freq,
        BiphonProb.avg,
        PNDC.avg
    ) |>
    left_join(vertex_ids, by = "word") |>
    as_tibble()

filter(phono_baseline, is.na(vid))
summary(phono_baseline)


# LONG VERSION ----
modelvars <- growthvalues |>
    filter(vocab_step != "(540, Inf]") |>
    left_join(phono_baseline, by = "word")

saveRDS(modelvars, file = "./network/modelvars-vsoa-long.rds")


# WIDE VERSION ----
# Read growth values ----
growthvalues <- readRDS(
    "./network/growthvalues-autistic-nonautistic.rds"
)

factor_with <- function(levels, labels) {
    key <- tibble(levels, labels) |> distinct() |> drop_na()
    factor(levels, key$levels, key$labels)
}

modelvars <- growthvalues |>
    tidyr::pivot_wider(
        id_cols = c("group", "word", "month", "known", "learned", "vocab_step"),
        names_from = c(model, network),
        values_from = "value"
    ) |>
    left_join(phono_baseline, by = c("word")) |>
    left_join(
        vsoa_df |>
            select(
                group,
                vid,
                word,
                vsoa,
                vsoa_bin = by_20
            ),
        by = c("group", "vid", "word")
    ) |>
    mutate(
        group = factor(group, c("autistic", "nonautistic")),
        word = factor_with(vid, word)
    ) |>
    filter(vocab_step != "(540, Inf]")


saveRDS(modelvars, file = "./network/modelvars-vsoa.rds")
