meta <- readRDS("./data/cdi-metadata.rds")

d <- readRDS("data/asd_na-osg-2025-05-20.rds")
new_guids <- readRDS("data/autistic-guis-added-20250507.rds")
old_guids <- d |> filter(group == "ASD") |> pull(subjectkey) |> setdiff(new_guids)

autistic_guid_key <- list(
    old = tibble(subjectkey = old_guids),
    new = tibble(subjectkey = new_guids)
) |>
    list_rbind(names_to = "dataset") |>
    mutate(dataset = factor(dataset, c("old", "new", "combined")))
d <- d |>
    as_tibble() |>
    left_join(autistic_guid_key) |>
    left_join(select(meta, num_item_id, word), by = "num_item_id")


d <- bind_rows(
    d, mutate(d, dataset = "combined")
) |>
    mutate(dataset = factor(dataset, c("old", "new", "combined")))



id_selection <- c(357, 589, 379, 367, 436, 440, 565, 113, 155, 590, 679, 58)

d |>
    filter(
        group == "ASD",
        num_item_id %in% id_selection
    ) |>
    mutate(
        bin = cut(nproduced, breaks = seq(0, 280, by = 20))
    ) |>
    group_by(group, dataset, bin, word) |>
    summarize(
        p = sum(produced) / length(produced)
    ) |>
    ggplot(aes(x = bin, y = p, fill = dataset)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap(~word) +
    xlab("Number of words produced, 20 word bins from 0 to 680") +
    theme(axis.text.x = element_blank())
