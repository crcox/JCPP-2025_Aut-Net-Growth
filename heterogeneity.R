library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)

meta <- readRDS("data/cdi-metadata.rds")
guids_to_exclude_sfx <- read_csv("data/GUIDS_to_exclude_SoundEffect_words_VSOA.csv") |> pull(subjectkey)

# !!! CRITICAL NOTE (20 May 2025) !!!
# Models were rerun, and data seems to be correct

# Note: We are aware of 65 autistic children for whom data was not
# collected/reported for the first 12 items on the CDI.
#  * Models/VSOAs associated with cluster ID 29354226 included these 65 children
#    when modeling these 12 items, which skewed the results.
#  * Models/VSOAs were rerun for these 12 items, excluding these 65 children.
#    These refit data are associated with cluster ID 29679346.
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
read_vsoa_models <- function(clust_id, proc_id, num_item_id, label) {
    readRDS(file.path(
        "results-20250520",
        "ci_bonf",
        "glm",
        sprintf("%d-%d-%03d-%s.rds", clust_id, proc_id, num_item_id, label)
    ))
}
vsoa_models <- pmap(
    models_to_load,
    read_vsoa_models,
    .progress = TRUE
)

vsoa <- readRDS("data/vsoa-autistic-nonautistic-ndar-id-fix-remodel-v2.rds")

cdi <- readRDS("data/asd_na-osg-2025-05-20.rds")

d <- left_join(
    cdi,
    vsoa |> select(group, num_item_id, vsoa)
) |> as_tibble()

dppt <- d |>
    select(subjectkey, nproduced, sex, group, interview_age) |>
    distinct() |>
    mutate(percent_rank = percent_rank(nproduced))

dppt |>
    group_by(group) |>
    summarize(r = cor(nproduced, interview_age))

dppt |>
    filter(percent_rank <= .2) |>
    ggplot(aes(x = interview_age, color = group, fill = group)) +
    geom_density()

predictions <- imap(vsoa_models, ~{
    data <- if (.y > 12) {
        cdi |> filter(num_item_id == .y)
    } else {
        cdi |> filter(num_item_id == .y, !(subjectkey %in% guids_to_exclude_sfx))
    }
    bind_cols(data, prob = predict(.x, data, type = "response"))
}, .progress = TRUE) |> list_rbind(names_to = "num_item_id") |> as_tibble()

loglik <- function(p, y) {
    sum((y * log(p)) + (!y * log(1 - p)))
}

likelihood <- function(p, y) {
    mean((y * p) + (!y * (1 - p)))
}


q <- d |>
    left_join(predictions) |>
    mutate(a = log(prob), b = log(1 - prob), a = a * produced, b = b * !produced) |>
    group_by(subjectkey, nproduced, sex, group, interview_age) |>
    summarize(
        loglik = mean(a + b),
        likelihood = likelihood(p = prob, y = produced),
        mprob_yes = mean((prob[ produced]), na.rm = TRUE),
        mprob_no  = mean((prob[!produced]), na.rm = TRUE),
        prop = mean( produced == (vsoa <= nproduced)),
        tpr  = mean( produced &  (vsoa <= nproduced)),
        fpr  = mean(!produced &  (vsoa <= nproduced)),
        tnr  = mean(!produced & !(vsoa <= nproduced)),
        fnr  = mean( produced & !(vsoa <= nproduced))
    ) |>
    mutate(
        adjacc = (tpr + tnr) / 2,
        dprime = qnorm(tpr) - qnorm(fpr)
    ) |>
    ungroup()

q |>
    group_by(group) |>
    summarize(
        y = mean(likelihood, na.rm = TRUE),
        se = sd(likelihood, na.rm = TRUE) / sqrt(n()),
        ci = se * qt(0.025, n() - 1),
        ymin = y - se,
        ymax = y + se
    ) |>
    ggplot(aes(x = group, y = y)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
    ylab("likelihood")

t.test(likelihood ~ group, data = q)

q |>
    group_by(group) |>
    summarize(
        y = mean(loglik, na.rm = TRUE),
        se = sd(loglik, na.rm = TRUE) / sqrt(n()),
        ci = se * qt(0.025, n() - 1),
        ymin = y - se,
        ymax = y + se
    ) |>
    ggplot(aes(x = group, y = y)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
    ylab("log likelihood")

t.test(loglik ~ group, data = q)

q |>
    group_by(group) |>
    summarize(
        y = mean(adjacc),
        se = sd(adjacc) / sqrt(n()),
        ci = se * qt(0.025, n() - 1),
        ymin = y - se,
        ymax = y + se
    ) |>
    ggplot(aes(x = group, y = y)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
    ylab("adjusted accuracy: (TPR + TNR) / 2")

t.test(fpr ~ group, data = q)
