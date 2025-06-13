library(dplyr)
library(readr)
source("./R/lemmatize.R")
source("./R/recode_nonwords.R")
source("./R/tokenize_phrase.R")

# Get CHILDES tokens ----
# Local version acquired 13 June 2020 using:
#    roles <- c("Adult", "Father", "Mother", "Aunt", "Uncle", "Grandmother",
#              "Grandfather", "Teacher", "Babysitter", "Nurse", "Doctor",
#              "Clinician", "Therapist")
#    childes_tokens_preproc <- childesr::get_tokens(collection = "Eng-NA", role = roles, token='*')
#    save(childes_tokens_preproc, file = './data/childes_tokens_preproc_Eng-NA_2020June13_raw.Rdata')

# Preprocess ----
cdi_metadata_preproc <- readRDS("./data/cdi-metadata-preproc.rds")
childes_tokens <- readRDS("./data/childes-tokens.rds")
lemma_dict_childes <- readr::read_csv(
    "./data/lemma-dict-childes.csv",
    col_types = cols(
        token = readr::col_character(),
        lemma = readr::col_character()
    )
)

childes_tokens_preproc <- childes_tokens |>
    filter(target_child_age <= 60) |>
    tokenize_phrase(c("so", "big"),       sort_tokens = TRUE) |>
    tokenize_phrase(c("go", "potty"),     sort_tokens = FALSE) |>
    tokenize_phrase(c("night", "night"),  sort_tokens = FALSE) |>
    tokenize_phrase(c("thank", "you"),    sort_tokens = FALSE) |>
    tokenize_phrase(c("all", "gone"),     sort_tokens = FALSE) |>
    tokenize_phrase(c("next", "to"),      sort_tokens = FALSE) |>
    tokenize_phrase(c("on", "top", "of"), sort_tokens = FALSE) |>
    tokenize_phrase(c("a", "lot"),        sort_tokens = FALSE) |>
    tokenize_phrase(c("going", "to"),     sort_tokens = FALSE) |>
    tokenize_phrase(c("got", "to"),       sort_tokens = FALSE) |>
    tokenize_phrase(c("have", "to"),      sort_tokens = FALSE) |>
    tokenize_phrase(c("let", "me"),       sort_tokens = FALSE) |>
    tokenize_phrase(c("want", "to"),      sort_tokens = FALSE) |>
    tokenize_phrase(c("did", "ya"), phrase_token = "did+you", sort_tokens = FALSE) |>
    tokenize_phrase(c("did", "you"),      sort_tokens = FALSE) |>
    tokenize_phrase(c("need", "to"),      sort_tokens = FALSE) |>
    tokenize_phrase(c("try", "to"),       sort_tokens = FALSE) |>
    tokenize_phrase(c("play", "pen"),     sort_tokens = FALSE) |>
    dplyr::mutate(
        lemma = gloss |>
            lemmatize(dict = lemma_dict_childes) |>
            recode_nonwords(part_of_speech)
    ) |>
    dplyr::filter(!is.na(lemma))

# Extract information about target children ----
childes_children <- childes_tokens_preproc |>
    dplyr::distinct(
        target_child_id,
        target_child_age,
        target_child_name,
        target_child_sex
    )
cat("Summary of target child age in CHILDES selection\n")
cat("------------------------------------------------\n")
print(summary(childes_children$target_child_age))
stem(childes_children$target_child_age)

# Confirm that all CDI word lemmas appear among CHILDES tokens ----
all_lemmas_appear_in_childes <- all(cdi_metadata_preproc$lemma %in% childes_tokens_preproc$lemma)
stopifnot(all_lemmas_appear_in_childes)
cat("All study lemmas occur in CHILDES.")

# Arrange tokens by utterances within transcripts ----
childes_tokens_preproc <- childes_tokens_preproc |>
    dplyr::arrange(
        transcript_id,
        utterance_id,
        token_order
    )

# Save processed CHILDES tokens ----
saveRDS(childes_tokens_preproc, file = './data/childes-tokens-preproc.rds')
saveRDS(childes_children, file = './data/childes-children.rds')
