library(dplyr)
library(purrr)
library(netbuildr)

# Load CHILDES tokens and CDI metadata ----
childes_tokens_preproc <- readRDS("./data/childes-tokens-preproc.rds")
cdi_metadata_preproc <- readRDS("./data/cdi-metadata-preproc.rds")

# Compute cooccurrence matrix with window size 5 ----
# N.B Riordan and Jones (2007) Cog. Sci. used windows size 10
k <- 5L # Hills et al. (2010) J. Mem. Lang.

# Ensure tokens are ordered and blocked by ordered utterances within
# transcripts
childes_tokens_preproc <- childes_tokens_preproc |>
    dplyr::arrange(
        transcript_id,
        utterance_id,
        token_order
    )

transcripts <- childes_tokens_preproc |>
    group_by(transcript_id) |>
    group_split() |>
    map(~ .x$lemma)

cooccurrences <- netbuildr::create_cooccurrence_matrix(
  tokens = transcripts,
  window_size = k,
  types = cdi_metadata_preproc$lemma
)

# Save network variants ----
# UNWEIGHTED
assocnet_childes_preproc <- cooccurrences > 0

if (!dir.exists("./network")) dir.create("./network")
saveRDS(assocnet_childes_preproc, file = "./network/assocnet-childes-preproc.rds")
