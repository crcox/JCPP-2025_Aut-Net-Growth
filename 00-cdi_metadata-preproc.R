cdi_metadata <- readRDS("data/cdi-metadata.rds")

# Exclude CDI words not included in study ----
cdi_metadata_preproc <- subset(x = cdi_metadata, cdi_metadata[["in_CoxHae"]])
cdi_metadata_preproc <- subset(x = cdi_metadata_preproc, cdi_metadata_preproc[["word"]] != "so big!")
cdi_metadata_preproc <- subset(x = cdi_metadata_preproc, cdi_metadata_preproc[["word"]] != "pet's name")
cdi_metadata_preproc <- subset(x = cdi_metadata_preproc, cdi_metadata_preproc[["word"]] != "hers")

# Exclude homonyms ----
## By stripping qualifiers and then removing all instances of any word that is
## duplicated, we are excluding homonyms from our cue set.
find_homonyms <- function(words) {
  strip_qualifiers <- function(x) {
    y <- gsub(" \\(.*\\)", "", x)
    return(gsub("\\*", "", y))
  }
  y <- strip_qualifiers(words)
  return(return(y %in% y[duplicated(y)]))
}
homonyms <- find_homonyms(cdi_metadata_preproc$word)
cdi_metadata_preproc <- subset(cdi_metadata_preproc, !homonyms)


# Save processed CDI metadata ----
saveRDS(cdi_metadata_preproc, file = "./data/cdi-metadata-preproc.rds")

