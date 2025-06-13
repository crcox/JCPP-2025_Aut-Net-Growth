library(dplyr)
library(purrr)
library(tidyr)
library(netbuildr)
# netbuildr can be obtained from https://github.com/crcox/netbuildr
# install with:
#     remotes::install_github("crcox/netbuilder")

trim_prefix <- function(x, prefix) {
    return(trimws(x, which = "left", whitespace = prefix))
}

add_prefix <- function(x, prefix) {
    str_prepend <- function(prefix, ...) paste(prefix, ..., sep = "")
    return(vapply(x, FUN = str_prepend, FUN.VALUE = character(1), prefix = prefix, USE.NAMES = FALSE))
}

add_suffix <- function(x, suffix) {
    str_append <- function(suffix, ...) paste(..., suffix, sep = "")
    return(vapply(x, FUN = str_append, FUN.VALUE = character(1), suffix = suffix, USE.NAMES = FALSE))
}

load_to_list <- function(files) {
    X <- new.env()
    lapply(files, load, envir = X)
    return(as.list(X))
}

# Load metadata ----
cdi_metadata_preproc <- readRDS("data/cdi-metadata-preproc.rds")

# Load processed word associations ----
associations <- list(
  adult = readRDS("./data/associations-adult-preproc.rds"),
  child = readRDS("./data/associations-child-preproc.rds")
)

# Create associative networks ----
assocnet <- associations |>
    map(\(.x, ix) {
        netbuildr::create_unweighted_network(.x$lemma, .x$RESPONSE)[ix, ix]
    }, ix = cdi_metadata_preproc$lemma)

# Save association networks ----
if (!dir.exists("./network")) dir.create("./network")
iwalk(assocnet, ~ saveRDS(.x, file.path("network", paste("assocnet", .y, "preproc.rds", sep = "-"))))

