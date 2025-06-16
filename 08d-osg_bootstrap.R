#!/usr/bin/env Rscript --vanilla --default-packages=methods,utils,stats,graphics

args <- commandArgs(trailingOnly = TRUE)

num_item_id <- as.integer(args[1])
niter <- as.integer(args[2])
data_path <- args[3]

guids_no_sfx <- c(
"NDARAK656UY4", "NDARAT218HWY", "NDARBE387XAU", "NDARBK488WYL", "NDARBK744FJ1",
"NDARBN364YAN", "NDARBV699RAK", "NDARCC522CZD", "NDARCJ346ALW", "NDARCM038WKM",
"NDARCN229HL4", "NDARDE601ZHU", "NDARDU664ALE", "NDARDV922MJP", "NDARDX026LMX",
"NDAREC003MFQ", "NDAREE112HYT", "NDARER059XZ4", "NDAREY594LDB", "NDARGJ326MNW",
"NDARJM897HH8", "NDARKE927LAD", "NDARKM810AM0", "NDARKN596MZK", "NDARLK074GU7",
"NDARLL416NBU", "NDARLV040PD8", "NDARLX581UAG", "NDARNA314BML", "NDARPE717FYP",
"NDARPL472LAB", "NDARRE163EMK", "NDARRG591WR3", "NDARRJ846ARN", "NDARRX042BGL",
"NDARTP065WRK", "NDARTU179NLD", "NDARTW493CNY", "NDARUG689YRF", "NDARUU205AHJ",
"NDARUX548PCB", "NDARVG360KNM", "NDARVH166ABT", "NDARVL353XDE", "NDARVV577EVG",
"NDARVZ694RPV", "NDARWF484TXC", "NDARWH387TY5", "NDARWM737WP6", "NDARXB600EZM",
"NDARXE128EUU", "NDARXF141JUR", "NDARXH065NFP", "NDARXU289BV6", "NDARXV501DRX",
"NDARYA737JZ1", "NDARYA964GGT", "NDARZD796LK2", "NDARZD900MDB", "NDARZF743PZK",
"NDARZG105NYF", "NDARZH949LUL", "NDARZU324ENK", "NDARZU928VZZ", "NDARZV785BAB"
)

print(str(list(num_item_id, niter, data_path)))

library(boot)

decision_boundary <- function(m, crit_p = .5) {
    L <- log(crit_p / (1 - crit_p))
    g <- contrasts(model.frame(m)$group)[, 1]
    b <- coef(m)
    return(-(b["(Intercept)"] + (b["groupASD"] * g) - L) / (b["nproduced"] + (b["nproduced:groupASD"] * g)))
}

bfun <- function(x, ix) {
    m <- glm(
        produced ~ nproduced * group,
        data = x[ix, ],
        family = "binomial"
    )
    d <- decision_boundary(m)
    d <- c("ASD-NA" = diff(d), "NA" = d[1], "ASD" = d[2])
    return(d)
}

d <- readRDS(data_path)
d <- d[d$num_item_id == num_item_id, ]

if (num_item_id < 13) {
   cat("The following records had no data input for the first 12 CDI items (sound effects. This is a sound effect word, so they are being excluding from the model.\n")
   print(guids_no_sfx)
   z <- d$subjectkey %in% guids_no_sfx  
   d <- d[!z, ]
}

str(d)

m <- glm(produced ~ nproduced * group, data = d, family = "binomial")

summary(m)

bs <- boot(d, bfun, R = niter, strata = d$group)

str(bs)

bs_ci <- lapply(1:3, function(i) {
    boot.ci(bs, conf = 1 - (0.05 / 680), type = c("basic", "perc", "bca"), index = i)
})

print(bs_ci)

saveRDS(m, file = sprintf("%03d_glm.rds", num_item_id))
saveRDS(bs, file = sprintf("%03d_bs.rds", num_item_id))
saveRDS(bs_ci, file = sprintf("%03d_bs_ci.rds", num_item_id))
