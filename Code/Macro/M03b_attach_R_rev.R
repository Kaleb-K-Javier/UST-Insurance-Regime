# M03b_attach_R_rev.R — attach R_rev to THIS machine's PM_Lookups from R_rev_long.csv.
# R_rev_long.csv travels via git (small, 216 rows); PM_Lookups.rds does not (gitignored).
# Use on the server: `git pull` then run this to put R_rev into the server's PM_Lookups
# WITHOUT re-running the M01->M02->M03 chain (whose raw inputs are local-only).
# Adds ONE element; every pre-existing PM_Lookups element left byte-identical.
suppressPackageStartupMessages({ library(data.table); library(here) })

long <- fread(here::here("Data", "Macro", "R_rev_long.csv"))
stopifnot(all(c("G", "g", "era", "R") %in% names(long)))
ERAS <- c("2006", "2014", "2019")
stopifnot(setequal(as.character(unique(long$era)), ERAS), setequal(sort(unique(long$G)), 1:4))
gs <- sort(unique(long$g))

R_rev <- array(NA_real_, dim = c(4L, length(gs), 3L),
               dimnames = list(G = as.character(1:4), g = gs, era = ERAS))
R_rev[cbind(match(as.character(long$G), dimnames(R_rev)[[1]]),
            match(long$g,                dimnames(R_rev)[[2]]),
            match(as.character(long$era), dimnames(R_rev)[[3]]))] <- long$R
stopifnot(!anyNA(R_rev), all(R_rev[, setdiff(gs, c("KS", "MD")), ] > 0))

lk_path <- here::here("Output", "Estimation_Results", "PM_Lookups.rds")
lk  <- readRDS(lk_path)
pre <- lk[setdiff(names(lk), "R_rev")]
lk$R_rev <- R_rev
saveRDS(lk, lk_path)
lk2 <- readRDS(lk_path)
for (nm in names(pre)) if (!identical(pre[[nm]], lk2[[nm]])) stop(sprintf("PM_Lookups element '%s' changed — abort", nm))
cat(sprintf("R_rev attached to %s\n  dim %s | range [%.4f, %.4f] | %d existing elements byte-identical [OK]\n",
            lk_path, paste(dim(R_rev), collapse = "x"), min(R_rev), max(R_rev), length(pre)))
