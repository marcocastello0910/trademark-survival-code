suppressPackageStartupMessages({ library(haven); library(dplyr) })

ROOT <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT  <- file.path(ROOT, "output")

# rows of the raw case file, read from the .dta header rather than by loading
# 2.4 GB: format 118 stores the observation count as 8 little-endian bytes
dta_nobs <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  h <- readBin(con, "raw", 512L)
  i <- grepRaw(charToRaw("<N>"), h, fixed = TRUE)
  sum(as.numeric(h[(i + 3):(i + 10)]) * 256^(0:7))
}
raw_filings <- dta_nobs(file.path(ROOT, "case_file.dta"))

d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  mutate(cancelled = as.numeric(cancelled) == 1,
         is_service = as.numeric(is_service),
         scheme = as_factor(svc_scheme))

cls <- d |> filter(is_service %in% c(0, 1))

# svc_scheme miscodes one mark (serial 72067259, class 37), so the pure counts
# come from the cross-tabulation with is_service rather than from the label.
pure_g <- sum(d$scheme == "Pure goods"    & d$is_service == 0, na.rm = TRUE)
pure_s <- sum(d$scheme == "Pure services" | (d$scheme == "Pure goods" & d$is_service == 1), na.rm = TRUE)
mixed  <- sum(d$scheme == "Mixed", na.rm = TRUE)
stopifnot(pure_g + mixed == sum(d$is_service == 0, na.rm = TRUE),
          pure_s          == sum(d$is_service == 1, na.rm = TRUE))
tab <- tibble::tibble(
  Quantity = c("Filings in the raw case file",
               "Marks in the analysis sample",
               "Pure goods", "Pure services", "Mixed", "Unclassified",
               "Marks with a defined goods/services status",
               "Of those, cancelled by the cut-off"),
  Count = c(raw_filings,
            nrow(d),
            pure_g,
            pure_s,
            mixed,
            sum(is.na(d$scheme)),
            nrow(cls),
            sum(cls$cancelled)))
tab$`% of classified` <- ifelse(tab$Quantity %in% c("Pure goods","Pure services","Mixed"),
                                sprintf("%.0f", 100 * tab$Count / nrow(cls)), "")
write.csv(tab, file.path(OUT, "tab_sample_composition.csv"), row.names = FALSE)
print(as.data.frame(tab), row.names = FALSE)
