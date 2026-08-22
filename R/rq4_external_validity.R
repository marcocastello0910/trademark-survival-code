suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readxl)
  library(ggplot2); library(knitr)
})

ROOT <- "/Users/marco/Desktop/Tesi magistrale"
OUT  <- file.path(ROOT, "output")
EXT  <- file.path(ROOT, "data_external")
ALP  <- file.path(EXT, "ALP_Trademark_v3", "NAICS_2007")
H     <- 8
YEARS <- 2008:2015

# ---- 1. trademark side: raw count and survival weight per Nice class --------
d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(cancelled = as.numeric(cancelled) == 1, prim_num = as.numeric(prim_num),
         age_obs = as.numeric(as.Date("2024-03-31") - registration_dt) / 365.25) |>
  filter(age_obs >= H) |>
  mutate(surv = as.integer(!(cancelled & t_years <= H)))
cls <- d |> group_by(nice = prim_num) |>
  summarise(N_raw = n(), S = mean(surv), .groups = "drop") |>
  mutate(Sector = ifelse(nice >= 35, "Services", "Goods"), adj = N_raw * S)

# ---- 2. ALP backward concordances (industry -> Nice), by level --------------
read_cw <- function(level) {
  f <- file.path(ALP, sprintf("NAICS_07_%d_to_nice.txt", level))
  read.csv(f, colClasses = "character") |>
    transmute(naics = .data[[sprintf("NAICS_07_%d", level)]],
              nice = as.numeric(nice), w = as.numeric(probability_weight))
}
CW <- list(`2` = read_cw(2), `3` = read_cw(3))

alloc_weights <- function(codes, level) {
  k <- length(codes)
  CW[[as.character(level)]] |> filter(naics %in% codes) |>
    group_by(nice) |> summarise(w = sum(w) / k, .groups = "drop")
}

# ---- 3. BERD reader ---------------------------------------------------------
# dash normaliser via CODE POINTS (\u escapes are ASCII in source, so encoding-safe):
# ‐-― = hyphen..horizontal-bar, − = minus ;  / /  = nbsp
dash <- function(x) {
  x <- enc2utf8(trimws(as.character(x)))
  x <- gsub("(*UTF)[\\x{2010}-\\x{2015}\\x{2212}]", "-", x, perl = TRUE)  # any dash -> hyphen
  gsub("(*UTF)[\\x{00a0}\\x{2007}\\x{202f}]", " ", x, perl = TRUE)         # nbsp -> space
}
read_berd <- function(file, years) {
  b   <- suppressMessages(read_excel(file.path(EXT, file), col_names = FALSE))
  hdr <- sub("\\.0+$", "", dash(unlist(b[4, ], use.names = FALSE)))
  yc  <- match(as.character(years), hdr); yc <- yc[!is.na(yc)]
  vals <- sapply(yc, function(j) suppressWarnings(as.numeric(gsub(",", "", dash(b[[j]])))))
  tibble(code = sub("\\.0+$", "", dash(b[[2]])), rd = rowMeans(vals, na.rm = TRUE)) |>
    filter(!is.na(code), code != "NA", !is.nan(rd), !is.na(rd))
}

# ---- 4. non-overlapping BERD partition, with the ALP level to use -----------
part <- tribble(
  ~code,      ~level, ~codes,
  "311", 3, "311",       "312", 3, "312",   "313-16", 3, "313,314,315,316",
  "321", 3, "321",       "322", 3, "322",   "323", 3, "323",
  "324", 3, "324",       "325", 3, "325",   "326", 3, "326",
  "327", 3, "327",       "331", 3, "331",   "332", 3, "332",
  "333", 3, "333",       "334", 3, "334",   "335", 3, "335",
  "336", 3, "336",       "337", 3, "337",   "339", 3, "339",
  "21",  2, "21",        "22",  2, "22",    "42",  2, "42",
  "48-49", 2, "48,49",
  "511", 3, "511",       "517", 3, "517",   "518", 3, "518",
  "other 51", 3, "512,515,519",
  "52", 2, "52",         "53", 2, "53",     "54", 2, "54",
  "621-23", 3, "621,622,623")

# ---- 5. allocate R&D into Nice space ---------------------------------------
rd_to_nice <- function(berd) {
  p <- part |> inner_join(berd, by = "code") |> filter(rd > 0)
  out <- lapply(seq_len(nrow(p)), function(i) {
    alloc_weights(strsplit(p$codes[i], ",")[[1]], p$level[i]) |>
      mutate(rd_alloc = w * p$rd[i]) |> select(nice, rd_alloc)
  })
  list(panel = bind_rows(out) |> group_by(nice) |> summarise(RD = sum(rd_alloc), .groups = "drop"),
       nrows = nrow(p), total = sum(p$rd), matched = p$code)
}
dom <- rd_to_nice(read_berd("nsf25354-tab058.xlsx", YEARS))
cls_dom <- cls |> left_join(dom$panel, by = "nice") |> filter(!is.na(RD), RD > 0)

cat(sprintf("R&D source: NSF BERD Table 58 (DOMESTIC), mean %d-%d\n", min(YEARS), max(YEARS)))
cat(sprintf("BERD rows matched: %d of %d target rows | $%.0fM allocated\n",
            length(dom$matched), nrow(part), dom$total))
cat("Unmatched target rows:", paste(setdiff(part$code, dom$matched), collapse = ", "), "\n")
cat(sprintf("Nice classes with R&D allocated: %d of 45 (%d goods, %d services)\n\n",
            nrow(cls_dom), sum(cls_dom$Sector == "Goods"), sum(cls_dom$Sector == "Services")))

# ---- 6. THE TESTS (class level) --------------------------------------------
ctest <- function(df, lab) {
  if (nrow(df) < 4) return(tibble(Sample = lab, n = nrow(df), sp_raw = NA, sp_adj = NA, d_sp = NA,
                                  lp_raw = NA, lp_adj = NA, d_lp = NA))
  tibble(Sample = lab, n = nrow(df),
         sp_raw = cor(df$N_raw, df$RD, method = "spearman"),
         sp_adj = cor(df$adj,   df$RD, method = "spearman"),
         d_sp   = cor(df$adj, df$RD, method = "spearman") - cor(df$N_raw, df$RD, method = "spearman"),
         lp_raw = cor(log(df$N_raw), log(df$RD)),
         lp_adj = cor(log(df$adj),   log(df$RD)),
         d_lp   = cor(log(df$adj), log(df$RD)) - cor(log(df$N_raw), log(df$RD)))
}
run_tests <- function(p) bind_rows(
  ctest(p, "All classes"),
  ctest(filter(p, Sector == "Goods"), "Goods classes only"),
  ctest(filter(p, Sector == "Services"), "Services classes only")) |>
  mutate(across(where(is.numeric), ~round(., 3)))

res <- run_tests(cls_dom)
cat("=== Correlation with allocated R&D (sp = Spearman, lp = log-Pearson) ===\n")
print(as.data.frame(res), row.names = FALSE)

mech <- bind_rows(
  tibble(Sample = "All classes", n = nrow(cls_dom),
         rho_S_RD = cor(cls_dom$S, cls_dom$RD, method = "spearman")),
  tibble(Sample = "Goods classes only", n = sum(cls_dom$Sector == "Goods"),
         rho_S_RD = cor(filter(cls_dom, Sector == "Goods")$S,
                        filter(cls_dom, Sector == "Goods")$RD, method = "spearman")),
  tibble(Sample = "Services classes only", n = sum(cls_dom$Sector == "Services"),
         rho_S_RD = cor(filter(cls_dom, Sector == "Services")$S,
                        filter(cls_dom, Sector == "Services")$RD, method = "spearman"))) |>
  mutate(rho_S_RD = round(rho_S_RD, 3))
cat("\n=== Mechanism: class durability S vs allocated R&D (Spearman) ===\n")
print(as.data.frame(mech), row.names = FALSE)

# ---- 7. ROBUSTNESS: worldwide R&D (Table 57) -------------------------------
ww  <- rd_to_nice(read_berd("nsf25354-tab057.xlsx", 2009:2015))
res_ww <- run_tests(cls |> left_join(ww$panel, by = "nice") |> filter(!is.na(RD), RD > 0))
cat("\n=== ROBUSTNESS: worldwide R&D (Table 57, mean 2009-2015) ===\n")
print(as.data.frame(res_ww), row.names = FALSE)

# ---- 8. FIGURE + TABLES -----------------------------------------------------
NICE <- c("Chemicals","Paints","Cosmetics","Fuels","Pharmaceuticals","Metal goods","Machinery",
  "Hand tools","Electrical & scientific","Medical apparatus","Appliances","Vehicles","Firearms",
  "Jewelry","Musical instr.","Paper & printed","Rubber & plastics","Leather","Building mat.",
  "Furniture","Housewares","Ropes & textiles","Yarns","Fabrics","Clothing","Lace & notions",
  "Floor coverings","Toys & sport","Meat & foods","Staple foods","Agriculture","Beers & drinks",
  "Alcoholic bev.","Tobacco","Advertising & business","Insurance & financial","Construction",
  "Telecommunications","Transport","Material treatment","Education & entertain.",
  "Science & technology","Food & drink svc","Medical & agri svc","Legal & security svc")
pl <- cls_dom |> mutate(label = sprintf("%02d %s", nice, NICE[nice])) |>
  select(label, Sector, RD, `Raw count` = N_raw, `Survival-adjusted` = adj) |>
  pivot_longer(c(`Raw count`, `Survival-adjusted`), names_to = "Indicator", values_to = "count") |>
  mutate(Indicator = factor(Indicator, levels = c("Raw count", "Survival-adjusted")))
fig <- ggplot(pl, aes(count, RD, color = Sector)) +
  geom_point(size = 2.2, alpha = .85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = .7, color = "grey40") +
  facet_wrap(~Indicator) +
  scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("Goods" = "#185FA5", "Services" = "#D85A30"), name = NULL) +
  labs(title = "External validity: trademark indicator vs industry R&D",
       subtitle = "Each point = a Nice class; R&D allocated from NSF BERD (domestic, mean 2008-2015). Log scales.",
       x = "Trademarks in the class", y = "Allocated business R&D, $ millions") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title.position = "plot",
        plot.subtitle = element_text(margin = margin(b = 8)),
        plot.margin = margin(8, 14, 8, 8))
ggsave(file.path(OUT, "fig_rq4_external.png"), fig, width = 10, height = 5.2, dpi = 300)
ggsave(file.path(OUT, "fig_rq4_external.pdf"), fig, width = 10, height = 5.2)

tabP <- cls_dom |> mutate(Class = sprintf("%02d %s", nice, NICE[nice])) |> arrange(desc(RD)) |>
  transmute(Class, Sector, `Allocated R&D ($M)` = round(RD), `Raw count` = N_raw,
            `Adjusted count` = round(adj), `Durability S` = round(S, 3))
write.csv(tabP, file.path(OUT, "tab_rq4_external_panel.csv"), row.names = FALSE)
write.csv(res,  file.path(OUT, "tab_rq4_external_corr.csv"),  row.names = FALSE)
css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
        th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
        caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
  knitr::kable(res,    format = "html", caption = "Correlation with allocated R&D (domestic, mean 2008-2015): raw vs survival-adjusted"),
  knitr::kable(res_ww, format = "html", caption = "Robustness: worldwide R&D (mean 2009-2015)"),
  knitr::kable(mech,   format = "html", caption = "Mechanism: class durability vs allocated R&D"),
  knitr::kable(tabP,   format = "html", caption = "Nice class panel: trademark counts and allocated R&D"),
  "</body></html>"), file.path(OUT, "risultati_rq4_external.html"))