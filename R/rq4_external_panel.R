suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readxl)
  library(ggplot2); library(knitr); library(sandwich)
})
ROOT <- Sys.getenv("THESIS_ROOT", unset = "."); OUT <- file.path(ROOT, "output")
EXT  <- file.path(ROOT, "data_external"); ALP <- file.path(EXT, "ALP_Trademark_v3", "NAICS_2007")
YEARS <- 2008:2015; H <- 8; MINCELL <- 50; set.seed(1)

dash <- function(x){ x <- enc2utf8(trimws(as.character(x)))
  x <- gsub("(*UTF)[\\x{2010}-\\x{2015}\\x{2212}]", "-", x, perl = TRUE)
  gsub("(*UTF)[\\x{00a0}\\x{2007}\\x{202f}]", " ", x, perl = TRUE) }
read_cw <- function(level) read.csv(file.path(ALP, sprintf("NAICS_07_%d_to_nice.txt", level)), colClasses = "character") |>
  transmute(naics = .data[[sprintf("NAICS_07_%d", level)]], nice = as.numeric(nice), w = as.numeric(probability_weight))
CW <- list(`2` = read_cw(2), `3` = read_cw(3))
alloc_weights <- function(codes, level){ k <- length(codes)
  CW[[as.character(level)]] |> filter(naics %in% codes) |> group_by(nice) |> summarise(w = sum(w)/k, .groups = "drop") }
part <- tribble(~code,~level,~codes,
  "311",3,"311","312",3,"312","313-16",3,"313,314,315,316","321",3,"321","322",3,"322","323",3,"323",
  "324",3,"324","325",3,"325","326",3,"326","327",3,"327","331",3,"331","332",3,"332","333",3,"333",
  "334",3,"334","335",3,"335","336",3,"336","337",3,"337","339",3,"339","21",2,"21","22",2,"22",
  "42",2,"42","48-49",2,"48,49","511",3,"511","517",3,"517","518",3,"518","other 51",3,"512,515,519",
  "52",2,"52","53",2,"53","54",2,"54","621-23",3,"621,622,623")

# ---- R&D allocated to Nice, per year ---------------------------------------
rd_panel <- function(file, years){
  b <- suppressMessages(read_excel(file.path(EXT, file), col_names = FALSE))
  hdr <- sub("\\.0+$", "", dash(unlist(b[4, ], use.names = FALSE))); codes <- sub("\\.0+$", "", dash(b[[2]]))
  bind_rows(lapply(years, function(y){ j <- match(as.character(y), hdr); if (is.na(j)) return(NULL)
    bl <- tibble(code = codes, rd = suppressWarnings(as.numeric(gsub(",", "", dash(b[[j]]))))) |>
      filter(!is.na(code), code != "NA", !is.na(rd), rd > 0)
    p <- part |> inner_join(bl, by = "code") |> filter(rd > 0)
    do.call(rbind, lapply(seq_len(nrow(p)), function(i)
      alloc_weights(strsplit(p$codes[i], ",")[[1]], p$level[i]) |> mutate(rd_alloc = w * p$rd[i]))) |>
      group_by(nice) |> summarise(RD = sum(rd_alloc), .groups = "drop") |> mutate(year = y) })) }

# ---- trademark panel (class x cohort year) ---------------------------------
d <- read_dta(file.path(OUT, "tm_survival.dta")) |> filter(is_service %in% c(0, 1)) |>
  mutate(cancelled = as.numeric(cancelled) == 1, prim_num = as.numeric(prim_num), reg_year = as.numeric(reg_year),
         age_obs = as.numeric(as.Date("2024-03-31") - registration_dt) / 365.25) |>
  filter(age_obs >= H) |> mutate(surv = as.integer(!(cancelled & t_years <= H)))
tm <- d |> filter(reg_year %in% YEARS) |> group_by(nice = prim_num, year = reg_year) |>
  summarise(N_raw = n(), S = mean(surv), .groups = "drop") |> filter(N_raw >= MINCELL) |>
  mutate(adj = N_raw * S, Sector = ifelse(nice >= 35, "Services", "Goods"))

build <- function(rdfile, years) inner_join(tm, rd_panel(rdfile, years), by = c("nice", "year")) |> filter(RD > 0)
pan <- build("nsf25354-tab058.xlsx", YEARS)
cat(sprintf("PANEL: %d class-year cells | %d classes | years %s | goods %d, services %d\n\n",
    nrow(pan), n_distinct(pan$nice), paste(range(pan$year), collapse = "-"),
    sum(pan$Sector == "Goods"), sum(pan$Sector == "Services")))

# ---- (1) pooled correlation + CLASS-CLUSTER BOOTSTRAP of the delta ----------
delta <- function(df, method) {
  if (method == "spearman") cor(df$adj, df$RD, method = "spearman") - cor(df$N_raw, df$RD, method = "spearman")
  else cor(log(df$adj), log(df$RD)) - cor(log(df$N_raw), log(df$RD))
}
boot_delta <- function(df, method, B = 2000) {
  cl <- unique(df$nice)
  replicate(B, { s <- sample(cl, replace = TRUE)
    dd <- bind_rows(lapply(s, function(k) df[df$nice == k, ]))
    tryCatch(delta(dd, method), error = function(e) NA) }) |> quantile(c(.025, .975), na.rm = TRUE)
}
corr_tab <- function(df, lab) {
  tibble(Sample = lab, cells = nrow(df), classes = n_distinct(df$nice),
         sp_raw = cor(df$N_raw, df$RD, method = "spearman"), sp_adj = cor(df$adj, df$RD, method = "spearman"),
         lp_raw = cor(log(df$N_raw), log(df$RD)), lp_adj = cor(log(df$adj), log(df$RD)),
         d_sp = sp_adj - sp_raw, d_lp = lp_adj - lp_raw)
}
res <- bind_rows(corr_tab(pan, "All"), corr_tab(filter(pan, Sector=="Goods"), "Goods"),
                 corr_tab(filter(pan, Sector=="Services"), "Services")) |> mutate(across(where(is.numeric), ~round(., 3)))
ci <- bind_rows(lapply(c("All","Goods","Services"), function(s){
  df <- if (s=="All") pan else filter(pan, Sector==s)
  b <- boot_delta(df, "spearman"); tibble(Sample=s, d_sp_lo=round(b[1],3), d_sp_hi=round(b[2],3)) }))
res <- left_join(res, ci, by = "Sample")
cat("=== Pooled panel correlation with R&D (Spearman + log-Pearson), cluster-bootstrap CI on delta ===\n")
print(as.data.frame(res), row.names = FALSE)

# ---- (2) two-way FE (class + year), class-clustered SE ----------------------
fe_row <- function(df, s) {
  if (n_distinct(df$nice) < 3) return(tibble(Sample = s, b_raw = NA, b_adj = NA))
  f <- function(x) { m <- lm(reformulate(c(x, "factor(nice)", "factor(year)"), "log(RD)"), df)
    c(b = unname(coef(m)[2]), se = unname(sqrt(diag(vcovCL(m, cluster = df$nice)))[2])) }
  a <- f("log(N_raw)"); b <- f("log(adj)")
  tibble(Sample = s, b_raw = round(a["b"],3), se_raw = round(a["se"],3), b_adj = round(b["b"],3), se_adj = round(b["se"],3)) }
fe <- bind_rows(fe_row(pan,"All"), fe_row(filter(pan,Sector=="Goods"),"Goods"), fe_row(filter(pan,Sector=="Services"),"Services"))
cat("\n=== Two-way FE (class+year): coef on log(indicator), class-clustered SE ===\n")
print(as.data.frame(fe), row.names = FALSE)

# ---- (3) mechanism + worldwide robustness ----------------------------------
cat("\n=== Mechanism: corr(class-year durability S, R&D) ===\n")
for (s in c("All","Goods","Services")){ df <- if (s=="All") pan else filter(pan, Sector==s)
  cat(sprintf("  %-9s rho %.3f\n", s, cor(df$S, df$RD, method = "spearman"))) }
ww <- build("nsf25354-tab057.xlsx", 2009:2015)
res_ww <- bind_rows(corr_tab(ww,"All"), corr_tab(filter(ww,Sector=="Goods"),"Goods"),
                    corr_tab(filter(ww,Sector=="Services"),"Services")) |> mutate(across(where(is.numeric), ~round(.,3)))
cat("\n=== ROBUSTNESS: worldwide R&D (Table 57) ===\n"); print(as.data.frame(res_ww |> select(Sample,cells,sp_raw,sp_adj,d_sp)), row.names=FALSE)

# ---- FIGURE: the delta (adj - raw correlation) by sector, with bootstrap CI -
fig_df <- res |> filter(Sample != "All") |>
  transmute(Sector = factor(Sample, levels = c("Services","Goods")), d = d_sp, lo = d_sp_lo, hi = d_sp_hi)
fig <- ggplot(fig_df, aes(d, Sector, color = Sector)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey55") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .18, linewidth = .8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Goods" = "#185FA5", "Services" = "#D85A30"), guide = "none") +
  labs(title = "Panel confirmation: does the survival adjustment improve the R&D correlation?",
       subtitle = "Spearman correlation with R&D, adjusted minus raw. Bars = 95% class-cluster bootstrap CI.",
       x = "Change in correlation (adjusted minus raw); positive = adjustment helps", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", plot.margin = margin(8, 16, 8, 8))
ggsave(file.path(OUT, "fig_rq4_panel.png"), fig, width = 10, height = 3.6, dpi = 300)
ggsave(file.path(OUT, "fig_rq4_panel.pdf"), fig, width = 10, height = 3.6)

# ---- tables -----------------------------------------------------------------
write.csv(res, file.path(OUT, "tab_rq4_panel.csv"), row.names = FALSE)
write.csv(res_ww, file.path(OUT, "tab_rq4_panel_ww.csv"), row.names = FALSE)
write.csv(fe, file.path(OUT, "tab_rq4_panel_fe.csv"), row.names = FALSE)
css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
        th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
        caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
  knitr::kable(res,    format="html", caption="Panel (class-year) correlation with R&D, raw vs survival-adjusted, with cluster-bootstrap CI on the delta"),
  knitr::kable(fe,     format="html", caption="Two-way (class+year) fixed-effects regression: coefficient on log(indicator), class-clustered SE"),
  knitr::kable(res_ww, format="html", caption="Robustness: worldwide R&D (Table 57)"),
  "</body></html>"), file.path(OUT, "risultati_rq4_panel.html"))