suppressPackageStartupMessages({ library(dplyr) })

ROOT <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT  <- file.path(ROOT, "output")
MINMARKS <- 5
set.seed(1)

f <- read.csv(file.path(OUT, "firm_panel.csv"), stringsAsFactors = FALSE) |>
  filter(!is.na(rd), rd > 0, N_raw >= MINMARKS)

d_sp <- function(df) cor(df$adj, df$rd, method = "spearman") -
                     cor(df$N_raw, df$rd, method = "spearman")

# --- (1) how much of the drop is pure measurement noise? --------------------
cat("=== (1) Permutation benchmark: what if S were pure noise? ===\n")
perm <- do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
  d <- if (s == "All") f else filter(f, Sector == s)
  obs <- d_sp(d)
  nul <- replicate(1000, {
    p <- d; p$S <- sample(p$S); p$adj <- p$N_raw * p$S; d_sp(p)
  })
  data.frame(Sample = s, firms = nrow(d), observed = obs,
             noise_mean = mean(nul), noise_lo = quantile(nul, .025),
             noise_hi = quantile(nul, .975),
             p_better = mean(nul >= obs))
}))
print(perm, row.names = FALSE, digits = 3)

# --- (2) does the class-level result reappear on aggregation? ---------------
cat("\n=== (2) Aggregating the SAME firms to industries ===\n")
f$naics3 <- substr(as.character(f$naics), 1, 3)
f$naics2 <- substr(as.character(f$naics), 1, 2)

agg_test <- function(df, key, minfirms = 3) {
  a <- df |>
    filter(!is.na(.data[[key]]), .data[[key]] != "", .data[[key]] != "NA") |>
    group_by(ind = .data[[key]]) |>
    # goods_share must be weighted before N_raw is replaced by its own sum
    summarise(firms = n(),
              goods_share = weighted.mean(goods_share, N_raw),
              N_tot = sum(N_raw), adj = sum(adj), rd = sum(rd), .groups = "drop") |>
    filter(firms >= minfirms) |>
    rename(N_raw = N_tot) |>
    mutate(S = adj / N_raw,
           Sector = ifelse(goods_share > 0.5, "Goods", "Services"))
  do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
    d <- if (s == "All") a else filter(a, Sector == s)
    if (nrow(d) < 6) return(NULL)
    data.frame(level = key, Sample = s, units = nrow(d),
               sp_raw = cor(d$N_raw, d$rd, method = "spearman"),
               sp_adj = cor(d$adj, d$rd, method = "spearman"),
               d_sp = d_sp(d),
               mech = cor(d$S, d$rd, method = "spearman"))
  }))
}
agg <- rbind(agg_test(f, "naics2"), agg_test(f, "naics3"))
print(agg, row.names = FALSE, digits = 3)

cat("\n  Bootstrap over firms, propagated through the aggregation (95% CI on d_sp):\n")
boot_agg <- function(key, sample_name, B = 600) {
  n <- nrow(f)
  v <- replicate(B, {
    r <- agg_test(f[sample.int(n, n, replace = TRUE), ], key)
    if (is.null(r)) return(NA_real_)
    x <- r$d_sp[r$Sample == sample_name]
    if (length(x) == 0) NA_real_ else x
  })
  v <- v[is.finite(v)]
  c(lo = unname(quantile(v, .025)), hi = unname(quantile(v, .975)),
    stable = 100 * max(mean(v > 0), mean(v < 0)))
}
stab <- do.call(rbind, lapply(c("naics2", "naics3"), function(k) {
  do.call(rbind, lapply(c("Goods", "Services"), function(s) {
    got <- agg$d_sp[agg$level == k & agg$Sample == s]
    if (!length(got)) return(NULL)
    b <- boot_agg(k, s)
    data.frame(level = k, Sample = s, d_sp = got,
               lo = b["lo"], hi = b["hi"], sign_stable_pct = b["stable"])
  }))
}))
for (i in seq_len(nrow(stab))) {
  cat(sprintf("    %-7s %-9s d_sp %+.3f  CI [%+.3f, %+.3f]  (sign stable in %.0f%% of resamples)\n",
              stab$level[i], stab$Sample[i], stab$d_sp[i],
              stab$lo[i], stab$hi[i], stab$sign_stable_pct[i]))
}

cat("\n  Firm level, same sample, for comparison:\n")
firmlvl <- do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
  d <- if (s == "All") f else filter(f, Sector == s)
  data.frame(level = "firm", Sample = s, units = nrow(d),
             sp_raw = cor(d$N_raw, d$rd, method = "spearman"),
             sp_adj = cor(d$adj, d$rd, method = "spearman"),
             d_sp = d_sp(d), mech = cor(d$S, d$rd, method = "spearman"))
}))
print(firmlvl, row.names = FALSE, digits = 3)

# --- (3) geography -----------------------------------------------------------
cat("\n=== (3) US-based owners only ===\n")
us <- f |> filter(is.na(own_country) | own_country == "" | own_country == "US")
geo <- do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
  d <- if (s == "All") us else filter(us, Sector == s)
  data.frame(Sample = s, firms = nrow(d),
             sp_raw = cor(d$N_raw, d$rd, method = "spearman"),
             sp_adj = cor(d$adj, d$rd, method = "spearman"),
             d_sp = d_sp(d), mech = cor(d$S, d$rd, method = "spearman"))
}))
print(geo, row.names = FALSE, digits = 3)

write.csv(perm, file.path(OUT, "tab_rq4_firm_perm.csv"), row.names = FALSE)
write.csv(rbind(agg, firmlvl), file.path(OUT, "tab_rq4_firm_agg.csv"), row.names = FALSE)
write.csv(stab, file.path(OUT, "tab_rq4_firm_signstability.csv"), row.names = FALSE)
write.csv(geo, file.path(OUT, "tab_rq4_firm_usonly.csv"), row.names = FALSE)

# --- results as HTML ----------------------------------------------------------
tab_perm <- with(perm, data.frame(
  Sample, Firms = firms,
  `Observed change` = sprintf("%+.3f", observed),
  `Expected if S were noise` = sprintf("%+.3f", noise_mean),
  `Noise 95% band` = sprintf("[%+.3f, %+.3f]", noise_lo, noise_hi),
  `p (share of permutations at least as good)` = round(p_better, 3),
  check.names = FALSE))

both <- rbind(firmlvl, agg)
both$level <- factor(both$level, levels = c("firm", "naics3", "naics2"),
                     labels = c("Firm", "Industry (NAICS-3)", "Industry (NAICS-2)"))
tab_agg <- with(both[order(both$Sample, both$level), ], data.frame(
  `Unit of analysis` = level, Sample, Units = units,
  `rho raw` = round(sp_raw, 3), `rho adjusted` = round(sp_adj, 3),
  Change = sprintf("%+.3f", d_sp),
  `Mechanism rho(S, R&D)` = round(mech, 3),
  check.names = FALSE))

tab_stab <- with(stab, data.frame(
  `Aggregation level` = ifelse(level == "naics2", "NAICS-2", "NAICS-3"),
  Sample, Change = sprintf("%+.3f", d_sp),
  `95% CI (bootstrap over firms)` = sprintf("[%+.3f, %+.3f]", lo, hi),
  `Sign stable in % of resamples` = round(sign_stable_pct, 0),
  check.names = FALSE))

tab_geo <- with(geo, data.frame(
  Sample, Firms = firms,
  `rho raw` = round(sp_raw, 3), `rho adjusted` = round(sp_adj, 3),
  Change = sprintf("%+.3f", d_sp),
  `Mechanism rho(S, R&D)` = round(mech, 3),
  check.names = FALSE))

css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
        th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
        caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
  knitr::kable(tab_perm, format = "html", row.names = FALSE,
    caption = "Permutation benchmark: the change in correlation that pure measurement noise in S would produce. The correct null is this band, not zero, because adj = N x S inherits error that N raw does not carry"),
  knitr::kable(tab_agg, format = "html", row.names = FALSE,
    caption = "The same matched firms at three units of analysis: aggregating flips the sign of the change"),
  knitr::kable(tab_stab, format = "html", row.names = FALSE,
    caption = "Bootstrap over firms propagated through the aggregation: how often the aggregated change keeps its sign"),
  knitr::kable(tab_geo, format = "html", row.names = FALSE,
    caption = "Robustness: US-based owners only"),
  "</body></html>"), file.path(OUT, "risultati_rq4_firm_diag.html"))