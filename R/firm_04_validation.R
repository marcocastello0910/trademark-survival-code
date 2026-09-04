suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(sandwich)
})

ROOT <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT  <- file.path(ROOT, "output")
MINMARKS <- 5      # below this, a firm's S is 0/1 noise rather than a rate
B <- 2000
set.seed(1)

f0 <- read.csv(file.path(OUT, "firm_panel.csv"), stringsAsFactors = FALSE)
f0 <- f0 |> filter(!is.na(rd), rd > 0, N_raw > 0)

cat(sprintf("matched firms with R&D > 0: %d\n", nrow(f0)))

# --- 1. how the sample and the result move with the minimum mark count -------
# log1p, not log: a firm whose marks all died has adj = 0.
cor_set <- function(df) c(
  sp_raw = cor(df$N_raw, df$rd, method = "spearman"),
  sp_adj = cor(df$adj,   df$rd, method = "spearman"),
  lp_raw = cor(log1p(df$N_raw), log(df$rd)),
  lp_adj = cor(log1p(df$adj),   log(df$rd))
)

cat("\n=== Sensitivity to the minimum trademark count ===\n")
sens <- do.call(rbind, lapply(c(1, 3, 5, 10, 20), function(k) {
  do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
    d <- f0 |> filter(N_raw >= k)
    if (s != "All") d <- d |> filter(Sector == s)
    if (nrow(d) < 15) return(NULL)
    r <- cor_set(d)
    data.frame(minN = k, Sample = s, firms = nrow(d),
               sp_raw = r["sp_raw"], sp_adj = r["sp_adj"], d_sp = r["sp_adj"] - r["sp_raw"],
               d_lp = r["lp_adj"] - r["lp_raw"])
  }))
}))
print(sens, row.names = FALSE, digits = 3)

# --- 2. primary specification ------------------------------------------------
f <- f0 |> filter(N_raw >= MINMARKS)
cat(sprintf("\n=== PRIMARY (N_raw >= %d): %d firms | goods %d | services %d ===\n",
            MINMARKS, nrow(f), sum(f$Sector == "Goods"), sum(f$Sector == "Services")))

boot_delta <- function(df, B = 2000) {
  n <- nrow(df)
  d <- replicate(B, {
    s <- df[sample.int(n, n, replace = TRUE), ]
    c(cor(s$adj, s$rd, method = "spearman") - cor(s$N_raw, s$rd, method = "spearman"),
      cor(log1p(s$adj), log(s$rd)) - cor(log1p(s$N_raw), log(s$rd)))
  })
  c(sp_lo = quantile(d[1, ], .025), sp_hi = quantile(d[1, ], .975),
    lp_lo = quantile(d[2, ], .025), lp_hi = quantile(d[2, ], .975))
}

main <- do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
  d <- if (s == "All") f else filter(f, Sector == s)
  r <- cor_set(d); b <- boot_delta(d, B)
  data.frame(Sample = s, firms = nrow(d),
             sp_raw = r["sp_raw"], sp_adj = r["sp_adj"], d_sp = r["sp_adj"] - r["sp_raw"],
             sp_lo = b["sp_lo.2.5%"], sp_hi = b["sp_hi.97.5%"],
             lp_raw = r["lp_raw"], lp_adj = r["lp_adj"], d_lp = r["lp_adj"] - r["lp_raw"],
             lp_lo = b["lp_lo.2.5%"], lp_hi = b["lp_hi.97.5%"],
             mech = cor(d$S, d$rd, method = "spearman"))
}))
print(main, row.names = FALSE, digits = 3)

# --- 3. does durability survive a size control? ------------------------------
cat("\n=== Size-controlled regression: log(rd) ~ log(N_raw) + log(S) + log(assets) ===\n")
size <- do.call(rbind, lapply(c("All", "Goods", "Services"), function(s) {
  d <- if (s == "All") f else filter(f, Sector == s)
  d <- d |> filter(!is.na(at), at > 0, S > 0)
  if (nrow(d) < 30) return(NULL)
  m <- lm(log(rd) ~ log(N_raw) + log(S) + log(at), data = d)
  cf <- summary(m)$coefficients
  data.frame(Sample = s, firms = nrow(d),
             b_logS = cf["log(S)", 1], se_logS = cf["log(S)", 2],
             t_logS = cf["log(S)", 3], p_logS = cf["log(S)", 4],
             b_logN = cf["log(N_raw)", 1])
}))
print(size, row.names = FALSE, digits = 3)

# --- 4. figure ---------------------------------------------------------------
plt <- main |>
  filter(Sample != "All") |>
  transmute(Sector = factor(Sample, levels = c("Services", "Goods")),
            d = d_sp, lo = sp_lo, hi = sp_hi, n = firms)

fig <- ggplot(plt, aes(d, Sector, colour = Sector)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .16, linewidth = .8) +
  geom_point(size = 3.4) +
  geom_text(aes(label = sprintf("n = %d firms", n)), vjust = -1.4, size = 3.4,
            show.legend = FALSE) +
  scale_colour_manual(values = c(Goods = "#1f5c99", Services = "#c0442e"), guide = "none") +
  labs(title = "Firm-level validation: does the survival adjustment improve the R&D correlation?",
       subtitle = sprintf("Spearman correlation with observed company R&D, adjusted minus raw. Firms with at least %d marks. Bars = 95%% bootstrap CI.", MINMARKS),
       x = "Change in correlation (adjusted minus raw); positive = adjustment helps",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", plot.margin = margin(8, 16, 8, 8))
ggsave(file.path(OUT, "fig_rq4_firm.png"), fig, width = 10, height = 3.6, dpi = 300)
ggsave(file.path(OUT, "fig_rq4_firm.pdf"), fig, width = 10, height = 3.6)

write.csv(main, file.path(OUT, "tab_rq4_firm.csv"), row.names = FALSE)
write.csv(sens, file.path(OUT, "tab_rq4_firm_sensitivity.csv"), row.names = FALSE)

# BIGPORT cuts on portfolio size only; the analysis keeps MINMARKS.
BIGPORT <- 300
durab <- f0 |> filter(N_raw >= BIGPORT) |> arrange(desc(S)) |>
  transmute(Company = conm, Sector, `S(8)` = round(S, 3), N = N_raw)
write.csv(durab, file.path(OUT, "tab_rq4_firm_durability.csv"), row.names = FALSE)

# --- 5. results as HTML ------------------------------------------------------
r3 <- function(d, k = 3) {
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], round, k)
  d
}

tab_main <- with(main, data.frame(
  Sample, Firms = firms,
  `rho raw` = round(sp_raw, 3), `rho adjusted` = round(sp_adj, 3),
  Change = sprintf("%+.3f", d_sp),
  `95% CI` = sprintf("[%+.3f, %+.3f]", sp_lo, sp_hi),
  `log rho raw` = round(lp_raw, 3), `log rho adjusted` = round(lp_adj, 3),
  `log change` = sprintf("%+.3f", d_lp),
  `log 95% CI` = sprintf("[%+.3f, %+.3f]", lp_lo, lp_hi),
  `Mechanism rho(S, R&D)` = round(mech, 3),
  check.names = FALSE))

tab_sens <- with(sens, data.frame(
  `Min marks` = minN, Sample, Firms = firms,
  `rho raw` = round(sp_raw, 3), `rho adjusted` = round(sp_adj, 3),
  Change = sprintf("%+.3f", d_sp), `log change` = sprintf("%+.3f", d_lp),
  check.names = FALSE))

tab_size <- with(size, data.frame(
  Sample, Firms = firms,
  `coef. log(S)` = round(b_logS, 3), `SE` = round(se_logS, 3),
  `t` = round(t_logS, 2), `p` = round(p_logS, 3),
  `coef. log(N raw)` = round(b_logN, 3),
  check.names = FALSE))

css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
        th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
        caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
  knitr::kable(tab_main, format = "html", row.names = FALSE,
    caption = sprintf("Firm-level correlation with observed company R&D, raw vs survival-adjusted, by sector (firms with at least %d marks; bootstrap over firms)", MINMARKS)),
  knitr::kable(tab_sens, format = "html", row.names = FALSE,
    caption = "Sensitivity to the minimum trademark count per firm"),
  knitr::kable(tab_size, format = "html", row.names = FALSE,
    caption = "Size-controlled regression: log(R&D) ~ log(N raw) + log(S) + log(assets)"),
  "</body></html>"), file.path(OUT, "risultati_rq4_firm.html"))