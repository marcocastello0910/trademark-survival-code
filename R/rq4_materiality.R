suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
  library(ggplot2); library(scales); library(knitr)
})

ROOT <- "/Users/marco/Desktop/Tesi magistrale"
OUT  <- file.path(ROOT, "output")
H    <- 8

NICE <- c("Chemicals","Paints","Cosmetics & cleaning","Lubricants & fuels","Pharmaceuticals",
  "Metal goods","Machinery","Hand tools","Electrical & scientific","Medical apparatus",
  "Appliances","Vehicles","Firearms","Jewelry","Musical instruments","Paper & printed matter",
  "Rubber & plastics","Leather goods","Building materials","Furniture","Housewares & glass",
  "Ropes & raw textiles","Yarns & threads","Fabrics","Clothing","Lace & notions","Floor coverings",
  "Toys & sporting goods","Meat & processed foods","Staple foods","Agricultural products",
  "Beers & soft drinks","Alcoholic beverages","Tobacco","Advertising & business",
  "Insurance & financial","Construction & repair","Telecommunications","Transport & storage",
  "Material treatment","Education & entertainment","Science & technology","Food & drink services",
  "Medical & agriculture svc","Legal & security svc")

d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(cancelled = as.numeric(cancelled) == 1, is_service = as.numeric(is_service),
         prim_num = as.numeric(prim_num), reg_year = as.numeric(reg_year),
         age_obs  = as.numeric(as.Date("2024-03-31") - registration_dt) / 365.25) |>
  filter(age_obs >= H) |>
  mutate(surv = as.integer(!(cancelled & t_years <= H)),
         Sector = ifelse(is_service == 1, "Services", "Goods"))

# =============================================================================
# (1) LEAGUE TABLE: raw vs survival-adjusted ranking of the 45 classes
# =============================================================================
cls <- d |> group_by(prim_num, Sector) |> summarise(N_raw = n(), S = mean(surv), .groups = "drop") |>
  mutate(N_adj    = N_raw * S,
         rank_raw = rank(-N_raw, ties.method = "first"),
         rank_adj = rank(-N_adj, ties.method = "first"),
         shift    = rank_raw - rank_adj,                       # >0 = rises when adjusted
         label    = sprintf("%02d %s", prim_num, NICE[prim_num]))

n_move <- sum(cls$shift != 0); max_move <- max(abs(cls$shift))

# --- slope chart of the top-15 league table ---
TOP <- 15
sl <- cls |> filter(rank_raw <= TOP) |>
  select(label, shift, rank_raw, rank_adj) |>
  pivot_longer(c(rank_raw, rank_adj), names_to = "measure", values_to = "rank") |>
  mutate(measure = factor(ifelse(measure == "rank_raw", "Raw count", "Survival-adjusted"),
                          levels = c("Raw count", "Survival-adjusted")),
         dir = ifelse(shift > 0, "Rises", ifelse(shift < 0, "Falls", "Unchanged")))
fig1 <- ggplot(sl, aes(measure, rank, group = label, color = dir)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.6) +
  geom_text(data = filter(sl, measure == "Raw count"), aes(label = label),
            hjust = 1.06, size = 3, color = "grey25") +
  geom_text(data = filter(sl, measure == "Survival-adjusted"), aes(label = label),
            hjust = -0.06, size = 3, color = "grey25") +
  scale_y_reverse(breaks = 1:20) +
  scale_x_discrete(expand = expansion(add = c(1.15, 1.15))) +
  scale_color_manual(values = c("Rises" = "#185FA5", "Falls" = "#D85A30", "Unchanged" = "grey70"),
                     name = NULL) +
  labs(title = "Does the correction reorder the league table of Nice classes?",
       subtitle = sprintf("Rank by trademark activity, raw count vs survival-adjusted (top %d shown; %d of 45 classes move)",
                          TOP, n_move),
       x = NULL, y = "Rank") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())
ggsave(file.path(OUT, "fig_rq4_ranks.png"), fig1, width = 9, height = 6.5, dpi = 300)
ggsave(file.path(OUT, "fig_rq4_ranks.pdf"), fig1, width = 9, height = 6.5)

# =============================================================================
# (2) TREND: services share per cohort, raw vs survival-adjusted
# =============================================================================
coh <- d |> group_by(reg_year, is_service) |> summarise(N = n(), S = mean(surv), .groups = "drop") |>
  mutate(adj = N * S) |> group_by(reg_year) |>
  summarise(raw_sh = 100 * sum(N[is_service == 1]) / sum(N),
            adj_sh = 100 * sum(adj[is_service == 1]) / sum(adj), .groups = "drop") |>
  mutate(gap = raw_sh - adj_sh)

fig2 <- ggplot(coh, aes(reg_year)) +
  geom_ribbon(aes(ymin = adj_sh, ymax = raw_sh), fill = "grey86") +
  geom_line(aes(y = raw_sh, color = "Raw count"), linewidth = 1) +
  geom_line(aes(y = adj_sh, color = "Survival-adjusted"), linewidth = 1) +
  geom_point(aes(y = raw_sh, color = "Raw count"), size = 1.6) +
  geom_point(aes(y = adj_sh, color = "Survival-adjusted"), size = 1.6) +
  scale_color_manual(values = c("Raw count" = "#9A9A9A", "Survival-adjusted" = "#185FA5"), name = NULL) +
  scale_x_continuous(breaks = seq(1995, 2015, 5)) +
  labs(title = "Does the correction change the goods/services story?",
       subtitle = "Services share of trademark activity by cohort. Shaded = the correction; it is widest at the 2008-2012 peak.",
       x = "Registration cohort (year)", y = "Services share of goods + services (%)") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_rq4_trend.png"), fig2, width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT, "fig_rq4_trend.pdf"), fig2, width = 9, height = 5)

# =============================================================================
# TABLES
# =============================================================================
tabA <- cls |> arrange(rank_raw) |>
  transmute(`Nice class` = label, Sector, `Raw count` = N_raw, `S(8)` = round(S, 3),
            `Adjusted count` = round(N_adj), `Rank (raw)` = rank_raw,
            `Rank (adjusted)` = rank_adj, `Shift` = shift)
tabB <- coh |> transmute(Cohort = reg_year, `Services share, raw (%)` = round(raw_sh, 1),
                         `Services share, adjusted (%)` = round(adj_sh, 1),
                         `Correction (pp)` = round(gap, 2))
write.csv(tabA, file.path(OUT, "tab_rq4_classes.csv"), row.names = FALSE)
write.csv(tabB, file.path(OUT, "tab_rq4_cohort.csv"), row.names = FALSE)
save_html <- function(tables, titles, file) {
  css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
          th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
          caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
  body <- mapply(function(t, ti) knitr::kable(t, format = "html", caption = ti), tables, titles)
  writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
                    paste(body, collapse = "\n"), "</body></html>"), file)
}
save_html(list(tabA, tabB),
          c("Table - League table of the 45 Nice classes: raw vs survival-adjusted rank (h = 8)",
            "Table - Services share by cohort: raw vs survival-adjusted"),
          file.path(OUT, "risultati_rq4.html"))

# ---- console ---------------------------------------------------------------
cat(sprintf("Classes changing rank: %d of 45 | max shift: %d positions | mean |shift|: %.1f\n\n",
            n_move, max_move, mean(abs(cls$shift))))
cat("=== Top 10 league table ===\n")
print(as.data.frame(head(tabA, 10)), row.names = FALSE)
cat("\n=== Biggest movers ===\n")
print(as.data.frame(cls |> arrange(desc(abs(shift))) |>
      transmute(label, Sector, `S(8)` = round(S, 3), rank_raw, rank_adj, shift) |> head(6)), row.names = FALSE)
cat(sprintf("\nRaw services share 1995 -> 2012 peak: %.1f%% -> %.1f%% (growth %.1f pp)\n",
            coh$raw_sh[1], max(coh$raw_sh), max(coh$raw_sh) - coh$raw_sh[1]))
cat(sprintf("Adjusted                       : %.1f%% -> %.1f%% (growth %.1f pp)\n",
            coh$adj_sh[1], max(coh$adj_sh), max(coh$adj_sh) - coh$adj_sh[1]))
cat(sprintf("Correction: min %.2f pp, max %.2f pp (cohort %d)\n",
            min(coh$gap), max(coh$gap), coh$reg_year[which.max(coh$gap)]))