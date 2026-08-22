suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
  library(ggplot2); library(scales); library(knitr)
})

ROOT <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT  <- file.path(ROOT, "output")
H    <- 8
COL  <- c("Goods" = "#185FA5", "Services" = "#D85A30")

# short Nice class titles (ASCII, for readable axes)
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

agg_over <- 100 * (mean(d$surv[d$is_service == 0]) / mean(d$surv[d$is_service == 1]) - 1)

# =============================================================================
# (A) COHORT EVOLUTION
# =============================================================================
coh <- d |> group_by(reg_year, is_service) |> summarise(S = mean(surv), .groups = "drop") |>
  pivot_wider(names_from = is_service, values_from = S, names_prefix = "S") |>
  mutate(overstated = 100 * (S0 / S1 - 1))

figA <- ggplot(coh, aes(reg_year, overstated)) +
  geom_hline(yintercept = agg_over, linetype = 2, color = "grey60") +
  geom_smooth(method = "loess", span = 0.6, se = FALSE, color = "#185FA5", linewidth = 1) +
  geom_point(size = 2.4, color = "grey30") +
  annotate("text", x = 2015.2, y = agg_over + 0.7, label = "aggregate", hjust = 1, size = 3, color = "grey50") +
  scale_x_continuous(breaks = seq(1995, 2015, 5)) +
  labs(title = "The goods/services bias over registration cohorts",
       subtitle = "Services-to-goods ratio overstated by raw counts, at the 8-year horizon (points = cohorts; line = trend)",
       x = "Registration cohort (year)", y = "Services/Goods ratio overstated (%)") +
  theme_minimal(base_size = 13)
ggsave(file.path(OUT, "fig_rq3_cohort.png"), figA, width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT, "fig_rq3_cohort.pdf"), figA, width = 9, height = 5)

tabA <- coh |> transmute(`Cohort` = reg_year, `Goods S(8)` = round(S0, 3),
                         `Services S(8)` = round(S1, 3), `Ratio overstated (%)` = round(overstated, 1))
write.csv(tabA, file.path(OUT, "tab_rq3_cohort.csv"), row.names = FALSE)

# =============================================================================
# (B) CLASS DISTRIBUTION
# =============================================================================
cls <- d |> group_by(prim_num, Sector) |> summarise(S = mean(surv), n = n(), .groups = "drop") |>
  mutate(label = sprintf("%02d %s", prim_num, NICE[prim_num]))
mG <- mean(cls$S[cls$Sector == "Goods"]); mS <- mean(cls$S[cls$Sector == "Services"])

# --- ranking figure: 45 classes by reliability ---
figB <- ggplot(cls, aes(S, reorder(label, S), color = Sector)) +
  geom_vline(xintercept = c(mG, mS), linetype = 3, color = c(COL["Goods"], COL["Services"])) +
  geom_segment(aes(x = min(cls$S) - 0.01, xend = S, yend = reorder(label, S)), color = "grey85", linewidth = 0.4) +
  geom_point(size = 2.6) +
  scale_color_manual(values = COL, name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = "Reliability by Nice class: survival to the first maintenance (8 years)",
       subtitle = "Higher = more reliable (raw count little distorted). Dotted lines = sector means.",
       x = "Share of marks surviving to 8 years  (S(8))", y = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_rq3_classes.png"), figB, width = 9, height = 10, dpi = 300)
ggsave(file.path(OUT, "fig_rq3_classes.pdf"), figB, width = 9, height = 10)

# --- heatmap: class x cohort ---
hm <- d |> group_by(prim_num, reg_year) |> summarise(S = mean(surv), n = n(), .groups = "drop") |>
  mutate(S = ifelse(n >= 100, S, NA_real_),                   # mask tiny cells: classes 43-45 exist only from 2002 (Nice 8th ed.)
         label = sprintf("%02d %s", prim_num, NICE[prim_num]))
ord <- cls |> arrange(S) |> pull(label)                       # order classes by overall reliability
hm <- hm |> mutate(label = factor(label, levels = ord))
figC <- ggplot(hm, aes(factor(reg_year), label, fill = S)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#D85A30", mid = "#F5F5F0", high = "#185FA5",
                       midpoint = mean(d$surv), labels = percent, name = "S(8)", na.value = "grey90") +
  scale_x_discrete(breaks = seq(1995, 2015, 5)) +
  labs(title = "Trademark reliability by Nice class and registration cohort",
       subtitle = "S(8) = share surviving to first maintenance; ordered least to most reliable (grey = too few marks)",
       x = "Registration cohort", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = 7))
ggsave(file.path(OUT, "fig_rq3_heatmap.png"), figC, width = 9, height = 10, dpi = 300)
ggsave(file.path(OUT, "fig_rq3_heatmap.pdf"), figC, width = 9, height = 10)

# --- class table (all 45, ranked) ---
tabB <- cls |> arrange(desc(S)) |>
  transmute(`Nice class` = label, Sector, `S(8)` = round(S, 3), N = n)
write.csv(tabB, file.path(OUT, "tab_rq3_classes.csv"), row.names = FALSE)
save_html <- function(tables, titles, file) {
  css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
          th,td{border:1px solid #ccc;padding:5px 9px;text-align:right}th{background:#eef}
          caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
  body <- mapply(function(t, ti) knitr::kable(t, format = "html", caption = ti), tables, titles)
  writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
                    paste(body, collapse = "\n"), "</body></html>"), file)
}
save_html(list(tabA, tabB),
          c("Table - Bias by registration cohort (h = 8)",
            "Table - Reliability of the 45 Nice classes (h = 8), ranked"),
          file.path(OUT, "risultati_rq3.html"))

# ---- console ---------------------------------------------------------------
cat(sprintf("Aggregate overstatement at h=%d: %.1f%%  (goods mean S %.3f, services mean S %.3f)\n\n",
            H, agg_over, mG, mS))
cat("=== Cohort trend ===\n"); print(as.data.frame(tabA), row.names = FALSE)
cat("\n=== 6 least reliable classes ===\n"); print(as.data.frame(head(arrange(cls, S)[c("label","Sector","S","n")], 6)), row.names = FALSE)
cat("=== 6 most reliable classes ===\n");  print(as.data.frame(tail(arrange(cls, S)[c("label","Sector","S","n")], 6)), row.names = FALSE)