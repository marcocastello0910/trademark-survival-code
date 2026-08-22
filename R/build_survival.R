suppressPackageStartupMessages({
  library(haven); library(dplyr); library(survival)
  library(ggplot2); library(broom); library(knitr); library(scales)
})

ROOT   <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT    <- file.path(ROOT, "output")
CUTOFF <- as.Date("2024-03-31")
COL    <- c("Goods" = "#185FA5", "Services" = "#D85A30")   # consistent palette across figures

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = w, height = h)
}
save_html <- function(tables, titles, file) {
  css <- "<style>body{font-family:sans-serif}table{border-collapse:collapse;margin-bottom:24px}
          th,td{border:1px solid #ccc;padding:6px 10px;text-align:right}th{background:#eef}
          caption{font-weight:bold;text-align:left;margin-bottom:6px;font-size:15px}</style>"
  body <- mapply(function(t, ti) knitr::kable(t, format = "html", caption = ti), tables, titles)
  writeLines(paste0("<html><head><meta charset='utf-8'>", css, "</head><body>",
                    paste(body, collapse = "\n"), "</body></html>"), file)
}

# ---- data ------------------------------------------------------------------
df <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(across(c(cancelled, is_service, std_char, opp_pending, basis_itu,
                  reg_year, n_classes), as.numeric),
         cancelled = cancelled == 1,
         svc    = ifelse(is_service == 1, "Services", "Goods"),
         dead10 = as.integer(cancelled & t_years <= 10))
cat(sprintf("Sample: %s registered marks (goods/services)\n", format(nrow(df), big.mark = ",")))

# ---- models ----------------------------------------------------------------
km  <- survfit(Surv(t_years, cancelled) ~ is_service, data = df)
lr  <- survdiff(Surv(t_years, cancelled) ~ is_service, data = df)
cox <- coxph(Surv(t_years, cancelled) ~ is_service + std_char + n_classes +
               opp_pending + basis_itu, data = df, ties = "breslow")
sub <- df |> filter(reg_year <= as.integer(format(CUTOFF, "%Y")) - 10)
lg  <- glm(dead10 ~ is_service * std_char + is_service * n_classes +
             opp_pending + basis_itu, data = sub, family = binomial)

cat(sprintf("Log-rank chi2 = %.1f (df=%d)\n", lr$chisq, length(lr$n) - 1))
cat("Cox HR:\n"); print(round(exp(coef(cox)), 4))

# =============================================================================
# FIGURES
# =============================================================================
# (1) Kaplan-Meier curves
kmd <- tidy(km) |> mutate(Group = ifelse(strata == "is_service=1", "Services", "Goods"))
p_km <- ggplot(kmd, aes(time, estimate, color = Group)) +
  geom_vline(xintercept = c(6, 10, 20), linetype = "dashed", color = "grey65", linewidth = 0.3) +
  geom_step(linewidth = 0.9) +
  annotate("text", x = c(6, 10, 20), y = 1.02, label = c("Sec. 8", "renewal", "renewal"),
           size = 3, color = "grey45") +
  scale_color_manual(values = COL, name = NULL) +
  scale_y_continuous(labels = percent, limits = c(0, 1.03)) +
  scale_x_continuous(breaks = c(0, 6, 10, 20, 30)) +
  coord_cartesian(xlim = c(0, 30)) +
  labs(title = "Trademark survival: goods vs services",
       subtitle = "Kaplan-Meier curves (dashed lines = maintenance deadlines)",
       x = "Years since registration", y = "Share of marks still registered") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_fig(p_km, "fig_km_curve")

# (2) Cox hazard-ratio forest plot
lab <- c(is_service = "Service (vs good)", std_char = "Standard characters",
         n_classes = "No. of classes", opp_pending = "Opposition pending",
         basis_itu = "Intent-to-use")
hr <- tidy(cox, exponentiate = TRUE, conf.int = TRUE) |>
  mutate(term = recode(term, !!!lab))
p_hr <- ggplot(hr, aes(estimate, reorder(term, estimate))) +
  geom_vline(xintercept = 1, color = "grey55") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "#185FA5", linewidth = 0.7) +
  scale_x_log10() +
  labs(title = "Cox model: hazard ratio of trademark cancellation",
       subtitle = "Point = HR; bars = 95% confidence interval (log scale)",
       x = "Hazard ratio  (>1 = higher risk = lower survival)", y = NULL) +
  theme_minimal(base_size = 12)
save_fig(p_hr, "fig_hr_forest", h = 3.5)

# =============================================================================
# TABLES (CSV + single HTML)
# =============================================================================
t1 <- df |> group_by(Group = svc) |>
  summarise(N = n(),
            `% cancelled (total)` = round(100 * mean(cancelled), 1),
            `% cancelled within 10y` = round(100 * mean(dead10[reg_year <= 2014]), 1),
            .groups = "drop")
s3 <- summary(km, times = c(6, 10, 20))
t2 <- data.frame(Group = ifelse(s3$strata == "is_service=1", "Services", "Goods"),
                 Years = s3$time, Survival = round(s3$surv, 3))
t3 <- tidy(cox, exponentiate = TRUE, conf.int = TRUE) |>
  transmute(Variable = recode(term, !!!lab), HR = round(estimate, 3),
            `95% CI low` = round(conf.low, 3), `95% CI high` = round(conf.high, 3),
            p = signif(p.value, 3))
t4 <- tidy(lg) |>
  transmute(Term = term, Coefficient = round(estimate, 3),
            `Odds ratio` = round(exp(estimate), 3), p = signif(p.value, 3))

for (nm in c("t1","t2","t3","t4"))
  write.csv(get(nm), file.path(OUT, paste0("tab_base_", nm, ".csv")), row.names = FALSE)
save_html(list(t1, t2, t3, t4),
          c("Table 1 - Sample and cancellation rates",
            "Table 2 - Kaplan-Meier survival at the milestones",
            "Table 3 - Cox model (hazard ratios)",
            "Table 4 - Logit: cancellation within 10 years"),
          file.path(OUT, "risultati_base.html"))