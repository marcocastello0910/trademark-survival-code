suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
  library(ggplot2); library(broom); library(knitr); library(scales)
})

ROOT   <- "/Users/marco/Desktop/Tesi magistrale"
OUT    <- file.path(ROOT, "output")
CUTOFF <- as.Date("2024-03-31")
G8 <- 7; G10 <- 11; G20 <- 21
COL    <- c("Goods" = "#185FA5", "Services" = "#D85A30")
MLAB   <- c("Section 8 (~6y)", "1st renewal (~10y)", "2nd renewal (~20y)")

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

# ---- 1-3. data, death windows, risk sets -----------------------------------
df <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(across(c(cancelled, is_service, std_char, basis_itu, opp_pending, n_classes), as.numeric),
         cancelled = cancelled == 1,
         age_obs = as.numeric(CUTOFF - registration_dt) / 365.25,
         died_m1 = cancelled & t_years <= G8,
         died_m2 = cancelled & t_years >  G8  & t_years <= G10,
         died_m3 = cancelled & t_years >  G10 & t_years <= G20,
         atrisk_m1 = age_obs >= G8,
         atrisk_m2 = !died_m1 & age_obs >= G10,
         atrisk_m3 = !died_m1 & !died_m2 & age_obs >= G20)

# ---- 4. raw discrete hazard (H1) -------------------------------------------
haz <- bind_rows(
  df |> filter(atrisk_m1) |> group_by(is_service) |> summarise(milestone = 1, hazard = mean(died_m1), n = n(), .groups = "drop"),
  df |> filter(atrisk_m2) |> group_by(is_service) |> summarise(milestone = 2, hazard = mean(died_m2), n = n(), .groups = "drop"),
  df |> filter(atrisk_m3) |> group_by(is_service) |> summarise(milestone = 3, hazard = mean(died_m3), n = n(), .groups = "drop"))
cat("Discrete hazard (goods=0, services=1):\n"); print(haz |> mutate(hazard = round(hazard, 3)))

# ---- 5. per-milestone logit: collect odds ratios ---------------------------
or_list <- list()
for (m in 1:3) {
  d <- df |> filter(.data[[paste0("atrisk_m", m)]]) |>
       mutate(fail = as.integer(.data[[paste0("died_m", m)]]))
  mod <- glm(fail ~ is_service * std_char + is_service * n_classes + basis_itu + opp_pending,
             data = d, family = binomial)
  or_list[[m]] <- tibble(milestone = m, Term = names(coef(mod)), OR = round(exp(coef(mod)), 3))
}

# ---- 6. pooled person-period model: cloglog + staircase test ---------------
long <- df |>
  select(is_service, std_char, n_classes, basis_itu, opp_pending,
         atrisk_m1, atrisk_m2, atrisk_m3, died_m1, died_m2, died_m3) |>
  pivot_longer(c(atrisk_m1, atrisk_m2, atrisk_m3, died_m1, died_m2, died_m3),
               names_to = c(".value", "milestone"), names_pattern = "(.*)_m(\\d)") |>
  filter(atrisk) |>
  mutate(milestone = factor(milestone), fail = as.integer(died))
full <- glm(fail ~ milestone * is_service + std_char + n_classes + basis_itu + opp_pending,
            data = long, family = binomial(link = "cloglog"))
redu <- glm(fail ~ milestone + is_service + std_char + n_classes + basis_itu + opp_pending,
            data = long, family = binomial(link = "cloglog"))
lrt <- anova(redu, full, test = "Chisq")
cat(sprintf("\nStaircase test (milestone x service): LRT chi2 = %.1f, p = %.2g\n",
            lrt$Deviance[2], lrt$`Pr(>Chi)`[2]))

# =============================================================================
# FIGURE — the U-shape
# =============================================================================
hazp <- haz |> mutate(Group = ifelse(is_service == 1, "Services", "Goods"),
                      Milestone = factor(milestone, 1:3, labels = c("Section 8\n(~6 years)",
                                        "1st renewal\n(~10 years)", "2nd renewal\n(~20 years)")))
p_h <- ggplot(hazp, aes(Milestone, hazard, fill = Group)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_text(aes(label = percent(hazard, accuracy = 0.1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = COL, name = NULL) +
  scale_y_continuous(labels = percent, limits = c(0, 0.58),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Discrete-time hazard: probability of non-renewal at each milestone",
       subtitle = "Conditional on reaching the milestone (goods vs services)",
       x = NULL, y = "P(non-renewal)") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
save_fig(p_h, "fig_discrete_hazard")

# =============================================================================
# TABLES (CSV + single HTML)
# =============================================================================
t5 <- haz |> mutate(Group = ifelse(is_service == 1, "Services", "Goods"),
                    Milestone = factor(milestone, 1:3, labels = MLAB)) |>
  select(Milestone, Group, hazard) |>
  pivot_wider(names_from = Group, values_from = hazard) |>
  mutate(`Gap (Serv-Goods)` = round(Services - Goods, 3),
         Goods = round(Goods, 3), Services = round(Services, 3))
t6 <- bind_rows(or_list) |>
  pivot_wider(names_from = milestone, values_from = OR, names_prefix = "M") |>
  rename(`Section 8 (M1)` = M1, `Renewal 10y (M2)` = M2, `Renewal 20y (M3)` = M3)
t7 <- tidy(full) |> transmute(Term = term, Coefficient = round(estimate, 4),
                              p = signif(p.value, 3))

for (nm in c("t5","t6","t7"))
  write.csv(get(nm), file.path(OUT, paste0("tab_disc_", nm, ".csv")), row.names = FALSE)
save_html(list(t5, t6, t7),
          c("Table 5 - Discrete hazard (non-renewal) by milestone and sector",
            "Table 6 - Per-milestone logit: odds ratios",
            "Table 7 - Pooled cloglog model (milestone x service interaction = the staircase)"),
          file.path(OUT, "risultati_discreto.html"))