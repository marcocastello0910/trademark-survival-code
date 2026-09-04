suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(survival)
  library(ggplot2); library(scales); library(knitr)
})

ROOT   <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT    <- file.path(ROOT, "output")
CUTOFF <- as.Date("2024-03-31")
H0     <- 7

# --- data + CAUSE variable (censored / maintenance / other) -----------------
d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  mutate(cancelled  = as.numeric(cancelled) == 1,
         is_service = as.numeric(is_service),
         age_obs    = as.numeric(CUTOFF - registration_dt) / 365.25,
         cause = ifelse(!cancelled, "censored",
                 ifelse(reg_cancel_cd == "2", "maintenance", "other")),
         status = factor(cause, levels = c("censored", "maintenance", "other"))) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(grp = ifelse(is_service == 1, "Services", "Goods"))

cat("Distribution of exit cause:\n"); print(table(d$cause))

n_cause <- table(d$cause)
n_canc  <- n_cause[["maintenance"]] + n_cause[["other"]]
t_cause <- data.frame(Outcome = c("Censored at the cut-off",
                                  "Cancelled: non-maintenance (Section 8, code 2)",
                                  "Cancelled: other causes",
                                  "Cancelled: all causes"),
                      Marks = c(n_cause[["censored"]], n_cause[["maintenance"]],
                                n_cause[["other"]], n_canc),
                      `% of cancellations` = c(NA,
                                round(100 * n_cause[["maintenance"]] / n_canc, 1),
                                round(100 * n_cause[["other"]]       / n_canc, 1),
                                100),
                      check.names = FALSE)
write.csv(t_cause, file.path(OUT, "tab_cr_cause.csv"), row.names = FALSE)

# =============================================================================
# (1) Competing-risks CIF (Aalen-Johansen), goods vs services
# =============================================================================
fit <- survfit(Surv(t_years, status) ~ is_service, data = d)
# fit$states = "(s0)","maintenance","other"  -> columns 1,2,3 of fit$pstate

strata_vec <- rep(names(fit$strata), fit$strata)
cif_curve <- tibble(time = fit$time,
                    grp  = ifelse(grepl("=1", strata_vec), "Services", "Goods"),
                    `Non-maintenance` = fit$pstate[,2],
                    `Other causes`    = fit$pstate[,3]) |>
  pivot_longer(c(`Non-maintenance`, `Other causes`), names_to = "cause", values_to = "cif") |>
  filter(time <= 12)

figCIF <- ggplot(cif_curve, aes(time, cif, color = grp, linetype = cause)) +
  geom_vline(xintercept = c(6, 10), linetype = 3, color = "grey65") +
  geom_step(linewidth = 1) +
  scale_color_manual(values = c("Goods" = "#185FA5", "Services" = "#D85A30")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Cumulative incidence of cancellation, by cause",
       subtitle = "Competing risks: non-maintenance (Section 8) vs other causes. Dashed = deadlines.",
       x = "Years since registration", y = "Cumulative probability of cancellation",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_cif.png"), figCIF, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(OUT, "fig_cif.pdf"), figCIF, width = 9, height = 5.5)

# --- CIF at key horizons (table) ---
s <- summary(fit, times = c(6, 7, 10))
cif_h <- tibble(Group = ifelse(grepl("=1", as.character(s$strata)), "Services", "Goods"),
                h = s$time,
                Maintenance   = round(s$pstate[,2], 3),
                `Other causes` = round(s$pstate[,3], 3),
                Total          = round(s$pstate[,2] + s$pstate[,3], 3))
write.csv(cif_h, file.path(OUT, "tab_cif.csv"), row.names = FALSE)
writeLines(kable(cif_h, format = "html"), file.path(OUT, "tab_cif.html"))

# =============================================================================
# (2) Decomposition of the 7-year gap: all causes = maintenance + other
# =============================================================================
dh <- d |> filter(age_obs >= H0) |>
  mutate(a = as.integer(cancelled & t_years <= H0),                          # all causes
         m = as.integer(cancelled & reg_cancel_cd == "2" & t_years <= H0),   # maintenance
         o = as.integer(cancelled & reg_cancel_cd != "2" & t_years <= H0))   # other causes

prop_gap <- function(df, var){
  x <- df |> group_by(is_service) |> summarise(p = mean(.data[[var]]), n = n(), .groups = "drop")
  pg <- x$p[x$is_service==0]; ng <- x$n[x$is_service==0]
  ps <- x$p[x$is_service==1]; ns <- x$n[x$is_service==1]
  se <- sqrt(pg*(1-pg)/ng + ps*(1-ps)/ns)
  tibble(var = var, goods = pg, services = ps, gap = ps-pg, lo = ps-pg-1.96*se, hi = ps-pg+1.96*se)
}
dec <- bind_rows(prop_gap(dh,"a"), prop_gap(dh,"m"), prop_gap(dh,"o")) |>
  mutate(cause = recode(var, a = "All causes",
                             m = "Non-maintenance (Section 8)", o = "Other causes"),
         cause = factor(cause, levels = c("Other causes","Non-maintenance (Section 8)","All causes")),
         gap_pp = gap*100, lo_pp = lo*100, hi_pp = hi*100)

figDec <- ggplot(dec, aes(gap_pp, cause)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey55") +
  geom_errorbarh(aes(xmin = lo_pp, xmax = hi_pp), height = .18, linewidth = .8, color = "#185FA5") +
  geom_point(size = 3.5, color = "#185FA5") +
  labs(title = "Services - Goods gap by cause of cancellation (at 7 years)",
       subtitle = "The gap lies entirely in non-maintenance; bars = 95% CI",
       x = "Services - Goods gap (percentage points)", y = NULL) +
  theme_minimal(base_size = 13)
ggsave(file.path(OUT, "fig_cr_decomp.png"), figDec, width = 9, height = 4, dpi = 300)
ggsave(file.path(OUT, "fig_cr_decomp.pdf"), figDec, width = 9, height = 4)

tab_dec <- dec |> transmute(Cause = as.character(cause),
             Goods = round(goods,3), Services = round(services,3),
             `Gap (pp)` = round(gap_pp,2), `95% CI` = sprintf("[%.2f, %.2f]", lo_pp, hi_pp))
write.csv(tab_dec, file.path(OUT, "tab_cr_decomp.csv"), row.names = FALSE)
writeLines(kable(tab_dec, format = "html"), file.path(OUT, "tab_cr_decomp.html"))

# =============================================================================
# (3) Why competing risks matter: correct CIF vs naive KM
#     (the KM that treats 'other causes' as censoring OVERSTATES maintenance)
# =============================================================================
dn  <- d |> mutate(ev = as.integer(cancelled & reg_cancel_cd == "2"))
kmn <- survfit(Surv(t_years, ev) ~ is_service, data = dn)
naive7 <- 1 - summary(kmn, times = H0)$surv          # 1 - KM (other causes = censoring)
cif7   <- summary(fit, times = H0)$pstate[,2]         # correct CIF (maintenance)

cat("\n=== Maintenance CIF at horizons (Aalen-Johansen) ===\n"); print(cif_h)
cat("\n=== Decomposition of the 7-year gap ===\n")
print(dec |> transmute(cause, goods = round(goods,3), services = round(services,3), gap_pp = round(gap_pp,2)))
cat(sprintf("\n=== Naive KM vs correct CIF (maintenance at 7 years) ===\n"))
cat(sprintf("  Goods    : naive KM %.3f  |  correct CIF %.3f  (overstatement %.3f)\n",
            naive7[1], cif7[1], naive7[1]-cif7[1]))
cat(sprintf("  Services : naive KM %.3f  |  correct CIF %.3f  (overstatement %.3f)\n",
            naive7[2], cif7[2], naive7[2]-cif7[2]))

t_naive <- data.frame(Group = c("Goods", "Services"),
                      `Naive KM` = round(naive7, 4),
                      `Competing-risks CIF` = round(cif7, 4),
                      `Overstatement (pp)` = round((naive7 - cif7) * 100, 3),
                      check.names = FALSE)
write.csv(t_naive, file.path(OUT, "tab_cr_naive_km.csv"), row.names = FALSE)