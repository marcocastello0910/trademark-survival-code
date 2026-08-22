suppressPackageStartupMessages({ library(haven); library(dplyr); library(ggplot2); library(knitr) })

ROOT   <- "/Users/marco/Desktop/Tesi magistrale"
OUT    <- file.path(ROOT, "output")
CUTOFF <- as.Date("2024-03-31")
H0     <- 7          # operational primary horizon (Section 8 due at year 6, resolved ~7)

d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  mutate(cancelled  = as.numeric(cancelled) == 1,
         is_service = as.numeric(is_service),
         scheme     = as_factor(svc_scheme),
         age_obs    = as.numeric(CUTOFF - registration_dt) / 365.25)

# --- f(h) and Services-Goods gap (percentage points), with binomial CI ------
fail_gap <- function(dat, h){
  x <- dat |> filter(is_service %in% c(0,1), age_obs >= h) |>
    mutate(fail = as.integer(cancelled & t_years <= h)) |>
    group_by(is_service) |> summarise(p = mean(fail), n = n(), .groups = "drop")
  pg <- x$p[x$is_service==0]; ng <- x$n[x$is_service==0]
  ps <- x$p[x$is_service==1]; ns <- x$n[x$is_service==1]
  se <- sqrt(pg*(1-pg)/ng + ps*(1-ps)/ns)
  tibble(fail_goods = pg, fail_serv = ps, gap = ps-pg, se = se,
         lo = ps-pg-1.96*se, hi = ps-pg+1.96*se, n = ng+ns)
}

# ---- (A) HORIZON ROBUSTNESS: baseline (primary class), h from 6 to 8 --------
dbase <- d |> filter(is_service %in% c(0,1))
hgrid <- c(6, 6.5, 7, 7.5, 8)
A <- bind_rows(lapply(hgrid, function(h)
       fail_gap(dbase, h) |> mutate(spec = sprintf("h = %.1f years", h),
                block = if (h < 7) "Pre-grace (premature)" else "Horizon h (7-8, valid)")))

# ---- (B) RULE ROBUSTNESS: baseline vs pure-only, at h = H0 ------------------
dpure <- d |> filter(scheme %in% c("Pure goods","Pure services")) |>
  mutate(is_service = ifelse(scheme == "Pure services", 1, 0))
B <- bind_rows(
  fail_gap(dbase, H0) |> mutate(spec = "Baseline (primary class)", block = "Goods/services rule"),
  fail_gap(dpure, H0) |> mutate(spec = "Pure marks only (no mixed)", block = "Goods/services rule"))

# where do the 'mixed' marks sit?
dmix  <- d |> filter(scheme == "Mixed", age_obs >= H0) |> mutate(fail = as.integer(cancelled & t_years <= H0))
mix_f <- mean(dmix$fail)

allspecs <- bind_rows(A, B) |> mutate(gap_pp = gap*100, lo_pp = lo*100, hi_pp = hi*100,
  block = factor(block, levels = c("Pre-grace (premature)",
                                   "Horizon h (7-8, valid)", "Goods/services rule")))

# ---- ROBUSTNESS FIGURE (dot-and-whisker) -----------------------------------
ord <- c("h = 6.0 years","h = 6.5 years","h = 7.0 years","h = 7.5 years","h = 8.0 years",
         "Baseline (primary class)","Pure marks only (no mixed)")
allspecs <- allspecs |> mutate(spec = factor(spec, levels = rev(ord)))
gp <- ggplot(allspecs, aes(gap_pp, spec, color = block)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey55") +
  geom_errorbarh(aes(xmin = lo_pp, xmax = hi_pp), height = .22, linewidth = .7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Pre-grace (premature)" = "#9A9A9A",
                                "Horizon h (7-8, valid)" = "#185FA5",
                                "Goods/services rule" = "#993C1D")) +
  labs(title = "Robustness of the goods/services gap at the first milestone (Section 8)",
       subtitle = "Services - Goods gap in non-maintenance; bars = 95% CI.\nBelow 7 years the Section 8 outcome is not yet recorded (grace period).",
       x = "Services - Goods gap in Section 8 non-maintenance (percentage points)", y = NULL, color = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_robustness.png"), gp, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(OUT, "fig_robustness.pdf"), gp, width = 9, height = 5.5)

# ---- TABLE (CSV + HTML) ----------------------------------------------------
tab <- allspecs |> arrange(desc(block), spec) |>
  transmute(Specification = as.character(spec), Block = block,
            `Goods f(h)` = round(fail_goods,3), `Services f(h)` = round(fail_serv,3),
            `Gap (pp)` = round(gap_pp,2), `95% CI` = sprintf("[%.2f, %.2f]", lo_pp, hi_pp),
            N = n)
write.csv(tab, file.path(OUT, "tab_robustness.csv"), row.names = FALSE)
writeLines(kable(tab, format = "html"), file.path(OUT, "tab_robustness.html"))

# ---- console ---------------------------------------------------------------
cat("=== (A) HORIZON robustness (baseline, primary class) ===\n")
print(A |> transmute(spec, goods = round(fail_goods,3), services = round(fail_serv,3),
                     gap_pp = round(gap*100,2), n))
cat("\n=== (B) RULE robustness (h = 7) ===\n")
print(B |> transmute(spec, goods = round(fail_goods,3), services = round(fail_serv,3),
                     gap_pp = round(gap*100,2), n))
cat(sprintf("\n'Mixed' marks at h=7: f = %.3f  (baseline: goods %.3f, services %.3f)\n",
            mix_f, B$fail_goods[1], B$fail_serv[1]))