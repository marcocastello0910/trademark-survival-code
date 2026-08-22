suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(survival)
  library(ggplot2); library(scales); library(knitr)
})

ROOT <- Sys.getenv("THESIS_ROOT", unset = ".")
OUT  <- file.path(ROOT, "output")
COL  <- c("Goods" = "#185FA5", "Services" = "#D85A30")

d <- read_dta(file.path(OUT, "tm_survival.dta")) |>
  filter(is_service %in% c(0, 1)) |>
  mutate(cancelled = as.numeric(cancelled) == 1, is_service = as.numeric(is_service))

# ---- raw registration counts (the flow indicator) --------------------------
Ng <- sum(d$is_service == 0); Ns <- sum(d$is_service == 1)

# ---- survival weights per sector (KM) --------------------------------------
fitG <- survfit(Surv(t_years, cancelled) ~ 1, data = filter(d, is_service == 0))
fitS <- survfit(Surv(t_years, cancelled) ~ 1, data = filter(d, is_service == 1))
Sfun <- function(fit, t) summary(fit, times = t)$surv
rmst <- function(fit, tau) {                    # area under the KM curve up to tau
  tt <- c(0, fit$time); ss <- c(1, fit$surv); k <- tt <= tau
  tt <- c(tt[k], tau); ss <- c(ss[k], tail(ss[k], 1))
  sum(diff(tt) * head(ss, -1))
}

# ---- assemble specifications (Form 1 at h=7 and h=10; Form 2 mark-years) ----
raw_ratio <- Ns / Ng
specs <- tibble(
  spec  = c("Raw count", "Survivors @ 7y", "Survivors @ 10y", "Mark-years @ 10y"),
  wG    = c(1, Sfun(fitG, 7), Sfun(fitG, 10), rmst(fitG, 10)),
  wS    = c(1, Sfun(fitS, 7), Sfun(fitS, 10), rmst(fitS, 10))) |>
  mutate(goods = Ng * wG, services = Ns * wS,
         services_share = 100 * services / (goods + services),
         ratio_overstated_pct = 100 * (raw_ratio / (services / goods) - 1))

raw_sh <- specs$services_share[1]
# reported share to 1 dp; bias = difference of the REPORTED shares (so the table reconciles)
specs <- specs |> mutate(share_disp = round(services_share, 1),
                         bias_pp    = round(raw_sh, 1) - share_disp)

# =============================================================================
# FIGURE 1 — the bias: services share, raw vs adjusted (lollipop, zoomed y)
# =============================================================================
sdf <- specs |> mutate(spec = factor(spec, levels = spec),
                       type = ifelse(spec == "Raw count", "Raw", "Adjusted"))
fig1 <- ggplot(sdf, aes(spec, services_share)) +
  geom_hline(yintercept = raw_sh, linetype = 2, color = "grey55") +
  geom_segment(aes(xend = spec, y = raw_sh, yend = services_share), color = "grey75") +
  geom_point(aes(color = type), size = 4.5) +
  geom_text(aes(label = sprintf("%.1f%%", services_share)), vjust = -1.1, size = 3.4) +
  scale_color_manual(values = c("Raw" = "#9A9A9A", "Adjusted" = "#185FA5"), name = NULL) +
  coord_cartesian(ylim = c(33, 38)) +
  labs(title = "Raw counts overstate the services share of trademark activity",
       subtitle = "Services share of registered-mark activity, raw vs survival-adjusted (dashed line = raw level)",
       x = NULL, y = "Services share of goods + services (%)") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_rq2_share.png"), fig1, width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT, "fig_rq2_share.pdf"), fig1, width = 9, height = 5)

# =============================================================================
# FIGURE 2 — raw vs adjusted counts, by sector (primary horizon h=7)
# =============================================================================
cdf <- tibble(Sector  = rep(c("Goods", "Services"), 2),
              Measure = factor(rep(c("Raw registrations", "Survivors at 7 years"), each = 2),
                               levels = c("Raw registrations", "Survivors at 7 years")),
              count   = c(Ng, Ns, specs$goods[2], specs$services[2]) / 1e6)
fig2 <- ggplot(cdf, aes(Sector, count, fill = Measure)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.2fM", count)), position = position_dodge(0.7),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Raw registrations" = "#B4B2A9", "Survivors at 7 years" = "#185FA5"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Raw registrations vs survival-adjusted counts, by sector",
       subtitle = "Survivors to 7 years = raw count x S(7); services shrink proportionally more",
       x = NULL, y = "Registered marks (millions)") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(OUT, "fig_rq2_counts.png"), fig2, width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT, "fig_rq2_counts.pdf"), fig2, width = 9, height = 5)

# =============================================================================
# TABLE (CSV + HTML)
# =============================================================================
tab <- specs |> transmute(
  Specification = spec,
  `Goods (count)` = round(goods),
  `Services (count)` = round(services),
  `Services share (%)` = share_disp,
  `Bias vs raw (pp)` = bias_pp,
  `Services/Goods overstated (%)` = round(ratio_overstated_pct, 1))
write.csv(tab, file.path(OUT, "tab_rq2.csv"), row.names = FALSE)
writeLines(kable(tab, format = "html"), file.path(OUT, "tab_rq2.html"))

# ---- console ---------------------------------------------------------------
cat(sprintf("Raw registration counts: Goods %s | Services %s | services share %.1f%%\n\n",
            format(Ng, big.mark = ","), format(Ns, big.mark = ","), raw_sh))
print(as.data.frame(tab))