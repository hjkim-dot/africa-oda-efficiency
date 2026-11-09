suppressMessages({library(dplyr); library(tidyr); library(Benchmarking)})
df <- readRDS("analysis_df.rds") |> filter(year >= 2006)
yrs <- sort(unique(df$year))
run <- function(c0, mode = "shift") {
  bind_rows(lapply(yrs, function(yr) {
    s <- df |> filter(year == yr) |> drop_na(oda_pc, gdp_growth, infant_survival, sec_enroll)
    x <- as.matrix(s$oda_pc); y <- as.matrix(s[, c("gdp_growth", "infant_survival", "sec_enroll")])
    neg <- any(y[, 1] <= 0)
    if (mode == "shift" && neg) y[, 1] <- y[, 1] - min(y[, 1]) + c0
    if (mode == "always") y[, 1] <- y[, 1] - min(y[, 1]) + c0      # shift every year the same way
    tibble(iso3c = s$iso3c, year = yr, neg = neg,
           bcc = eff(dea(x, y, RTS = "vrs", ORIENTATION = "in")),
           ccr = eff(dea(x, y, RTS = "crs", ORIENTATION = "in")))
  }))
}
base <- run(1)
cat("years with growth <= 0 (shifted):", paste(unique(base$year[base$neg]), collapse = ", "), "\n")
cat("n negative obs:", sum(df$gdp_growth <= 0, na.rm = TRUE), "\n")
for (c0 in c(0.1, 1, 5, 10)) {
  r <- run(c0)
  cat(sprintf("shift c=%-4s  maxdiff BCC vs c=1: %.2e  mean CCR %.3f  mean SE %.3f\n", c0,
              max(abs(r$bcc - base$bcc)), mean(r$ccr), mean(r$ccr / r$bcc)))
}
for (c0 in c(1, 10)) {
  r <- run(c0, "always")
  cat(sprintf("ALWAYS shift c=%-3s maxdiff BCC: %.2e  mean CCR %.3f  mean SE %.3f\n", c0,
              max(abs(r$bcc - base$bcc)), mean(r$ccr), mean(r$ccr / r$bcc)))
}
cases <- c("ZAF", "MUS", "SEN", "MOZ")
tab <- function(r, lab) r |> filter(iso3c %in% cases) |> group_by(iso3c) |>
  summarise(se = round(mean(ccr / bcc), 3), ccr = round(mean(ccr), 3), .groups = "drop") |> mutate(spec = lab)
print(bind_rows(tab(base, "c=1 (논문)"), tab(run(0.1), "c=0.1"), tab(run(10), "c=10"), tab(run(1, "always"), "매년 c=1")) |>
        pivot_wider(names_from = spec, values_from = c(ccr, se)), width = 200)
# CCR only in years with no shift
nb <- base |> filter(!neg)
cat("years without shift:", length(unique(nb$year)), " mean CCR there", round(mean(nb$ccr), 3), "\n")
# rank correlation of country mean CCR across specs
m <- function(r) r |> group_by(iso3c) |> summarise(v = mean(ccr)) |> arrange(iso3c) |> pull(v)
cat("Spearman country CCR: c=1 vs c=0.1", round(cor(m(base), m(run(0.1)), method = "spearman"), 3),
    " c=1 vs c=10", round(cor(m(base), m(run(10)), method = "spearman"), 3),
    " c=1 vs always", round(cor(m(base), m(run(1, "always")), method = "spearman"), 3), "\n")
