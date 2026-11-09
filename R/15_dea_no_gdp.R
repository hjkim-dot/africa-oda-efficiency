# ============================================================
# 15_dea_no_gdp.R : GDP 성장률을 제외한 DEA 재추정 (부록 7)
# ============================================================
#  - 산출변수에서 GDP 성장률을 빼고 영아 생존율·중등교육 등록률만으로
#    투입지향 BCC 점수를 연도별로 다시 계산 (투입: 1인당 ODA, 01_dea.R 와 같은 방식)
#  - 새 점수로 본문 표 7의 (A)(B)(C)와 표 12의 호황기 모형을 다시 추정 -> 부록 표 11·12
#  - 국가별 평균 점수와 순위 -> 부록 표 13
#  필요한 파일(같은 폴더): data.xlsx, dea_results_full.csv(00~01 실행 후)
#  실행: source("15_dea_no_gdp.R", encoding = "UTF-8")
# ============================================================
suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(readxl); library(readr)
  library(Benchmarking); library(censReg); library(sandwich); library(lmtest)
})

d0 <- read_excel("data.xlsx", sheet = "data") |> filter(year >= 2006)

# 국가별 묶음 표준오차: Tobit 은 14_panel_tobit_level.R 과 같은 방식(G/(G-1) 보정), OLS 는 vcovCL
vc_cl <- function(m, cl) {
  if (inherits(m, "lm")) return(vcovCL(m, cluster = cl))
  G <- length(unique(cl)); sc <- rowsum(estfun(m), cl); V <- vcov(m)
  V %*% crossprod(sc) %*% V * G / (G - 1)
}

# ---- 1) GDP 성장률을 뺀 BCC 점수 (연도별) ----
nogdp <- map_dfr(sort(unique(d0$year)), function(yr) {
  s <- d0 |> filter(year == yr) |> drop_na(oda_pc, infant_survival, sec_enroll)
  f <- dea(as.matrix(s$oda_pc), as.matrix(s[, c("infant_survival", "sec_enroll")]),
           RTS = "vrs", ORIENTATION = "in")
  tibble(iso3c = s$iso3c, year = yr, bcc_nogdp = eff(f))
})
d <- d0 |> left_join(nogdp, by = c("iso3c", "year"))
cat("18개국 평균: 원래", round(mean(d$dea_score), 3), "/ 제외", round(mean(d$bcc_nogdp), 3),
    "/ 상관계수", round(cor(d$dea_score, d$bcc_nogdp), 3), "\n")

# ---- 2) 부록 표 11: 모형 (A)(B)(C), 국가별 묶음 표준오차 ----
X  <- "wgi_lag1 + nat_resource + mobile + ln_gdp_pc + factor(year)"
dB <- d |> filter(!iso3c %in% c("ZAF", "MUS"))
show <- function(lab, m, dd) {
  ct <- coeftest(m, vcov = vc_cl(m, dd$iso3c), df = if (inherits(m, "lm")) m$df.residual else Inf)
  v  <- c("wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc")
  cat(sprintf("%-14s", lab), sprintf("%s=%.4f[p=%.3f]", v, ct[v, 1], ct[v, 4]), "\n")
}
for (y in c("dea_score", "bcc_nogdp")) {
  cat("== 종속변수:", y, "==\n")
  show("(A) 풀링", censReg(as.formula(paste(y, "~", X)), left = 0, right = 1, data = d), d)
  show("(B) 국가더미", censReg(as.formula(paste(y, "~", X, "+ factor(iso3c)")), left = 0, right = 1, data = dB), dB)
  show("(C) FE OLS", lm(as.formula(paste(y, "~", X, "+ factor(iso3c)")), data = d), d)
}

# ---- 3) 부록 표 12: 호황기(2006~2014) 구분 ----
d2  <- d |> mutate(boom = as.numeric(year <= 2014), nat_boom = nat_resource * boom, nat_post = nat_resource * (1 - boom))
dB2 <- d2 |> filter(!iso3c %in% c("ZAF", "MUS"))
for (y in c("dea_score", "bcc_nogdp")) for (nm in c("A", "B")) {
  dd <- if (nm == "A") d2 else dB2
  fe <- if (nm == "B") " + factor(iso3c)" else ""
  m  <- censReg(as.formula(paste0(y, " ~ wgi_lag1 + nat_boom + nat_post + mobile + ln_gdp_pc + factor(year)", fe)),
                left = 0, right = 1, data = dd)
  V <- vc_cl(m, dd$iso3c); b <- coef(m); ct <- coeftest(m, vcov = V, df = Inf)
  dif <- b["nat_boom"] - b["nat_post"]
  se  <- sqrt(V["nat_boom", "nat_boom"] + V["nat_post", "nat_post"] - 2 * V["nat_boom", "nat_post"])
  cat(y, nm, sprintf("호황기=%.4f[p=%.3f] 이후=%.4f[p=%.3f] 차이=%.4f[p=%.3f]\n",
      ct["nat_boom", 1], ct["nat_boom", 4], ct["nat_post", 1], ct["nat_post", 4], dif, 2 * pnorm(-abs(dif / se))))
}

# ---- 4) 부록 표 13: 국가별 평균 점수와 순위 ----
tab13 <- d |> group_by(country) |>
  summarise(원래 = mean(dea_score), 제외 = mean(bcc_nogdp), .groups = "drop") |>
  mutate(원래순위 = rank(-round(원래, 3), ties.method = "min"),
         제외순위 = rank(-round(제외, 3), ties.method = "min")) |>
  arrange(제외순위)
print(tab13 |> mutate(across(c(원래, 제외), ~ round(.x, 3))), n = 20)
dir.create("output", showWarnings = FALSE)
write_excel_csv(nogdp, "output/dea_nogdp_scores.csv")
write_excel_csv(tab13, "output/dea_nogdp_by_country.csv")
