# ============================================================
# 16_extra_analysis.R : 추가 분석 (본문 표 11·12, 모형 선택 검정)
# ============================================================
#  - 표 11: 거버넌스 세부지표(CC, GE) 분리 / 2020년 제외
#  - 표 12: 천연자원 x 호황기(2006~2014) 상호작용과 두 계수의 차이 검정
#  - 모형 선택: (A) vs (B) 우도비 검정, 하우스만 검정(참고)
#  필요한 파일(같은 폴더): data.xlsx
#  실행: source("16_extra_analysis.R", encoding = "UTF-8")
#  주의: plm 패키지를 먼저 불러오면 lag()가 가려지므로 시차는 dplyr::lag()로 만든다.
# ============================================================
suppressMessages({
  library(dplyr); library(readxl); library(censReg)
  library(sandwich); library(lmtest); library(plm)
})

data <- read_excel("data.xlsx", sheet = "data") |>
  arrange(iso3c, year) |>
  group_by(iso3c) |>
  mutate(wgi_cc_l1 = dplyr::lag(wgi_cc), wgi_ge_l1 = dplyr::lag(wgi_ge)) |>
  ungroup() |>
  filter(year >= 2006) |>
  mutate(boom = as.numeric(year <= 2014),
         nat_boom = nat_resource * boom,
         nat_post = nat_resource * (1 - boom))

tob <- function(f, d) censReg(as.formula(f), left = 0, right = 1, data = d)
# 국가별 묶음 표준오차 (G/(G-1) 보정, 14_panel_tobit_level.R 과 같은 방식)
vc_cl <- function(m, cl) {
  G <- length(unique(cl)); sc <- rowsum(estfun(m), cl); V <- vcov(m)
  V %*% crossprod(sc) %*% V * G / (G - 1)
}
show <- function(lab, m, d, v) {
  ct <- coeftest(m, vcov = vc_cl(m, d$iso3c), df = Inf)
  cat(sprintf("%-22s", lab), sprintf("%s=%.4f[p=%.3f]", v, ct[v, 1], ct[v, 4]), " N =", nrow(d), "\n")
}

C  <- " + nat_resource + mobile + ln_gdp_pc + factor(year)"
V  <- c("nat_resource", "mobile", "ln_gdp_pc")
dB <- data |> filter(!iso3c %in% c("ZAF", "MUS"))   # 모형 (B) 표본

# ---- 표 11 ----
for (nm in c("A", "B")) {
  d  <- if (nm == "A") data else dB
  fe <- if (nm == "B") " + factor(iso3c)" else ""
  cat("===== 표 11 (", nm, ") =====\n")
  show("기준",          tob(paste0("dea_score ~ wgi_lag1", C, fe), d), d, c("wgi_lag1", V))
  show("CC만",          tob(paste0("dea_score ~ wgi_cc_l1", C, fe), d), d, c("wgi_cc_l1", V))
  show("GE만",          tob(paste0("dea_score ~ wgi_ge_l1", C, fe), d), d, c("wgi_ge_l1", V))
  show("CC·GE 동시",    tob(paste0("dea_score ~ wgi_cc_l1 + wgi_ge_l1", C, fe), d), d, c("wgi_cc_l1", "wgi_ge_l1", V))
  d20 <- d |> filter(year != 2020)
  show("2020년 제외",   tob(paste0("dea_score ~ wgi_lag1", C, fe), d20), d20, c("wgi_lag1", V))
}

# ---- 표 12: 호황기 상호작용 ----
cat("===== 표 12 =====\n")
for (nm in c("A", "B")) {
  d  <- if (nm == "A") data else dB
  fe <- if (nm == "B") " + factor(iso3c)" else ""
  m  <- tob(paste0("dea_score ~ wgi_lag1 + nat_boom + nat_post + mobile + ln_gdp_pc + factor(year)", fe), d)
  show(paste0("(", nm, ")"), m, d, c("wgi_lag1", "nat_boom", "nat_post", "mobile", "ln_gdp_pc"))
  Vc  <- vc_cl(m, d$iso3c); b <- coef(m)
  dif <- b["nat_boom"] - b["nat_post"]
  se  <- sqrt(Vc["nat_boom", "nat_boom"] + Vc["nat_post", "nat_post"] - 2 * Vc["nat_boom", "nat_post"])
  cat("   두 계수의 차이:", round(dif, 4), " p =", round(2 * pnorm(-abs(dif / se)), 3), "\n")
}

# ---- 모형 선택 ----
# (1) A vs B 우도비 검정 (국가 더미 15개; 표본은 ZAF·MUS 제외로 동일하게 맞춤)
mA <- tob("dea_score ~ wgi_lag1 + nat_resource + mobile + ln_gdp_pc + factor(year)", dB)
mB <- tob("dea_score ~ wgi_lag1 + nat_resource + mobile + ln_gdp_pc + factor(year) + factor(iso3c)", dB)
lr <- 2 * (as.numeric(logLik(mB)) - as.numeric(logLik(mA)))
cat("LR =", round(lr, 2), " p =", signif(pchisq(lr, df = 15, lower.tail = FALSE), 3), "\n")

# (2) 하우스만 검정 (연도 더미를 포함한 선형 FE vs RE, 참고용)
#     연도 더미를 제외한 4개 주요 계수로 통계량을 계산한다 (자유도 4).
pd <- pdata.frame(data, index = c("iso3c", "year"))
f  <- dea_score ~ wgi_lag1 + nat_resource + mobile + ln_gdp_pc + factor(year)
fe <- plm(f, pd, model = "within"); re <- plm(f, pd, model = "random")
v  <- c("wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc")
q  <- coef(fe)[v] - coef(re)[v]
H  <- as.numeric(t(q) %*% solve(vcov(fe)[v, v] - vcov(re)[v, v]) %*% q)
cat("Hausman chi2 =", round(H, 2), " df = 4  p =", round(pchisq(H, 4, lower.tail = FALSE), 3), "\n")
print(round(rbind(고정효과 = coef(fe)[v], 임의효과 = coef(re)[v]), 4))
