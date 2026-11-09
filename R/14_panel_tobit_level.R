# ============================================================
# 14. 수준 변수 패널 Tobit  (국가·연도 효과를 넣은 모형들)
# ============================================================
# 설명변수를 차분하지 않고 수준 그대로 쓰되, 국가 효과와 연도 효과로 나라 간 차이와 공통 추세를 통제합니다.
#   종속변수 : dea_score (0~1 양방향 Tobit)
#   설명변수 : 거버넌스(t-1), 천연자원, 모바일, ln(1인당GDP)   (수준값)
#   표본     : 18개국 x 2006~2022 = 306개
# 모형
#   (1) 풀링 Tobit + 연도 더미         표준오차: 국가별 묶음(cluster)
#   (2) 국가 더미 + 연도 더미 Tobit    (고정효과 방식). 효율성이 늘 1.0 인 남아공·모리셔스는 제외(16개국)
#   (3) 참고: 국가·연도 고정효과 OLS (절단 무시), 묶음 표준오차
#   (참고) 임의효과 패널 Tobit 은 계산이 불안정해(최적화 방법·적분 점 수에 따라 계수가 크게 달라짐)
#          결과표에서 제외하고 불안정한 정도만 output/re_tobit_diagnostics.csv 로 보고합니다.
# 읽는 법: (1)은 나라 간 수준 차이까지 포함한 관계, (2)·(3)은 한 나라 안에서 시간에 따른 변화만 본 관계입니다.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(censReg); library(plm); library(sandwich); library(lmtest)
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

xs  <- c("wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc")
LAB <- c(wgi_lag1 = "거버넌스(t-1)", nat_resource = "천연자원", mobile = "모바일", ln_gdp_pc = "ln(1인당GDP)")
d   <- df |> drop_na(dea_score, all_of(xs)) |> as.data.frame()
rhs <- paste(xs, collapse = " + ")
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
pack <- function(model, b, se, p, N, G, cens, note = "") {
  tibble(모형 = model, 변수 = unname(LAB[xs]), 계수 = unname(b[xs]), se = unname(se[xs]), p = unname(p[xs]),
         N = N, 국가수 = G, 절단 = cens, 비고 = note)
}
cl_p <- function(m, b, cluster) {                                  # 묶음 표준오차 기반 p
  vc <- vcovCL(m, cluster = cluster); se <- sqrt(diag(vc)); list(se = se, p = 2 * pnorm(-abs(b / se)))
}

# (1) 풀링 Tobit + 연도 더미
m1 <- censReg(as.formula(paste("dea_score ~", rhs, "+ factor(year)")), left = 0, right = 1, data = d)
b1 <- coef(m1); c1 <- cl_p(m1, b1, d$iso3c)
r1 <- pack("(1) 풀링 Tobit+연도", b1, c1$se, c1$p, nrow(d), n_distinct(d$iso3c), sum(d$dea_score >= 1 - 1e-9), "국가별 묶음 SE")

# (참고) 임의효과 패널 Tobit: 최적화 방법과 적분 점 수를 바꿔 가며 안정한지만 확인합니다.
pd <- pdata.frame(d, index = c("iso3c", "year"))
re_diag <- map_dfr(list(c("NR", 8), c("NR", 16), c("BFGS", 8), c("BFGS", 16)), function(a) {
  m <- tryCatch(suppressWarnings(censReg(as.formula(paste("dea_score ~", rhs, "+ factor(year)")), left = 0, right = 1,
                                         data = pd, method = a[1], nGHQ = as.integer(a[2]))), error = function(e) NULL)
  if (is.null(m)) return(tibble(방법 = a[1], 적분점 = as.integer(a[2]), 로그우도 = NA_real_, 수렴코드 = NA_integer_,
                                거버넌스 = NA_real_, 천연자원 = NA_real_, 모바일 = NA_real_, GDP = NA_real_))
  b <- coef(m)
  tibble(방법 = a[1], 적분점 = as.integer(a[2]), 로그우도 = round(as.numeric(logLik(m)), 3), 수렴코드 = as.integer(m$code),
         거버넌스 = round(unname(b["wgi_lag1"]), 4), 천연자원 = round(unname(b["nat_resource"]), 4),
         모바일 = round(unname(b["mobile"]), 4), GDP = round(unname(b["ln_gdp_pc"]), 4))
})
cat("== 임의효과 패널 Tobit 안정성 점검 (설정에 따라 계수가 크게 달라지면 불안정) ==\n"); print(re_diag)
write_excel_csv(re_diag, "output/re_tobit_diagnostics.csv")

# (2) 국가 더미 + 연도 더미 Tobit
d3 <- d |> filter(!iso3c %in% c("ZAF", "MUS"))
m3 <- censReg(as.formula(paste("dea_score ~", rhs, "+ factor(year) + factor(iso3c)")), left = 0, right = 1, data = d3)
b3 <- coef(m3); c3 <- cl_p(m3, b3, d3$iso3c)
r3 <- pack("(2) 국가+연도 더미 Tobit", b3, c3$se, c3$p, nrow(d3), n_distinct(d3$iso3c), sum(d3$dea_score >= 1 - 1e-9), "남아공·모리셔스 제외, 묶음 SE")

# (3) 양방향 고정효과 OLS (참고)
m4 <- lm(as.formula(paste("dea_score ~", rhs, "+ factor(year) + factor(iso3c)")), data = d)
c4 <- coeftest(m4, vcov. = vcovCL(m4, cluster = d$iso3c))
r4 <- pack("(3) 참고: 양방향 FE OLS", c4[, 1], c4[, 2], c4[, 4], nrow(d), n_distinct(d$iso3c), NA, "절단 무시, 묶음 SE")

res <- bind_rows(r1, r3, r4) |> mutate(별 = stars(p), 표시 = ifelse(is.na(계수), "-", sprintf("%+.4f%s (%.3f)", 계수, 별, p)))
cat("== 수준 변수 패널 Tobit: 계수 (p값) ==\n")
wide <- res |> select(모형, 변수, 표시) |> pivot_wider(names_from = 모형, values_from = 표시)
print(wide, width = Inf)
cat("\n== 표본 ==\n"); print(res |> distinct(모형, N, 국가수, 절단, 비고), width = Inf)
write_excel_csv(res |> select(-표시), "output/panel_tobit_level.csv")

# ---- 변환(차분)한 설명변수의 기초통계 (09번 Tobit 표본: 모형 A, 모형 B) ----------------
xA <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp"); xB <- c("d2_nat", "d3_mobile", "d3_gdp")
LABA <- c(d3_ln_wgi = "거버넌스(로그+3차 차분)", d2_nat = "천연자원(2차 차분)", d3_mobile = "모바일(3차 차분)", d3_gdp = "1인당GDP(3차 차분)")
dA <- df |> filter(iso3c != "ETH") |> drop_na(dea_score, all_of(xA))
dB <- df |> drop_na(dea_score, all_of(xB))
desc_t <- function(dd, vv, lab) map_dfr(vv, function(v) { x <- dd[[v]]
  tibble(표본 = lab, 변수 = unname(LABA[v]), N = length(x), 평균 = round(mean(x), 4), 표준편차 = round(sd(x), 4), 최솟값 = round(min(x), 4), 최댓값 = round(max(x), 4)) })
ds <- bind_rows(desc_t(dA, xA, "모형 A"), desc_t(dB, xB, "모형 B"))
cat("\n== 변환한 설명변수 기초통계 (모형 A, 모형 B 표본) ==\n"); print(ds, n = 10)
write_excel_csv(ds, "output/transformed_descriptive.csv")

# ---- 수준 변수 VIF (5 넘으면 주의, 10 넘으면 심각) -----------------------------------
# 나라 효과까지 넣으면 "한 나라 안에서의 변화"만 남아, 같이 오르는 변수끼리의 상관이 커질 수 있습니다.
vif_set <- function(dd, fe) {
  sapply(xs, function(v) {
    rhs_v <- paste(c(setdiff(xs, v), if (fe >= 1) "factor(year)", if (fe >= 2) "factor(iso3c)"), collapse = " + ")
    1 / (1 - summary(lm(as.formula(paste(v, "~", rhs_v)), data = dd))$r.squared)
  })
}
vif_level <- tibble(변수 = unname(LAB[xs]),
                    `VIF(효과 없음)` = round(vif_set(d, 0), 2),
                    `VIF(연도 더미)` = round(vif_set(d, 1), 2),
                    `VIF(연도+국가 더미)` = round(vif_set(d, 2), 2))
cat("\n== 수준 변수 VIF (306개 관측치) ==\n"); print(vif_level)
write_excel_csv(vif_level, "output/vif_level.csv")
