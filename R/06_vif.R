# ============================================================
# 06. VIF (다중공선성)  5 넘으면 주의, 10 넘으면 심각
# ============================================================
# VIF 는 설명변수들끼리의 관계만 보므로 Tobit 에도 그대로 적용됩니다.
# "연도 더미 있음" 은 연도 더미를 함께 넣은 상태의 VIF 입니다.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(tibble); library(readr)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")

vif_manual <- function(d, xvars, year_dummy) {
  d <- d |> drop_na(dea_score, all_of(xvars))
  sapply(xvars, function(v) {
    rhs <- paste(c(setdiff(xvars, v), if (year_dummy) "factor(year)"), collapse = " + ")
    r2 <- summary(lm(as.formula(paste(v, "~", rhs)), data = d))$r.squared
    1 / (1 - r2)
  })
}

for (mod in c("A", "B", "C")) {
  s <- get_sample(df, mod)
  tab <- tibble(변수 = unname(LAB[s$x]),
                `VIF(연도더미 없음)` = round(vif_manual(s$data, s$x, FALSE), 2),
                `VIF(연도더미 있음)` = round(vif_manual(s$data, s$x, TRUE), 2))
  cat("\n== 모형", mod, "==\n"); print(tab)
}
cat("\n== 상관계수 (모형 A 표본) ==\n")
sA <- get_sample(df, "A")
print(round(cor(sA$data |> drop_na(dea_score, all_of(sA$x)) |> select(all_of(sA$x))), 2))

# ============================================================
# 차분 설명변수의 VIF  (09번 Tobit = 논문 주 분석의 설명변수)
# ============================================================
# 주 분석에는 수준 변수가 아니라 차분한 변수가 들어가므로, 같은 표본에서 그 변수들의 VIF 를 따로 계산합니다.
#   모형 A : 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분) / 에티오피아 제외
#   모형 B : 거버넌스를 뺀 나머지 3개 / 18개국
# 표본은 09번 Tobit 과 같습니다 (종속변수와 설명변수가 모두 있는 관측치).
xA <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
xB <- c("d2_nat", "d3_mobile", "d3_gdp")
LABD <- c(d3_ln_wgi = "거버넌스(로그+3차 차분)", d2_nat = "천연자원(2차 차분)",
          d3_mobile = "모바일(3차 차분)", d3_gdp = "1인당GDP(3차 차분)")
vif_plain <- function(d, xs) {
  sapply(xs, function(v) {
    r2 <- summary(lm(as.formula(paste(v, "~", paste(setdiff(xs, v), collapse = " + "))), data = d))$r.squared
    1 / (1 - r2)
  })
}
dA <- df |> filter(iso3c != "ETH") |> drop_na(dea_score, all_of(xA)) |> as.data.frame()
dB <- df |> drop_na(dea_score, all_of(xB)) |> as.data.frame()
vif_diff <- bind_rows(
  tibble(모형 = "A", N = nrow(dA), 변수 = unname(LABD[xA]), VIF = round(vif_plain(dA, xA), 2)),
  tibble(모형 = "B", N = nrow(dB), 변수 = unname(LABD[xB]), VIF = round(vif_plain(dB, xB), 2))
)
cat("\n== 차분 설명변수 VIF (09번 Tobit 표본) ==\n"); print(vif_diff)
cat("\n== 차분 설명변수 상관계수 (모형 A 표본) ==\n")
cor_diff <- round(cor(dA[, xA]), 2); dimnames(cor_diff) <- list(unname(LABD[xA]), unname(LABD[xA])); print(cor_diff)
dir.create("output", showWarnings = FALSE)
write_excel_csv(vif_diff, "output/vif_diff.csv")
