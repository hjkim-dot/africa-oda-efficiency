# ============================================================
# 09. 차분 변수 Tobit (비교용)
# ============================================================
# 종속변수 dea_score 는 수준값 그대로, 설명변수만 차분합니다.
#   거버넌스: 로그 후 3차 차분(d3_ln_wgi)   천연자원: 2차 차분(d2_nat)
#   모바일  : 3차 차분(d3_mobile)           1인당GDP: 3차 차분(d3_gdp)
# 모형 A : 거버넌스 포함, 에티오피아 제외 / 모형 B : 거버넌스 제외, 18개국
# 3차 차분이라 앞의 몇 개 연도가 사라집니다 (모형 A 약 238개, 모형 B 약 270개 관측치).
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(tibble); library(readr)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

xA <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
xB <- c("d2_nat", "d3_mobile", "d3_gdp")
rA <- fit_tobit(df |> filter(iso3c != "ETH"), xA, year_dummy = FALSE)
rB <- fit_tobit(df, xB, year_dummy = FALSE)

for (nm in c("A", "B")) {
  r <- if (nm == "A") rA else rB
  cat("\n=== 모형", nm, "(차분) | N =", r$N, "| 국가 =", r$G, "| 1.0절단 =", r$cens, "===\n")
  print(r$table |> mutate(across(where(is.numeric), ~ round(.x, 4)), 별 = stars(p)), width = Inf)
  print(summary(r$model))
}
write_excel_csv(bind_rows(rA$table |> mutate(모형 = "A", .before = 1),
                          rB$table |> mutate(모형 = "B", .before = 1)),
                "output/tobit_diff_results.csv")
