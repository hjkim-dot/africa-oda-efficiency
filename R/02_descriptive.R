# ============================================================
# 02. 기술통계와 상관관계
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
df <- readRDS("analysis_df.rds") |> filter(year >= 2006)   # 분석 기간 2006~2022 (306개 관측치). 2005년은 기준연도라 제외

vars <- intersect(c("oda_pc", "gdp_growth", "infant_survival", "sec_enroll",
                    "dea_score", "wgi_lag1", "nat_resource", "mobile", "gdp_pc_const", "ln_gdp_pc"), names(df))
skew <- function(x) { x <- x[!is.na(x)]; mean((x - mean(x))^3) / (sqrt(mean((x - mean(x))^2))^3) }

desc <- map_dfr(vars, function(v) {
  x <- df[[v]]
  tibble(변수 = v, n = sum(!is.na(x)), 평균 = mean(x, na.rm = TRUE), 표준편차 = sd(x, na.rm = TRUE),
         최솟값 = min(x, na.rm = TRUE), 최댓값 = max(x, na.rm = TRUE), 왜도 = skew(x))
}) |> mutate(across(where(is.numeric), ~ round(.x, 3)))
print(desc, n = 20)

cat("\n== 상관관계 (설명변수 + 효율성) ==\n")
cormat <- df |> select(any_of(c("dea_score", "wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc"))) |>
  cor(use = "pairwise.complete.obs") |> round(3)
print(cormat)

dir.create("output", showWarnings = FALSE)
write_excel_csv(desc, "output/descriptive_stats.csv")
write_excel_csv(as.data.frame(cormat) |> rownames_to_column("변수"), "output/correlation.csv")
