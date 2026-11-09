# ============================================================
# 03. ADF 정상성 검정 (국가별로 반복)
# ============================================================
# 귀무가설 = "비정상(단위근 있음)".  p < 0.05 이면 정상.
# 주의: R 의 tseries::adf.test 는 p값을 0.01 ~ 0.99 범위로만 보여줍니다.
#       (0.01 보다 작으면 0.01 로 표시하고 경고를 냅니다. 아래에서 경고는 숨겼습니다.)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(tseries)
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

adf_p <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 8) return(NA_real_)                 # 관측치가 8개 미만이면 검정 불가
  tryCatch(suppressWarnings(tseries::adf.test(x, k = 1)$p.value), error = function(e) NA_real_)
}
by_country <- function(v) {
  df |> group_by(iso3c) |> summarise(p = adf_p(.data[[v]]), .groups = "drop") |> mutate(변수 = v)
}

# ---- 1) 변환 단계별 비교 (원본 -> 차분 1·2·3차) --------------------------
series <- c("dea_score",
            "wgi_lag1", "ln_wgi", "d1_ln_wgi", "d2_ln_wgi", "d3_ln_wgi",
            "nat_resource", "d1_nat", "d2_nat", "d3_nat",
            "mobile", "d1_mobile", "d2_mobile", "d3_mobile",
            "ln_gdp_pc", "d1_gdp", "d2_gdp", "d3_gdp")
adf_all <- map_dfr(series, by_country)
adf_ratio <- adf_all |> group_by(변수) |>
  summarise(검정국가수 = sum(!is.na(p)), 비정상국가수 = sum(p >= 0.05, na.rm = TRUE),
            비정상비율 = round(비정상국가수 / 검정국가수, 3), .groups = "drop") |>
  arrange(match(변수, series))
cat("== 변수·변환별 비정상 비율 ==\n"); print(adf_ratio, n = 30)
write_excel_csv(adf_all, "output/adf_all_detail.csv")
write_excel_csv(adf_ratio, "output/adf_ratio.csv")

# ---- 2) 최종 처리 형태: 국가별 원본 p -> 처리 후 p ---------------------------
#   거버넌스: 로그+3차 차분 / 천연자원: 2차 차분 / 모바일: 3차 차분 / 1인당GDP: 3차 차분
pairs <- tribble(~lab, ~orig, ~trans,
                 "거버넌스", "wgi_lag1", "d3_ln_wgi",
                 "천연자원", "nat_resource", "d2_nat",
                 "모바일", "mobile", "d3_mobile",
                 "1인당GDP", "ln_gdp_pc", "d3_gdp")
final_tab <- pmap_dfr(pairs, function(lab, orig, trans) {
  a <- by_country(orig) |> select(iso3c, 원본p = p)
  b <- by_country(trans) |> select(iso3c, 변환p = p)
  left_join(a, b, by = "iso3c") |> mutate(변수 = lab, .before = 1)
})
fmt <- function(o, t) sprintf("%.2f->%.2f %s", o, t, ifelse(t < 0.05, "O", "X"))   # O=정상(통과) X=비정상
cnt <- final_tab |> group_by(iso3c) |>
  summarise(통과수 = sum(변환p < 0.05, na.rm = TRUE), .groups = "drop")
compact <- final_tab |>
  mutate(셀 = fmt(원본p, 변환p)) |> select(iso3c, 변수, 셀) |>
  pivot_wider(names_from = 변수, values_from = 셀) |>
  left_join(cnt, by = "iso3c") |> arrange(desc(통과수), iso3c)
cat("\n== 국가별 (원본p -> 처리 후 p, O=통과 X=비정상) ==\n")
print(compact, n = 20, width = Inf)
cat("\n4개 모두 통과한 국가 수:", sum(compact$통과수 == 4),
    "| 3개 이상 통과한 국가 수:", sum(compact$통과수 >= 3), "\n")
write_excel_csv(final_tab, "output/adf_final_by_country.csv")
write_excel_csv(compact, "output/adf_final_compact.csv")
