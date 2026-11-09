# ============================================================
# 01. DEA 효율성 분석 (BCC / CCR / 규모효율성)
# ============================================================
# 투입 1개 : oda_pc
# 산출 3개 : gdp_growth(불변가격), infant_survival, sec_enroll
# 모형     : 투입지향, 연도별로 그 해 18개국을 서로 비교
# 결과     : dea_results_full.csv 저장 + analysis_df.rds 의 dea_score 갱신
# (dea_score 가 이미 있어도 이 스크립트는 새로 계산해서 비교해 줍니다)
# 필요한 컬럼: oda_pc, gdp_growth, infant_survival, sec_enroll
#  -> 컬럼이 없는 축소판 data.xlsx 라면 이 스크립트는 건너뛰어도 됩니다.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(Benchmarking)

df <- readRDS("analysis_df.rds")
need <- c("oda_pc", "gdp_growth", "infant_survival", "sec_enroll")
miss <- setdiff(need, names(df))
if (length(miss) > 0) stop("DEA 에 필요한 컬럼이 없습니다: ", paste(miss, collapse = ", "))

if (isTRUE(sd(df$gdp_growth, na.rm = TRUE) == 0) || all(is.na(df$gdp_growth)))
  stop("gdp_growth 가 비정상입니다(전부 0 또는 결측). 00_data_prep.R 를 먼저 다시 실행하세요.")

run_dea_year <- function(d, yr) {
  sub <- d |> filter(year == yr) |> drop_na(all_of(need))
  if (nrow(sub) < 3) return(NULL)                    # 2005년처럼 성장률이 없는 해는 자동 제외
  x <- as.matrix(sub$oda_pc)
  y <- as.matrix(sub[, c("gdp_growth", "infant_survival", "sec_enroll")])
  # 성장률이 0 이하인 나라가 있으면 그 해 최솟값만큼 올려서 양수로 (BCC 는 이 변환에 영향 없음)
  if (any(y[, 1] <= 0)) y[, 1] <- y[, 1] - min(y[, 1]) + 1
  fit_bcc <- dea(x, y, RTS = "vrs", ORIENTATION = "in")   # 가변규모수익
  fit_ccr <- dea(x, y, RTS = "crs", ORIENTATION = "in")   # 불변규모수익
  # (주의) 열 이름과 같은 이름의 객체를 쓰면 tibble 안에서 충돌하므로 fit_ 접두사를 붙였습니다
  tibble(country = sub$country, iso3c = sub$iso3c, year = yr,
         bcc = eff(fit_bcc), ccr = eff(fit_ccr), se = eff(fit_ccr) / eff(fit_bcc))
}

dea_results <- map_dfr(sort(unique(df$year)), ~ run_dea_year(df, .x))

summary_by_country <- dea_results |>
  group_by(country) |>
  summarise(BCC평균 = round(mean(bcc), 3), BCC_sd = round(sd(bcc), 3),
            CCR평균 = round(mean(ccr), 3), CCR_sd = round(sd(ccr), 3),
            규모효율성 = round(mean(se), 3), .groups = "drop") |>
  arrange(desc(BCC평균))
print(summary_by_country, n = 20)
cat("\n전체 평균  BCC:", round(mean(dea_results$bcc), 3),
    " CCR:", round(mean(dea_results$ccr), 3), "\n")
cat("계산된 관측치:", nrow(dea_results), "(306 이면 정상)\n")

# 저장
write_excel_csv(dea_results, "dea_results_full.csv")
write_excel_csv(summary_by_country, "output/dea_summary_by_country.csv")

# 기존 dea_score 와 비교한 뒤 갱신
old <- df$dea_score
df <- df |> select(-any_of(c("dea_score", "ccr", "se"))) |>
  left_join(dea_results |> select(iso3c, year, dea_score = bcc, ccr, se),
            by = c("iso3c", "year"))
if (!is.null(old) && sum(!is.na(old)) > 0)
  cat("기존 dea_score 와의 최대 차이:", signif(max(abs(old - df$dea_score), na.rm = TRUE), 3), "\n")
saveRDS(df, "analysis_df.rds")
