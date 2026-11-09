# ============================================================
# 04. 패널단위근 검정 - Hadri (변수 하나에 결과 하나)
# ============================================================
# IPS, Levin-Lin-Chu 는 T=17 이 너무 짧아 계산이 되지 않습니다.
# Hadri 검정은 계산이 되지만 귀무가설이 반대입니다.
#   귀무가설 = "모든 국가가 정상"  ->  p < 0.05 이면 "비정상"
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tibble); library(purrr); library(readr); library(plm)
df <- readRDS("analysis_df.rds")
vars <- c("dea_score", "wgi_lag1", "nat_resource", "mobile", "ln_gdp_pc")

pd <- df |> filter(year >= 2006) |> select(iso3c, year, all_of(vars)) |> as.data.frame()
cat("국가별 관측치 수 (모두 같아야 균형패널):\n"); print(table(pd$iso3c))
pdata <- pdata.frame(pd, index = c("iso3c", "year"))

res <- map_dfr(vars, function(v) {
  t <- tryCatch(purtest(pdata[[v]], test = "hadri", exo = "trend"), error = function(e) NULL)
  if (is.null(t)) return(tibble(변수 = v, z = NA_real_, p = NA_real_, 판정 = "계산 실패"))
  z <- tryCatch(unname(t$statistic$statistic), error = function(e) NA_real_)
  p <- tryCatch(t$statistic$p.value, error = function(e) NA_real_)
  tibble(변수 = v, z = round(z, 3), p = signif(p, 3), 판정 = ifelse(p < 0.05, "비정상", "정상"))
})
print(res)
dir.create("output", showWarnings = FALSE)
write_excel_csv(res, "output/hadri_result.csv")

# ---- 2) 설명변수 4개: 처리 전 vs 처리 후 (변수당 결과 1개) --------------------
# 처리 형태: 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분)
# 차분하면 앞 연도가 사라지므로 4개 변수가 모두 있는 2009~2022년(18개국 x 14년 균형패널)으로 비교합니다.
# 귀무가설 = "모든 국가가 정상"  ->  p >= 0.05 이면 정상, p < 0.05 이면 비정상
vars_t <- c("거버넌스" = "d3_ln_wgi", "천연자원" = "d2_nat", "모바일" = "d3_mobile", "1인당GDP" = "d3_gdp")
vars_o <- c("거버넌스" = "wgi_lag1", "천연자원" = "nat_resource", "모바일" = "mobile", "1인당GDP" = "ln_gdp_pc")
hadri_p <- function(v, exo) {
  d <- df |> dplyr::filter(year >= 2009) |> dplyr::select(iso3c, year, dplyr::all_of(v)) |> as.data.frame()
  pdx <- pdata.frame(d, index = c("iso3c", "year"))
  t <- tryCatch(purtest(pdx[[v]], test = "hadri", exo = exo), error = function(e) NULL)
  if (is.null(t)) NA_real_ else as.numeric(t$statistic$p.value)
}
hadri_x <- tibble(
  변수 = names(vars_t),
  처리 = c("로그+3차 차분", "2차 차분", "3차 차분", "3차 차분"),
  처리전_p = sapply(names(vars_t), function(n) signif(hadri_p(vars_o[[n]], "trend"), 3)),
  처리후_p_상수 = sapply(names(vars_t), function(n) round(hadri_p(vars_t[[n]], "intercept"), 3)),
  처리후_p_추세 = sapply(names(vars_t), function(n) round(hadri_p(vars_t[[n]], "trend"), 3))
) |> mutate(처리후_판정 = ifelse(처리후_p_상수 >= 0.05, "정상", "비정상"))
cat("\n== 설명변수 4개: Hadri 패널 검정, 처리 전 vs 처리 후 (2009~2022, 18개국 x 14년) ==\n")
print(hadri_x)
write_excel_csv(hadri_x, "output/hadri_transformed.csv")
