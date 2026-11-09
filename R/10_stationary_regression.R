# ============================================================
# 10. 정상성 있는 변수만 쓴 회귀  (변화 대 변화)
# ============================================================
# 종속변수 : Δ효율성 (dea_score 의 1차 차분)  -> 효율성이 해마다 얼마나 변했나
# 설명변수 : 정상성 검정을 통과한 처리 형태
#   S형 : 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분)
#   U형 : 4개 모두 1차 차분으로 통일 (해석이 가장 쉬움)
# 모든 변수가 차분되어 0~1 에 갇히지 않으므로 Tobit 이 아니라 OLS 를 씁니다.
# 표준오차는 국가별 묶음(cluster) 입니다.  p 는 모두 묶음 기준.
# 미리 정해 둔 5개 모형만 돌립니다 (결과를 보고 모형을 더 만들지 않습니다).
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(tseries); library(sandwich); library(lmtest)
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

df <- df |> arrange(iso3c, year) |> group_by(iso3c) |>
  mutate(d1_dea = dea_score - dplyr::lag(dea_score)) |> ungroup()
if (isTRUE(sd(df$d1_dea, na.rm = TRUE) == 0)) stop("d1_dea 가 전부 0 입니다. Session > Restart R 후 다시 실행하세요.")

# ---- 1) 효율성 점수를 차분하면 정상이 되는가 ----------------------------
adf_p <- function(x) { x <- x[!is.na(x)]; if (length(x) < 8) return(NA_real_)
  tryCatch(suppressWarnings(tseries::adf.test(x, k = 1)$p.value), error = function(e) NA_real_) }
dea_adf <- map_dfr(c("dea_score", "d1_dea"), function(v) {
  p <- df |> group_by(iso3c) |> summarise(p = adf_p(.data[[v]]), .groups = "drop")
  tibble(변수 = v, 검정국가수 = sum(!is.na(p$p)), 비정상국가수 = sum(p$p >= 0.05, na.rm = TRUE),
         정상국가수 = sum(p$p < 0.05, na.rm = TRUE))
})
cat("== 효율성 점수의 정상성 (원본 vs 1차 차분) ==\n"); print(dea_adf)
cat("(ZAF, MUS 는 늘 1.0 이라 검정 자체가 되지 않습니다)\n")

# ---- 2) 회귀 5개 -------------------------------------------------------
S <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
U <- c("d1_ln_wgi", "d1_nat", "d1_mobile", "d1_gdp")
LAB <- c(d3_ln_wgi = "거버넌스", d2_nat = "천연자원", d3_mobile = "모바일", d3_gdp = "1인당GDP",
         d1_ln_wgi = "거버넌스", d1_nat = "천연자원", d1_mobile = "모바일", d1_gdp = "1인당GDP")
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

run <- function(d, xs, id, desc) {
  d <- d |> drop_na(d1_dea, all_of(xs)) |> as.data.frame()
  m <- lm(as.formula(paste("d1_dea ~", paste(xs, collapse = " + "))), data = d)
  ct <- coeftest(m, vcov. = vcovCL(m, cluster = d$iso3c, type = "HC1"))
  tibble(모형 = id, 설명 = desc, N = nrow(d), 국가수 = n_distinct(d$iso3c),
         R2 = round(summary(m)$r.squared, 3), 변수 = unname(LAB[rownames(ct)[-1]]),
         계수 = round(ct[-1, 1], 4), p_묶음 = round(ct[-1, 4], 3), 별 = stars(ct[-1, 4]))
}
res <- bind_rows(
  run(df,                                          S, "S-1", "정상 처리형태 / 18개국"),
  run(df |> filter(!iso3c %in% c("ZAF", "MUS")),    S, "S-2", "정상 처리형태 / 16개국 (ZAF, MUS 제외)"),
  run(df |> filter(iso3c %in% c("KEN","MOZ","TUN","UGA")), S, "S-3", "정상 처리형태 / 모두 정상인 4개국"),
  run(df,                                          U, "U-1", "1차 차분 통일 / 18개국"),
  run(df |> filter(!iso3c %in% c("ZAF", "MUS")),    U, "U-2", "1차 차분 통일 / 16개국")
)
print(res, n = 30, width = Inf)
write_excel_csv(res, "output/stationary_regression.csv")
write_excel_csv(dea_adf, "output/stationary_dea_adf.csv")
