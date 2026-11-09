# ============================================================
# 13. 소지역별 Tobit  (원래 논문의 표 6 형식)
# ============================================================
# 주 분석(09번 모형 A)과 같은 설정을 소지역별로 따로 추정합니다.
#   종속변수 : dea_score (수준값, 0~1 양방향 Tobit)
#   설명변수 : 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분)
#   표본     : 2009~2022년. 에티오피아는 모형 A 와 같은 이유로 제외 (동아프리카는 4개국)
# 주의
#  - 그룹당 국가가 3~5개뿐이라 국가별 묶음 표준오차를 쓸 수 없고, 일반 표준오차만 보고합니다.
#  - 남부·중앙의 남아공·모리셔스는 효율성이 늘 1.0 이라 절단 관측치로만 들어갑니다.
#  - 매우 불안정한 탐색적 결과입니다.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr); library(censReg)
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

xs  <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
LAB <- c(d3_ln_wgi = "거버넌스(로그+3차 차분)", d2_nat = "천연자원(2차 차분)", d3_mobile = "모바일(3차 차분)", d3_gdp = "1인당GDP(3차 차분)")
REG <- c(KEN = "동아프리카", ETH = "동아프리카", TZA = "동아프리카", UGA = "동아프리카", RWA = "동아프리카",
         GHA = "서아프리카", NGA = "서아프리카", SEN = "서아프리카", CIV = "서아프리카", BEN = "서아프리카",
         ZAF = "남부·중앙", BWA = "남부·중앙", MUS = "남부·중앙", CMR = "남부·중앙", MOZ = "남부·중앙",
         EGY = "북아프리카", MAR = "북아프리카", TUN = "북아프리카")
df$소지역 <- unname(REG[df$iso3c])
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))

fit_group <- function(d, label) {
  d <- d |> drop_na(dea_score, all_of(xs)) |> as.data.frame()
  m <- censReg(as.formula(paste("dea_score ~", paste(xs, collapse = " + "))), left = 0, right = 1, data = d)
  s <- summary(m)$estimate
  tibble(그룹 = label, 국가수 = n_distinct(d$iso3c), N = nrow(d), 절단 = sum(d$dea_score >= 1 - 1e-9),
         비절단 = sum(d$dea_score < 1 - 1e-9), 로그우도 = as.numeric(logLik(m)),
         항목 = c(unname(LAB[xs]), "상수항"),
         계수 = s[c(xs, "(Intercept)"), 1], se = s[c(xs, "(Intercept)"), 2], p = s[c(xs, "(Intercept)"), 4])
}

base <- df |> filter(iso3c != "ETH")
res <- bind_rows(
  fit_group(base, "전체(모형 A)"),
  fit_group(base |> filter(소지역 == "동아프리카"), "동아프리카"),
  fit_group(base |> filter(소지역 == "서아프리카"), "서아프리카"),
  fit_group(base |> filter(소지역 == "남부·중앙"), "남부·중앙"),
  fit_group(base |> filter(소지역 == "북아프리카"), "북아프리카")
) |> mutate(별 = stars(p), 표시 = sprintf("%+.4f%s (%.3f)", 계수, 별, p))

cat("== 소지역별 Tobit: 계수(별) (p값) ==\n")
wide <- res |> select(그룹, 항목, 표시) |> pivot_wider(names_from = 그룹, values_from = 표시)
print(wide, width = Inf)
cat("\n== 표본 ==\n")
print(res |> distinct(그룹, 국가수, N, 절단, 비절단, 로그우도) |> mutate(로그우도 = round(로그우도, 2)))
write_excel_csv(res |> select(-표시), "output/subregion_tobit_long.csv")
write_excel_csv(wide, "output/subregion_tobit_table.csv")
