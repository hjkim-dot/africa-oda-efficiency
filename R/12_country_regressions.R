# ============================================================
# 12. 국가별 회귀  (18개국 각각 따로 추정)
# ============================================================
# 종속변수 : dea_score (수준값)           -> 주 분석(09번 모형 A)과 같은 설정
# 설명변수 : 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분)
# 표본     : 국가마다 2009~2022년 14개 관측치, 추정할 계수 5개 + 오차 크기 1개
#
# 방법
#  - 1.0 에 절단된 관측치가 있으면 Tobit(0, 1 양방향 절단)
#  - 절단된 관측치가 하나도 없으면 Tobit 과 OLS 의 계수가 같으므로 OLS 로 추정
#  - 효율성이 14년 내내 1.0 인 나라(남아공, 모리셔스)는 추정할 수 없음
# 표시
#  - 별(*)은 p<0.05, **, *** 는 p<0.01, p<0.001
#  - † 는 그 나라에서 해당 설명변수가 ADF 검정으로 정상성이 확인되지 않은 경우
#  - 비절단 관측치가 6개 미만이면 "불안정"  (계수 6개를 추정하기에 정보가 부족)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(censReg); library(tseries)
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

xs  <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
LAB <- c(d3_ln_wgi = "거버넌스", d2_nat = "천연자원", d3_mobile = "모바일", d3_gdp = "1인당GDP")
KR  <- c(BEN = "베냉", BWA = "보츠와나", CIV = "코트디부아르", CMR = "카메룬", EGY = "이집트", ETH = "에티오피아",
         GHA = "가나", KEN = "케냐", MAR = "모로코", MOZ = "모잠비크", MUS = "모리셔스", NGA = "나이지리아",
         RWA = "르완다", SEN = "세네갈", TUN = "튀니지", TZA = "탄자니아", UGA = "우간다", ZAF = "남아공")
REG <- c(KEN = "동아프리카", ETH = "동아프리카", TZA = "동아프리카", UGA = "동아프리카", RWA = "동아프리카",
         GHA = "서아프리카", NGA = "서아프리카", SEN = "서아프리카", CIV = "서아프리카", BEN = "서아프리카",
         ZAF = "남부·중앙", BWA = "남부·중앙", MUS = "남부·중앙", CMR = "남부·중앙", MOZ = "남부·중앙",
         EGY = "북아프리카", MAR = "북아프리카", TUN = "북아프리카")
stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))

# 국가별 ADF (03번과 같은 방식): p < 0.05 이면 정상
adf_p <- function(x) { x <- x[!is.na(x)]; if (length(x) < 8) return(NA_real_)
  tryCatch(suppressWarnings(tseries::adf.test(x, k = 1)$p.value), error = function(e) NA_real_) }

fit_one <- function(iso) {
  d <- df |> filter(iso3c == iso) |> arrange(year)
  stat <- sapply(xs, function(v) adf_p(d[[v]]) < 0.05)          # TRUE = 정상
  dd <- d |> drop_na(dea_score, all_of(xs)) |> as.data.frame()
  y <- dd$dea_score; n <- length(y); ncens <- sum(y >= 1 - 1e-9); nfree <- n - ncens
  base <- tibble(iso3c = iso, 국가 = unname(KR[iso]), 소지역 = unname(REG[iso]), N = n, 절단 = ncens, 비절단 = nfree)
  if (sd(y) < 1e-9)
    return(base |> mutate(방법 = "추정 불가(효율성 상수 1.0)", 변수 = NA_character_, 계수 = NA_real_, se = NA_real_, p = NA_real_, 정상 = NA))
  f <- as.formula(paste("dea_score ~", paste(xs, collapse = " + ")))
  if (ncens > 0) {                                              # Tobit
    m <- tryCatch(suppressWarnings(censReg(f, left = 0, right = 1, data = dd)), error = function(e) NULL)
    s <- if (is.null(m)) NULL else tryCatch(summary(m)$estimate, error = function(e) NULL)
    if (is.null(s) || !all(is.finite(s[, 2])))
      return(base |> mutate(방법 = "Tobit 수렴 실패", 변수 = NA_character_, 계수 = NA_real_, se = NA_real_, p = NA_real_, 정상 = NA))
    s <- s[xs, , drop = FALSE]
    return(base |> mutate(방법 = "Tobit") |> slice(rep(1, length(xs))) |>
             mutate(변수 = unname(LAB[xs]), 계수 = s[, 1], se = s[, 2], p = s[, 4], 정상 = unname(stat)))
  }
  m <- lm(f, data = dd); s <- summary(m)$coefficients[xs, , drop = FALSE]   # 절단 없음: OLS
  base |> mutate(방법 = "OLS(절단 없음)") |> slice(rep(1, length(xs))) |>
    mutate(변수 = unname(LAB[xs]), 계수 = s[, 1], se = s[, 2], p = s[, 4], 정상 = unname(stat))
}

res <- map_dfr(sort(unique(df$iso3c)), fit_one) |>
  mutate(불안정 = !is.na(계수) & 비절단 < 6, 별 = stars(p),
         표시 = ifelse(is.na(계수), "-", sprintf("%.4f%s%s", 계수, 별, ifelse(!is.na(정상) & !정상, "†", ""))))

# ---- 국가별 한 줄 표 ---------------------------------------------------
wide <- res |> filter(!is.na(변수)) |>
  select(iso3c, 국가, 소지역, 방법, N, 절단, 비절단, 불안정, 변수, 표시) |>
  pivot_wider(names_from = 변수, values_from = 표시)
none <- res |> filter(is.na(변수)) |> distinct(iso3c, 국가, 소지역, 방법, N, 절단, 비절단, 불안정)
tab <- bind_rows(wide, none) |>
  mutate(across(all_of(unname(LAB)), ~ ifelse(is.na(.x), "-", .x))) |>
  arrange(match(소지역, c("동아프리카", "서아프리카", "남부·중앙", "북아프리카")), 국가) |>
  select(소지역, 국가, 방법, N, 절단, 비절단, 불안정, all_of(unname(LAB)))
cat("== 국가별 회귀 결과 (계수, * p<0.05, † 해당 국가에서 변수가 비정상) ==\n")
print(tab, n = 20, width = Inf)

# ---- 요약: 우연히 나올 수 있는 유의 수와 비교 ------------------------------
est <- res |> filter(!is.na(계수))
summ <- est |> group_by(변수) |> summarise(추정국가수 = n(), `p<0.05` = sum(p < 0.05), 양수 = sum(계수 > 0), 음수 = sum(계수 < 0),
                                       `우연히 기대되는 유의 수` = round(n() * 0.05, 1), .groups = "drop") |>
  arrange(match(변수, unname(LAB)))
cat("\n== 변수별 요약 (국가별 회귀 16개) ==\n"); print(summ)
cat("\n불안정(비절단 6개 미만) 국가:", paste(unique(est$국가[est$불안정]), collapse = ", "), "\n")

write_excel_csv(res |> select(-표시), "output/country_regressions_long.csv")
write_excel_csv(tab, "output/country_regressions_table.csv")
write_excel_csv(summ, "output/country_regressions_summary.csv")
