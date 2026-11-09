# ============================================================
# 00. 데이터 준비 (가장 먼저 실행)
# ============================================================
# 하는 일
#  1) data.xlsx 불러오기
#  2) dea_score 가 비어 있으면 dea_results_full.csv 의 bcc 값으로 채우기
#  3) GDP 성장률(불변가격), 로그·차분 변수 만들기
#  4) analysis_df.rds 로 저장 (이후 모든 스크립트가 이 파일을 사용)
# 필요한 파일(같은 폴더): data.xlsx , dea_results_full.csv(dea_score 가 비어 있을 때)
# 주의: 데이터 이름을 data 대신 df 로 씁니다 (data 는 R 내장 함수 이름이라 충돌)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(readxl); library(readr)
dir.create("output", showWarnings = FALSE)

# ---- 1) 불러오기 ------------------------------------------------
raw <- read_excel("data.xlsx", sheet = "data")
# 파일을 못 찾는다는 에러가 나면 위 줄 대신 아래 줄의 # 를 지우고 실행 (파일 선택 창)
# raw <- read_excel(file.choose(), sheet = "data")
cat("불러온 컬럼:", paste(names(raw), collapse = ", "), "\n")

iso_map <- c("Benin" = "BEN", "Botswana" = "BWA", "Cameroon" = "CMR",
             "Cote d'Ivoire" = "CIV", "Egypt, Arab Rep." = "EGY", "Ethiopia" = "ETH",
             "Ghana" = "GHA", "Kenya" = "KEN", "Mauritius" = "MUS", "Morocco" = "MAR",
             "Mozambique" = "MOZ", "Nigeria" = "NGA", "Rwanda" = "RWA", "Senegal" = "SEN",
             "South Africa" = "ZAF", "Tanzania" = "TZA", "Tunisia" = "TUN", "Uganda" = "UGA")
if (!"iso3c" %in% names(raw)) raw$iso3c <- unname(iso_map[raw$country])

need <- c("country", "iso3c", "year", "nat_resource", "mobile", "ln_gdp_pc", "wgi_lag1")
miss <- setdiff(need, names(raw))
if (length(miss) > 0) stop("필요한 컬럼이 없습니다: ", paste(miss, collapse = ", "))

df <- raw |>
  filter(iso3c %in% unname(iso_map)) |>       # 18개국만 (알제리·잠비아 제외)
  mutate(year = as.integer(year)) |>
  arrange(iso3c, year)

# ---- 2) dea_score 채우기 ----------------------------------------
has_dea <- "dea_score" %in% names(df) && sum(!is.na(df$dea_score)) > 0
if (!has_dea) {
  if (file.exists("dea_results_full.csv")) {
    dea <- read_csv("dea_results_full.csv", show_col_types = FALSE) |>
      select(iso3c, year, dea_score = bcc)
    df <- df |> select(-any_of("dea_score")) |> left_join(dea, by = c("iso3c", "year"))
    cat("dea_score 를 dea_results_full.csv 에서 채웠습니다.\n")
  } else {
    warning("dea_score 가 비어 있고 dea_results_full.csv 도 없습니다. 01_dea.R 를 먼저 실행하세요.")
  }
}

# ---- 3) GDP 성장률: 반드시 불변가격 기준 ---------------------------
if ("gdp_pc_const" %in% names(df)) {
  df <- df |> group_by(iso3c) |>
    mutate(gdp_growth_new = (gdp_pc_const - dplyr::lag(gdp_pc_const)) / dplyr::lag(gdp_pc_const) * 100) |>
    ungroup()
  if ("gdp_growth" %in% names(df)) {
    dif <- abs(df$gdp_growth - df$gdp_growth_new)
    cat("gdp_growth 기존 값과의 최대 차이:", signif(max(dif, na.rm = TRUE), 3), "\n")
  }
  df <- df |> mutate(gdp_growth = gdp_growth_new) |> select(-gdp_growth_new)
}

# ---- 4) 로그·차분 변수 (국가별로, 연도순) ---------------------------
#   거버넌스 : 로그 -> 1·2·3차 차분      천연자원 : 1·2·3차 차분
#   모바일   : 1·2·3차 차분              1인당GDP : (이미 로그값) 1·2·3차 차분
df <- df |> group_by(iso3c) |> arrange(year, .by_group = TRUE) |>
  mutate(
    ln_wgi    = log1p(wgi_lag1),
    d1_ln_wgi = ln_wgi - dplyr::lag(ln_wgi), d2_ln_wgi = d1_ln_wgi - dplyr::lag(d1_ln_wgi),
    d3_ln_wgi = d2_ln_wgi - dplyr::lag(d2_ln_wgi),
    d1_nat    = nat_resource - dplyr::lag(nat_resource), d2_nat = d1_nat - dplyr::lag(d1_nat),
    d3_nat    = d2_nat - dplyr::lag(d2_nat),
    d1_mobile = mobile - dplyr::lag(mobile), d2_mobile = d1_mobile - dplyr::lag(d1_mobile),
    d3_mobile = d2_mobile - dplyr::lag(d2_mobile),
    d1_gdp    = ln_gdp_pc - dplyr::lag(ln_gdp_pc), d2_gdp = d1_gdp - dplyr::lag(d1_gdp),
    d3_gdp    = d2_gdp - dplyr::lag(d2_gdp)
  ) |> ungroup()


# ---- 안전장치: lag() 가 망가졌는지 확인 --------------------------------------
# 다른 패키지(예: plm)가 먼저 불러져 있으면 lag() 가 엉뚱하게 동작해서
# 성장률과 차분 변수가 전부 0 이 될 수 있습니다. 이 경우 여기서 멈춥니다.
chk <- c(df$d1_nat, df$d1_mobile, df$d1_gdp)
if (all(is.na(chk)) || isTRUE(sd(chk, na.rm = TRUE) == 0))
  stop("차분 계산이 0 이 됩니다. lag() 가 다른 패키지에 가려진 상태입니다. ",
       "RStudio 에서 Session > Restart R 을 한 뒤 다시 실행하세요.")
if ("gdp_growth" %in% names(df) && isTRUE(sd(df$gdp_growth, na.rm = TRUE) == 0))
  stop("gdp_growth 가 전부 0 입니다. Session > Restart R 을 한 뒤 다시 실행하세요.")

# ---- 5) 저장 및 점검 ---------------------------------------------
saveRDS(df, "analysis_df.rds")
cat("\n== 점검 ==\n")
cat("행 수:", nrow(df), "| 국가 수:", n_distinct(df$iso3c),
    "| 연도:", min(df$year), "~", max(df$year), "\n")
cat("dea_score 채워진 개수:", sum(!is.na(df$dea_score)), "(306 이면 정상)\n")
