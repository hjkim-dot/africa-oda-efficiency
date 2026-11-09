# ============================================================
# 11. 정상인 나라만 쓴 Tobit  (09번 모형과 같은 설정, 표본만 다름)
# ============================================================
# 종속변수 : dea_score (수준값 그대로, 0~1 양방향 Tobit)
# 설명변수 : 거버넌스(로그+3차 차분), 천연자원(2차 차분), 모바일(3차 차분), 1인당GDP(3차 차분)
# 표본 3가지 (미리 정해 둔 것만 돌립니다)
#   T-1 : 09번 모형 A 그대로 (에티오피아 제외 17개국)
#   T-2 : 설명변수 4개 중 3개 이상이 정상인 나라
#   T-3 : 설명변수 4개가 모두 정상인 나라
# T-2, T-3 의 나라 목록은 이 스크립트가 ADF 로 직접 계산합니다 (손으로 정하지 않습니다).
# p       : 일반 계산 /  p_cluster : 국가별 묶음 계산 (국가가 적으면 참고용)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr); library(tseries)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

xs   <- c("d3_ln_wgi", "d2_nat", "d3_mobile", "d3_gdp")
LABX <- c(d3_ln_wgi = "거버넌스", d2_nat = "천연자원", d3_mobile = "모바일", d3_gdp = "1인당GDP")

# ---- 1) 설명변수가 정상인 나라 찾기 (국가별 ADF, p < 0.05 = 정상) --------------
adf_p <- function(x) { x <- x[!is.na(x)]; if (length(x) < 8) return(NA_real_)
  tryCatch(suppressWarnings(tseries::adf.test(x, k = 1)$p.value), error = function(e) NA_real_) }
pv <- map_dfc(setNames(xs, xs), function(v)
  df |> group_by(iso3c) |> summarise(p = adf_p(.data[[v]]), .groups = "drop") |> pull(p))
isos   <- df |> distinct(iso3c) |> arrange(iso3c) |> pull(iso3c)
n_pass <- rowSums(pv < 0.05, na.rm = TRUE)
all4   <- isos[n_pass == 4]
ge3    <- isos[n_pass >= 3]
cat("설명변수 4개가 모두 정상인 나라(", length(all4), "):", paste(all4, collapse = ", "), "\n")
cat("설명변수 4개 중 3개 이상 정상인 나라(", length(ge3), "):", paste(ge3, collapse = ", "), "\n")

# ---- 2) Tobit 3개 --------------------------------------------------------
samples <- list(
  "T-1" = list(desc = "09번 모형 A 그대로 (에티오피아 제외)", d = df |> filter(iso3c != "ETH")),
  "T-2" = list(desc = "설명변수 3개 이상 정상인 나라",        d = df |> filter(iso3c %in% ge3)),
  "T-3" = list(desc = "설명변수 4개 모두 정상인 나라",        d = df |> filter(iso3c %in% all4))
)
res <- imap_dfr(samples, function(s, id) {
  r <- fit_tobit(s$d, xs, year_dummy = FALSE)
  r$table |> mutate(모형 = id, 표본 = s$desc, N = r$N, 국가수 = r$G, `1.0절단` = r$cens,
                    변수 = unname(LABX[term]), .before = 1) |> select(-term)
}) |> mutate(across(c(coef, se, se_cluster), ~ round(.x, 4)), across(c(p, p_cluster), ~ round(.x, 3)), 별 = stars(p))
print(res, n = 20, width = Inf)
write_excel_csv(res, "output/tobit_stationary_samples.csv")
