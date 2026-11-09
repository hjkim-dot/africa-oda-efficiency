# ============================================================
# 07. 한 나라씩 빼고 다시 추정 (특정 국가가 결과를 좌우하는지 확인)
# ============================================================
# 각 모형(A, B, C)에서 국가를 하나씩 제외하고 Tobit(연도 더미 포함)을 반복합니다.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

loco <- function(mod) {
  s <- get_sample(df, mod)
  full <- fit_tobit(s$data, s$x)$table |> select(term, full_coef = coef, full_p = p, full_pc = p_cluster)
  countries <- sort(unique(s$data$iso3c))
  rows <- map_dfr(countries, function(cn) {
    fit_tobit(s$data |> filter(iso3c != cn), s$x)$table |> mutate(제외국가 = cn, .before = 1)
  })
  rows <- left_join(rows, full, by = "term")
  summ <- rows |> group_by(term) |>
    summarise(전체계수 = round(first(full_coef), 4),
              최소 = round(min(coef), 4), 최대 = round(max(coef), 4),
              `p<0.05(일반)` = paste0(sum(p < 0.05), "/", n()),
              `p<0.05(묶음)` = paste0(sum(p_cluster < 0.05, na.rm = TRUE), "/", n()),
              부호바뀜 = ifelse(any(sign(coef) != sign(first(full_coef))),
                              paste(제외국가[sign(coef) != sign(first(full_coef))], collapse = ","), "없음"),
              가장영향큰국가 = 제외국가[which.max(abs(coef - first(full_coef)))],
              .groups = "drop") |>
    mutate(변수 = unname(LAB[term]), .before = 1) |> select(-term)
  list(rows = rows |> mutate(모형 = mod, .before = 1), summary = summ)
}

detail <- list()
for (mod in c("A", "B", "C")) {
  r <- loco(mod)
  cat("\n=== 모형", mod, ": 한 나라씩 제외 ===\n"); print(r$summary, width = Inf)
  detail[[mod]] <- r$rows
}
write_excel_csv(bind_rows(detail), "output/leave_one_out_detail.csv")
cat("\n저장: output/leave_one_out_detail.csv\n")
