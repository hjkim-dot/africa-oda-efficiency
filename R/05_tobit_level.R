# ============================================================
# 05. 수준(원본) 변수 Tobit  ->  주 분석
# ============================================================
# 모형 A : 거버넌스 포함, 에티오피아 제외 (17개국)
# 모형 B : 거버넌스 제외, 18개국
# 모형 C : 거버넌스 포함, 18개국  (에티오피아 거버넌스는 수준에서 정상이라 추가)
# 각 모형을 "연도 더미 있음 / 없음" 두 가지로 추정
# p        : 일반 계산 (censReg 가 주는 값)
# p_cluster: 국가별 묶음 계산 (더 엄격, 국가 수가 적어 참고용)
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

all_res <- list()
for (mod in c("A", "B", "C")) {
  for (yd in c(TRUE, FALSE)) {
    s <- get_sample(df, mod)
    r <- fit_tobit(s$data, s$x, year_dummy = yd)
    cat("\n=======================================================\n")
    cat("모형", mod, "| 연도더미", ifelse(yd, "있음", "없음"),
        "| N =", r$N, "| 국가 =", r$G, "| 1.0절단 =", r$cens,
        "| logLik =", round(r$ll, 2), "\n")
    print(r$table |> mutate(across(where(is.numeric), ~ round(.x, 4)),
                            별 = stars(p)), width = Inf)
    all_res[[paste(mod, yd)]] <- r$table |>
      mutate(모형 = mod, 연도더미 = yd, N = r$N, 국가수 = r$G, 절단 = r$cens, .before = 1)
    if (yd) { cat("\n-- summary() 전체 출력 (모형", mod, ") --\n"); print(summary(r$model)) }
  }
}
out <- bind_rows(all_res)
write_excel_csv(out, "output/tobit_level_results.csv")
cat("\n저장: output/tobit_level_results.csv\n")
