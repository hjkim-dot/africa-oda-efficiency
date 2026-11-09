# ============================================================
# 08. Beta 회귀 / 분수 로짓 (Tobit 결과가 방식에 따라 흔들리는지 확인)
# ============================================================
# Beta①   : 1.0 을 0.999 로 바꿔서 Beta 회귀 (원래 스크립트 방식)
# Beta②   : 표준 보정식 (y*(n-1)+0.5)/n 로 바꿔서 Beta 회귀
# 분수로짓 : 1.0 을 그대로 쓰는 quasi-binomial (경계값이 많을 때 가장 자연스러움)
# 표의 p 는 모두 국가별 묶음(cluster) 계산입니다.
# 계수 크기는 방식마다 단위가 달라서 비교하지 말고, 부호와 유의성만 비교하세요.
# ============================================================
# (다른 패키지가 dplyr 함수를 가리지 않도록 dplyr 를 맨 앞으로 다시 가져옵니다)
if ("package:dplyr" %in% search()) detach("package:dplyr", character.only = TRUE)
library(dplyr); library(tidyr); library(purrr); library(tibble); library(readr)
library(betareg); library(lmtest); library(sandwich)
source("helpers.R", encoding = "UTF-8")
df <- readRDS("analysis_df.rds")
dir.create("output", showWarnings = FALSE)

cluster_tab <- function(m, d, xvars, name) {
  cc <- coeftest(m, vcov. = vcovCL(m, cluster = d$iso3c))
  cc <- cc[rownames(cc) %in% xvars, , drop = FALSE]
  tibble(term = rownames(cc), !!paste0(name, "_계수") := cc[, 1], !!paste0(name, "_p") := cc[, 4])
}

for (mod in c("A", "B", "C")) {
  s <- get_sample(df, mod)
  d <- s$data |> drop_na(dea_score, all_of(s$x)) |> as.data.frame()
  n <- nrow(d)
  d$y999 <- ifelse(d$dea_score >= 1 - 1e-9, 0.999, ifelse(d$dea_score <= 1e-9, 0.001, d$dea_score))
  d$ysv  <- (d$dea_score * (n - 1) + 0.5) / n
  rhs <- paste(c(s$x, "factor(year)"), collapse = " + ")

  b1 <- betareg(as.formula(paste("y999 ~", rhs)), data = d)
  b2 <- betareg(as.formula(paste("ysv ~", rhs)), data = d)
  fl <- glm(as.formula(paste("dea_score ~", rhs)), data = d, family = quasibinomial("logit"))
  tb <- fit_tobit(s$data, s$x)$table |> transmute(term, Tobit_계수 = coef, Tobit_p = p_cluster)

  out <- tb |>
    left_join(cluster_tab(b1, d, s$x, "Beta1"), by = "term") |>
    left_join(cluster_tab(b2, d, s$x, "Beta2"), by = "term") |>
    left_join(cluster_tab(fl, d, s$x, "분수로짓"), by = "term") |>
    mutate(변수 = unname(LAB[term]), .before = 1) |> select(-term) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
  cat("\n=== 모형", mod, "| N =", n, "===\n"); print(out, width = Inf)
  write_excel_csv(out, paste0("output/beta_fractional_model", mod, ".csv"))
}
