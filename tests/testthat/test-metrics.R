# Metric maths. Phase 4 swaps these for yardstick, so these tests are the
# contract that migration has to satisfy.

test_that("rmse_of and mae_of compute against known values", {
  actual <- c(1, 2, 3, 4)
  pred <- c(1, 2, 3, 8)
  expect_equal(rmse_of(actual, pred), 2)
  expect_equal(mae_of(actual, pred), 1)

  expect_equal(rmse_of(c(1, 2), c(1, 2)), 0)
})

test_that("rmse_of and mae_of drop non-finite pairs", {
  expect_equal(rmse_of(c(1, 2, NA), c(1, 2, 99)), 0)
  expect_equal(mae_of(c(1, NA), c(1, Inf)), 0)
  expect_true(is.na(rmse_of(c(NA_real_), c(NA_real_))))
})

test_that("r2_of matches the standard definition", {
  actual <- c(1, 2, 3, 4, 5)
  expect_equal(r2_of(actual, actual), 1)

  # Predicting the mean everywhere gives R2 = 0.
  expect_equal(r2_of(actual, rep(mean(actual), 5)), 0)

  # Fewer than 3 usable pairs, or zero variance, is undefined rather than 0.
  expect_true(is.na(r2_of(c(1, 2), c(1, 2))))
  expect_true(is.na(r2_of(rep(3, 5), rep(3, 5))))
})

test_that("mape_of ignores near-zero actuals to avoid exploding", {
  # |actual| must exceed 1 to be counted.
  expect_equal(mape_of(c(100, 200), c(110, 220)), 10)
  expect_true(is.na(mape_of(c(0.5, 0.2), c(1, 1))))
})

test_that("auc_binary matches hand-computed ranks", {
  # Perfect separation.
  expect_equal(auc_binary(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9)), 1)
  # Perfectly inverted.
  expect_equal(auc_binary(c(1, 1, 0, 0), c(0.1, 0.2, 0.8, 0.9)), 0)
  # No signal - all scores tied.
  expect_equal(auc_binary(c(0, 1, 0, 1), rep(0.5, 4)), 0.5)
})

test_that("auc_binary is undefined when a class is absent", {
  expect_true(is.na(auc_binary(c(1, 1, 1), c(0.2, 0.5, 0.9))))
  expect_true(is.na(auc_binary(c(0, 0, 0), c(0.2, 0.5, 0.9))))
})

test_that("binarize_target picks the rarer level as positive", {
  x <- c(rep("No", 8), rep("Yes", 2))
  bin <- binarize_target(x)
  expect_equal(bin$positive, "Yes")
  expect_equal(bin$negative, "No")
  expect_equal(sum(bin$y), 2)
})

test_that("binarize_target prefers an affirmative label over rarity", {
  # "Yes" wins even though it is the majority here.
  x <- c(rep("Yes", 8), rep("No", 2))
  bin <- binarize_target(x)
  expect_equal(bin$positive, "Yes")
  expect_equal(sum(bin$y), 8)
})

test_that("binarize_target refuses anything that is not two-level", {
  expect_null(binarize_target(c("a", "b", "c")))
  expect_null(binarize_target(rep("a", 5)))
})

test_that("r2/rmse agree with lm on a trivial perfect fit", {
  d <- data.frame(x = 1:20, y = (1:20) * 3 + 5)
  fit <- stats::lm(y ~ x, data = d)
  pred <- as.numeric(stats::predict(fit, d))
  expect_lt(rmse_of(d$y, pred), 1e-8)
  expect_gt(r2_of(d$y, pred), 1 - 1e-8)
})

test_that("fit_supervised via tidymodels matches an lm on a perfect line", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("workflows")
  d <- data.frame(x = 1:40, z = rnorm(40), y = (1:40) * 3 + 5)
  res <- fit_supervised(
    d,
    method = "regression",
    target = "y",
    features = "x",
    title = "line",
    plot = "actual_pred",
    prepare_sql = "",
    cv_folds = 0L
  )
  expect_true(res$ok)
  expect_equal(res$engine, "lm")
  expect_lt(res$metrics$rmse, 1e-6)
  expect_true(any(grepl("x", res$coefs$term)))
})

test_that("make_formula builds the expected right-hand side", {
  f <- make_formula("revenue", c("quantity", "channel"))
  expect_equal(deparse(f), "revenue ~ quantity + channel")

  f2 <- make_formula("revenue", c("quantity"), "quantity:channel")
  expect_equal(deparse(f2), "revenue ~ quantity + quantity:channel")

  # No features at all degrades to an intercept-only model.
  expect_equal(deparse(make_formula("revenue", character())), "revenue ~ 1")
})

test_that("make_formula backtick-quotes non-syntactic names", {
  f <- make_formula("Unit Price", c("Order Qty"))
  expect_equal(deparse(f), "`Unit Price` ~ `Order Qty`")
})

test_that("parse_interaction_terms resolves pairwise and polynomial terms", {
  df <- data.frame(price = 1, quantity = 1, channel = "a", stringsAsFactors = FALSE)

  out <- parse_interaction_terms("price:quantity", df, character())
  expect_equal(out$terms, "price:quantity")
  expect_setequal(out$extra_features, c("price", "quantity"))

  # `*` is accepted as a synonym for `:` here.
  expect_equal(parse_interaction_terms("price*channel", df, character())$terms, "price:channel")

  poly <- parse_interaction_terms("price^2", df, character())
  expect_equal(poly$terms, "I(price^2)")
  expect_equal(poly$extra_features, "price")
})

test_that("parse_interaction_terms reports unusable terms instead of failing", {
  df <- data.frame(price = 1, stringsAsFactors = FALSE)

  missing <- parse_interaction_terms("price:nope", df, character())
  expect_length(missing$terms, 0)
  expect_match(missing$notes, "not found")

  malformed <- parse_interaction_terms("price", df, character())
  expect_length(malformed$terms, 0)
  expect_match(malformed$notes, "expected")
})

test_that("parse_name_list splits on commas and semicolons and resolves names", {
  df <- data.frame(quantity = 1, channel = "a", promo = "b", stringsAsFactors = FALSE)

  expect_equal(parse_name_list("quantity, channel", df), c("quantity", "channel"))
  expect_equal(parse_name_list("quantity; promo", df), c("quantity", "promo"))
  expect_equal(parse_name_list("Quantity", df), "quantity")

  # Unknown names are dropped silently; duplicates collapse.
  expect_equal(parse_name_list("quantity, nope, quantity", df), "quantity")
  expect_length(parse_name_list("", df), 0)
})

test_that("one supervised fit can break holdout error down by a category", {
  set.seed(1)
  df <- data.frame(
    revenue = c(rnorm(40, 20), rnorm(40, 30)),
    quantity = rnorm(80, 5),
    region = rep(c("East", "West"), each = 40),
    stringsAsFactors = FALSE
  )
  res <- fit_supervised(
    df,
    method = "regression",
    target = "revenue",
    features = "quantity",
    title = "by region",
    plot = "subgroup",
    prepare_sql = "",
    subgroup = "region"
  )
  expect_true(res$ok)
  expect_equal(res$subgroup, "region")
  expect_true(!is.null(res$subgroup_table) && nrow(res$subgroup_table) >= 2)
  expect_true(all(c("East", "West") %in% res$subgroup_table$region))
})
