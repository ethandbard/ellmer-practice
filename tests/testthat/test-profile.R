# The profiler is the Phase 1 centerpiece: roles from statistics, names only
# as a small tiebreak.

test_that("profile_table returns one row per column with a known role", {
  df <- data.frame(
    id = 1:10,
    grp = rep(c("a", "b"), 5),
    amount = rnorm(10),
    when = as.Date("2024-01-01") + 0:9,
    stringsAsFactors = FALSE
  )
  prof <- profile_table(df)
  expect_equal(nrow(prof), 4)
  expect_true(all(prof$role %in% profile_roles))
  expect_equal(prof$role[prof$name == "id"], "id")
  expect_equal(prof$role[prof$name == "grp"], "category")
  expect_equal(prof$role[prof$name == "amount"], "measure")
  expect_equal(prof$role[prof$name == "when"], "clock")
})

test_that("integer encodings with few levels are categories", {
  mt <- datasets::mtcars
  prof <- profile_table(mt)
  expect_equal(prof$role[prof$name == "cyl"], "category")
  expect_equal(prof$role[prof$name == "gear"], "category")
  expect_equal(prof$role[prof$name == "mpg"], "measure")
})

test_that("a count-named small integer is a count, not a category", {
  fd <- read.csv(app_path("data", "food_delivery.csv"), stringsAsFactors = FALSE)
  prof <- profile_table(fd)
  expect_equal(prof$role[prof$name == "items"], "count")
  expect_equal(prof$role[prof$name == "cuisine"], "category")
})

test_that("integer year is not a clock", {
  df <- data.frame(year = 2000:2020, value = runif(21))
  prof <- profile_table(df)
  expect_false(prof$role[prof$name == "year"] == "clock")
  expect_null(pick_clock(prof))
})

test_that("date strings are clocks", {
  df <- data.frame(
    order_date = as.character(as.Date("2024-01-01") + seq(0, 80, by = 7)),
    total = runif(12, 10, 40),
    stringsAsFactors = FALSE
  )
  prof <- profile_table(df)
  expect_equal(prof$role[prof$name == "order_date"], "clock")
  expect_equal(pick_clock(prof), "order_date")
})

test_that("format_profile_md names roles and ranges", {
  df <- data.frame(
    region = c("East", "West", "East"),
    revenue = c(10, 20, 15),
    stringsAsFactors = FALSE
  )
  md <- format_profile_md(profile_table(df), "demo", 3)
  expect_match(md, "region")
  expect_match(md, "category")
  expect_match(md, "revenue")
  expect_match(md, "measure")
})
