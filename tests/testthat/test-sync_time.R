test_that("Test join_timeseries to gather two unsyncronised data frames", {
  # Generate dummy data
  df_a <- tibble(time = seq(0, 2, 1),
                 value = seq(1, 3, 1))
  df_b <- tibble(time = seq(0.5, 2.5, 0.5),
                 other_value = seq(-5, -4, 0.25))

  # Run the default function with non-conflicting column names
  df_test_default <- df_a %>%
    join_timeseries(
      y = df_b,
      by = "time")
  # With conflicting column names, default suffix
  df_test_default_suffix <- df_a %>%
    join_timeseries(
      y = df_a,
      by = "time")

  # Expected output (made with `constructive::construct()`)
  expected_default <- tibble(
    time = seq(0, 2.5, by = 0.5),
    value = c(1, NA, 2, NA, 3, NA),
    other_value = c(NA, -5, -4.75, -4.5, -4.25, -4)
    )

  expected_default_suffix <- tibble(
    time = c(0, 1, 2),
    value.x = c(1, 2, 3),
    value.y = c(1, 2, 3)
    )

  expect_equal(df_test_default, expected_default)
  expect_equal(df_test_default_suffix, expected_default_suffix)
})
