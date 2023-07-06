test_that("Test join_timeseries to gather two unsyncronised data frames", {
  # Generate dummy data
  df_a <- tibble(time = seq(0, 2, 1),
                 value = seq(1, 3, 1))
  df_b <- tibble(time = seq(0.5, 2.5, 0.5),
                 other_value = seq(-5, -4, 0.25))
  df_id_single <- tibble(
    time = c(1,2,3,1,2,3),
    id = c(1,1,1,2,2,2),
    value = c(1,2,3,4,5,6)
    ) %>%
    group_by(id)
  df_id_single_2 <- tibble(
    time = c(1,2,3,4,2,3),
    id = c(1,1,1,1,2,2),
    value2 = c(1,2,3,4,5,6)
    ) %>%
    group_by(id)

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

  # With a single group
  df_test_single_group <- df_id_single %>%
    join_timeseries(
      y = df_id_single_2,
      by = "time")

  # Expected output (made with `constructive::construct()`)
  expected_default <- tibble(
    time = seq(0, 2.5, by = 0.5),
    value = c(1, NA, 2, NA, 3, NA),
    other_value = c(NA, -5, -4.75, -4.5, -4.25, -4)
    )

  expected_default_suffix <- tibble(
    time = c(0, 1, 2),
    value = c(1, 2, 3),
    value.y = c(1, 2, 3)
    )

  expected_single_group <- tibble::tibble(
    time = c(1, 2, 3, 4, 1, 2, 3),
    id = rep(c(1, 2), 4:3),
    value = c(1, 2, 3, NA, 4, 5, 6),
    value2 = c(1, 2, 3, 4, NA, 5, 6),
  )

  expect_equal(df_test_default, expected_default)
  expect_equal(df_test_default_suffix, expected_default_suffix)
  expect_equal(df_test_single_group, expected_single_group)
})
