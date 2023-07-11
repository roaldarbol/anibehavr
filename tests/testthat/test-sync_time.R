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
    id = c(1,1,1,2,2,2),
    value2 = c(1,2,3,4,5,6)
    ) %>%
    group_by(id)

  df_id_multi <- tibble(
    time = c(1,2,3,1,2,4),
    id = c(1,1,1,2,2,2),
    trial = c(1,1,2,1,1,2),
    value = c(1,2,3,4,5,6)
  ) %>%
    group_by(id, trial)
  df_id_multi_2 <- tibble(
    time = c(1,2,3,4,2,3),
    id = c(1,1,1,2,2,2),
    trial = c(1,1,2,1,1,2),
    value2 = c(1,2,3,4,5,6)
  ) %>%
    group_by(id, trial)
  df_id_multi_wrong_group <- tibble(
    time = c(1,2,3,4,2,3),
    id = c(1,1,1,2,2,2),
    trial = c(1,1,2,1,1,2),
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

  # Group_by, with a single group
  df_test_single_group <- df_id_single %>%
    join_timeseries(
      y = df_id_single_2,
      by = "time")

  # Group_by, with multiple groups
  df_test_multi_group <- df_id_multi %>%
    join_timeseries(
      y = df_id_multi_2,
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
    time = c(1, 2, 3, 1, 2, 3, 4),
    id = rep(c(1, 2), 3:4),
    value = c(1, 2, 3, 4, 5, 6, NA),
    value2 = c(1, 2, 3, NA, 5, 6, 4),
  )

  expected_multi_group <- tibble::tibble(
    time = c(1, 2, 3, 1, 2, 4, 3, 4),
    id = rep(c(1, 2), c(3L, 5L)),
    trial = c(1, 1, 2, 1, 1, 1, 2, 2),
    value = c(1, 2, 3, 4, 5, NA, NA, 6),
    value2 = c(1, 2, 3, NA, 5, 4, 6, NA),
  )

  expect_equal(df_test_default, expected_default)
  # expect_equal(df_test_default_suffix, expected_default_suffix)
  expect_equal(df_test_single_group, expected_single_group)
  expect_equal(df_test_multi_group, expected_multi_group)
  # expect_equal() # For unmatched and dropped groupings
  expect_error(join_timeseries(
    .x = df_test_multi_group,
    y = df_id_multi_wrong_group,
    by = "time"),
    "Data frames are grouped by different variables"
    ) # For different groupings
})
