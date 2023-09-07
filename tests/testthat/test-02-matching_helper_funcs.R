df1 <- data.frame(A = sample(1:10)) #non-empty dataframe
df2 <- data.frame(A = numeric(0), B = character(0), C = logical(0)) #empty dataframe
df3 <- data.frame(A = c('test1', 'test2')) #non-empty dataframe


test_that("any_empty_dfs() correctly identifies that one df is empty?", {
  expect_true(any_empty_dfs(df1, df2))
})

test_that("any_empty_dfs() correctly identifies that none of its arguments is an empty df?", {
  expect_false(any_empty_dfs(df1, df3))
})
