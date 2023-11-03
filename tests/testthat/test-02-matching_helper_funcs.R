### any_empty_dfs()

df1 <- data.frame(A = sample(1:10)) #non-empty dataframe
df2 <- data.frame(A = numeric(0), B = character(0), C = logical(0)) #empty dataframe
df3 <- data.frame(A = c('test1', 'test2')) #non-empty dataframe


test_that("any_empty_dfs() correctly identifies that one df is empty?", {
  expect_true(any_empty_dfs(df1, df2))
})

test_that("any_empty_dfs() correctly identifies that none of its arguments is an empty df?", {
  expect_false(any_empty_dfs(df1, df3))
})

### covert_to_sparse_matrix()

sample_matrix <- matrix(c(1, -2, NA, 4, 5, 0), nrow = 2, ncol = 3) # Creating a sample matrix

expected_sparse_matrix <- Matrix::Matrix(c(1, 0, 0, 4, 5, 0), nrow = 2, ncol = 3, sparse = TRUE) # Simulating expected output (with NA and negative number replaced by 0, then converted to sparse

test_that("convert_to_sparse_matrix() correctly replaces values and converts matrix to sparse?", {
  expect_equal( convert_to_sparse_matrix(sample_matrix), expected_sparse_matrix )
})

