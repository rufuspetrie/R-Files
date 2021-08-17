######################### Matrix Algebra Examples #########################

# Create a (column) vector
x <- c(1,2,3)

# Create a matrix
X <- matrix(
  c(4, 1, 4,
    1, 6, 4,
    1, 4, 9),
  nrow = 3, byrow = TRUE
)

# Another matrix
Y <- matrix(
  c(10, 1, 0,
    1, 3, -2,
    0, -2, 5),
  nrow = 3, byrow = TRUE
)

# Transposes
t(x)
t(X)

# Diagonal, Trace
diag(X)
sum(diag(X))

# Inverse
solve(X)

# Elementwise matrix multiplication
X*Y

# Matrix times vector
X %*% x

# Matrix times matrix
X %*% Y

# Eigendecomposition
v <- eigen(X)
v$values
v$vectors

# Single value decomposition
v <- svd(X)
v$u
v$d
v$v