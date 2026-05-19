suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
})

P <- readRDS("Output/Estimation_Results/Estimated_Primitives.rds")
cat("=== Estimated_Primitives.rds ===\n")
cat("Top-level names:", paste(names(P), collapse=", "), "\n\n")
if ("states" %in% names(P)) {
  cat("nrow(states):", nrow(P$states), "\n")
  cat("Unique A values:", paste(sort(unique(P$states$A)), collapse=","), "\n")
  cat("Unique w:", paste(sort(unique(as.character(P$states$w))), collapse=","), "\n")
  cat("Unique rho:", paste(sort(unique(as.character(P$states$rho))), collapse=","), "\n\n")
}
for (n in names(P)) {
  x <- P[[n]]
  if (inherits(x, "Matrix") || is.matrix(x)) {
    cat(sprintf("  %s : dim = %s\n", n, paste(dim(x), collapse=" x ")))
  } else if (is.numeric(x) && !is.list(x)) {
    cat(sprintf("  %s : length = %d\n", n, length(x)))
  } else if (is.list(x) && !is.data.table(x) && !is.data.frame(x)) {
    cat(sprintf("  %s : list with %d items: %s\n", n, length(x), paste(names(x), collapse=",")))
    for (m in names(x)) {
      y <- x[[m]]
      if (inherits(y, "Matrix") || is.matrix(y)) {
        cat(sprintf("    -> %s : dim = %s\n", m, paste(dim(y), collapse=" x ")))
      } else if (is.numeric(y)) {
        cat(sprintf("    -> %s : length = %d\n", m, length(y)))
      }
    }
  }
}

cat("\n=== Model_B_Estimates.rds ===\n")
E <- readRDS("Output/Estimation_Results/Model_B_Estimates.rds")
cat("Top-level names:", paste(names(E), collapse=", "), "\n")
if (!is.null(E$cache)) {
  cat("cache$n_states:", E$cache$n_states, "\n")
  if (!is.null(E$cache$F_maintain)) cat("cache$F_maintain dim:", paste(dim(E$cache$F_maintain), collapse=" x "), "\n")
}
if (!is.null(E$P_hat)) cat("P_hat dim:", paste(dim(E$P_hat), collapse=" x "), "\n")
if (!is.null(E$V_hat)) cat("V_hat length:", length(E$V_hat), "\n")
if (!is.null(E$theta_hat)) {
  cat("theta_hat:\n")
  print(E$theta_hat)
}
