make_feature_ids <- function(n, prefix = "F") {
  paste0(prefix, seq_len(n))
}

make_sample_ids <- function(n, prefix = "S") {
  paste0(prefix, seq_len(n))
}

make_sample_matrix <- function(n_features = 10, n_samples = 10,
                               feature_prefix = "Feature",
                               sample_prefix = "S") {
  mat <- matrix(
    seq_len(n_features * n_samples),
    nrow = n_features,
    ncol = n_samples
  )
  dimnames(mat) <- list(
    paste0(feature_prefix, seq_len(n_features)),
    paste0(sample_prefix, seq_len(n_samples))
  )
  mat
}

make_sample_long_df <- function(samples = c("S1", "S2", "S3"),
                                n_per_sample = 10,
                                sample_col = "name",
                                value_col = "value",
                                bases = NULL) {
  if (is.null(bases)) {
    bases <- seq_along(samples)
  }
  stopifnot(length(bases) == length(samples))

  sample_values <- unlist(
    lapply(bases, function(base) base + seq_len(n_per_sample) - 1),
    use.names = FALSE
  )

  data.frame(
    stats::setNames(list(rep(samples, each = n_per_sample)), sample_col),
    stats::setNames(list(sample_values), value_col),
    stringsAsFactors = FALSE
  )
}

make_sample_design_df <- function(samples = c("S1", "S2", "S3"),
                                  conditions = c("Control", "Treated", "Control"),
                                  sample_col = "Sample",
                                  condition_col = "Condition") {
  stopifnot(length(samples) == length(conditions))
  data.frame(
    stats::setNames(list(samples), sample_col),
    stats::setNames(list(conditions), condition_col),
    stringsAsFactors = FALSE
  )
}

make_feature_df <- function(features = c("A", "B", "C"),
                            feature_col = "Feature",
                            sample_cols = c("s1", "s2"),
                            start = 1) {
  stopifnot(length(sample_cols) >= 1)
  values <- matrix(
    start + seq_len(length(features) * length(sample_cols)) - 1,
    nrow = length(features),
    ncol = length(sample_cols)
  )
  colnames(values) <- sample_cols

  out <- data.frame(
    stats::setNames(list(features), feature_col),
    as.data.frame(values, stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )
  out
}
