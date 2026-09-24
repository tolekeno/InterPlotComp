# A small, known marker matrix used by several tests below.
toy_markers <- function(n = 30L, m = 200L, seed = 7L) {
  set.seed(seed)
  M <- matrix(stats::rbinom(n * m, 2, 0.35), n, m,
              dimnames = list(sprintf("G%02d", seq_len(n)), NULL))
  M
}

test_that("kinship_to_ginv reproduces the ainverse() representation", {
  K <- diag(4)
  dimnames(K) <- list(letters[1:4], letters[1:4])
  g <- kinship_to_ginv(K)

  expect_s3_class(g, "ginv")
  expect_equal(colnames(g), c("Row", "Column", "Ainverse"))
  expect_equal(attr(g, "rowNames"), letters[1:4])
  expect_true(all(g[, "Row"] >= g[, "Column"]))   # lower triangle only
  # log|K|, not log|K inverse|: the sign matters to the reported log-likelihood.
  expect_equal(attr(g, "logdet"), 0, tolerance = 1e-10)
  expect_equal(unname(attr(g, "inbreeding")), rep(0, 4))
})

test_that("kinship_to_ginv inverts a non-trivial matrix correctly", {
  K <- matrix(c(1, 0.5, 0.5, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  g <- kinship_to_ginv(K)
  back <- ginv_to_matrix(g)
  expect_equal(back, K, tolerance = 1e-8)
  expect_equal(attr(g, "logdet"), log(det(K)), tolerance = 1e-10)
})

test_that("kinship_to_ginv demands identifiers and refuses a singular matrix", {
  expect_error(kinship_to_ginv(diag(3)), "row names")
  K <- matrix(1, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  expect_error(kinship_to_ginv(K), "could not be inverted")
})

test_that("ginv_to_matrix declines to densify a very large matrix", {
  K <- diag(5)
  dimnames(K) <- list(sprintf("g%d", 1:5), sprintf("g%d", 1:5))
  expect_null(ginv_to_matrix(kinship_to_ginv(K), max_n = 3L))
})

test_that("sort_pedigree places every parent before its progeny", {
  ped <- data.frame(
    ID   = c("kid", "dad", "mum"),
    Sire = c("dad", NA, NA),
    Dam  = c("mum", NA, NA),
    stringsAsFactors = FALSE
  )
  out <- sort_pedigree(ped)
  expect_lt(match("dad", out$ID), match("kid", out$ID))
  expect_lt(match("mum", out$ID), match("kid", out$ID))
})

test_that("sort_pedigree reports a pedigree loop instead of hanging", {
  ped <- data.frame(ID = c("a", "b"), Sire = c("b", "a"),
                    Dam = c(NA, NA), stringsAsFactors = FALSE)
  expect_error(sort_pedigree(ped), "loop")
})

test_that("markers_to_grm builds a positive-definite G with VanRaden scaling", {
  G <- markers_to_grm(toy_markers())
  expect_equal(dim(G), c(30L, 30L))
  expect_equal(G, t(G), tolerance = 1e-10)
  expect_gt(min(eigen(G, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_equal(attr(G, "blend"), 0.01)
  expect_lte(attr(G, "n_markers"), 200L)
  expect_equal(rownames(G), rownames(toy_markers()))
})

test_that("markers_to_grm accepts -1/0/1 coding as well as 0/1/2", {
  M <- toy_markers()
  a <- markers_to_grm(M)
  b <- markers_to_grm(M - 1)
  expect_equal(a, b, tolerance = 1e-8)
})

test_that("markers_to_grm mean-imputes missing calls", {
  M <- toy_markers()
  M[1, 1:5] <- NA
  expect_silent(G <- markers_to_grm(M))
  expect_false(anyNA(G))
})

test_that("markers_to_grm refuses a matrix with too little information", {
  M <- matrix(2, 5, 50, dimnames = list(sprintf("G%d", 1:5), NULL))  # monomorphic
  expect_error(markers_to_grm(M), "informative marker")
  expect_error(markers_to_grm(unname(toy_markers())), "genotype identifiers")
})

test_that("read_square_matrix validates shape, symmetry and completeness", {
  ok <- data.frame(id = c("a", "b"), a = c(1, 0.5), b = c(0.5, 1))
  K <- read_square_matrix(ok)
  expect_equal(dimnames(K), list(c("a", "b"), c("a", "b")))

  expect_error(read_square_matrix(data.frame(id = c("a", "b"), a = c(1, 0.5))),
               "must be square")
  asym <- data.frame(id = c("a", "b"), a = c(1, 0.9), b = c(0.1, 1))
  expect_error(read_square_matrix(asym), "not symmetric")
  gap <- data.frame(id = c("a", "b"), a = c(1, NA), b = c(NA, 1))
  expect_error(read_square_matrix(gap), "missing values")
})

test_that("build_relationship returns NULL when no relationship is wanted", {
  expect_null(build_relationship("none"))
  expect_null(build_relationship("kinship", raw = NULL))
})

test_that("build_relationship assembles a genomic matrix end to end", {
  rel <- build_relationship("markers",
                            data.frame(id = rownames(toy_markers()),
                                       toy_markers(), check.names = FALSE))
  expect_named(rel, c("ginv", "ids", "type", "label", "matrix", "diagnostics"))
  expect_equal(rel$type, "markers")
  expect_length(rel$ids, 30L)
  expect_match(rel$label, "VanRaden")
  expect_equal(rel$diagnostics$n, 30L)
})

test_that("build_relationship blends a singular kinship matrix rather than refusing", {
  # A rank-deficient matrix is routine for marker-derived kinship; the user
  # intent is unambiguous, so it is repaired, not rejected.
  K <- matrix(1, 4, 4, dimnames = list(letters[1:4], letters[1:4]))
  raw <- data.frame(id = letters[1:4], as.data.frame(K), check.names = FALSE)
  rel <- build_relationship("kinship", raw, blend = 0.05)
  expect_equal(rel$type, "kinship")
  expect_length(rel$ids, 4L)
})

test_that("build_relationship rejects an unknown source", {
  expect_error(build_relationship("astrology", raw = data.frame(a = 1)),
               "Unknown relationship source")
})

test_that("align_relationship re-levels the genetic factors onto the matrix ids", {
  nb <- prepared_single()
  ids <- levels(nb$data$Geno)
  rel <- list(ginv = NULL, ids = c(ids, "ANCESTOR_1"), type = "pedigree")

  out <- align_relationship(nb$data, nb$names, rel)
  expect_equal(levels(out$data$Geno), rel$ids)
  expect_equal(levels(out$data$N1), rel$ids)
  expect_equal(out$coverage$n_extra, 1L)
  expect_equal(out$coverage$n_ids, length(ids) + 1L)
})

test_that("align_relationship refuses to drop an observed genotype silently", {
  nb <- prepared_single()
  ids <- levels(nb$data$Geno)
  rel <- list(ginv = NULL, ids = ids[-1], type = "pedigree")
  expect_error(align_relationship(nb$data, nb$names, rel),
               "absent from the relationship matrix")
})

test_that("align_relationship is a no-op when there is no relationship", {
  nb <- prepared_single()
  out <- align_relationship(nb$data, nb$names, NULL)
  expect_identical(out$data, nb$data)
  expect_null(out$coverage)
})

test_that("a pedigree builds an A-inverse through ASReml", {
  skip_without_asreml()
  rel <- build_relationship(
    "pedigree", sample_pedigree(),
    list(id = "Genotype", sire = "Male_parent", dam = "Female_parent"))

  expect_equal(rel$type, "pedigree")
  expect_s3_class(rel$ginv, "ginv")
  expect_equal(rel$diagnostics$n_founders, 10L)
  expect_true(all(sample_single_trial()$Genotype %in% rel$ids))
})

test_that("a pedigree with a duplicated or self-referential entry is rejected", {
  ped <- sample_pedigree()
  map <- list(id = "Genotype", sire = "Male_parent", dam = "Female_parent")

  dup <- rbind(ped, ped[nrow(ped), ])
  expect_error(build_relationship("pedigree", dup, map), "more than once")

  self <- ped
  self$Male_parent[nrow(self)] <- self$Genotype[nrow(self)]
  expect_error(build_relationship("pedigree", self, map), "its own parent")
})
