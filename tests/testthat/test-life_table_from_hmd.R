test_that("simeq_life_table_from_hmd builds a year-indexed table", {
  make <- function(year, shift) {
    data.frame(
      Year = year,
      Age = c(as.character(0:109), "110+"),
      qx = c(rep(0.001 + shift, 110), 1),
      ex = c(seq(80, 1, length.out = 110), 1),
      stringsAsFactors = FALSE
    )
  }
  tables <- list(
    men = rbind(make(1998, 0.002), make(2024, 0.001)),
    women = rbind(make(1998, 0.001), make(2024, 0))
  )

  lt <- simeq_life_table_from_hmd(tables)

  expect_equal(nrow(lt), 2 * 2 * 65)
  expect_equal(range(lt$age), c(20L, 84L))
  expect_setequal(lt$sex, c("men", "women"))
  expect_equal(
    lt$qx[lt$year == 1998 & lt$sex == "men" & lt$age == 30],
    0.003
  )
  expect_silent(simeq_validate_life_table(lt))

  one_year <- simeq_select_life_table_year(lt, 2010L)
  expect_equal(one_year$qx[one_year$sex == "women"][1], 0.002)
})

test_that("simeq_life_table_from_hmd reads HMD text files", {
  path <- tempfile(fileext = ".txt")
  writeLines(
    c(
      "Portugal, Life tables (period 1x1), Males",
      "",
      "  Year          Age         mx       qx    ax      lx      dx      Lx       Tx     ex",
      sprintf(
        "  2020  %10s  0.00100  0.00100  0.50  100000  100  99950  7000000  70.00",
        c(as.character(20:109), "110+")
      )
    ),
    path
  )

  lt <- simeq_life_table_from_hmd(list(men = path))
  expect_equal(nrow(lt), 65L)
  expect_equal(lt$ex[1], 70)
})

test_that("simeq_life_table_from_hmd rejects unnamed input", {
  expect_error(
    simeq_life_table_from_hmd(list(data.frame())),
    "men"
  )
})
