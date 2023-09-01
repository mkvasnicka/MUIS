test_that("url creation", {
  cred <- credentials(
    key = "Z6o4VCwTOPQYGWI9",
    faculty = 1456,
    course = "BPE_MIE1"
  )
  expect_equal(
    is_operation(cred, "predemet-info", a = "a", b = letters[1:3]),
    paste0(
      "https://is.muni.cz/export/pb_blok_api?klic=Z6o4VCwTOPQYGWI9;",
      "fakulta=1456;kod=BPE_MIE1;operace=predemet-info;a=a;b=a;b=b;b=c"
    )
  )
})
