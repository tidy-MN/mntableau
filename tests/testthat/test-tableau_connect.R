test_that("can connect", {
  expect_no_error(tableau_connect("mdh_internal_dev",
                                  username = "nageld1",
                                  password = keyring::key_get("login", "nageld1")
  ))
})

test_that("only one site allowed", {
  expect_error(tableau_connect(c("mdh_internal_dev", "mdh_internal_prod"),
                                  username = "nageld1",
                                  password = keyring::key_get("login", "nageld1")
  ))
})

test_that("site must be on list", {
  expect_error(tableau_connect("not_a_tableau_site",
                               username = "nageld1",
                               password = keyring::key_get("login", "nageld1")
  ))
})

test_that("get project id", {
  conn <- tableau_connect("mdh_internal_dev",
                                  username = "nageld1",
                                  password = keyring::key_get("login", "nageld1")
  )

  expect_identical(get_project_id(conn, "MNIT-MDH"), "cc059b37-8270-4975-a635-c88de4619c39")

})

test_that("error if no matching project", {
  conn <- tableau_connect("mdh_internal_dev",
                          username = "nageld1",
                          password = keyring::key_get("login", "nageld1")
  )

  expect_error(get_project_id(conn, "abcdef"))

})

test_that("error if multiple matching projects", {
  conn <- tableau_connect("mdh_internal_dev",
                          username = "nageld1",
                          password = keyring::key_get("login", "nageld1")
  )

  expect_error(get_project_id(conn, "Newborn"))

})

test_that("no error if parent_name is provided", {
  conn <- tableau_connect("mdh_internal_dev",
                          username = "nageld1",
                          password = keyring::key_get("login", "nageld1")
  )

  expect_identical(get_project_id(conn, "Newborn", parent_name = "PHL"), "a5adeafa-8c66-4fea-b6d8-52422107129b")

})
