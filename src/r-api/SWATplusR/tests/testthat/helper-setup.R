library(SWATplusR)
library(testthat)
library(DBI)
library(RSQLite)

# Create a fresh in-memory database for each test
new_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  SWATplusR::create_project_tables(con)
  con
}
