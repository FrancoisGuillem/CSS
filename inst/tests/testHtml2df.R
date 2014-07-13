context("Creating a data.frame with html2df")

doc <- "<html>
<head></head>
<body>
  <div id='player1' class='player'>
    <span class='name'>Mike</SPAN>
    <span class='level'>10</span>
    <span class='link'>
      <a href='http://someurl.com'>Complete profile</a>
    </span>
  </div>
  <div id='player2' class='player'>
    <span class='name'>Stan</span>
    <span class='link'>
      <a href='http://someurl2.com'>Complete profile</a> 
    </span>
  </div>
  <div id='player3' class='player'>
    <span class='name'>Bruce</span>
    <span class='level'>21</span>
    <span class='link'>
      <a href='http://someurl3.com'>Complete profile</a>
    </span>
  </div>
</body>
</html>"

doc <- htmlParse(doc)

players <- html2df(doc, ".player", 
                   c(Id=".", Name=".name", Level=".level", URL="a"), 
                   c(cssId, cssCharacter, cssNumeric, cssLink))

test_that("result has good dimensions", {
  expect_is(players, "data.frame")
  expect_equal(dim(players), c(3, 4))
})

test_that("colnames are OK", {
  expect_equal(names(players), c("Id", "Name", "Level", "URL"))
})

test_that("Col classes are OK", {
  expect_equivalent(sapply(players, class), 
                    c("factor", "factor", "numeric", "factor"))
})

test_that("Values are OK", {
  expect_equal(as.character(players[,1]), c("player1", "player2", "player3"))
  expect_equal(as.character(players[,2]), c("Mike", "Stan", "Bruce"))
  expect_equal(as.character(players[,3]), c("10", NA, "21"))
  expect_equal(as.character(players[,4]), c("http://someurl.com", 
                                            "http://someurl2.com", 
                                            "http://someurl3.com"))
})