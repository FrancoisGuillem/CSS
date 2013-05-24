context("Extracting data from a html document")

doc <- "<html>
<head></head>
<body>
<div id='player1' class='player'>
<span class='name'>Mike</SPAN>
<span class='level'>10</span>
<a href='http://someurl.com'>Complete profile</a>
</div>
<div id='player2' class='player'>
<span class='name'>Stan</span>
<a href='http://someurl2.com'>Complete profile</a> </div>
<div id='player3' class='player'>
<span class='name'>Bruce</span>
<span class='level'>21</span>
<a href='http://someurl3.com'>Complete profile</a>
  </div>
<DIV CLASS='TEST'>insensitive</DIV>
</body>
</html>"

doc <- htmlParse(doc)

test_that("Function cssApply extracts desired data.", {
  expect_equal(cssApply(doc, ".player>.name", cssCharacter), 
               c("Mike", "Stan", "Bruce"))
  expect_equal(cssApply(doc, "#player1>.name", cssCharacter), 
               "Mike")
  expect_equal(cssApply(doc, ".player>a", cssLink), 
               c("http://someurl.com", 
                 "http://someurl2.com", 
                 "http://someurl3.com"))
  expect_equal(cssApply(doc, ".player>.level", cssNumeric), 
               c(10, 21))
  expect_equal(cssApply(doc, "*[@id]", cssId),
               c("player1", "player2", "player3"))
  expect_equal(cssApply(doc, "*[@id @class]", cssId),
               c("player1", "player2", "player3"))
})

test_that("cssApplyInNodeSet works correctly", {
  expect_equal(cssApplyInNodeSet(doc, ".player", ".level", cssNumeric),
               list(10, NA, 21))
})

test_that("functions are case insensitive", {
  expect_equal(cssApply(doc, "div.TEST", cssCharacter), 
               "insensitive")
  expect_equal(cssApply(doc, "DIV.test", cssCharacter), 
               "insensitive")
  expect_equal(cssApply(doc, ".test[@CLASS]", cssCharacter),
               "insensitive")
  expect_equal(cssApply(doc, "div.test", cssCharacter),
               "insensitive")
})

test_that("functions are insensitive to extra spaces", {
  expect_equal(cssApply(doc, ".player> .name", cssCharacter), 
               c("Mike", "Stan", "Bruce"))
  expect_equal(cssApply(doc, ".player >.name", cssCharacter), 
               c("Mike", "Stan", "Bruce"))
  expect_equal(cssApply(doc, ".player > .name", cssCharacter), 
               c("Mike", "Stan", "Bruce"))
  expect_equal(cssApply(doc, "*[ @id]", cssId),
               c("player1", "player2", "player3"))
  expect_equal(cssApply(doc, "*[@id ]", cssId),
               c("player1", "player2", "player3"))
  expect_equal(cssApply(doc, "*[ @id='player1' ]", cssId),
               c("player1"))
  expect_equal(cssApply(doc, "*[ @id = 'player1' ]", cssId),
               c("player1"))
})