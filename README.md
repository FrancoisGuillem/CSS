CSS
===

Extracting data from an html file may be done in R with package XML, but it is quite tedious.
This is why I have developed this package : it provides wrapper functions that take as input
CSS path and translate them in xpath queries. Moreover some functions facilitate extraction
of specific information like numbers or urls.



INSTALLATION
------------

In R console, execute the following commands

    install.packages("devtools")
    library(devtools)
    install_github("CSS", "cuche27")



USAGE
-----

    library(CSS)

    # Let's create a fake html page
    doc <- "<html>
    <head></head>
    <body>
      <div id='character1' class='character'>
        <span class='name'>Mike</span>
        <span class='level digit'>10</span>
        <a href='http://someurl.com'>Complete profile</a>
      </div>
      <div id='character2' class='character'>
        <span class='name'>Stan</span>
        <a href='http://someurl2.com'>Complete profile</a>
      </div>
    </body>
    </html>"

    # parse the html
    doc <- htmlParse(doc)

    # Extract the names of the characters
    cssApply(doc, ".character>.name", cssCharacter)

    # Extract the name of character1
    cssApply(doc, "#character1>.name", cssCharacter)

    # Urls of the profiles
    cssApply(doc, ".character>a", cssLink)

    # Level of characters
    cssApply(doc, ".character>.level", cssNumeric)

    # character 2 does not have level, we would want to have a NA value instead of nothing
    cssApplyInNodeSet(doc, ".character", ".level", cssNumeric)
