### Fifth submission (Version 1.0.4)

>   Days since last update: 4
Why is such a quick update needed? Please explain. Please also re-read
the CRAN policies about submission frequency.

This was entirely my fault, sorry. The fixes since my last submission were actually pretty small, so the update was not necessary at all. It's my first package and I had forgotten about submission frequency policies. Rest assured that it won't happen again and thanks for your time!

### Fourth submission (Version 1.0.3)

> Note: found 573 marked UTF-8 strings 

Actually, the package expects data to be in UTF-8 (as is the case of most exports from bibliographic databases) in order to deal with it accordingly, so the test data reflects this. Converting it to ASCII would reduce its usefulness in debugging. The [R packages' section about non-ASCII data](https://r-pkgs.org/data.html#sec-data-non-ascii) also helped me choose to keep the UTF-8 strings. I'm willing to reconsider this if necessary, though. 

### Third submission

> We still see one print() call in R/05-biblioverlap.R. Please also change this one to a message() call.

Sorry about that. It's been fixed now.


### Second submission

> Please proof-read your description text. 
Currently it reads: "...  identification of overlapping documents overlap through the  ..."
Probably it should be: "...  identification of overlapping documents through the  ..."

Sorry for the mistake. It has been corrected. Thanks!

> Please write references in the description of the DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: authors (year) <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title") 

Unfortunately, there are no references to include regarding this package yet.

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user.
Does not seem necessary.
Please unwrap the examples if they are executable in < 5 sec, or replace \dontrun{} with \donttest{}.
Functions which are supposed to only run interactively (e.g. shiny) should be wrapped in if(interactive()).
e.g.: biblioverApp.Rd

Thanks for the suggestion! I've wrapped the shiny app function with if(interactive()). I've also reduced example runtime by using only a fraction of the package data.

> You write information messages to the console that cannot be easily suppressed. It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. 
Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions) 

Every print() call has been replaced by message(). Thanks!

> Please ensure that you do not use more than 2 cores in your examples, vignettes, etc. e.g.: inst/biblioverApp/ui_components.R

The contents of inst/biblioverApp/ui_components.R have been added to inst/biblioverApp/ui.R. Thus, the shiny app only uses 2 cores now.

> Please do not install packages in your functions, examples or vignette. This can make the functions,examples and cran-check very slow. e.g.: inst/biblioverApp/app.R 

Sorry about that. It's been fixed now!

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
