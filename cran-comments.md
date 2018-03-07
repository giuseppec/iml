# v0.2

## Re-submission comment
Re-Submission 

According to the feedback, I made the following changes: 
- Rephrased the Description in DESCRIPTION
- Referenced relevant methods in the Description with doi/arxiv
- Added examples to all exported functions
- Increased the version from 0.2 to 0.2.1

Feedback: 

Please omit "The iml package" and just stat with "Provides model-agnostic...".

Please add a reference for the method in the 'Description' field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

Please add small executable examples in your Rd-files.
Something like
\examples{
       examples for users and checks:
       executable in < 5 sec
       \dontshow{
              examples for checks:
              executable in < 5 sec together with the examples above
              not shown to users
       }
       donttest{
              further examples for users (not used for checks)
       }
}
would be desirable.

Please fix and resubmit.


## Feedback
Thanks, please write package names and software names in single quotes (e.g. 'iml') in title and description.

Please omit "The iml package" and just stat with "Provides model-agnostic...".

Please add a reference for the method in the 'Description' field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

Please add small executable examples in your Rd-files.
Something like
\examples{
       examples for users and checks:
       executable in < 5 sec
       \dontshow{
              examples for checks:
              executable in < 5 sec together with the examples above
              not shown to users
       }
       donttest{
              further examples for users (not used for checks)
       }
}
would be desirable.

Please fix and resubmit.


## Test environments
* local Debian 9.3 stretch, R 3.3.3
* local Mac OS High Sierra 10.13.3 (17D102)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
  
  *  NOTE
Maintainer: 'Christoph Molnar <christoph.molnar@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Interpretable (3:8)
  iml (9:18)
  interpretability (9:63)

