### Thank you for your Pull Request!

We have developed a Pull Request template to aid you and our reviewers. Completing the below tasks helps to ensure our reviewers can maximize their time on your code as well as making sure the graphicalMCP codebase remains robust and consistent.

### Changes Description

_(descriptions of changes)_

### Task List

This is a list of tasks that can be done by contributors to make reviewing faster and more straightforward. If you're unsure how to do any or all of these, please make your contributions anyway. We are happy to work with you on any of them! However, know that your contributions may be more likely to be accepted, or be accepted more quickly, the more of these you can accomplish. They are roughly sorted in descending order of helpfulness.

- [ ] Fill out **Changes Description** above
- [ ] Update relevant unit tests or write new unit tests
- [ ] Create/update relevant roxygen headers and examples
- [ ] Run `pkgdown::build_site()` and check that all affected examples are displayed correctly and that all new/updated functions occur on the "Reference" page
- [ ] Update NEWS.md with a brief summary of changes made - Note the related issue number(s) at the end like (#000)
- [ ] Make sure that the package versions in the NEWS.md and DESCRIPTION file are the same
- [ ] Place Closes #<insert_issue_number> into the beginning of your Pull Request Title (Use Edit button in top-right if you need to update)
- [ ] Format code according to the [tidyverse style guide](https://style.tidyverse.org/) (Run `styler::style_pkg()`)
- [ ] Run `devtools::document()` so all `.Rd` files in the `man` folder and the `NAMESPACE` file in the project root are updated appropriately
- [ ] Run `codemetar::write_codemeta()` to update codemeta.json
- [ ] Address any updates needed for vignettes and/or templates
- [ ] Link the issue Development Panel so that the related issue closes after successful merging
- [ ] Fix merge conflicts
- [ ] Pat yourself on the back for a job well done! Here's a random cat gif for your hard work :)

<img src="http://edgecats.net/' + Math.random() + 0 + '"/>
