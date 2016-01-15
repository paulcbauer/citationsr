# Wrong ecodings that came out of xpdf extraction
# x <- stringr::str_replace_all(x, "\\<U\\+FB00\\>", "JUNIOR")
# x <- stringr::str_replace_all(x, "\\<U\\+FB01\\>", "fi")
# x <- stringr::str_replace_all(x, "\\<U\\+FB02\\>", "fl")
# x <- stringr::str_replace_all(x, "\\<U\\+03B8\\>", "THETA")
# x <- stringr::str_replace_all(x, "\\<U\\+2AFA\\>", "-")
# x <- stringr::str_replace_all(x, "\\<U\\+2AF9\\>", "+")
# x <- stringr::str_replace_all(x, "\\<U\\+2AFD\\>", "=")
# x <- stringr::str_replace_all(x, "\\<U\\+02C7\\>", "PI")
# ADD VOCABULARY HERE




#

finds <- agrep(pattern, x, max.distance = 0.7, costs = NULL,
               ignore.case = TRUE, value = TRUE, fixed = TRUE,
               useBytes = FALSE)


finds <- agrep(pattern, x, max.distance = 0.7, costs = NULL,
               ignore.case = TRUE, value = TRUE, fixed = TRUE,
               useBytes = FALSE)

if(table(finds)[1]>=5){
  pattern <- names(table(finds)[1])
  locations1 <- grep(pattern, x, ignore.case = FALSE)
  locations1.names <- grep(pattern, x, ignore.case = FALSE, value = T)
  x <- x[-locations1]
  print(locations1.names)
}
