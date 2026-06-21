fixed_terms_to_regex = function(txt, space_to_ws = TRUE) {
  regex = stringi::stri_escape_unicode(txt)
  regex = stringi::stri_replace_all_regex(
    regex,
    "([][{}()+*^$|?.\\\\-])",
    "\\\\$1"
  )

  if (space_to_ws) {
    regex = stringi::stri_replace_all_fixed(regex, " ", "\\s+")
  }

  stringi::stri_join("(?:", regex, ")", collapse = "|")
}
