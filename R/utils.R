alphabet <- data.frame(original = c("А",
                                    "Б",
                                    "В",
                                    "Г",
                                    "Ґ",
                                    "Д",
                                    "Е",
                                    "Є",
                                    "Ж",
                                    "З",
                                    "И",
                                    "Й",
                                    "І",
                                    "Ї",
                                    "К",
                                    "Л",
                                    "М",
                                    "Н",
                                    "О",
                                    "П",
                                    "Р",
                                    "С",
                                    "Т",
                                    "У",
                                    "Ф",
                                    "Х",
                                    "Ц",
                                    "Ч",
                                    "Ш",
                                    "Щ",
                                    "Ь",
                                    "Ю",
                                    "Я",
                                    "а",
                                    "б",
                                    "в",
                                    "г",
                                    "ґ",
                                    "д",
                                    "е",
                                    "є",
                                    "ж",
                                    "з",
                                    "и",
                                    "й",
                                    "і",
                                    "ї",
                                    "к",
                                    "л",
                                    "м",
                                    "н",
                                    "о",
                                    "п",
                                    "р",
                                    "с",
                                    "т",
                                    "у",
                                    "ф",
                                    "х",
                                    "ц",
                                    "ч",
                                    "ш",
                                    "щ",
                                    "ь",
                                    "ю",
                                    "я",
                                    letters,
                                    LETTERS,
                                    0:9,
                                    "!",
                                    "@",
                                    "#",
                                    "$",
                                    "%",
                                    "^",
                                    "&",
                                    "*",
                                    "(",
                                    ")",
                                    "-",
                                    "_",
                                    "+",
                                    "=",
                                    "`",
                                    "~",
                                    "[",
                                    "]",
                                    "{",
                                    "}",
                                    ";",
                                    ":",
                                    "'",
                                    "<",
                                    ">",
                                    ",",
                                    ".",
                                    "/",
                                    " "),
                       stringsAsFactors = FALSE)


encrypt_decrypt <- function(text, .alphabet, decrypt) {
  if (is.na(text)) {
    return(NA)
  }
  if (is.null(text)) {
    return(NA)
  }

  if (decrypt == FALSE) {
    for (i in 1:nchar(text)) {
      index_num <- which(substr(text, i, i) == .alphabet$original)
      substr(text, i, i) <- .alphabet$cipher[index_num]
    }
  } else {
    for (i in 1:nchar(text)) {
      index_num <- which(substr(text, i, i) == .alphabet$cipher)
      substr(text, i, i) <- .alphabet$original[index_num]
    }
  }
  return(text)
}
