# a hack before https://github.com/r-lib/pkgdown/issues/1173 is fixed
f <- list.files("man", "^[A-Z].*\\.Rd", full.names = TRUE)
update_ref <- function (Rd) {
    if (!length(Rd)) return(NULL)
    l <- data.table::fread(Rd, sep = NULL, header = FALSE, col.names = "string")
    ln <- l[stringi::stri_detect_fixed(string, '\\item \\out{<span class="pkg-link" data-pkg="eplusr"'), which = TRUE]
    if (!length(ln)) return(NULL)

    l[ln, string := stringi::stri_replace_all_fixed(string,
        "\\href{../../eplusr/html",
        "\\href{https://hongyuanjia.github.io/eplusr/reference",
    )]

    data.table::fwrite(l, Rd, col.names = FALSE, quote = FALSE)
    TRUE
}
lapply(f, update_ref)
return(1L)
