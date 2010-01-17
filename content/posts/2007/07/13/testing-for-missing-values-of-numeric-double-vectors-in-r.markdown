I just came across this bit of <a href="http://www.r-project.org">R</a> trickiness when trying to improve the error messages for one of our packages.  I tried adding a check at the C level for NA_REAL, but to no avail.  The test failed even when I was certain that I was passing down a vector containing missing values.  Consulting the <a href="http://cran.r-project.org/doc/manuals/R-exts.html#Missing-and-special-values">Writing R Extensions Manual (WREM)</a> led me quickly to ISNA.  The WREM says that ISNA only applies to numeric values of type double and that other types can be checked by equality comparison to NA_INTEGER, NA_LOGICAL, etc.  What could perhaps be better emphasized is that ISNA is the only appropriate way to test for missingness of REALSXP elements and that equality testing with NA_REAL does not work.

It is an easy mistake to make since one might be lulled into complacency by the repeating patterns of a switch statement.  Here's how NOT to do it:


<quickcode:noclick>
SEXP hello_hasNA(SEXP v)
{
    int i, found_na = 0;

    for (i = 0; i < length(v); i++) {
        switch (TYPEOF(v)) {
        case INTSXP:
            if (INTEGER(v)[i] == NA_INTEGER)
                found_na = 1;
            break;
        case LGLSXP:
            if (LOGICAL(v)[i] == NA_LOGICAL)
                found_na = 1;
            break;
        case REALSXP:
            if (REAL(v)[i] == NA_REAL) /* WRONG, must use ISNA() */
                found_na = 1;
            break;
        case STRSXP:
            if (STRING_ELT(v, i) == NA_STRING)
                found_na = 1;
            break;
        default:
            error("no support for type");
        }
        if (found_na)
            break;
    }
    return ScalarLogical(found_na);
}
</quickcode>


To fix things, replace the REALSXP case like this:

<quickcode:noclick>
        case REALSXP:
            if (ISNA(REAL(v)[i]))
                found_na = 1;
            break;
</quickcode>


<!-- technorati tags start --><p style="text-align:right;font-size:10px;">Technorati Tags: <a href="http://www.technorati.com/tag/programming" rel="tag">programming</a>, <a href="http://www.technorati.com/tag/R" rel="tag">R</a></p><!-- technorati tags end -->
