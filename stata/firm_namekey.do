capture program drop namekey
program define namekey
    version 17
    syntax varname(string), GENerate(name)

    quietly {
        tempvar s
        gen `s' = ustrtrim(ustrupper(`varlist'))

        * Bracketed text is descriptive, not part of the name.
        replace `s' = ustrregexra(`s', "\([^)]*\)", " ")

        * "ACME INC DBA WIDGETS" / "... F/K/A ..." -> keep the head.
        replace `s' = ustrregexra(`s', "\s+D/?B/?A\s+.*$", " ")
        replace `s' = ustrregexra(`s', "\s+F/?K/?A\s+.*$", " ")
        replace `s' = ustrregexra(`s', "\s+N/?K/?A\s+.*$", " ")

        * "ACME CORP, A DELAWARE CORPORATION" -> "ACME CORP"
        replace `s' = ustrregexra(`s', ",\s*(A|AN|THE)\s+.*$", " ")

        * Protect the ampersand across the punctuation strip, then restore it.
        replace `s' = subinstr(`s', "&", " AND ", .)
        replace `s' = ustrregexra(`s', "[^A-Z0-9 ]+", " ")
        replace `s' = stritrim(strtrim(`s'))

        * Word-level standardisations, applied before suffixes are stripped.
        * "from:to" pairs; no element contains a space, so no quoting is needed
        * (quoted elements break macro expansion inside the loop).
        local abbrev ///
            MANUFACTURING:MFG MANUFACTURERS:MFG MANUFACTURER:MFG ///
            TECHNOLOGIES:TECH TECHNOLOGY:TECH ///
            LABORATORIES:LAB LABORATORY:LAB LABS:LAB ///
            PHARMACEUTICALS:PHARM PHARMACEUTICAL:PHARM ///
            INDUSTRIES:IND INDUSTRY:IND INDUSTRIAL:IND ///
            SYSTEMS:SYS SYSTEM:SYS ///
            SERVICES:SVC SERVICE:SVC ///
            PRODUCTS:PROD PRODUCT:PROD ///
            COMMUNICATIONS:COMM COMMUNICATION:COMM ///
            RESOURCES:RES RESOURCE:RES ///
            SOLUTIONS:SOLN SOLUTION:SOLN ///
            ASSOCIATES:ASSOC ASSOCIATION:ASSOC ///
            BROTHERS:BROS SAINT:ST AND:&
        foreach pair of local abbrev {
            local cut  = strpos("`pair'", ":")
            local from = substr("`pair'", 1, `cut' - 1)
            local to   = substr("`pair'", `cut' + 1, .)
            replace `s' = ustrregexra(`s', "\b`from'\b", "`to'")
        }
        replace `s' = stritrim(strtrim(`s'))

        * Leading article: "THE PROCTER & GAMBLE" -> "PROCTER & GAMBLE"
        replace `s' = ustrregexra(`s', "^(THE|A|AN)\s+", "")

        * Trailing legal forms, stripped repeatedly so that chains such as
        * "ACME CORP HOLDINGS INC" reduce to "ACME". Each pass also clears a
        * connector left dangling by the strip ("MERCK &" -> "MERCK").
        local suf "INCORPORATED|INC|CORPORATION|CORP|COMPANY|COMPANIES|CO"
        local suf "`suf'|LIMITED|LTD|LLC|LLP|LP|PLC|HOLDINGS|HOLDING|GROUP|GRP"
        local suf "`suf'|NV|BV|SA|SPA|AG|GMBH|KG|AB|AS|OY|SE"
        local suf "`suf'|THE|A|AN|OF|AND|CL|CLASS|ADR|ADS|SPN|SPONSORED"
        local suf "`suf'|TRUST|PARTNERS|PARTNERSHIP|ENTERPRISES|ENTERPRISE"
        local suf "`suf'|INTERNATIONAL|INTL|WORLDWIDE|GLOBAL|USA|US|AMERICA|AMERICAN"
        forvalues k = 1/8 {
            replace `s' = ustrregexra(`s', "\s+(`suf')$", "")
            replace `s' = ustrregexra(`s', "\s*&\s*$", "")
            replace `s' = strtrim(`s')
        }
        replace `s' = ustrregexra(`s', "^\s*&\s*", "")

        gen `generate' = stritrim(strtrim(`s'))
    }
end
