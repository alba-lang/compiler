include Fmlib_pretty.Print


module Doc =
struct
    let p (lst: doc list): doc =
        pack " " lst <+> cut

    let ps = paragraphs
end
