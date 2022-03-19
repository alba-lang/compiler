(** Process File and Directory Paths *)




(** {1 Normalization}

    Paths to the same file or directory are not unique. The following shows a
    list of path pairs which are equivalent.

    {[
        path                        equivalent path
        "././."                     "."

        "/.."                       "/"

        "a/b/.."                    "a"

        "a//b////c"                 "a/b/c"

        ""                          "."
    ]}

    The process of {e normalization} removes duplicate separators and redundant
    redirections. The result of normalization is never the empty string.


*)


val normalize: char -> string -> string
(** [normalize sep path] Normalize the path [path] i.e. remove duplicate
 * separators and redundant [.] and [a/..]. *)


val resolve: char -> string list -> string
(** [resolve sep paths]

    Join the paths in [paths] from right to left, normalize the result and stop
    joining, when an absolute path is the result. The paths in [paths] need not
    be normalized. The result is normalized.
*)




(** {1 Split and Join} *)

val join: char -> string list -> string
(** [join sep paths]

    Join the normalized paths in the list [paths]. All elements must be in
    normal form and all except the first must be relative.

    If the precondition is not satisfied, better use {!resolve}.

    If the precondition is satisfied, then the result is guaranteed to be
    normalized i.e. redundant paths ["."] are removed.
*)


val split: char -> string -> string list
(** [split sep path]

    Split the normalized path into its components. If the input is normalized,
    then the components are normalized as well. If the path is absolute, then
    the first element in the result list is the separator i.e. ["/"] on unix
    systems.

    Join and split are invers operations.
*)



(** {1 Relative Paths} *)

val relative: char -> string -> string -> string
(** [relative char a b]

    Assume that [a] and [b] are absolute paths. Construct a relative path from
    [a] to [b].
*)



(** {1 Dirname and Basename} *)

val dirname: char -> string -> string

val basename: char -> string -> string

val is_root: char -> string -> bool
