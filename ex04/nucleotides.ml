type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide =
    {
        phosphate:phosphate;
        deoxyribose:deoxyribose;
        nucleobase:nucleobase
    }

let chr_to_nucleobase chr = match chr with
    | 'A'   -> A
    | 'C'   -> C
    | 'T'   -> T
    | 'U'   -> U
    | 'G'   -> G
    | _     -> None

let generate_nucleotide chr =
    {
        phosphate   = "phosphate";
        deoxyribose = "deoxyribose";
        nucleobase  = chr_to_nucleobase chr
    }
