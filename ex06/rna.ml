(* -------------------------------------------------------------------------- *)
(* Ex04 *)
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
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Ex05 *)
type helix = nucleotide list

let random_nucleobase n = match n with
    | 0 -> 'A'
    | 1 -> 'T'
    | 2 -> 'C'
    | 3 -> 'G'
    | _ -> 'O'

let nucleobase_to_str nucleo = match nucleo with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | U -> "U"
    | _ -> "?"

(* 42 doesn't allow the String modules, except concatenation, for this
 * exercices --> repeat for chr *)
let nucleobase_to_chr nucleo = match nucleo with
    | A -> 'A'
    | T -> 'T'
    | C -> 'C'
    | G -> 'G'
    | U -> 'U'
    | _ -> '?'

let nucleobase_pairing nucleo = match nucleo with
    | A -> T
    | T -> A
    | C -> G
    | G -> C
    | _ -> None

let rec generate_helix n : helix = match n with
    | 1 -> [(generate_nucleotide (random_nucleobase (Random.int 5)))]
    | y when n > 1 ->
            (generate_nucleotide (random_nucleobase (Random.int 5))) ::
                (generate_helix (n - 1))
    | _ -> []

let rec helix_to_string (h : helix) = match h with
    | [] -> ""
    | head::queue -> (nucleobase_to_str head.nucleobase) ^ (helix_to_string
    queue)

let rec complementary_helix (h : helix) : helix = match h with
    | [] -> []
    | head::queue ->
            (generate_nucleotide (nucleobase_to_chr (nucleobase_pairing
            head.nucleobase))) ::
                (complementary_helix queue)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Ex06 *)
type rna = nucleotide list

let rna_pairing nucleo = match nucleo with
    | A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None

let rec generate_rna (h : helix) : rna = match h with
    | [] -> []
    |  head::queue ->
            (generate_nucleotide (nucleobase_to_chr (rna_pairing
            head.nucleobase))) ::
                (generate_rna queue)
(* -------------------------------------------------------------------------- *)
