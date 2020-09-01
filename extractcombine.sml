
functor ExtractCombine (A : sig
                                structure Key : ORDERED
                                structure MR : MAP_REDUCE
                            end) : EXTRACT_COMBINE =
struct

    structure MR = A.MR

    structure D = Dict(A.Key)

    fun extractcombine e c s =
        MR.mapreduce (fn doc=> Seq.mapreduce (fn x => D.insert D.empty x)  D.empty (D.merge (c)) (e doc)) D.empty (D.merge (c)) s


end
