
functor NaiveBayes (ClassSpec : sig
                                  structure Category : ORDERED
                                  val default_category : Category.t

                                  structure Dataset : MAP_REDUCE
                                end) : NAIVE_BAYES_CLASSIFIER =
struct

    type category = ClassSpec.Category.t

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq

    structure Dataset = ClassSpec.Dataset

    structure CatEC = ExtractCombine(struct
                                         structure Key = ClassSpec.Category
                                         structure MR = ClassSpec.Dataset
                                     end)
    structure CatDict = CatEC.D

    structure WordEC = ExtractCombine(struct
                                      (* TASK fill in the structure here
                                         to make a an extractcombine
                                         whose keys are words and whose
                                         mapreducible type is from Dataset
                                         *)
                                         structure Key = StringLt
                                         structure MR = ClassSpec.Dataset
                                      end)
    structure WordDict = WordEC.D

    structure CatWordEC = ExtractCombine(struct
                                               structure Key = PairOrder(struct
                                                                             structure O1 = ClassSpec.Category
                                                                             structure O2 = StringLt
                                                                         end)

                                               structure MR = ClassSpec.Dataset
                                         end)
    structure CatWordDict = CatWordEC.D

    type statistics =
          int CatDict.dict     (* maps each category to number of documents with that category *)
        * int CatDict.dict     (* maps each category to number of words in documents with that category *)
        * int CatWordDict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq     (* list of categories (no duplicates) *)
        * int                  (* total number of documents *)
        * int                  (* total number of different words *)

    (* TASK *)
    fun gather (train : labeled_document Dataset.mapreducable) : statistics =
        let val numDocsPerCat = CatEC.extractcombine (fn (x,y) => Seq.singleton(x,1)) (fn (x,y)=> x+y) train
            val numWordsPerCat = CatEC.extractcombine (fn (x,y) => Seq.singleton(x,Seq.length(y))) (fn (x,y)=> x+y) train
            val CatWordFreq = CatWordEC.extractcombine (fn (cat,doc)=> Seq.map (fn (str) =>((cat, str),1)) doc) (fn (x,y)=> x+y) train
            val CatList = Seq.map (fn (x,y)=>x) (CatDict.toSeq(numDocsPerCat))
            val TotalDocs = Seq.mapreduce (fn (x,y) => y) 0 (fn (x,y)=>x+y) (CatDict.toSeq(numDocsPerCat))
            val TotalWords = WordDict.size(WordEC.extractcombine (fn (x,y) => Seq.map (fn a => (a,1)) y) (fn (x,y)=> x+y) train)
        in
            (numDocsPerCat, numWordsPerCat, CatWordFreq, CatList, TotalDocs,TotalWords)
        end

    (* TASK *)
    fun possible_classifications
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories,
          total_num_docs,
          total_num_words) : statistics)
        (test_doc : document) : (category * real) Seq.seq =
        let fun f(c) =
        (c, Math.ln (real(valOf(CatDict.lookup num_docs_by_cat c))/real(total_num_docs)) +
            Seq.mapreduce (fn x => case (CatWordDict.lookup freqs (c, x)) of
                                    NONE => Math.ln(1.0/real(total_num_words))
                                    | SOME a => Math.ln(real(a)/real(CatDict.lookup' num_words_by_cat c))) 0.0 (fn (x,y) => x+y) test_doc)
        in
            Seq.map f all_categories
        end



    (* TASK *)
    fun classify (stats : statistics)
                 (test_doc : document) : (category * real) =
        let fun max((x1,x2),(y1,y2)) = (case x2>=y2 of
                                true => (x1,x2)
                                | false => (y1,y2))
        in
            Seq.reduce max (ClassSpec.default_category,Real.negInf) (possible_classifications stats test_doc)
        end

    (* TASK *)
    fun train_classifier (train : labeled_document Dataset.mapreducable) : document -> (category * real) =
        let val stats = gather(train)
        in
            classify stats
        end

end
