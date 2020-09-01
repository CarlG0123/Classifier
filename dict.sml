
functor TreeDict(Key : ORDERED) : DICT =
struct

  fun log2 (n : int) : int =
      case n of
          0 => 0 (* hack *)
        | 1 => 1
        | _ => 1 + log2 (n div 2)

  structure Key : ORDERED = Key

  (* invariant: sorted according to Key.compare *)
  datatype 'v tree =
      Empty
    | Node of 'v tree * (Key.t * 'v) * 'v tree

  type 'v dict = 'v tree

  val empty = Empty

  fun depth (t : 'v tree) : int =
      case t of
          Empty => 0
        | Node(l,_,r) => 1 + Int.max(depth l, depth r)

  (* TASK: copy size here *)
  fun size d =
    case d of
        Empty => 0
    | Node(L, (k', v'), R) => (size L) + (size R) + 1

  fun print_stats (t : 'v tree) : unit =
      print ("depth : " ^ Int.toString (depth t) ^
             " log2 size : " ^ Int.toString (log2 (size t)) ^ "\n")

  fun lookup d k =
      let fun lk d =
          case d of
              Empty => NONE
            | Node (L, (k', v'), R) =>
                  case Key.compare (k,k') of
                      EQUAL => SOME v'
                    | LESS => lk L
                    | GREATER => lk R
      in
          lk d
      end

  fun lookup' d k = valOf (lookup d k)

  fun insert d (k, v) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, v), R)
        | LESS => Node (insert L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insert R (k, v))

  fun insertWith (c : 'v * 'v -> 'v) (d : 'v dict) (k : Key.t, v : 'v) : 'v dict =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case Key.compare (k,k') of
          EQUAL => Node (L, (k, (c(v,v'))), R)
        | LESS => Node (insertWith c L (k, v), (k', v'), R)
        | GREATER => Node (L, (k', v'), insertWith c R (k, v))

  fun map f d =
      case d of
          Empty => Empty
        | Node(l,(k,v),r) => Node (map f l , (k, f v) , map f r)


  (* TASK: copy your split and merge here and rename merge to merge' *)
  fun splitAt d k =
    case d of
        Empty => (Empty, Empty, NONE)
    | Node(L, (k', v'), R) =>
        (case Key.compare (k,k') of
            LESS => let val (LL,LR,v) = splitAt L k
                    in (LL, Node(LR, (k', v'),R), v)
                    end
          | GREATER => let val (RL,RR,v) = splitAt R k
                        in (Node(L, (k', v'),RL),RR, v)
                       end
          | EQUAL => (L,R, SOME v')
          )


  fun merge' c (d1, d2) =
      case d1 of
          Empty => d2
      | Node(L1, (k1', v1'), R1) =>
          let val(L2, R2, v) = splitAt d2 k1'
          in
              case v of
                  NONE => Node(merge' c (L1,L2), (k1', v1'), merge' c (R1,R2))
              | SOME v2' => Node(merge' c (L1,L2), (k1', c(v1',v2')), merge' c (R1,R2))
          end

  (* optimize inserts: if merging with a 1-element dictionary, insert instead *)
  fun merge c (d1,d2) =
      case d1 of
          Node(Empty, kv1, Empty) => insertWith c d2 kv1
        | _ => case d2 of
                 Node(Empty, kv2, Empty) => insertWith c d1 kv2
               | _ => merge' c (d1,d2)

  fun fromSeq s =
      Seq.mapreduce (fn (k,v) => insert empty (k,v)) empty (merge (fn(v1,v2) => v1)) s

  (* TASK: copy toSeq here *)
  fun toSeq d =
    case d of
        Empty => Seq.empty ()
    | Node(L, e, R) => (Seq.append (toSeq L) (Seq.cons e (toSeq R)))

  fun valueSeq d =
      case d of
          Empty => Seq.empty()
        | Node (l , (_,v) , r) => Seq.append (valueSeq l) (Seq.cons v (valueSeq r))

  fun max d =
      case d of
          Empty => NONE
        | Node (l, (k,v), r) =>
              case max r of
                  NONE => SOME (k,v)
                | SOME m' => SOME m'
end

functor Dict(K : ORDERED) = TreeDict(K)
