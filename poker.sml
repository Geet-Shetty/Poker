datatype suite = Clubs | Diamonds | Hearts | Spades
datatype value = Ace | King | Queen | Jack | Num of int
type card = suite * value

fun createDeck () = (* creates deck using tail recursion *)
    let
	val v = [Ace, King, Queen, Jack, Num 10, Num 9, Num 8, Num 7, Num 6, Num 5, Num 4, Num 3, Num 2]
	fun createSet (s',v,acc) =
	    case v of
		[] => acc
	      | hd::tl => createSet(s', tl, (s',hd)::acc)
    in
	createSet( Clubs,    v, 
	createSet( Diamonds, v,
        createSet( Hearts,   v,
	createSet( Spades,   v, [] ) ) ) )
    end 

val Deck = createDeck()

(* RNG stuff could be set up better but this is fine *)
val nextInt = Random.randRange (0,51)
val r = Random.rand(1,2) (* change seed here to different roll patterns *)
(* fun getCards () = List.tabulate(7, fn i => nextInt r) *)

fun getCards () = (* get 7 cards with no duplicates, number of cards could be changed *)
    let
	fun getIndices (acc,L) =
	    if L = 0
	    then acc
	    else 
		let val i = nextInt r in
		    if (List.exists (fn x => x = i) acc)
		    then getIndices(acc,L)
		    else getIndices(i::acc,L-1)
		end
	fun get (acc, pos) =
	    case pos of
		[] => acc
	     |  hd::tl  => get(( List.nth(Deck, hd)::acc ), tl)  
    in
	get([],	getIndices([],7))
    end 

fun num_value (value) = (* set int values to make comparisons between Num type and other value types easier *) 
    case value of
	Ace   => 14
      | King  => 13
      | Queen => 12
      | Jack  => 11
      | Num v => v

fun compare (v1,v2) = num_value(v1) > num_value(v2)

(* booleans are negated to make the sort be from greatest to least *)
fun compareS (c1, c2) = (*could have done something like num_value for suites but I wanted to do it differently*)
	case (c1, c2) of
	        ((Spades,v1),       (Spades,v2))       => not (compare(v1,v2))
	 |  	((Hearts,v1),       (Hearts,v2))       => not (compare(v1,v2))
	 |  	((Diamonds,v1),     (Diamonds,v2))     => not (compare(v1,v2))
	 |  	((Clubs,v1),        (Clubs,v2))        => not (compare(v1,v2))
	 |  	((Spades,_),        (_,_))             => false
	 |  	((_,_),             (Spades,_))        => true				   
	 |  	((Hearts,_),        (_,_))             => false
	 |  	((_,_),             (Hearts,_))        => true
	 |  	((Diamonds,_),      (_,_))             => false
	 |  	((_,_),             (Diamonds,_))      => true 				   

fun compareV (c1, c2) = case (c1,c2) of ((_,v1), (_,v2)) => not (compare(v1,v2))

fun sort_s clist = ListMergeSort.sort compareS clist (* sort by suite Spades -> Clubs *)
fun sort_v clist = ListMergeSort.sort compareV clist (* sort by value Ace -> Num 2 *)

(* functions for finding patterns *)
fun s_check ((s1,_), (s2,_))   = s1 = s2
fun v_check_d ((_,v1) ,(_,v2)) = (num_value(v1) - 1) = num_value(v2)
fun v_check_e ((_,v1) ,(_,v2)) = num_value(v1) = num_value(v2)
fun a_check ((s1,v1),(s2,v2))  = (s1=s2) andalso ((num_value(v1)-1)=num_value(v2)) (* could have made with s_check and v_check_d but it was just as verbose *)
			       
fun sliding_window (cards, interval, f) = (* pretty much all hands in poker can be found using a sliding window type search *)
    let
	fun loop (hd_suite,hd_value, hdacc, tl, i, L) = (* interval search *)
	    case tl of
		[] => (hdacc,L)
	      | (s,v)::[] => if L < interval andalso f((hd_suite,hd_value), (s,v))
			     then ((s,v)::hdacc,L+1)
			     else (hdacc,L) 
	      | (s,v)::tl' => if f((hd_suite,hd_value), (s,v)) andalso i<>1 
			      then loop(s,v,(s,v)::hdacc,tl',i-1,L+1)
			      else (hdacc,L)
    in	
	case cards of (* does search till pattern is found or end of list *)
	    [] => NONE
	  | (s,v)::tl => let
	                    val (list,L) = loop(s,v, [(s,v)],tl,interval,1)
                         in
			    if L = interval
			    then SOME list
			    else  sliding_window (tl,interval,f)
                         end
    end

fun find_dup (e,list) = List.exists(fn x => x = e) list

fun add_high_cards (cards,pattern,pL) = (* adding high cards if hand pattern isn't length 5 *)
    if pL = 5
    then pattern
    else
	case cards of
	    [] => pattern
	  | hd::tl => if find_dup(hd,pattern)
		      then add_high_cards(tl,pattern,pL)
		      else add_high_cards(tl,hd::pattern,pL+1)

fun remove_patterns (pattern,cards) = 
(* removing cards is only realy useful for two pair and full house, I think its a better way than having slide_window becoming more complex*)
    case pattern of
	[] => cards
      | hd::tl => remove_patterns(tl, (List.filter (fn x => x <> hd) cards)) 
				 
fun run (cards) = 
(* creates the two cards lists and then defines all hand functions, then has a switch statement to return the cards list used, hand name and hand pattern *)
    let
        val s_cards = sort_s cards
	val v_cards = sort_v cards

(*1*)	fun straight_flush () = sliding_window (s_cards,5,a_check) (*can be used for RF hd = Num 10*)
					       
(*2*)	fun four_kind ()      = case sliding_window (v_cards,4,v_check_e) of
			            SOME result => SOME (add_high_cards(v_cards, result, 4))
			          | NONE => NONE
	    
(*4*)	fun flush ()          = sliding_window (s_cards,5,s_check)
				      
(*5*)	fun straight ()       = sliding_window (v_cards,5,v_check_d)
					 
(*6*)	fun three_kind ()     = case sliding_window (v_cards,3,v_check_e) of
			            SOME result => SOME (add_high_cards(v_cards, result, 3))
			          | NONE => NONE
					    
(*7*)	fun two_pair ()       = case sliding_window (v_cards,2,v_check_e) of 
	                            SOME result => (case sliding_window(remove_patterns(result,v_cards),2,v_check_e) of 
						        SOME r => SOME (add_high_cards(v_cards,result@r,4))
						      | NONE => NONE)
		          	  | NONE => NONE 
							      
(*8*)	fun pair ()           = case sliding_window (v_cards,2,v_check_e) of
			            SOME result => SOME (add_high_cards(v_cards, result, 2))
			          | NONE => NONE

(*3*)	fun full_house ()     = case sliding_window(v_cards,3,v_check_e) of
			            SOME result => (case sliding_window(remove_patterns(result,v_cards),2,v_check_e) of
						        SOME r => SOME (result@r)
						      | NONE => NONE)
			          | NONE => NONE
							       							       
(*9*)	fun high_cards () = add_high_cards(v_cards,[],0)	  
    in
	case straight_flush() of
	    SOME ((s,v)::tl) => if v = Num 10 then (cards,"Royal Flush",(s,v)::tl) else (cards,"Straight Flush",(s,v)::tl)
	| NONE =>
	case four_kind() of
	    SOME result => (cards,"Four of a Kind",result)
	| NONE =>
	case full_house() of
	    SOME result => (cards,"Full House",result)
        | NONE =>
	case flush() of
	    SOME result => (cards,"Flush",result)
	| NONE => 
	case straight() of
	    SOME result => (cards,"Straight",result)
	| NONE => 
        case three_kind() of
	    SOME result => (cards,"Three of a Kind",result)
	| NONE => 
	case two_pair() of
	    SOME result => (cards,"Two Pair",result)
	| NONE =>
	case pair() of
            SOME result => (cards,"Pair",result)
	| NONE => (cards,"High Cards",high_cards()) 
    end
	
fun r_run() = run(getCards()) (* randomized cards passed to run *)
