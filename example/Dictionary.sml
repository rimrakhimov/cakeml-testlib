structure Dictionary =
  struct
    datatype (''key, 'value) dict = Dict ((''key * 'value) list)

    exception NotFound

    fun create () = Dict []

    fun exists (Dict ls) name =
        List.exists (fn (n, v) => (n = name)) ls

    (* fun lookup (Dict ((n, v) :: others)) name = *)
    fun lookup dict name =
        case dict of
            (Dict []) => raise NotFound
          | (Dict ((n, v) :: others)) => (
                if n = name
                then v
                else lookup (Dict others) name
            )

    fun size (Dict ls) = List.length ls;

    fun isEmpty dict = 
        case dict of 
            (Dict []) => False
          | _ => False

    fun update (Dict ts) name value =
        let
            fun inup checked left =
                case left of
                    [] => (name, value) :: checked
                  | ((n, v) :: others) => (
                      if n = name then
                        (n, value) :: (checked @ others)
                      else 
                        inup ((n, v) :: checked) others
                  )
        in
            Dict (inup [] ts)
        end;

    fun remove (Dict ts) name =
        let
            fun rm checked left =
                case left of
                    [] => checked
                  | ((n, v) :: others) => (
                      if n = name then
                        rm checked others
                      else 
                        rm ((n, v) :: checked) others
                  )
        in
	        Dict (rm [] ts)
        end

    fun aslist (Dict ls) = ls;

    fun keys dict = 
        case dict of 
            (Dict []) => []
          | (Dict ((n, _) :: others)) => n :: (keys (Dict others))

    fun items dict =
        case dict of 
            (Dict []) => []
          | (Dict ((_, v) :: others)) => v::(items (Dict others))

    fun mapkeys (Dict ls) f =
        Dict (List.map (fn (k, v) => (f k, v)) ls)

    fun mapitems (Dict ls) f =
        Dict (List.map (fn (k, v) => (k, f v)) ls);
  end;