function order () {
  return sprintf("%03d", NR)
}

function massage_type (prefix, type) {
  if (type ~ /^[[:alnum:]_']*$/) {
    return prefix type
  } else if (type ~ /^\(.*\) [[:alnum:]_']*$/) {
    main = gensub(/^\(.*\) ([[:alnum:]_']*)$/, "\\1", "1", type)
    arg1 = gensub(/^\(([^,]*), *.*\) [[:alnum:]_']*$/, "\\1", "1", type)
    arg2 = gensub(/^\([^,]*, *(.*)\) [[:alnum:]_']*$/, "\\1", "1", type)
    return prefix main " (" massage_type(prefix,arg1) ") (" massage_type(prefix,arg2) ")"
  } else if (type ~ /^.* [[:alnum:]_'][[:alnum:]_']*$/) {
    main = gensub(/^.* ([[:alnum:]_'][[:alnum:]_']*)$/, "\\1", "1", type)
    arg = gensub(/^ *(.*) [[:alnum:]_'][[:alnum:]_']*$/, "\\1", "1", type)
    return prefix main " (" massage_type(prefix,arg) ")"
  } else {
    return "\"CANNOT HANDLE " type "\""
  }
}

/^ *type/ {
  o = order()
  print "(*0" o "src*) " $0
  print "(*1" o "ppr*) let pp_" $2 " par fmt x = let open Util.Pp in match x with"
  print "(*2" o "shw*) let show_" $2 " = Util.Pp.to_show pp_" $2
  print "(*3" o "gen*) let gen_" $2 " = let open QCheck.Gen in oneof ["
  print "(*4" o "equ*) let equal_" $2 " x y = let open Util.Equal in match x, y with"
}

/^ *\| [[:alpha:]_']+ *$/ {
  o = order()
  print "(*0" o "src*) " $0
  print "(*1" o "ppr*) | " $2 " -> cst0 \"" $2 "\" fmt"
  print "(*3" o "gen*) pure " $2 ";"
  print "(*4" o "equ*) | " $2 "," $2 " -> true"
}

/^ *\| [[:alpha:]_']+ of [^*]*$/ {
  o = order()
  typ = gensub(/^ *\| [[:alpha:]_']+ of ([^*]*)$/, "\\1", "1")
  print "(*0" o "src*) " $0
  print "(*1" o "ppr*) | " $2 " x -> cst1 (" massage_type("pp_",typ) ") \"" $2 "\" par fmt x"
  print "(*3" o "gen*) map (fun x -> " $2 " x) (" massage_type("",typ) ");"
  print "(*4" o "equ*) | " $2 " x," $2 " y -> " massage_type("equal_",typ) " x y"
}

/^ *\| [[:alpha:]_']+ of [^*]* \* [^*]*$/ {
  o = order()
  typ1 = gensub(/^ *\| [[:alpha:]_']+ of ([^*]*) \* [^*]*$/, "\\1", "1")
  typ2 = gensub(/^ *\| [[:alpha:]_']+ of [^*]* \* ([^*]*)$/, "\\1", "1")
  print "(*0" o "src*) " $0
  print "(*1" o "ppr*) | " $2 "(x,y) -> cst2 (" massage_type("pp_",typ1) ") (" massage_type("pp_",typ2) ") \"" $2 "\" par fmt x y"
  print "(*3" o "gen*) map2 (fun x y -> " $2 "(x,y)) (" massage_type("",typ1) ") (" massage_type("",typ2) ");"
  print "(*4" o "equ*) | " $2 "(x,y)," $2 "(a,b) -> " massage_type("equal_",typ1) " x a && " massage_type("equal_",typ2) " y b"
}

/^ *\| [[:alpha:]_']+ of [^*]* \* [^*]* \* [^*]*$/ {
  o = order()
  typ1 = gensub(/^ *\| [[:alpha:]_']+ of ([^*]*) \* [^*]* \* [^*]*$/, "\\1", "1")
  typ2 = gensub(/^ *\| [[:alpha:]_']+ of [^*]* \* ([^*]*) \* [^*]*$/, "\\1", "1")
  typ3 = gensub(/^ *\| [[:alpha:]_']+ of [^*]* \* [^*]* \* ([^*]*)$/, "\\1", "1")
  print "(*0" o "src*) " $0
  print "(*1" o "ppr*) | " $2 "(x,y,z) -> cst3 (" massage_type("pp_",typ1) ") (" massage_type("pp_",typ2) ") (" massage_type("pp_",typ3) ") \"" $2 "\" par fmt x y z"
  print "(*3" o "gen*) map3 (fun x y z -> " $2 "(x,y,z)) (" massage_type("",typ1) ") (" massage_type("",typ2) ") (" massage_type("",typ3) ");"
  print "(*4" o "equ*) | " $2 "(x,y,z)," $2 "(a,b,c) -> " massage_type("equal_",typ1) " x a && " massage_type("equal_",typ2) " y b && " massage_type("equal_",typ3) " z c"
}

END {
  o = sprintf("%03d", NR+1)
  print "(*3" o "gen*) ]"
  print "(*4" o "equ*) | _, _ -> false"
}
