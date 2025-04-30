
type Option 't = #none | #some t

module Option = {
  def none 't: Option t = #none
  def empty = none
  def some 't (t: t): Option t = #some t
  def unwrap 't (o: Option t): t =
    match o
      case #some t -> t
      case #none -> ???
  def map 't 'u (f: t -> u)(o: Option t): Option u =
    match o
      case #some t -> some (f t)
      case #none -> none
  def map2 't 'u 'v (f: t -> u -> v)(o0: Option t)(o1: Option u): Option v =
    match (o0, o1)
      case (#none, _) -> none
      case (_, #none) -> none
      case (#some t, #some u) -> some (f t u)
  def isnone 't (o: Option t): bool =
    match o
      case #none -> true
      case #some _ -> false
  def null = isnone
  def issome 't (o: Option t): bool = ! isnone o
  def orelse 't (o: Option t)(t: t): t =
    match o
      case #some t0 -> t0
      case #none -> t
  def fold 't 'u (f: t -> u)(ne: u)(o: Option t): u =
    (map f o) `orelse` ne 
  def zip 't (o0: Option t)(o1: Option t): Option (t, t) =
    if issome o0 && issome o1 then some (unwrap o0, unwrap o1)
    else none
  def unzip 't (os: Option (t, t)): (Option t, Option t) =
    match os
      case #some t -> (#some t.0, #some t.1)
      case _ -> (#none, #none) 
  def (*>) 't (o0: Option t)(_ : Option t): Option t = o0
  def (<*) 't (_ : Option t)(o1: Option t): Option t = o1
  def (<>) 't (o0: Option t)(o1: Option t): Option t =
    if issome o0 then o0 else o1
  def flatten 't (oo: Option (Option t)): Option t =
    match oo
      case #some o -> o
      case #none -> #none 
}
