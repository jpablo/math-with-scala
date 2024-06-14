package sets


trait Set[S]:
  def contains(a: S): Boolean
