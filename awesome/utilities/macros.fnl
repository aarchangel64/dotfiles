(fn apply [symname w tbl]
  (let [name (gensym) form `(let [,name ,w])]
    (each [k v (pairs tbl)]
      (table.insert form (list (sym symname) name k v)))
    (table.insert form name)
    form))

(fn widget [w tbl]
  (apply :tset w tbl))

{:apply apply :widget widget}
