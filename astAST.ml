ast.ml
==>
[{pstr_desc =
   Pstr_extension
    (({txt = "hashcons"},
      PStr
       [{pstr_desc =
          Pstr_type (Recursive,
           [{ptype_name = {txt = "term"}; ptype_params = [];
             ptype_cstrs = [];
             ptype_kind =
              Ptype_variant
               [{pcd_name = {txt = "K"}; pcd_args = Pcstr_tuple [];
                 pcd_res = None};
                {pcd_name = {txt = "Var"};
                 pcd_args =
                  Pcstr_tuple
                   [{ptyp_desc = Ptyp_constr ({txt = Lident "int"}, [])}];
                 pcd_res = None};
                {pcd_name = {txt = "Lam"};
                 pcd_args =
                  Pcstr_tuple
                   [{ptyp_desc =
                      Ptyp_extension
                       ({txt = "hcons"},
                        PStr
                         [{pstr_desc =
                            Pstr_eval
                             ({pexp_desc = Pexp_ident {txt = Lident "id_lam"}},
                             ...)}])};
                    {ptyp_desc = Ptyp_constr ({txt = Lident "term"}, [])}];
                 pcd_res = None};
                {pcd_name = {txt = "App"};
                 pcd_args =
                  Pcstr_tuple
                   [{ptyp_desc =
                      Ptyp_extension
                       ({txt = "hcons"},
                        PStr
                         [{pstr_desc =
                            Pstr_eval
                             ({pexp_desc = Pexp_ident {txt = Lident "id_app"}},
                             ...)}])};
                    {ptyp_desc = Ptyp_constr ({txt = Lident "term"}, []);
                     ptyp_attributes =
                      [({txt = "hash"},
                        PStr
                         [{pstr_desc =
                            Pstr_eval
                             ({pexp_desc =
                                Pexp_ident
                                 {txt = Ldot (Lident "Hashtbl", "hash")}},
                             ...)}])]};
                    {ptyp_desc = Ptyp_constr ({txt = Lident "term"}, [])}];
                 pcd_res = None}];
             ptype_private = Public; ptype_manifest = None}])}]),
    ...)}]
=========
