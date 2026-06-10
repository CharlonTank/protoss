type t = Sexp.t

let parse = Sexp.parse

let to_string = Sexp.to_string

let human_grammar_text =
  {|protoss-human-grammar-v1

source ::= sexp_program | elm_program

sexp_program ::= declaration*
declaration ::= (module ModuleName)
              | (export Name*)
              | (import String)
              | (capabilities CapabilityName*)
              | (type Name type)
              | (type Name (TypeParam*) type)
              | (record Name record_type_params? field_type*)
              | (variant Name variant_type_params? case_type*)
              | (def Name type expr)
              | (defpoly Name (params TypeParam*) type expr)
              | (defcap Name (capabilities CapabilityName*) type expr)
              | (defpolycap Name (params TypeParam*) (capabilities CapabilityName*) type expr)
              | (defrec Name type recursion_spec recursion_body)
              | (defrecpoly Name (params TypeParam*) type recursion_spec recursion_body)

record_type_params ::= (params TypeParam*)
variant_type_params ::= (params TypeParam*)
field_type ::= (Name type)
case_type ::= (Name type)

type ::= Unit | Bool | Nat | String
       | (-> type type)
       | (Record field_type*)
       | (Variant case_type*)
       | (List type)
       | (View type)
       | (Attr type)
       | (Process type)
       | (Process (capabilities CapabilityName*) type)
       | (SecretRef Scope type)
       | (forall Nat type)
       | (Name type*)

expr ::= Name | Nat | true | false | String | unit
       | (lambda (Name type) expr) | (lambda Name expr)
       | (let (Name expr) expr) | (let (Name type expr) expr)
       | (strict expr)
       | (record (Name expr)*)
       | (get expr Name)
       | (variant type Name expr) | (variant Name expr)
       | (case expr branch*)
       | (caseList expr (Nil expr) (Cons Name Name expr))
       | (foldNat expr expr expr)
       | (foldList expr expr expr)
       | (foldVariant type type expr branch*)
       | (recur expr)
       | (Nil type?) | (Cons type? expr expr)
       | (inst Name type*)
       | (done expr) | (bind expr expr) | request
       | (text expr) | (image expr expr) | (button expr expr)
       | (input expr) | (column expr) | (row expr) | (list expr expr)
       | (when expr expr) | (node expr expr expr) | (attr expr expr) | (on expr expr)
       | (expr expr+)

branch ::= (Name Name expr) | (Name expr) | (_ expr)
request ::= (AskHuman String)
          | (HttpGet String)
          | ReadClock
          | (SaveLocal String String)
          | (LoadLocal String)
          | (ServerRequest String String)

recursion_spec ::= (nat Name) | (list Name) | (variant Name)
recursion_body ::= (zero expr) (step Name expr)
                 | (nil expr) (cons Name Name expr)
                 | branch+

elm_program ::= elm_declaration*
elm_declaration ::= module_decl | import_decl | capabilities_decl
                  | type_alias_decl | union_decl | signature value_decl
module_decl ::= module ModuleName exposing (exposing_list)
import_decl ::= import String exposing (exposing_list)
capabilities_decl ::= capabilities CapabilityName*
signature ::= Name : elm_type
value_decl ::= Name pattern* = elm_expr
type_alias_decl ::= type alias Name type_params? = elm_type
union_decl ::= type Name type_params? = union_case+

elm_type ::= Unit | Bool | Nat | String | Name | Name elm_type+
           | elm_type -> elm_type
           | { field_type (, field_type)* }
           | [ elm_type ]
           | Process { CapabilityName* } elm_type

elm_expr ::= literal | Name | \Name+ -> elm_expr
           | if elm_expr then elm_expr else elm_expr
           | let elm_block(value_decl+) in elm_block(elm_expr)
           | case elm_expr of elm_block(elm_case+)
           | { field = elm_expr (, field = elm_expr)* }
           | { elm_expr | field = elm_expr (, field = elm_expr)* }
           | [ elm_expr (, elm_expr)* ]
           | elm_expr . Name
           | elm_expr |> elm_expr
           | elm_expr binary_op elm_expr
           | elm_expr elm_expr+

elm_case ::= pattern -> elm_expr
           | pattern -> elm_block(elm_expr)
elm_block(x) ::= NEWLINE INDENT x+ DEDENT
binary_op ::= + | == | /= | < | <= | > | >= | && | ||
exposing_list ::= .. | Name (, Name)*
Name ::= identifier
ModuleName ::= identifier(.identifier)*
CapabilityName ::= ModuleName.Name
TypeParam ::= identifier
|}
