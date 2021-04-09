open Syntax.Ast
open OUnit2

let parse (str: string) =
  let lexbuf = Lexing.from_string str in
  Syntax.Parser.prog Syntax.Lexer.token lexbuf

let show_exp expr = print_string (show_with_pos pp_exp expr)

let example_source_pos: Syntax.Sourcepos.pos = { lnum = 0; bol = 0 }

let ex e = { value = e; pos = example_source_pos };;

let assert_eq = assert_equal ~cmp: (equal_with_pos equal_exp) ~printer:(show_with_pos pp_exp)
let st prg ast = assert_eq ast (parse prg)

let (>>:) name (prg, ast) = name >:: (fun _ -> st prg ast)

let suite = "Test" >::: [
  "test_nil" >>: ("nil", (ex NilExp));

  "test_int" >>: (
    "1",
    (ex (IntExp 1))
  );

  "test_plus" >>: (
    "4 + 5",
    (ex (BinOpExp { 
      op = PlusOp;
      left = (ex (IntExp 4));
      right = (ex (IntExp 5)) 
    }))
  );

  "test_minus" >>: (
    "4 - 5",
    (ex (BinOpExp {
      op = MinusOp;
      left = (ex (IntExp 4));
      right = (ex (IntExp 5));
    }))
  );

  "test_mul" >>: (
    "1234 * 51234",
    (ex (BinOpExp {
      op = MulOp;
      left = (ex (IntExp 1234));
      right = (ex (IntExp 51234));
    }))
  );

  "test_div" >>: (
    "4 / 54",
    (ex (BinOpExp {
      op = DivOp;
      left = (ex (IntExp 4));
      right = (ex (IntExp 54));
    }))
  );

  "test_string" >>: (
    {|"aaa"|},
    ex (StringExp "aaa")
  );

  "test_simple_var" >>: (
    {|hello|},
    (ex (VarExp (SimpleVar "hello")))
  );

  "test_field_var" >>: (
    {|hello.value|},
    (ex (VarExp (FieldVar ((SimpleVar "hello"),"value"))))
  );

  "test_subscript_var" >>: (
    {|hello.[345]|},
    (ex (VarExp (SubscriptVar ((SimpleVar "hello"), (ex(IntExp 345))))))
  );

  "test_neg" >>: (
    {|-40|},
    (ex (UnaryOpExp {
      rator = NegOp;
      rand = ex (IntExp 40)
    }))
  );

  "test_record_exp" >>: (
    {|test_type {f1 = 1, f2 = 2, f3 = "hello"}|},
    (ex (RecordExp {
      fields = [
        { key = "f1"; exp = (ex (IntExp 1)) };
        { key = "f2"; exp = (ex (IntExp 2)) };
        { key = "f3"; exp = (ex (StringExp "hello")) }
      ];
      typ = "test_type";
    }))
  );

  "test_seq" >>: (
    {|(123; 456)|},
    (ex (SeqExp [
      ex (IntExp 123);
      ex (IntExp 456);
    ]))
  );

  "test_if_then" >>: (
    {|if 123 then 456|},
    (ex (IfExp {
      cond = ex (IntExp 123);
      texp = ex (IntExp 456);
      fexp = None
    }))
  );

  "test_if_then_else" >>: (
    {|if 123 then 456 else 789|},
    (ex (IfExp {
      cond = ex (IntExp 123);
      texp = ex (IntExp 456);
      fexp = Some (ex (IntExp 789))
    }))
  );

  "test_for" >>: (
    {|for i := 0 to 100 do 123|},
    (ex (ForExp {
      var = "i";
      from = ex (IntExp 0);
      til = ex (IntExp 100);
      body = ex (IntExp 123);
    }))
  );

  "test_while" >>: (
    {|while 123 do 456|},
    ex (WhileExp {
      cond = ex (IntExp 123);
      body = ex (IntExp 456);
    })
  );

  "test_while_break" >>: (
    {|while 123 do break|},
    ex (WhileExp {
      cond = ex (IntExp 123);
      body = ex (BreakExp);
    })
  );

  "test_array" >>: (
    {|test_type [500] of 0|},
    ex (ArrayExp {
      typ = "test_type";
      size = ex (IntExp 500);
      init = ex (IntExp 0);
    })
  );

  "test_let" >>: (
    {|
      let
        var N := 0
        var M: int := 0
        type intArray = array of int
        type strct = { f1: int, f2: intArray }
        function f1() = hello()
        function f2(): test_type = hello()
        function f3(a: int) = hello()
        function f4(a: int, b: intArray): test_type = hello()
        var O: int := nil
        function f5() = nil
        in f4(f3(f2()), f1())
      end
    |},
    ex (LetExp {
      decs = [
        VarDec { name = "N"; typ = None; init = ex (IntExp 0); };
        VarDec { name = "M"; typ = Some "int"; init = ex (IntExp 0); };
        TypeDec { name = "intArray"; typ = ArrayType "int"; };
        TypeDec { name = "strct"; typ = RecordType [
          { fld_name = "f1"; fld_type = "int" };
          { fld_name = "f2"; fld_type = "intArray" };
        ]};
        FunctionDec [
          { fun_name = "f1"; params = []; res_type = None; body = ex (CallExp { rator = "hello"; rands = []; }); };
          { fun_name = "f2"; params = []; res_type = Some "test_type"; body = ex (CallExp { rator = "hello"; rands = []; }); };
          { fun_name = "f3"; params = [ { fld_name = "a"; fld_type = "int"; }; ]; res_type = None; body = ex (CallExp { rator = "hello"; rands = []; }); };
          { fun_name = "f4"; params = [ { fld_name = "a"; fld_type = "int"; }; { fld_name = "b"; fld_type = "intArray"; }; ]; res_type = Some "test_type"; body = ex (CallExp { rator = "hello"; rands = []; }); };
        ];
        VarDec { name = "O"; typ = Some "int"; init = ex NilExp; };
        FunctionDec [
          { fun_name = "f5"; params = []; res_type = None; body = ex NilExp; };
        ]
      ];
      body = ex (CallExp {
        rator = "f4";
        rands = [
          ex (CallExp {
            rator = "f3";
            rands = [
              ex (CallExp {
                rator = "f2";
                rands = [];
              });
            ];
          });
          ex (CallExp {
            rator = "f1";
            rands = [];
          })
        ];
      });
    })
  )
]

let _ = run_test_tt_main suite
