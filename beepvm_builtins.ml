open Types
open Opcodes

let builtin cname bname tp = (cname, bname, tp)

let fst_a = ref (TAny( Parser_ctx.uniq_id() ))
let fst_b = ref (TAny( Parser_ctx.uniq_id() ))

let snd_a = ref (TAny( Parser_ctx.uniq_id() ))
let snd_b = ref (TAny( Parser_ctx.uniq_id() ))

let hd_t  = ref (TAny( Parser_ctx.uniq_id() ))
let tl_t  = ref (TAny( Parser_ctx.uniq_id() ))

let tl_t  = ref (TAny( Parser_ctx.uniq_id() ))

let vect_t = ref (TAny( Parser_ctx.uniq_id() ))

let any () = TAny( Parser_ctx.uniq_id() + 22 )


let builtins () = [
    builtin "beepvm_put_char" "putc"         (TFunNative(0,  [TInt],TVoid))           ;
    builtin "beepvm_put_int"  "put_int"      (TFunNative(1,  [TInt],TVoid))           ;
    builtin "beepvm_put_str"  "puts"         (TFunNative(2,  [TString],TVoid))        ;
    builtin ""                "putsn"        (TFunNative(3,  [TString],TVoid))        ;
    builtin ""                "strtoul"      (TFunNative(4,  [TString;TInt],TInt))    ;
    builtin ""                "strmake"      (TFunNative(5,  [TInt;TInt],TString))    ;
    builtin ""                "utoa"         (TFunNative(6,  [TInt;TInt],TString))    ;
    builtin ""                "strconcat"    (TFunNative(7,  [TString;TString],TString)) ;
    builtin ""                "startswith"   (TFunNative(8,  [TString;TString],TBool));
    builtin ""                "strfind"      (TFunNative(9,  [TString;TInt;TInt],TInt));
    builtin ""                "strnth"       (TFunNative(10, [TString;TInt],TInt))    ;
    builtin ""                "strsub"       (TFunNative(11, [TString;TInt;TInt],TString)) ;
    builtin ""                "strlen"       (TFunNative(12, [TString],TInt))         ;
    builtin ""                "streq"        (TFunNative(13, [TString;TString],TBool));
    builtin ""                "strdup"       (TFunNative(14, [TString],TString))      ;
    builtin ""                "strfindall"   (TFunNative(15, [TString;TInt], TVect(TInt))) ;
    builtin ""                "sleep_ms"     (TFunNative(16, [TInt],TVoid))           ;
    
(*     builtin ""                "vect_make"    (TFunNative(16, [TInt], TVect(any()) )) ;  *)
    builtin ""                "vect_get"     (TFunNative(17, [TVect(!vect_t);TInt], !vect_t))  ;
    builtin ""                "vect_set"     (TFunNative(18, [TVect(!vect_t);TInt; !vect_t], TVoid)) ;
    builtin ""                "vect_len"     (TFunNative(19, [TVect(!vect_t)], TInt)) ;
    
    builtin ""                "debug_dump_mem" (TFunNative(0x3F, [],TVoid))           ;


    builtin ""                "alloc"        (TFunEmit("alloc", [TInt],TInt,[ALLOC]))          ;

    (* CANDIDATES TO OPCODES *)
    builtin ""                "fst"          (TFunEmit("fst",    [TPair(!fst_a, !fst_b)], !fst_a, [RW])) ;
    builtin ""                "snd"          (TFunEmit("snd",    [TPair(!snd_a, !snd_b)], !snd_b, [INCA;RW])) ;
    builtin ""                "head"         (TFunEmit("head",   [TList(!hd_t)], !hd_t, [RW]))  ;     
    builtin ""                "tail"         (TFunEmit("tail",   [TList(!tl_t)], TList(!tl_t), [INCA;RW])) ;
    builtin ""                "nil"          (TFunEmit("nil",    [TList(any())], TBool, [FALSE;EQ;])) ;

    builtin ""                "gc"           (TFunEmit("gc",   [],TVoid,[GC]))                      ;
    builtin ""                "gc_cheap"     (TFunEmit("gc_cheap", [],TVoid,[CGC]))                 ;
    builtin ""                "debug_dump"   (TFunEmit("debug_dump", [],TVoid,[DUMP]))              ;
]

