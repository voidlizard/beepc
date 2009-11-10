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
    builtin "beepvm_put_char" "putc"         (TFunNative(Some(0),  [TInt],TVoid))           ;
    builtin "beepvm_put_int"  "put_int"      (TFunNative(Some(1),  [TInt],TVoid))           ;
    builtin "beepvm_put_str"  "puts"         (TFunNative(Some(2),  [TString],TVoid))        ;
    builtin ""                "putsn"        (TFunNative(Some(3),  [TString],TVoid))        ;
    builtin ""                "strtoul"      (TFunNative(Some(4),  [TString;TInt],TInt))    ;
    builtin ""                "strmake"      (TFunNative(Some(5),  [TInt;TInt],TString))    ;
    builtin ""                "utoa"         (TFunNative(Some(6),  [TInt;TInt],TString))    ;
    builtin ""                "strconcat"    (TFunNative(Some(7),  [TString;TString],TString)) ;
    builtin ""                "startswith"   (TFunNative(Some(8),  [TString;TString],TBool));
    builtin ""                "strfind"      (TFunNative(Some(9),  [TString;TInt;TInt],TInt));
    builtin ""                "strnth"       (TFunNative(Some(10), [TString;TInt],TInt))    ;
    builtin ""                "strsub"       (TFunNative(Some(11), [TString;TInt;TInt],TString)) ;
    builtin ""                "strlen"       (TFunNative(Some(12), [TString],TInt))         ;
    builtin ""                "streq"        (TFunNative(Some(13), [TString;TString],TBool));
    builtin ""                "strdup"       (TFunNative(Some(14), [TString],TString))      ;
    builtin ""                "strfindall"   (TFunNative(Some(15), [TString;TInt], TVect(TInt))) ;
    builtin ""                "sleep_ms"     (TFunNative(Some(16), [TInt],TVoid))           ;
 
(*     builtin ""                "vect_make"    (TFunNative(16, [TInt], TVect(any()) )) ;  *)
    builtin ""                "vect_get"     (TFunNative(Some(17), [TVect(!vect_t);TInt], !vect_t))  ;
    builtin ""                "vect_set"     (TFunNative(Some(18), [TVect(!vect_t);TInt; !vect_t], TVoid)) ;
    builtin ""                "vect_len"     (TFunNative(Some(19), [TVect(!vect_t)], TInt)) ;
 
    builtin ""                "strfindsub"   (TFunNative(Some(20), [TString;TString], TInt)) ;
    builtin ""                "mem_stats"    (TFunNative(Some(21), [], TVect(TInt)))         ;

    builtin ""                "debug_dump_mem" (TFunNative(Some(0x3F), [],TVoid))            ;


    builtin ""                "alloc"        (TFunEmit("alloc", [TInt],TInt,[ALLOC]))        ;

    (* CANDIDATES TO OPCODES *)
    builtin ""                "fst"          (TFunEmit("fst",    [TPair(!fst_a, !fst_b)], !fst_a, [RW])) ;
    builtin ""                "snd"          (TFunEmit("snd",    [TPair(!snd_a, !snd_b)], !snd_b, [INCA;RW])) ;
    builtin ""                "head"         (TFunEmit("head",   [TList(!hd_t)], !hd_t, [RW]))  ;     
    builtin ""                "tail"         (TFunEmit("tail",   [TList(!tl_t)], TList(!tl_t), [INCA;RW])) ;
    builtin ""                "nil"          (TFunEmit("nil",    [TList(any())], TBool, [FALSE;EQ;])) ;

    builtin ""                "gc"           (TFunEmit("gc",   [],TVoid,[GC]))                      ;
    builtin ""                "gc_cheap"     (TFunEmit("gc_cheap", [],TVoid,[CGC]))                 ;
]

