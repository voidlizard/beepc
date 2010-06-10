
syn keyword beepStatement  if then else elif while ret type def of continue break

syn keyword	beepType       int string bool void

syn keyword beepBool       true false

syn match   beepComment  "#.*$"  contains=beepTodo,@Spell
syn match   beepComment2 "%%.*$" contains=beepTodo,@Spell

syn keyword beepTodo  TODO FIXME XXX contained


syn match   beepNumber	"\<0x\x\+[Ll]\=\>"
syn match   beepNumber	"\<\d\+[LljJ]\=\>"

syn region	beepString    start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=beepEscape,@Spell
syn region  beepParen     transparent start='(' end=')'

syn keyword beepStatement	def type nextgroup=beepFunction skipwhite

syn match   beepDirective   "\s*@[a-zA-Z0-9_]*"
syn match   beepQuote       "`[a-zA-Z0-9_]*"

syn match   beepFunction	"[a-zA-Z_][a-zA-Z0-9_]*" contained

syn match	beepCharacter	"'[^\\]'"
syn match	beepCharacter	"'[^']*'" contains=beepSpecial
syn match	beepSpecialCharacter "'\\['\"?\\abfnrtv]'"
syn match	beepSpecialCharacter display "'\\\o\{1,3}'"
syn match	beepSpecialCharacter display "'\\x\x\{1,2}'"
syn match	beepSpecialCharacter display "'\\x\x\+'"


syn match  beepEscape		+\\[abfnrtv'"\\]+ contained
syn match  beepEscape		"\\\o\{1,3}" contained
syn match  beepEscape		"\\x\x\{2}" contained
syn match  beepEscape		"\(\\u\x\{4}\|\\U\x\{8}\)" contained
syn match  beepEscape		"\\$"


hi def link beepStatement Statement
hi def link beepStructure Type
hi def link beepComment   Comment 
hi def link beepComment2  Comment 
hi def link beepTodo      Todo 
hi def link beepNumber    Number
hi def link beepBool      Number
hi def link beepString    String
hi def link beepEscape    Special 
hi def link beepCharacter Character
hi def link beepSpecialCharacter SpecialChar 
hi def link beepFunction  Function
hi def link beepDirective PreCondit
hi def link beepQuote     PreCondit
hi def link beepType      Type 

