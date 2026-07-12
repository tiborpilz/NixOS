" Vim syntax file for the Pie language (The Little Typer).
" Pie is an s-expression language, so we start from Lisp-ish rules and add
" Pie's type constructors, eliminators, and forms.

if exists("b:current_syntax")
  finish
endif

" Treat it like a Lisp for word boundaries / parens. Multibyte chars
" (λ Π Σ) are always keyword chars and can't be listed here (E474).
setlocal iskeyword+=-,+,:,=,?,!,<,>,*,/

syn case match

" Comments: Racket-style ; line comments and #| ... |# blocks.
syn match   pieComment ";.*$"
syn region  pieBlockComment start="#|" end="|#" contains=pieBlockComment

" Top-level declaration forms.
syn keyword pieDecl claim define check-same

" Core forms / annotations.
syn keyword pieForm the lambda λ Pi Π Sigma Σ TODO

" Type constructors.
syn keyword pieType U Nat Atom Trivial Absurd List Vec Either Pair
syn keyword pieType Sigma Pi =

" Constructors.
syn keyword pieCtor zero add1 sole nil vecnil same cons left right quote

" `::` and `vec::` are constructors too.
syn match   pieCtor "\<vec::\>"
syn match   pieCtor "::"

" Eliminators / recursors.
syn keyword pieElim which-Nat iter-Nat rec-Nat ind-Nat
syn keyword pieElim rec-List ind-List
syn keyword pieElim head tail ind-Vec
syn keyword pieElim car cdr
syn keyword pieElim ind-Either
syn keyword pieElim replace symm trans cong ind-= ind-Absurd
syn keyword pieElim ind-Trivial

" Atoms are 'quoted-symbols.
syn match   pieAtom "'[a-zA-Z][a-zA-Z0-9-]*"

" Numbers (Pie sugar for Nat literals).
syn match   pieNumber "\<\d\+\>"

" Parens
syn match   pieParen "[()]"

hi def link pieComment      Comment
hi def link pieBlockComment Comment
hi def link pieDecl         Keyword
hi def link pieForm         Statement
hi def link pieType         Type
hi def link pieCtor         Constant
hi def link pieElim         Function
hi def link pieAtom         String
hi def link pieNumber       Number
hi def link pieParen        Delimiter

let b:current_syntax = "pie"
