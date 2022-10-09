" Folding
" function! CustomFold()
"   return printf('   %6-d%s', v:foldend - v:foldstart + 1, getline(v:foldstart))
" endfunction
" set fillchars=fold:\ | set foldtext=CustomFold()
