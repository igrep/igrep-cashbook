set makeprg=rake\ SRC=%
" Terminal IDE doesn't support ghc of course so far.
if !exists("$ANDROID_ROOT")
  autocmd FileType haskell autocmd BufWritePost * GhcModCheck
endif
