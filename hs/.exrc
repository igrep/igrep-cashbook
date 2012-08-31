set makeprg=ghc\ --make\ -Wall\ %
" Terminal IDE doesn't support ghc of course so far.
if !exists("$ANDROID_ROOT")
  autocmd FileType haskell autocmd BufWritePost * silent make
endif
