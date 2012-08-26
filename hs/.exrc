set makeprg=ghc
autocmd FileType haskell autocmd BufWritePost * silent make
