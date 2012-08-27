set makeprg=rake\ -s\ SRC=%
if !exists("$ANDROID_ROOT")
  autocmd FileType haskell autocmd BufWritePost * silent make
endif
