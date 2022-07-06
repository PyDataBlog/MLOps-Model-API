@call ts
@set _t=%CD%
@call jake local
@popd
@node --inspect-brk %_t%\built\local\tsc.js %*
