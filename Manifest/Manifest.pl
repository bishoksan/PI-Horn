:- bundle('PI-Horn').
version('1.0').
depends([
    core-[version>='1.18'],
    chclibs,
    'github.com/ciao-lang/ciao_ppl',
    'github.com/jfmc/ciao_yices'
]).
alias_paths([
    pihorn = 'src'
]).
lib('src').
cmd('src/pihorn').
