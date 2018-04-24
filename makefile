install:
	wget "https://mirror.racket-lang.org/installers/6.12/racket-6.12-x86_64-macosx.dmg" -O racket.dmg
	hdiutil attach ./racket.dmg
	raco test main.rkt
	raco test smc.rkt
	raco pkg install peg

uninstall:
	hdiutil detach /Volumes/racket
