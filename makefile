run: install
	echo "Os testes foram executados, para limpar, make uninstall"

install:
	wget "https://mirror.racket-lang.org/installers/6.12/racket-6.12-x86_64-macosx.dmg" -O racket.dmg
	hdiutil attach ./racket.dmg
	export PATH=$PATH:/Volumes/racket/
	raco pkg install peg
	racket main.rkt

uninstall:
	hdiutil detach /Volumes/racket
