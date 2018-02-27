# This is not for the project, this allows to install another dependencies.
# And haskell-stack must be installed before you execute this :D
all: install

install:
	install_liquidhaskell

#TODO: Don't depend on yaourt (a package manager for ArchLinux)
# Please see https://github.com/ucsd-progsys/liquidhaskell/blob/develop/INSTALL.md
install_liquidhaskell:
	yaourt -S z3
	git clone https://github.com/ucsd-progsys/liquidhaskell /tmp/liquidhaskell
	cd /tmp/liquidhaskell
	stack install
