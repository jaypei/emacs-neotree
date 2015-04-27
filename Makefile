EMACS=emacs
CURL=curl --silent
ERT_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3

.PHONY: ert test test-batch

package: *.el
	@ver=`grep -o "Version: .*" neotree.el | cut -c 10-`; \
	tar cjvf neotree-$$ver.tar.bz2 --mode 644 `git ls-files '*.el' | xargs`

clean:
	@rm -rf neotree-*/ neotree-*.tar neotree-*.tar.bz2 *.elc ert.el

test: compile
	${EMACS} -Q -nw -L . -L ./test \
		-l test/test-utils.el \
		-l test/test-cmds.el \
		-l test/test-buffer.el \
		-l test/test-vc.el \
		--eval "(let (pop-up-windows) (ert t))"

test-batch: compile
	${EMACS} -Q --batch -L .  -L ./test \
		-l test/test-utils.el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

downloads:
	${EMACS} -Q --batch -l ert || \
	${CURL} ${ERT_URL} > ert.el

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile neotree.el
