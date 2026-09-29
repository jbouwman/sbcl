#!/bin/sh
set -e
. ./subr.sh

command -v git >/dev/null 2>&1 || exit $EXIT_TEST_WIN
version_script="$SBCL_PWD/../generate-version.sh"
use_test_subdirectory
git init -q
git config user.name "Version test"
git config user.email "version-test@example.invalid"
git config commit.gpgsign false
touch run-sbcl.sh
echo 'changes in sbcl-2.6.9 relative to sbcl-2.6.8:' > NEWS
git add run-sbcl.sh NEWS
git commit -q -m fixture
head=`git rev-parse HEAD`

# A tagless checkout bootstraps with its actual source identity.
sh "$version_script"
test "`cat version.lisp-expr`" = "\"2.6.9-untagged-$head\""

# Package builders and release archives retain their supplied version.
printf '"package-version"\n' > version.lisp-expr
sh "$version_script"
test "`cat version.lisp-expr`" = '"package-version"'

# A generated identity records tracked local changes.
rm version.lisp-expr
echo '# modified' >> run-sbcl.sh
sh "$version_script"
test "`cat version.lisp-expr`" = "\"2.6.9-untagged-$head-WIP\""
git checkout -- run-sbcl.sh

# Normal release-tag version generation still wins over the fallback.
git -c tag.gpgsign=false tag -a sbcl-2.6.9 -m fixture
git update-ref refs/remotes/origin/master HEAD
sh "$version_script"
test "`tail -n 1 version.lisp-expr`" = '"2.6.9"'

exit $EXIT_TEST_WIN
