# Branches of this fork

This repository is the SBCL that Epsilon
(github.com/kreislerians/epsilon-internal) builds: upstream SBCL plus
fibers, local heaps and core fragments, kept as a set of branches over
upstream master. It is not a GitHub fork of sbcl/sbcl and publishes no
release tags, so the workflows fetch upstream's tags before building,
and generate-version.sh writes `<series>-untagged-<commit>` when no
tag is reachable.

| branch | role |
|---|---|
| `master` | Mirrors upstream master. Fast-forwarded after each monthly SBCL release; nothing is committed to it. |
| `sb-local-heaps` | The integration branch, and the only branch that `sbcl/SBCL_REV` in epsilon-internal may name. It is upstream master, the `upstream-*` branches and the fork-only commits (the workflows, the version fallback), and is rebuilt from them after each release. Nothing is committed to it directly except fork-only commits. |
| `upstream-<name>` | One upstream candidate each: a fix, or a feature series, based on upstream master and rebased after each monthly release. A fix goes to sbcl/sbcl as a pull request; a feature opens an sbcl-devel thread first. Commits carry no `Co-Authored-By` or `Claude-Session` trailers. |
| `fix/<issue>-<name>` | Work in progress that has not yet reached an upstream branch or the integration branch; deleted once it has. |

A change lands in an upstream branch first and reaches Epsilon through
the rebuild of `sb-local-heaps` and a pin move there.

## CI

Every push runs the nine workflows. `linux.yml` builds x86-64 with
fibers on gencgc and on mark-region, and in the Epsilon configuration
(`--fancy --with-sb-fiber --without-gencgc --with-mark-region-gc
--with-sb-local-heaps`); `linux-arm64.yml` and `mac.yml` build the
Epsilon configuration on arm64 Linux and on both Darwin architectures;
`fiber-callbacks.yml` builds it on arm64 Darwin and boots a saved
core's callbacks. The test suites run with `--slow`, which includes
the five slow fiber tests. The tag fetch step stays in every workflow
until the version fallback is on every branch CI builds.

The upstream candidates are named `upstream-<name>` rather than
`upstream/<name>`: upstream's Windows workflows build an installer
whose product name carries the version string, which
generate-version.sh derives from the branch name, and WiX rejects a
slash in it.
