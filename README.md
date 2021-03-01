auxiliary utilities
===================

see [i-dotfiles emacs](https://github.com/idcrook/i-dotfiles/tree/main/emacs) for add'l details

python
------

```shell
pip3 install --user yamllint
pip3 install --user grip
pip3 install --user wakatime
pip3 install --user jedi flake8 autopep8 yapf black rope

```

node
----

```shell
npm list --global --parseable --depth=0
npm install --global marked
# npm install --global vmd
npm install --global tern
npm install --global js-beautify
npm install --global jsxhint
# npm install --global gulp-cli
# npm install --global jsonlint
# npm install --global jshint
```

golang
------

```shell
# no front matter support # go get -u github.com/shurcooL/markdownfmt

GO111MODULE=on go get -v github.com/moorereason/mdfmt
```

See also [moorereason/mdfmt: Like gofmt, but for Markdown with front matter.](https://github.com/moorereason/mdfmt)

-	Working build instructions here: [i-dotfiles/golang at main · idcrook/i-dotfiles](https://github.com/idcrook/i-dotfiles/tree/main/golang#mdfmt---markdownfmt-replacement)
