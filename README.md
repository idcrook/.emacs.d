auxiliary utilities
===================

see [i-dotfiles emacs](https://github.com/idcrook/i-dotfiles/tree/main/emacs) for add'l details

python
------

```shell
# pip3 install --user jedi flake8 autopep8 yapf black rope
# pip3 install --user virtualenvwrapper
pip3 install --user yamllint
pip3 install --user grip
pip3 install --user wakatime
pip3 install --user "debugpy"
```

node
----

I prefer `nvm` to manage node.js/npm packages.

```shell
nvm which node
npm list --global --parseable --depth=0
```

```shell
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
GO111MODULE=on go get -v github.com/moorereason/mdfmt
```

See [moorereason/mdfmt: Like gofmt, but for Markdown with front matter.](https://github.com/moorereason/mdfmt)

-	Working build instructions here: [i-dotfiles/golang at main · idcrook/i-dotfiles](https://github.com/idcrook/i-dotfiles/tree/main/golang#mdfmt---markdownfmt-replacement)
