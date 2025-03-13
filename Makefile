jsdoc:
	jsdoc -c jsdoc.json -t ./node_modules/ink-docstrap/template tinker.js

watch:
	(cd doc && browser-sync start --server --files .) &
	while inotifywait -e close_write tinker.js; do $(MAKE); done
