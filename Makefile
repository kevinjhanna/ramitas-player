deploy:
	git checkout gh-pages 
	git merge master -m "Release"
	elm-make App.elm --output=dist/app.js
	git add -f dist/app.js
	git commit -m "Release"
	git push
	git checkout master

