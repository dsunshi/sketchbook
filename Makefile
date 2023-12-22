

img/evergreen.png: build/scad/evergreen.scad
	./depends/scad2png build/scad/evergreen.scad img/evergreen.png

build/scad/evergreen.scad: src/Tree.hs
	cd src && runghc Tree.hs ../build/scad/evergreen.scad
