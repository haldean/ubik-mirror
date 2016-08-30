# Builds website materials. Requires:
#	python
#	svgwrite python module (pip install svgwrite)
#   imagemagick

all: logo.svg favicon.png

logo.svg: logo.py
	python logo.py logo

favicon.svg: logo.py
	python logo.py favicon

favicon.png: favicon.svg
	inkscape -z -e favicon.png -w 200 -h 200 favicon.svg
