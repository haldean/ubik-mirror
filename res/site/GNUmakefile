# Builds website materials. Requires:
#	python
#	svgwrite python module (pip install svgwrite)
#   imagemagick

all: logo.svg favicon.png

logo.svg: logo.py
	python logo.py

favicon.png: logo.svg
	convert logo.svg favicon.png
