#!/bin/bash

#############
# FUNCTIONS
#############

function h1 {
	echo
	echo "###################"
	echo "## $1"
	echo "###################"
	echo
}


############
# GLOBAL
############

WORKDIR=".youtube/"


#######
# MAIN
#######

h1 "Preliminary"

if [ ! -d "$WORKDIR" ]; then
	echo "$WORKDIR not found; creating..."
	mkdir "$WORKDIR"
fi

if [ -z "$1" ]; then
	echo -n "YouTube link: "
	read url
else
	url="$1"
	echo "Downloading $1..."
fi

filename=youtube # final output filename without extension

if [ ! -n "$url" ]; then
	echo "Link is empty; assuming you meant Never Gonna Give You Up"
	url="http://www.youtube.com/watch?v=ZOU8GIRUd_g"
else
	echo -n "Title: "
	read title

	filename="$title" # output will be named <title>.mp3

	if [ ! -n "$title" ]; then
		echo "No title given; defaulting to youtube"
		title="youtube"
	else
		echo -n "Artist: "
		read artist

		if [ -n "$artist" ]; then
			filename="$artist - $title" # output will be named <artist> - <title>.mp3
		fi

		echo -n "Album: "
		read album

		echo -n "Year: "
		read year
	fi
fi

h1 "Downloading"
youtube-dl "$url" -o "$WORKDIR/youtube" | tee -a "$WORKDIR/log"


h1 "Extracting sound"
mplayer -vo null -ao pcm:file="$WORKDIR/youtube.wav" "$WORKDIR/youtube" | tee -a "$WORKDIR/log"

h1 "Making mp3"
lame --replaygain-accurate -q 0 "$WORKDIR/youtube.wav" -o "$WORKDIR/$filename.mp3" --tt "$title" --ta "$artist" --tl "$album" --ty "$year" | tee -a "$WORKDIR/log"

h1 "Finishing"
rm "$WORKDIR/youtube" "$WORKDIR/youtube.wav"
mv "$WORKDIR/$filename.mp3" "$filename.mp3"

echo
echo "Finished downloading $filename.mp3"
