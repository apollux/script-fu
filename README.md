Script-Fu
=========
> Script Miscellanea

## heartbeater

Poll a web address as a way of announcing your presence to the world.
Note that this spawns a background daemon.

    ./heartbeater

## ig

Grab a bunch of RSS feeds and create a basic webpage with them.  This is meant to replace the now-moribund `iGoogle`.

    ./ig

## tcpdump.sh

> Poorman's tcpdump: an intercepting transparent proxy with a pipe and two netcats

    ./tcpdump.sh 8080 www3.imperial.ac.uk 80

Point your browser at [localhost:8080](http://localhost:8080).  Two
files will be created: `incoming.dump` and `outgoing.dump`,
representing the incoming and outgoing data transferred respectively.

## permamake.sh

Run `make` when watched files change.

## supervise.tcl

Start a program and supervise it (i.e. restart it if it fails within
certain bounds).

    % # Run offlineimap.  Restart it if it closes with a non-zero exit, at most 5 times in 1 second.
    % supervise.tcl -maxr 5 -maxt 1 /src/offlineimap/offlineimap.py -u Curses.Blinkenlights

## rabbitmq-multi.tcl

Start and stop multiple RabbitMQ brokers with a single command!

> Assumes ```rabbitmq-multi``` is in ```PATH```.  Alternatively, set
  the ```RABBITMQ_BINARY``` environment variable.

    % rabbitmq-multi start rabbit hare   # start 'rabbit' and 'hare' on localhost
    % rabbitmq-multi reset hare rabbit   # reset both nodes
    % rabbitmq-multi stop hare           # stop just 'hare'
    % rabbitmq-multi reload rabbit       # restart 'rabbit' completely using new code
    % rabbitmq-multi status hare         # check the status of 'hare'

## youtube-mp3.sh

> Requires [youtube-dl](http://rg3.github.com/youtube-dl/), and
  [mplayer](http://www.mplayerhq.hu/)

    # Download and extract the MP3 from the YouTube video.  Duh.
    % youtube-mp3.sh 'http://www.youtube.com/watch?v=ZOU8GIRUd_g'

## cat

If the only argument is a directory ```ls``` it, otherwise, behaves
exactly like ```cat```.

## fromTo.py

A Tkinter UI for copying files from damaged media such as scratched
DVDs.  It's particularly useful on Windows where Explorer refuses to
copy a file if any block in it is unreadable; when this happens, this
script the corresponding 1Kb block with zeros.

## cloudfiles-sh.py

A shell for interacting with Rackspace
[CloudFiles](http://www.rackspace.com/cloud/cloud_hosting_products/files/).
This is effectively a REPL that provides basic file operations
(e.g. `ls`, `info`, `copy`, `remove`).

## sm.py

A clone of the `sm` screen-typing program.
