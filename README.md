Script-Fu
=========
> Script Miscellanea

## rabbitmq-multi.tcl

Start and stop multiple RabbitMQ brokers with a single command!

> Assumes ```rabbitmq-multi``` is in ```PATH```.  Alternatively, set the ```RABBITMQ_BINARY``` environment variable.

    % rabbitmq-multi start rabbit hare   # start 'rabbit' and 'hare' on localhost
    % rabbitmq-multi reset hare rabbit   # reset both nodes
    % rabbitmq-multi stop hare           # stop just 'hare'
    % rabbitmq-multi reload rabbit       # restart 'rabbit' completely using new code
    % rabbitmq-multi status hare         # check the status of 'hare'

## youtube-mp3.tcl

> Requires [youtube-dl](http://rg3.github.com/youtube-dl/), and [mplayer](http://www.mplayerhq.hu/)

    # Download and extract the MP3 from the YouTube video.  Duh.
    % youtube-mp3.sh 'http://www.youtube.com/watch?v=ZOU8GIRUd_g'

