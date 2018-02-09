Simple VNC viewer that is built with
[Twisted-Python](https://twistedmatrix.com/trac/) and
[PyGame](http://www.pygame.org/). Originally written by
[Chris Liechti](http://homepage.hispeed.ch/py430/python/).

The viewer supports the following encodings:
  `Hextile, CoRRE, RRE, RAW, CopyRect`

The display is done using pygame because of it's good graphics
performance, but any GUI system can be used as the code is
modular and can be easily adapted. Two good examples of code
reuse are
[VNC client in browser](http://arkaitzj.wordpress.com/2011/11/12/vnc-in-your-browser-through-websockets-handled-by-gevent/)
by Arkaitz Jimenez, and most recent
[vncdotool](https://github.com/sibson/vncdotool) by Marc Sibson.
Pygame version supports clipboard transfer, but it's not used in
the sample application.

Usage
-----
You can simply start `vncviewer.py` and it will ask for a hostname
and password if required. Hostnames are in the "host:display"
form, where "display" is the VNC dispaly number.

These settings can be passed through command line, but note
it's a bad idea to pass the password in this way as it can be
snooped by other users with a simple process listing!
Try `-h` or `--help` to see supported options.

Please keep in mind that VNC transimts keypresses in cleartext,
so don't type in passwords on non-encrypted connection over
insecure networks.

With "--depth" a display depth can be gived, use "8" for
slower connections.

The "--fast" option uses only `RAW` and `CopyRect` encodings
and is thus only suitable for fast connections. But it delivers
better performance than other encodings.

What is it good for?
--------------------
Nothing ;-) Use the original VNC viewer for better performance.

However it works very well and I think with a good speed.
It could be embedded in other Python applications or it can
server as a base of various supervision or remote desktop
applications, automated tests of GUIs or be embedded in a tool
for remote support...

Bugs, etc
---------
- Properties dialog is missing. Like for specifying encodings etc.
- Listen mode not implemented.
- Key repetition does not work (pygame?)
- It does not reduce update requests when minimized.
- Screen cannot be scolled, impractical if remote desktop is larger
  than local
- The password dialog blocks twisted and everthing else

References:
-----------
- http://homepage.hispeed.ch/py430/python/
- http://code.google.com/p/vnc2flv/
- http://arkaitzj.wordpress.com/2011/11/12/vnc-in-your-browser-through-websockets-handled-by-gevent/
- http://sibson.github.io/vncdotool/
- http://www.python.org
- http://twistedmatrix.com/
- http://www.pygame.org
- http://www.realvnc.org

-------
- (c) 2003 chris <cliechti@gmx.net>
- (c) 2009 techtonik <techtonik@gmail.com>

Released under the MIT License.

You're free to use it for commercial and noncommercial
application, modify and redistribute it as long as the
copyright notices are intact. There are no warranties, not
even that it does what it says to do ;-)


Changes:
--------
2015.08.29 - expored to Github
2009.12.14 - 4. another update
 * replaced crippled_des.py with pyDes
 * TAB and BACKSPACE keys now work
2009.12.3 - 3. update
 * changed license to MIT with Chris consent as Python license
   is not supported by Google Code
 * works with twisted 8.2.0
 * works with pygame 1.9.1 (blit failed on locked surfaces)
 * don't refuse to connect to 3.7 and 3.8 VNC servers
2003.3.4 - 2. release
 * improved performance with RRE, CoRRE
 * color depth can be choosen (32, 8)
 * added "fast" option
2003.3.3 - 1. public release
