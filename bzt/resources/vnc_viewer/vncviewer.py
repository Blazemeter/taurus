#!/usr/bin/env python
"""
Python VNC Viewer
PyGame version
(C) 2003 <cliechti@gmx.net>

MIT License
"""

# twisted modules
from twisted.python import usage, log
from twisted.internet import reactor, protocol
# ~ from twisted.internet import defer
from twisted.internet.protocol import Factory, Protocol

# import pygame stuff
import pygame
from pygame.locals import *

# std stuff
import sys, struct

# local
import rfb
import inputbox

POINTER = tuple([(8, 8), (4, 4)] + list(pygame.cursors.compile((
    # 01234567
    "        ",  # 0
    "        ",  # 1
    "        ",  # 2
    "   .X.  ",  # 3
    "   X.X  ",  # 4
    "   .X.  ",  # 5
    "        ",  # 6
    "        ",  # 7
), 'X', '.')))

# keyboard mappings pygame -> vnc
KEYMAPPINGS = {
    K_BACKSPACE: rfb.KEY_BackSpace,
    K_TAB: rfb.KEY_Tab,
    K_RETURN: rfb.KEY_Return,
    K_ESCAPE: rfb.KEY_Escape,
    K_KP0: rfb.KEY_KP_0,
    K_KP1: rfb.KEY_KP_1,
    K_KP2: rfb.KEY_KP_2,
    K_KP3: rfb.KEY_KP_3,
    K_KP4: rfb.KEY_KP_4,
    K_KP5: rfb.KEY_KP_5,
    K_KP6: rfb.KEY_KP_6,
    K_KP7: rfb.KEY_KP_7,
    K_KP8: rfb.KEY_KP_8,
    K_KP9: rfb.KEY_KP_9,
    K_KP_ENTER: rfb.KEY_KP_Enter,
    K_UP: rfb.KEY_Up,
    K_DOWN: rfb.KEY_Down,
    K_RIGHT: rfb.KEY_Right,
    K_LEFT: rfb.KEY_Left,
    K_INSERT: rfb.KEY_Insert,
    K_DELETE: rfb.KEY_Delete,
    K_HOME: rfb.KEY_Home,
    K_END: rfb.KEY_End,
    K_PAGEUP: rfb.KEY_PageUp,
    K_PAGEDOWN: rfb.KEY_PageDown,
    K_F1: rfb.KEY_F1,
    K_F2: rfb.KEY_F2,
    K_F3: rfb.KEY_F3,
    K_F4: rfb.KEY_F4,
    K_F5: rfb.KEY_F5,
    K_F6: rfb.KEY_F6,
    K_F7: rfb.KEY_F7,
    K_F8: rfb.KEY_F8,
    K_F9: rfb.KEY_F9,
    K_F10: rfb.KEY_F10,
    K_F11: rfb.KEY_F11,
    K_F12: rfb.KEY_F12,
    K_F13: rfb.KEY_F13,
    K_F14: rfb.KEY_F14,
    K_F15: rfb.KEY_F15,
}

MODIFIERS = {
    K_NUMLOCK: rfb.KEY_Num_Lock,
    K_CAPSLOCK: rfb.KEY_Caps_Lock,
    K_SCROLLOCK: rfb.KEY_Scroll_Lock,
    K_RSHIFT: rfb.KEY_ShiftRight,
    K_LSHIFT: rfb.KEY_ShiftLeft,
    K_RCTRL: rfb.KEY_ControlRight,
    K_LCTRL: rfb.KEY_ControlLeft,
    K_RALT: rfb.KEY_AltRight,
    K_LALT: rfb.KEY_AltLeft,
    K_RMETA: rfb.KEY_MetaRight,
    K_LMETA: rfb.KEY_MetaLeft,
    K_LSUPER: rfb.KEY_Super_L,
    K_RSUPER: rfb.KEY_Super_R,
    K_MODE: rfb.KEY_Hyper_R,  # ???
    # ~ K_HELP:             rfb.
    # ~ K_PRINT:            rfb.
    K_SYSREQ: rfb.KEY_Sys_Req,
    K_BREAK: rfb.KEY_Pause,  # ???
    K_MENU: rfb.KEY_Hyper_L,  # ???
    # ~ K_POWER:            rfb.
    # ~ K_EURO:             rfb.
}


class TextSprite(pygame.sprite.Sprite):
    """a text label"""
    SIZE = 20

    def __init__(self, pos, color=(255, 0, 0, 120)):
        self.pos = pos
        # self.containers = containers
        # pygame.sprite.Sprite.__init__(self, self.containers)
        pygame.sprite.Sprite.__init__(self)
        self.font = pygame.font.Font(None, self.SIZE)
        self.lastmsg = None
        self.update()
        self.rect = self.image.get_rect().move(pos)

    def update(self, msg=' '):
        if msg != self.lastmsg:
            self.lastscore = msg
            self.image = self.font.render(msg, 0, (255, 255, 255))


# ~ class PyGameApp(pb.Referenceable, Game.Game):
class PyGameApp:
    """Pygame main application"""

    def __init__(self):
        width, height = 640, 480
        self.setRFBSize(width, height)
        pygame.display.set_caption('Python VNC Viewer')
        pygame.mouse.set_cursor(*POINTER)
        pygame.key.set_repeat(500, 30)
        self.clock = pygame.time.Clock()
        self.alive = 1
        self.loopcounter = 0
        self.sprites = pygame.sprite.RenderUpdates()
        self.statustext = TextSprite((5, 0))
        self.sprites.add(self.statustext)
        self.buttons = 0
        self.protocol = None

    def setRFBSize(self, width, height, depth=32):
        """change screen size"""
        self.width, self.height = width, height
        self.area = Rect(0, 0, width, height)
        winstyle = pygame.RESIZABLE  # 0  # |FULLSCREEN
        if depth == 32:
            self.screen = pygame.display.set_mode(self.area.size, winstyle, 32)
        elif depth == 8:
            self.screen = pygame.display.set_mode(self.area.size, winstyle, 8)
            # default palette is perfect ;-)
            # ~ pygame.display.set_palette([(x,x,x) for x in range(256)])
        # ~ elif depth is None:
        # ~ bestdepth = pygame.display.mode_ok((width, height), winstyle, 32)
        # ~ print "bestdepth %r" % bestdepth
        # ~ self.screen = pygame.display.set_mode(self.area.size, winstyle, best)
        # then communicate that to the protocol...
        else:
            # ~ self.screen = pygame.display.set_mode(self.area.size, winstyle, depth)
            raise ValueError("color depth not supported")
        self.background = pygame.Surface((self.width, self.height), depth)
        self.background.fill(0)  # black

    def setProtocol(self, protocol):
        """attach a protocol instance to post the events to"""
        self.protocol = protocol

    def checkEvents(self):
        """process events from the queue"""
        seen_events = 0
        for e in pygame.event.get():
            seen_events = 1
            # ~ print e
            if e.type == QUIT:
                self.alive = 0
                reactor.stop()
            # ~ elif e.type == KEYUP and e.key == K_ESCAPE:
            # ~ self.alive = 0
            # ~ reactor.stop()
            elif e.type == pygame.VIDEORESIZE:
                # The main code that resizes the window:
                # (recreate the window with the new size)
                surface = pygame.display.set_mode((e.w, e.h),
                                                  pygame.RESIZABLE)
            if self.protocol is not None:
                if 1 == 2:  # TODO: Disable interaction
                    if e.type == KEYDOWN:
                        if e.key in MODIFIERS:
                            self.protocol.keyEvent(MODIFIERS[e.key], down=1)
                        elif e.key in KEYMAPPINGS:
                            self.protocol.keyEvent(KEYMAPPINGS[e.key])
                        elif e.unicode:
                            self.protocol.keyEvent(ord(e.unicode))
                        else:
                            print("warning: unknown key %r" % e)
                    elif e.type == KEYUP:
                        if e.key in MODIFIERS:
                            self.protocol.keyEvent(MODIFIERS[e.key], down=0)
                        # ~ else:
                        # ~ print "unknown key %r" % (e)
                    elif e.type == MOUSEMOTION:
                        self.buttons = e.buttons[0] and 1
                        self.buttons |= e.buttons[1] and 2
                        self.buttons |= e.buttons[2] and 4
                        self.protocol.pointerEvent(e.pos[0], e.pos[1], self.buttons)
                        # ~ print e.pos
                    elif e.type == MOUSEBUTTONUP:
                        if e.button == 1: self.buttons &= ~1
                        if e.button == 2: self.buttons &= ~2
                        if e.button == 3: self.buttons &= ~4
                        if e.button == 4: self.buttons &= ~8
                        if e.button == 5: self.buttons &= ~16
                        self.protocol.pointerEvent(e.pos[0], e.pos[1], self.buttons)
                    elif e.type == MOUSEBUTTONDOWN:
                        if e.button == 1: self.buttons |= 1
                        if e.button == 2: self.buttons |= 2
                        if e.button == 3: self.buttons |= 4
                        if e.button == 4: self.buttons |= 8
                        if e.button == 5: self.buttons |= 16
                        self.protocol.pointerEvent(e.pos[0], e.pos[1], self.buttons)
            return not seen_events
        return not seen_events

    def mainloop(self, dum=None):
        """gui 'mainloop', it is called repeated by twisteds mainloop 
           by using callLater"""
        # ~ self.clock.tick()
        no_work = self.checkEvents()

        # ~ self.sprites.clear(self.screen, self.background)
        # ~ dirty = self.sprites.draw(self.screen)
        # ~ pygame.display.update(dirty)

        # ~ self.statustext.update("iteration %d" % self.loopcounter)
        # ~ self.loopcounter += 1

        # ~ pygame.display.flip()

        if self.alive:
            # ~ d = defer.Deferred()
            # ~ d.addCallback(self.mainloop)
            # ~ d.callback(None)
            reactor.callLater(no_work and 0.020, self.mainloop)

    # ~ def error(self):
    # ~ log.msg('error, stopping reactor')
    # ~ reactor.stop()


class RFBToGUI(rfb.RFBClient):
    """RFBClient protocol that talks to the GUI app"""

    def vncConnectionMade(self):
        """choose appropriate color depth, resize screen"""
        # ~ print "Screen format: depth=%d bytes_per_pixel=%r" % (self.depth, self.bpp)
        # ~ print "Desktop name: %r" % self.name

        # ~ print "redmax=%r, greenmax=%r, bluemax=%r" % (self.redmax, self.greenmax, self.bluemax)
        # ~ print "redshift=%r, greenshift=%r, blueshift=%r" % (self.redshift, self.greenshift, self.blueshift)

        self.remoteframebuffer = self.factory.remoteframebuffer
        self.screen = self.remoteframebuffer.screen
        self.remoteframebuffer.setProtocol(self)
        self.remoteframebuffer.setRFBSize(self.width, self.height, 32)
        self.setEncodings(self.factory.encodings)
        self.setPixelFormat()  # set up pixel format to 32 bits
        self.framebufferUpdateRequest()  # request initial screen update

    def vncRequestPassword(self):
        if self.factory.password is not None:
            self.sendPassword(self.factory.password)
        else:
            # XXX hack, this is blocking twisted!!!!!!!
            screen = pygame.display.set_mode((220, 40))
            screen.fill((255, 100, 0))  # redish bg
            self.sendPassword(inputbox.ask(screen, "Password", password=1))

    # ~ def beginUpdate(self):
    # ~ """start with a new series of display updates"""

    def beginUpdate(self):
        """begin series of display updates"""
        # ~ log.msg("screen lock")

    def commitUpdate(self, rectangles=None):
        """finish series of display updates"""
        # ~ log.msg("screen unlock")
        pygame.display.update(rectangles)
        self.framebufferUpdateRequest(incremental=1)

    def updateRectangle(self, x, y, width, height, data):
        """new bitmap data"""
        # ~ print "%s " * 5 % (x, y, width, height, len(data))
        # ~ log.msg("screen update")
        self.screen.blit(
            pygame.image.fromstring(data, (width, height), 'RGBX'),  # TODO color format
            (x, y)
        )

    def copyRectangle(self, srcx, srcy, x, y, width, height):
        """copy src rectangle -> destinantion"""
        # ~ print "copyrect", (srcx, srcy, x, y, width, height)
        self.screen.blit(self.screen,
                         (x, y),
                         (srcx, srcy, width, height)
                         )

    def fillRectangle(self, x, y, width, height, color):
        """fill rectangle with one color"""
        # ~ remoteframebuffer.CopyRect(srcx, srcy, x, y, width, height)
        self.screen.fill(struct.unpack("BBBB", color), (x, y, width, height))

    def bell(self):
        print("katsching")

    def copy_text(self, text):
        print("Clipboard: %r" % text)


# use a derrived class for other depths. hopefully with better performance
# that a single class with complicated/dynamic color conversion.
class RFBToGUIeightbits(RFBToGUI):
    def vncConnectionMade(self):
        """choose appropriate color depth, resize screen"""
        self.remoteframebuffer = self.factory.remoteframebuffer
        self.screen = self.remoteframebuffer.screen
        self.remoteframebuffer.setProtocol(self)
        self.remoteframebuffer.setRFBSize(self.width, self.height, 8)
        self.setEncodings(self.factory.encodings)
        self.setPixelFormat(bpp=8, depth=8, bigendian=0, truecolor=1,
                            redmax=7, greenmax=7, bluemax=3,
                            redshift=5, greenshift=2, blueshift=0
                            )
        self.palette = self.screen.get_palette()
        self.framebufferUpdateRequest()

    def updateRectangle(self, x, y, width, height, data):
        """new bitmap data"""
        # ~ print "%s " * 5 % (x, y, width, height, len(data))
        # ~ assert len(data) == width*height
        bmp = pygame.image.fromstring(data, (width, height), 'P')
        bmp.set_palette(self.palette)
        self.screen.blit(bmp, (x, y))

    def fillRectangle(self, x, y, width, height, color):
        """fill rectangle with one color"""
        self.screen.fill(ord(color), (x, y, width, height))


class VNCFactory(rfb.RFBFactory):
    """A factory for remote frame buffer connections."""

    def __init__(self, remoteframebuffer, depth, fast, *args, **kwargs):
        rfb.RFBFactory.__init__(self, *args, **kwargs)
        self.remoteframebuffer = remoteframebuffer
        if depth == 32:
            self.protocol = RFBToGUI
        elif depth == 8:
            self.protocol = RFBToGUIeightbits
        else:
            raise ValueError("color depth not supported")

        if fast:
            self.encodings = [
                rfb.COPY_RECTANGLE_ENCODING,
                rfb.RAW_ENCODING,
            ]
        else:
            self.encodings = [
                rfb.COPY_RECTANGLE_ENCODING,
                rfb.HEXTILE_ENCODING,
                rfb.CORRE_ENCODING,
                rfb.RRE_ENCODING,
                rfb.RAW_ENCODING,
            ]

    def buildProtocol(self, addr):
        display = addr.port - 5900
        pygame.display.set_caption('Python VNC Viewer on %s:%s' % (addr.host, display))
        return rfb.RFBFactory.buildProtocol(self, addr)

    def clientConnectionLost(self, connector, reason):
        log.msg("connection lost: %r" % reason.getErrorMessage())
        reactor.stop()

    def clientConnectionFailed(self, connector, reason):
        log.msg("cannot connect to server: %r\n" % reason.getErrorMessage())
        reactor.stop()


class Options(usage.Options):
    optParameters = [
        ['display', 'd', '0', 'VNC display'],
        ['host', 'h', None, 'remote hostname'],
        ['outfile', 'o', None, 'Logfile [default: sys.stdout]'],
        ['password', 'p', None, 'VNC password'],
        ['depth', 'D', '32', 'Color depth'],
    ]
    optFlags = [
        ['shared', 's', 'Request shared session'],
        ['fast', 'f', 'Fast connection is used'],
    ]


# ~ def eventcollector():
# ~ while remoteframebuffer.alive:
# ~ pygame.event.pump()
# ~ e = pygame.event.poll()
# ~ if e.type != NOEVENT:
# ~ print e
# ~ reactor.callFromThread(remoteframebuffer.processEvent, e)
# ~ print 'xxxxxxxxxxxxx'


class VncViewer(object):

    def __init__(self, host, port, password, depth=32):
        pygame.init()
        remoteframebuffer = PyGameApp()

        reactor.connectTCP(
            host,  # remote hostname
            port,  # TCP port number
            VNCFactory(
                remoteframebuffer,  # the application/display
                depth,  # color depth
                False,  # if a fast connection is used
                password,  # password or none
                1,  # shared session flag
            )
        )

        # run the application
        reactor.callLater(0.1, remoteframebuffer.mainloop)
        reactor.run()


def main():
    o = Options()
    try:
        o.parseOptions()
    except usage.UsageError, errortext:
        print("%s: %s" % (sys.argv[0], errortext))
        print("%s: Try --help for usage details." % (sys.argv[0]))
        raise SystemExit(1)

    depth = int(o.opts['depth'])

    logFile = sys.stdout
    if o.opts['outfile']:
        logFile = o.opts['outfile']
    log.startLogging(logFile)

    pygame.init()
    remoteframebuffer = PyGameApp()

    # ~ from twisted.python import threadable
    # ~ threadable.init()
    # ~ reactor.callInThread(eventcollector)

    host = o.opts['host']
    display = int(o.opts['display'])
    if host is None:
        screen = pygame.display.set_mode((220, 40))
        screen.fill((0, 100, 255))  # blue bg
        host = inputbox.ask(screen, "Host")
        if host == '':
            raise SystemExit
        if ':' in host:
            host, display = host.split(':')
            if host == '':  host = 'localhost'
            display = int(display)

    # connect to this host and port, and reconnect if we get disconnected
    reactor.connectTCP(
        host,  # remote hostname
        display + 5900,  # TCP port number
        VNCFactory(
            remoteframebuffer,  # the application/display
            depth,  # color depth
            o.opts['fast'],  # if a fast connection is used
            o.opts['password'],  # password or none
            int(o.opts['shared']),  # shared session flag
        )
    )

    # run the application
    reactor.callLater(0.1, remoteframebuffer.mainloop)
    reactor.run()


if __name__ == '__main__':
    main()
