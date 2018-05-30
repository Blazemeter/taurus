import logging

import pygame
from twisted.internet import reactor
from vncdotool import rfb


class VNCViewer(object):
    def __init__(self, label):
        pygame.display.set_caption(label)
        self.alive = 1
        self.loopcounter = 0
        self.sprites = pygame.sprite.RenderUpdates()
        self.buttons = 0
        self.protocol = None

    def setProtocol(self, protocol):
        """attach a protocol instance to post the events to"""
        self.protocol = protocol

    def mainloop(self):
        if self.alive:
            reactor.callLater(0.020, self.mainloop)


class RFBToGUI(rfb.RFBClient, object):
    """RFBClient protocol that talks to the GUI app"""

    def _handleDecodeZRLE(self, block):
        raise NotImplementedError()

    def vncConnectionMade(self):
        """choose appropriate color depth, resize screen"""
        self.remoteframebuffer = self.factory.remoteframebuffer
        self.screen = pygame.display.set_mode((self.width, self.height))
        self.remoteframebuffer.setProtocol(self)
        self.setEncodings([
            rfb.COPY_RECTANGLE_ENCODING,
            rfb.HEXTILE_ENCODING,
            rfb.CORRE_ENCODING,
            rfb.RRE_ENCODING,
            rfb.RAW_ENCODING,
        ])
        self.setPixelFormat()
        self.framebufferUpdateRequest()

    def vncRequestPassword(self):
        if self.factory.password is not None:
            self.sendPassword(self.factory.password)
        else:
            raise ValueError("Password required")

    def commitUpdate(self, rectangles=None):
        pygame.display.update(rectangles)
        self.framebufferUpdateRequest(incremental=1)

    def updateRectangle(self, x, y, width, height, data):
        self.screen.blit(
            pygame.image.fromstring(data, (width, height), 'RGBX'),  # TODO color format
            (x, y)
        )


class VNCFactory(rfb.RFBFactory, object):
    """A factory for remote frame buffer connections."""

    def __init__(self, remoteframebuffer,*args, **kwargs):
        rfb.RFBFactory.__init__(self, *args, **kwargs)
        self.remoteframebuffer = remoteframebuffer
        self.protocol = RFBToGUI

    def buildProtocol(self, addr):
        display = addr.port - 5900
        return rfb.RFBFactory.buildProtocol(self, addr)

    def clientConnectionLost(self, connector, reason):
        logging.warning("connection lost: %r" % reason.getErrorMessage())
        reactor.stop()

    def clientConnectionFailed(self, connector, reason):
        logging.warning("cannot connect to server: %r\n" % reason.getErrorMessage())
        reactor.stop()


def main(host, password, label, display=0):
    pygame.init()
    remoteframebuffer = VNCViewer(label)

    # connect to this host and port, and reconnect if we get disconnected
    reactor.connectTCP(
        host,  # remote hostname
        display + 5900,  # TCP port number
        VNCFactory(
            remoteframebuffer,  # the application/display
            password,  # password or none
        )
    )

    # run the application
    reactor.callLater(0.1, remoteframebuffer.mainloop)
    reactor.run()


if __name__ == '__main__':
    main("localhost", "secret", "demo", 1)
