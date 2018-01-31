"""
Simple text input box.

havily based on inputbox.py from Timothy Downs
added 3d box style, escape -> abort function
changed event handling so that it uses the unicode
attribute -> handle localized keyboard

<cliechti@gmx.net>
"""

import pygame, pygame.font, pygame.event, pygame.draw, string
from pygame.locals import *

def get_key():
  while 1:
    event = pygame.event.poll()
    if event.type == KEYDOWN:
        return event.unicode and ord(event.unicode) or event.key
    elif event.type == QUIT:
        return K_ESCAPE
    else:
        pass

def display_box(screen, message):
  "Print a message in a box in the middle of the screen"
  fontobject = pygame.font.Font(None,18)
  #ease bg
  pygame.draw.rect(screen, (0,0,0),
                   ((screen.get_width() / 2) - 102,
                    (screen.get_height() / 2) - 10,
                    202,20), 0)
  #draw border
  pygame.draw.rect(screen, (255,255,255),
                   ((screen.get_width() / 2) - 104,
                    (screen.get_height() / 2) - 12,
                    204,24), 2)
  #3d appearance
  x = (screen.get_width() / 2) - 104
  y = (screen.get_height() / 2) - 12
  pygame.draw.line(screen, (155,155,155),
                   ((screen.get_width() / 2) - 104,
                    y+24),
                    (x+204,y+24), 2)
  pygame.draw.line(screen, (100,100,100),
                   (x+204,
                    (screen.get_height() / 2) - 12),
                    (x+204,y+24), 2)
  if len(message) != 0:
    screen.blit(fontobject.render(message, 1, (255,255,255)),
                ((screen.get_width() / 2) - 100, (screen.get_height() / 2) - 8))
  pygame.display.flip()

def ask(screen, question, default='', password=0):
    "ask(screen, question) -> answer"
    pygame.font.init()
    current_string = list(default)
    display_box(screen, question + ": " + string.join(current_string,""))
    while 1:
        inkey = get_key()
        if inkey == K_BACKSPACE:
            current_string = current_string[0:-1]
        elif inkey == K_RETURN:
            break
        elif inkey == K_ESCAPE:
            return default
        #~ elif inkey == K_MINUS:
          #~ current_string.append("_")
        elif inkey <= 127:
            current_string.append(chr(inkey))
        if password:
            display_box(screen, question + ": " + "*"*len(current_string))
        else:
            display_box(screen, question + ": " + string.join(current_string,""))
    return string.join(current_string,"")

def main():
  screen = pygame.display.set_mode((220,40))
  screen.fill((0,100,255))
  print ask(screen, "Name") + " was entered"

if __name__ == '__main__': main()
