import logging
import os

from bzt.utils import MultiPartForm
from tests import BZTestCase, __dir__


class TestMultiPartForm(BZTestCase):
    def test___init__(self):
        body = MultiPartForm()

        additional_files = os.listdir(__dir__() + "/data")

        for extra_file in additional_files:
            extra_file = __dir__() + u"/data/" + extra_file
            with open(os.path.expanduser(extra_file)) as fd:
                fname = os.path.basename(extra_file)
                body.add_file_as_string(u"file_%s" % extra_file, fname, fd.read())

        txt = str(body)
        logging.debug("%s", len(txt))