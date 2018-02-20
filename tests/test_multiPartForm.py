import logging
import os

from bzt.utils import MultiPartForm
from tests import BZTestCase, RESOURCES_DIR


class TestMultiPartForm(BZTestCase):
    def test___init__(self):
        body = MultiPartForm()

        additional_files = os.listdir(RESOURCES_DIR)

        for extra_file in additional_files:
            extra_file = RESOURCES_DIR + extra_file
            if os.path.isdir(extra_file):
                continue
            with open(os.path.expanduser(extra_file), 'rb') as fd:
                file_data = fd.read()

            fname = os.path.basename(extra_file)
            encoded = "file_%s" % extra_file
            body.add_file_as_string(encoded, fname, file_data)

        txt = body.form_as_bytes()
        logging.debug("%s", len(txt))