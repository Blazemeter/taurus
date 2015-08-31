from net.grinder.script import Test
from net.grinder.plugin.http import HTTPRequest
 
test = Test(1, "BZT Requests")
request = HTTPRequest()
test.record(request)
 
class TestRunner(object):
    def __call__(self):
