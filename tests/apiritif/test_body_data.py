import apiritif


class TestRequests(apiritif.APITestCase):
    def setUp(self):
        super(TestRequests, self).setUp()
        
    def test_body_string(self):
        self.get('http://blazedemo.com/', data='MY PERFECT BODY')
        
    def test_body_json(self):
        self.get('http://blazedemo.com/', json={'foo': 'bar'})

    def test_url_params(self):
        self.get('http://blazedemo.com/', params={'foo': 'bar'})

