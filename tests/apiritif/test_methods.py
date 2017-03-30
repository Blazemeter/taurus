import apiritif


class TestRequests(apiritif.APITestCase):
    def setUp(self):
        super(TestRequests, self).setUp()
        self.keep_alive = True
        
    def test_get(self):
        self.get('http://blazedemo.com/?tag=get')

    def test_post(self):
        self.post('http://blazedemo.com/?tag=post')
        
    def test_put(self):
        self.put('http://blazedemo.com/?tag=put')
        
    def test_patch(self):
        self.patch('http://blazedemo.com/?tag=patch')
        
    def test_head(self):
        self.head('http://blazedemo.com/?tag=head')
        
    def test_delete(self):
        self.delete('http://blazedemo.com/?tag=delete')
        
