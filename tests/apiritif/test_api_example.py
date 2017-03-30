from apiritif import APITestCase


class MyTest(APITestCase):
    def test_index(self):
        response = self.request('http://example.com')
        self.assertOk(response)

    def test_api_version(self):
        response = self.get('https://api.github.com', headers={'user-agent': 'me'})
        self.assertOk(response)

    def test_github_api(self):
        response = self.get('https://api.github.com', headers={'user-agent': 'me'})
        self.assertOk(response)
        repos = self.get('https://api.github.com/users/kennethreitz/repos', headers={'user-agent': 'me'})
        self.assertOk(repos)
