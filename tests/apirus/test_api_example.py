from apirus import APITestCase


class MyTest(APITestCase):
    def test_index(self):
        response = self.request('http://example.com')
        self.assertOk(response)

    def test_api_version(self):
        response = self.get('https://api.github.com')
        self.assertOk(response)

    def test_github_api(self):
        response = self.get('https://api.github.com')
        self.assertOk(response)
        repos = self.get('https://api.github.com/users/kennethreitz/repos')
        self.assertOk(repos)
